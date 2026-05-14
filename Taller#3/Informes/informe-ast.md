# Informe AST y Traza de Evaluación

**Taller 3 — Fundamentos de Lenguajes de Programación 2026-1**  
**Universidad del Valle**

**Autores:**
- JHORMAN RICARDO LOAIZA — 2359710
- JUAN DIEGO OSPINA — 2359486
- MAURICIO ALEJANDRO ROJAS — 2359701
- JUAN FELIPE RUIZ — 2359397

---

## 4.1 Árbol de Sintaxis Abstracta (AST)

Expresión analizada:

```
var saldo : int = 100
in
let abonar : (int -> void) =
  proc(int monto) set saldo := +(saldo, monto) end
in
begin
  (abonar 50);
  (abonar 25);
  freeze saldo;
  saldo
end
```

```mermaid
flowchart TD
    A["var-exp\n ids=['saldo']\n tipos=[int-type-exp]\n rands=[lit-exp(100)]\n body=..."]
    A --> B["lit-exp(100)"]
    A --> C["let-exp\n ids=['abonar']\n tipos=[(int→void)-type-exp]\n rands=[proc-exp]\n body=begin-exp"]

    C --> D["proc-exp\n tipos=[int-type-exp]\n ids=['monto']\n body=set-exp"]
    D --> E["set-exp\n id='saldo'\n rand=prim-exp"]
    E --> F["prim-exp  +(...)"]
    F --> G["ident-exp('saldo')"]
    F --> H["ident-exp('monto')"]

    C --> I["begin-exp\n first=app-exp\n rest=[app-exp, freeze-exp, ident-exp]"]
    I --> J["app-exp\n rator=ident-exp('abonar')\n rands=[lit-exp(50)]"]
    J --> J1["ident-exp('abonar')"]
    J --> J2["lit-exp(50)"]

    I --> K["app-exp\n rator=ident-exp('abonar')\n rands=[lit-exp(25)]"]
    K --> K1["ident-exp('abonar')"]
    K --> K2["lit-exp(25)"]

    I --> L["freeze-exp\n id='saldo'"]
    I --> M["ident-exp('saldo')"]
```

---

## 4.2 Traza de Evaluación con Cadena de Ambientes

### Ambiente inicial (antes de evaluar la expresión)

El ambiente inicial predefinido contiene:

| Identificador | Referencia | Valor | Marca |
|:---:|:---:|:---:|:---:|
| `x` | ref₀ | 4 | `'let` |
| `y` | ref₁ | 2 | `'let` |
| `z` | ref₂ | 5 | `'let` |
| `a` | ref₃ | 4 | `'var` |
| `b` | ref₄ | 5 | `'var` |
| `c` | ref₅ | 6 | `'var` |

---

### Paso 1: Evaluar `var-exp`

**Expresión evaluada:**
```
var saldo : int = 100 in ...
```

**Ambiente:** $\Gamma_0 = \{x \mapsto \text{ref}_0, y \mapsto \text{ref}_1, z \mapsto \text{ref}_2, a \mapsto \text{ref}_3, b \mapsto \text{ref}_4, c \mapsto \text{ref}_5\}$

**Acción:** Se evalúa `100` → resultado `100`. Se crea nueva celda en el store:

| Referencia | Valor | Marca |
|:---:|:---:|:---:|
| ref₆ | 100 | `'var` |

Se extiende el ambiente:

$$\Gamma_1 = \Gamma_0[saldo \mapsto \text{ref}_6]$$

**Resultado del paso:** Continúa evaluando el cuerpo en $\Gamma_1$.

---

### Paso 2: Evaluar `let-exp` (ligadura `abonar`)

**Expresión evaluada:**
```
let abonar : (int -> void) = proc(int monto) set saldo := +(saldo, monto) end in ...
```

**Ambiente:** $\Gamma_1$

**Acción:** Se evalúa `proc(int monto) set saldo := +(saldo, monto) end` en $\Gamma_1$.  
Esto construye una clausura capturando $\Gamma_1$:

$$\text{procval}(\text{ids}=[\texttt{monto}],\ \text{body}=\texttt{set-exp},\ \text{env}=\Gamma_1)$$

Se crea la referencia (inmutable, tipo `'let`):

| Referencia | Valor | Marca |
|:---:|:---:|:---:|
| ref₇ | procval(monto, set-exp, Γ₁) | `'let` |

$$\Gamma_2 = \Gamma_1[abonar \mapsto \text{ref}_7]$$

**Resultado del paso:** Continúa evaluando `begin-exp` en $\Gamma_2$.

---

### Paso 3: Evaluar `begin-exp` — Primera aplicación `(abonar 50)`

**Expresión evaluada:**
```
(abonar 50)
```

**Ambiente:** $\Gamma_2$

**Acción:**
1. Se evalúa `abonar` → `deref(ref₇)` = procval con cuerpo `set saldo := +(saldo, monto)`, env = $\Gamma_1$.
2. Se evalúa `50` → `50`.
3. Se crea referencia `'let` para el parámetro:

| Referencia | Valor | Marca |
|:---:|:---:|:---:|
| ref₈ | 50 | `'let` |

$$\Gamma_3 = \Gamma_1[monto \mapsto \text{ref}_8]$$

4. Se evalúa el cuerpo `set saldo := +(saldo, monto)` en $\Gamma_3$:
   - `deref(apply-env-ref(Γ₃, saldo))` = `deref(ref₆)` = 100
   - `deref(apply-env-ref(Γ₃, monto))` = `deref(ref₈)` = 50
   - `+(100, 50)` = 150
   - `setref!(ref₆, 150)` — se actualiza el store

**Estado del store después del paso 3:**

| Referencia | Valor | Marca |
|:---:|:---:|:---:|
| ref₆ | **150** | `'var` |
| ref₇ | procval(...) | `'let` |
| ref₈ | 50 | `'let` |

**Resultado:** `void`

---

### Paso 4: Segunda aplicación `(abonar 25)`

**Expresión evaluada:**
```
(abonar 25)
```

**Ambiente:** $\Gamma_2$

**Acción:** Igual al paso anterior; se crea ref₉ para `monto=25`:

| Referencia | Valor | Marca |
|:---:|:---:|:---:|
| ref₉ | 25 | `'let` |

En el cuerpo: `+(150, 25)` = 175 → `setref!(ref₆, 175)`.

**Estado del store después del paso 4:**

| Referencia | Valor | Marca |
|:---:|:---:|:---:|
| ref₆ | **175** | `'var` |

**Resultado:** `void`

---

### Paso 5: `freeze saldo`

**Expresión evaluada:**
```
freeze saldo
```

**Ambiente:** $\Gamma_2$

**Acción:**
- `apply-env-ref(Γ₂, saldo)` = ref₆
- `deref-mark(ref₆)` = `'var` ✓
- `setref-mark!(ref₆, 'frozen)`

**Estado del store después del paso 5:**

| Referencia | Valor | Marca |
|:---:|:---:|:---:|
| ref₆ | 175 | **`'frozen`** |

**Resultado:** `void`

---

### Paso 6: Evaluar `saldo` (última expresión del `begin`)

**Expresión evaluada:** `saldo`

**Ambiente:** $\Gamma_2$

**Acción:** `apply-env(Γ₂, saldo)` = `deref(ref₆)` = **175**

**Resultado final del programa:** **175**

---

### Diagrama de cadena de ambientes al evaluar el cuerpo de `abonar` (primera invocación)

```mermaid
flowchart BT
    ENV0["∅  (ambiente-vacio)"]
    ENV1["Γ₀\n x→ref₀[4,'let]\n y→ref₁[2,'let]\n z→ref₂[5,'let]\n a→ref₃[4,'var]\n b→ref₄[5,'var]\n c→ref₅[6,'var]"]
    ENV2["Γ₁\n saldo→ref₆[175,'var→frozen]"]
    ENV3["Γ₂\n abonar→ref₇[procval,'let]"]
    ENV4["Γ₃  (cuerpo de abonar)\n monto→ref₈[50,'let]"]

    ENV1 -->|extiende| ENV0
    ENV2 -->|extiende| ENV1
    ENV3 -->|extiende| ENV2
    ENV4 -->|extiende clausura captura Γ₁| ENV2
```

> **Nota:** La clausura `abonar` captura $\Gamma_1$ (no $\Gamma_2$) porque fue construida en ese ambiente. Al invocarla, el ambiente del cuerpo $\Gamma_3$ extiende $\Gamma_1$ directamente con `monto`, saltando el frame de `abonar`. Por eso `monto` y `saldo` son visibles en el cuerpo, pero no `abonar` (no es recursiva).

Las referencias se anotan con su estado **al momento de la primera invocación**:
- ref₆: marcada `'var` (aún no congelada)
- ref₈: marcada `'let` (parámetro formal — inmutable)
