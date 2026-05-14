# Informe de Corrección

**Taller 3 — Fundamentos de Lenguajes de Programación 2026-1**  
**Universidad del Valle**

**Autores:**
- JHORMAN RICARDO LOAIZA — 2359710
- JUAN DIEGO OSPINA — 2359486
- MAURICIO ALEJANDRO ROJAS — 2359701
- JUAN FELIPE RUIZ — 2359397

---

## 5.1 Corrección de `set-exp` en el intérprete

### Pre-condición y post-condición

**Pre-condición:**

Sea $\Gamma$ el ambiente actual, $\sigma$ el store actual y $e$ la expresión a asignar. Se asume que:

1. El identificador $x$ está ligado en $\Gamma$: $\exists\, \text{ref}_i$ tal que $\Gamma(x) = \text{ref}_i$.
2. La celda $\text{ref}_i$ existe en $\sigma$ y contiene $(v_0,\ \texttt{'var})$, es decir, la marca es `'var`.
3. La expresión $e$ es evaluable en $\Gamma$ y su resultado es un valor $v_1$.

**Post-condición:**

Después de ejecutar `set-exp(x, e)`:

1. El store $\sigma'$ es idéntico a $\sigma$ salvo en la celda $\text{ref}_i$, que ahora contiene $(v_1,\ \texttt{'var})$.
2. El ambiente $\Gamma$ no se modifica (las referencias son las mismas).
3. El resultado retornado es `void`.

Formalmente:

$$\sigma' = \sigma[\text{ref}_i \mapsto (v_1,\ \texttt{'var})]$$

$$\sigma'(j) = \sigma(j) \quad \forall j \neq \text{ref}_i$$

---

### Demostración del caso "feliz" (marca `'var`)

La implementación de `set-exp` en el intérprete ejecuta los siguientes pasos:

1. `(apply-env-ref env id)` → retorna $\text{ref}_i$ (por la pre-condición, $x$ está ligado).
2. `(evaluar-expresion rand env)` → retorna $v_1$ (por la pre-condición, $e$ es evaluable).
3. `(deref-mark ref_i)` → retorna `'var` (por la pre-condición).
4. El `cond` selecciona la rama `[(eq? marca 'var) ...]`.
5. `(setref! ref_i v_1)` actualiza $\sigma[\text{ref}_i] \leftarrow (v_1,\ \texttt{'var})$.
6. Se retorna `(void-val)` = `'void`.

La función `setref!` preserva la marca (solo actualiza el `car` del par, no el `cdr`):

```scheme
(define setref!
  (lambda (ref v)
    (let ((marca (deref-mark ref)))
      (vector-set! the-store ref (cons v marca)))))
```

Por tanto, $\sigma' = \sigma[\text{ref}_i \mapsto (v_1,\ \texttt{'var})]$ y para todo $j \neq i$, $\sigma'(j) = \sigma(j)$. ✓

---

### Demostración del caso de error: marca `'let`

Si $\sigma(\text{ref}_i) = (v_0,\ \texttt{'let})$, entonces:

- `(deref-mark ref_i)` → `'let`.
- El `cond` selecciona la rama `[(eq? marca 'let) (eopl:error ...)]`.
- Se lanza `eopl:error` con el mensaje *"No se puede asignar al identificador inmutable 'x'"*.
- La función **no llama** `setref!`, por lo tanto el store no se modifica: $\sigma' = \sigma$.

El programa termina con error antes de alcanzar la asignación. ✓

---

### Demostración del caso de error: marca `'frozen`

Si $\sigma(\text{ref}_i) = (v_0,\ \texttt{'frozen})$, entonces:

- `(deref-mark ref_i)` → `'frozen`.
- El `cond` selecciona la rama `[(eq? marca 'frozen) (eopl:error ...)]`.
- Se lanza `eopl:error` con el mensaje *"No se puede asignar al identificador congelado 'x'"*.
- El store no se modifica: $\sigma' = \sigma$. ✓

En ambos casos de error, la semántica de MiniLang garantiza que ningún efecto secundario ocurre después de lanzar `eopl:error`, pues la excepción interrumpe la evaluación inmediatamente.

---

## 5.2 Corrección de `freeze-exp`

### Enunciado de la propiedad

**Propiedad (permanencia del congelamiento):**  
Si en algún momento de la evaluación la marca de la referencia asociada a $x$ es `'frozen`, entonces ningún paso posterior de la evaluación puede devolverla a `'var` o a `'let`.

Formalmente: sea $\sigma_t$ el store en el instante $t$ de la evaluación. Si $\sigma_t(\text{ref}_x) = (v,\ \texttt{'frozen})$ para algún $t$, entonces para todo $t' > t$:

$$\sigma_{t'}(\text{ref}_x) = (v',\ \texttt{'frozen})$$

para algún $v'$ (el valor puede cambiar solo si se permitiera `setref!`, pero ya demostramos que `set-exp` lo prohíbe sobre `'frozen`).

---

### Demostración

La demostración se apoya en dos observaciones exhaustivas sobre las operaciones que pueden modificar el campo de marca en el store:

**Observación 1:** Las únicas funciones que modifican una celda del store son `setref!` y `setref-mark!`.

- `setref!` preserva la marca (solo actualiza el valor): no puede cambiar `'frozen` a `'var`.
- `setref-mark!` solo es llamada desde `freeze-exp`.

**Observación 2:** `freeze-exp` sobre una referencia ya `'frozen` lanza error.

La implementación de `freeze-exp` en el intérprete:

```scheme
(freeze-exp (id)
  (let ((ref (apply-env-ref env id)))
    (let ((marca (deref-mark ref)))
      (cond
        [(eq? marca 'var)
         (setref-mark! ref 'frozen)
         (void-val)]
        [(eq? marca 'let)
         (eopl:error 'freeze-exp "No se puede congelar el identificador inmutable '~s'" id)]
        [(eq? marca 'frozen)
         (eopl:error 'freeze-exp "El identificador '~s' ya estaba congelado" id)]
        ...))))
```

Si `marca = 'frozen`, se entra en la rama del error, y `setref-mark!` **no es llamada**. Por tanto, la marca permanece `'frozen`.

**Observación 3:** Ninguna otra construcción del intérprete llama `setref-mark!`.

Una inspección exhaustiva del evaluador confirma que `setref-mark!` solo aparece en `freeze-exp`. Las demás construcciones (`let-exp`, `var-exp`, `set-exp`, `begin-exp`, `app-exp`, `letrec-exp`) solo llaman `newref` (que crea nuevas celdas) o `setref!` (que preserva la marca).

**Conclusión:** Una vez que $\sigma_t(\text{ref}_x) = (v,\ \texttt{'frozen})$:
- `set-exp` sobre $x$ lanza error → no modifica la celda.
- `freeze-exp` sobre $x$ lanza error → no modifica la marca.
- Ninguna otra construcción modifica la marca de celdas existentes.

Por tanto, $\forall t' > t: \sigma_{t'}(\text{ref}_x)$ tiene marca `'frozen`. ✓

---

## 5.3 Soundness débil del chequeador

### Enunciado informal

**Propiedad (soundness débil):**  
Si $\Gamma \vdash e : t$ (el chequeador acepta $e$ con tipo $t$ en el ambiente $\Gamma$), entonces al evaluar $e$ en un ambiente de valores compatible con $\Gamma$ (donde cada $x$ con tipo $t_x$ en $\Gamma$ tiene un valor de forma coherente con $t_x$), el intérprete **no produce un error de tipo en tiempo de ejecución** y, si termina, retorna un valor cuya forma es coherente con $t$.

La demostración es por inducción estructural sobre el AST. Se analiza cada construcción del fragmento sin procedimientos.

---

### Caso: `lit-exp(n)`

**Regla de tipado:**

$$\Gamma \vdash \texttt{lit-exp}(n) : \texttt{int}$$

**Paso del intérprete:**  
`evaluar-expresion` retorna $n$ directamente. $n$ es un número, que es coherente con `int`. No puede ocurrir error de tipo.

**Hipótesis de inducción:** No aplica (caso base).

**Conclusión:** ✓

---

### Caso: `ident-exp(x)`

**Regla de tipado:**

$$\frac{\Gamma(x) = (t,\ \_)}{\Gamma \vdash \texttt{ident-exp}(x) : t}$$

**Paso del intérprete:**  
`apply-env(env, x)` = `deref(apply-env-ref(env, x))`.  
Por compatibilidad del ambiente: si $\Gamma(x) = (t,\ \_)$, entonces la celda referenciada contiene un valor coherente con $t$.

**Hipótesis de inducción:** El ambiente de valores es compatible con $\Gamma$.

**Conclusión:** El resultado es coherente con $t$. No hay error de tipo. ✓

---

### Caso: `if-exp(e1, e2, e3)`

**Regla de tipado:**

$$\frac{\Gamma \vdash e_1 : \texttt{bool} \quad \Gamma \vdash e_2 : t \quad \Gamma \vdash e_3 : t}{\Gamma \vdash \texttt{if-exp}(e_1, e_2, e_3) : t}$$

**Paso del intérprete:**  
Por HI aplicada a $e_1$: el resultado es coherente con `bool`, es decir, es `#t` o `#f`.  
La comprobación `(boolean? test-val)` es verdadera → no lanza error de tipo.  
Si `test-val = #t`: se evalúa $e_2$; por HI retorna valor coherente con $t$.  
Si `test-val = #f`: se evalúa $e_3$; por HI retorna valor coherente con $t$.

**Hipótesis de inducción:** Las tres subexpresiones son sound en $\Gamma$.

**Conclusión:** El resultado es coherente con $t$. No hay error de tipo en tiempo de ejecución. ✓

---

### Caso: `let-exp(xi:ti=ei, body)`

**Regla de tipado:**

$$\frac{\Gamma \vdash e_i : t_i \quad \Gamma[x_i \mapsto (t_i,\ \texttt{let})] \vdash e_b : t_b}{\Gamma \vdash \texttt{let-exp}(x_i:t_i=e_i,\ e_b) : t_b}$$

**Paso del intérprete:**  
Por HI sobre cada $e_i$: el resultado $v_i$ es coherente con $t_i$.  
Se crean referencias `'let` con valores $v_i$: el nuevo ambiente es compatible con $\Gamma[x_i \mapsto (t_i,\ \texttt{let})]$.  
Por HI sobre $e_b$ en el nuevo ambiente: el resultado es coherente con $t_b$.

**Hipótesis de inducción:** Cada $e_i$ y $e_b$ son sound en sus respectivos ambientes.

**Conclusión:** El resultado es coherente con $t_b$. ✓

---

### Caso: `var-exp(xi:ti=ei, body)`

**Regla de tipado:**

$$\frac{\Gamma \vdash e_i : t_i \quad \Gamma[x_i \mapsto (t_i,\ \texttt{var})] \vdash e_b : t_b}{\Gamma \vdash \texttt{var-exp}(x_i:t_i=e_i,\ e_b) : t_b}$$

**Paso del intérprete:** Idéntico a `let-exp` salvo que las referencias se marcan `'var`.  
Por HI sobre cada $e_i$ y $e_b$: el resultado es coherente con $t_b$.

**Hipótesis de inducción:** Igual que `let-exp`.

**Conclusión:** ✓

---

### Caso: `set-exp(x, e)`

**Regla de tipado:**

$$\frac{\Gamma(x) = (t,\ \texttt{var}) \quad \Gamma \vdash e : t}{\Gamma \vdash \texttt{set-exp}(x, e) : \texttt{void}}$$

**Paso del intérprete:**  
- El chequeador garantizó $\Gamma(x) = (t,\ \texttt{var})$, luego la marca en el store es `'var` (o `'frozen` si hubo un `freeze` dinámico posterior — ver nota sobre soundness débil más adelante).
- En el caso normal (sin `freeze` previo): `deref-mark(ref)` = `'var` → se entra en la rama del caso feliz → `setref!` actualiza la celda → se retorna `void`.
- `void` es coherente con `void-type`. ✓

**Hipótesis de inducción:** $e$ es sound en $\Gamma$.

**Nota sobre soundness débil:** El chequeador **no rastrea** el congelamiento dinámico. Si en tiempo de ejecución la referencia fue congelada por un `freeze` anterior (que el chequeador no detectó como incorrecto porque el programa aún era bien tipado estáticamente), `set-exp` lanzará un error de mutabilidad en tiempo de ejecución. Esto constituye la limitación de soundness débil mencionada en el enunciado (§3, Ejemplo 3): el chequeador acepta el programa, pero el intérprete puede lanzar el error. Esta situación **no es un error de tipo** (el tipo de la expresión asignada coincide), sino un error de estado de mutabilidad.

---

### Caso: `begin-exp(e1, ..., en)`

**Regla de tipado:**

$$\frac{\Gamma \vdash e_1 : t_1 \quad \cdots \quad \Gamma \vdash e_n : t_n}{\Gamma \vdash \texttt{begin-exp}(e_1,\ldots,e_n) : t_n}$$

**Paso del intérprete:**  
Por HI sobre cada $e_i$: cada evaluación termina sin error de tipo.  
El resultado de `begin-exp` es el resultado de $e_n$, que es coherente con $t_n$.

**Hipótesis de inducción:** Cada $e_i$ es sound en $\Gamma$.

**Conclusión:** ✓

---

### Caso: `freeze-exp(x)`

**Regla de tipado:**

$$\frac{\Gamma(x) = (\_,\ \texttt{var})}{\Gamma \vdash \texttt{freeze-exp}(x) : \texttt{void}}$$

**Paso del intérprete:**  
El chequeador garantizó que $x$ tiene mutabilidad `'var` en $\Gamma$.  
Por compatibilidad del ambiente: la referencia en el store tiene marca `'var` (asumiendo que no fue congelada dinámicamente antes).  
`setref-mark!(ref, 'frozen)` actualiza la marca → retorna `void`.  
`void` es coherente con `void-type`. ✓

**Hipótesis de inducción:** No aplica (no hay subexpresiones).

**Nota:** Al igual que con `set-exp`, si dinámicamente la referencia ya estaba `'frozen`, se lanza error de mutabilidad en tiempo de ejecución. Esto es el mismo fenómeno de soundness débil.

---

### Resumen

| Construcción | Regla aplicada | HI utilizada | Conclusión |
|:---:|:---:|:---:|:---:|
| `lit-exp` | $\Gamma \vdash n : \texttt{int}$ | Base | ✓ |
| `ident-exp` | $\Gamma(x) = (t,\_)$ | Compatibilidad de ambientes | ✓ |
| `if-exp` | bool + ramas iguales | HI sobre $e_1, e_2, e_3$ | ✓ |
| `let-exp` | $e_i : t_i$, env extendido | HI sobre $e_i$ y $e_b$ | ✓ |
| `var-exp` | Igual a let con marca `'var` | HI sobre $e_i$ y $e_b$ | ✓ |
| `set-exp` | Mutable + tipo coincide | HI sobre $e$ | ✓ (débil) |
| `begin-exp` | Secuencia, tipo = última | HI sobre cada $e_i$ | ✓ |
| `freeze-exp` | Mutable → congelado | Base | ✓ (débil) |

La propiedad de soundness débil se establece para todos los casos del fragmento sin procedimientos. Los casos `set-exp` y `freeze-exp` tienen la limitación de soundness débil vinculada al congelamiento dinámico, que es inherente a un sistema de tipos sin seguimiento de estados de mutabilidad en tiempo de ejecución.
