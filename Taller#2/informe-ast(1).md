# Informe Teórico — Parte 4

**Taller 2: Mi primer intérprete (MiniLang)**  
**Curso:** Fundamentos de Lenguajes de Programación — 2026-1  
**Responsable:** Mauricio Alejandro Rojas

---

## 4.1 Árbol de Sintaxis Abstracta (AST)

Expresión analizada:

```
let m = +(3, 7)
    n = if >(m, 5) then *(m, 2) else sub1(m)
in
let* p = +(m, n)
     q = mod(p, 3)
in
cond
  ==(q, 0) ==> +(p, q)
  <(q, 2)  ==> -(p, q)
  else     ==> q
end
```

```mermaid
graph TD
    A["let-exp"] --> B["ids: m, n"]
    A --> C["rands"]
    A --> D["body: let*-exp"]

    C --> E["prim-exp: sum-prim"]
    E --> E1["lit-exp: 3"]
    E --> E2["lit-exp: 7"]

    C --> F["if-exp"]
    F --> F1["test: prim-exp mayor-prim"]
    F1 --> F1a["var-exp: m"]
    F1 --> F1b["lit-exp: 5"]
    F --> F2["true: prim-exp mult-prim"]
    F2 --> F2a["var-exp: m"]
    F2 --> F2b["lit-exp: 2"]
    F --> F3["false: prim-exp sub-prim"]
    F3 --> F3a["var-exp: m"]

    D --> G["ids: p, q"]
    D --> H["rands"]
    D --> I["body: cond-exp"]

    H --> H1["prim-exp sum-prim"]
    H1 --> H1a["var-exp: m"]
    H1 --> H1b["var-exp: n"]

    H --> H2["prim-exp mod-prim"]
    H2 --> H2a["var-exp: p"]
    H2 --> H2b["lit-exp: 3"]

    I --> J["conditions"]
    I --> K["actions"]
    I --> L["default: var-exp q"]

    J --> J1["prim-exp igual-prim"]
    J1 --> J1a["var-exp: q"]
    J1 --> J1b["lit-exp: 0"]

    J --> J2["prim-exp menor-prim"]
    J2 --> J2a["var-exp: q"]
    J2 --> J2b["lit-exp: 2"]

    K --> K1["prim-exp sum-prim"]
    K1 --> K1a["var-exp: p"]
    K1 --> K1b["var-exp: q"]

    K --> K2["prim-exp minus-prim"]
    K2 --> K2a["var-exp: p"]
    K2 --> K2b["var-exp: q"]
```

---

## 4.2 Traza de evaluación

Expresión analizada, partiendo del ambiente inicial $[\text{x}=4,\ \text{y}=2,\ \text{z}=5]$:

```
let a = +(x, y)
    b = *(y, z)
in
if >(a, b)
then
  let c = -(a, b)
  in +(c, z)
else
  let c = -(b, a)
  in *(c, x)
```

### Pasos de la traza

**Paso 1** — Evaluar `+(x, y)` en $\text{env}_0 = [\text{x}=4,\ \text{y}=2,\ \text{z}=5]$

$$\text{value-of}(+(x,y),\ \text{env}_0) = 4 + 2 = 6$$

**Paso 2** — Evaluar `*(y, z)` en $\text{env}_0$

$$\text{value-of}(*(y,z),\ \text{env}_0) = 2 \times 5 = 10$$

**Paso 3** — Extender ambiente: $\text{env}_1 = [\text{a}=6,\ \text{b}=10\ |\ \text{env}_0]$

**Paso 4** — Evaluar condición `>(a, b)` en $\text{env}_1$

$$\text{value-of}(>(a,b),\ \text{env}_1) = (6 > 10) = \#\text{false}$$

**Paso 5** — Condición es $\#\text{false}$ → se toma la rama `else`

Se evalúa `let c = -(b, a) in *(c, x)` en $\text{env}_1$.

**Paso 6** — Evaluar `-(b, a)` en $\text{env}_1$

$$\text{value-of}(-(b,a),\ \text{env}_1) = 10 - 6 = 4$$

**Paso 7** — Extender ambiente: $\text{env}_2 = [\text{c}=4\ |\ \text{env}_1]$

**Paso 8** — Evaluar `*(c, x)` en $\text{env}_2$

$$\text{value-of}(*(c,x),\ \text{env}_2) = 4 \times 4 = 16$$

**Resultado final: 16**

---

### Diagrama de cadena de ambientes

```mermaid
graph TD
    EV["ambiente-vacio"]
    E0["env0\nx=4, y=2, z=5"]
    E1["env1\na=6, b=10"]
    E2["env2\nc=4"]

    E2 --> E1
    E1 --> E0
    E0 --> EV
```
