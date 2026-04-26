# Informe de Corrección — Parte 5

**Taller 2: Mi primer intérprete (MiniLang)**  
**Curso:** Fundamentos de Lenguajes de Programación — 2026-1  
**Responsable:** Mauricio Alejandro Rojas

---

## 5.1 Corrección de `evaluar-expresion` para `cond-exp`

### Especificación formal

**Pre-condición:**

$$\text{conditions} = [c_1, c_2, \ldots, c_n],\quad \text{actions} = [a_1, a_2, \ldots, a_n],\quad \text{default} = d,\quad n \geq 0$$

Cada $c_i$ debe evaluar a un valor booleano en el ambiente $\text{env}$.

**Post-condición:**

$$\text{value-of}(\text{cond-exp},\ \text{env}) = \begin{cases} \text{value-of}(a_i,\ \text{env}) & \text{si } \exists\ i : \text{value-of}(c_i, \text{env}) = \#\text{true} \text{ (primer } i\text{)} \\ \text{value-of}(d,\ \text{env}) & \text{si } \forall\ i : \text{value-of}(c_i, \text{env}) = \#\text{false} \\ \text{error} & \text{si } \exists\ i : \text{value-of}(c_i, \text{env}) \notin \{\#\text{true}, \#\text{false}\} \end{cases}$$

### Demostración por casos

**Caso 1: Alguna condición es verdadera**

La función auxiliar `evaluar-cond` recorre la lista de condiciones de izquierda a derecha. Cuando `(car conds)` evalúa a `#true`, retorna `(evaluar-expresion (car acts) env)` inmediatamente sin evaluar el resto. Esto garantiza que se ejecuta la acción correspondiente a la **primera** condición verdadera.

**Caso 2: Ninguna condición es verdadera**

Cuando `(null? conds)` es verdadero (se agotaron todas las condiciones), se retorna `(evaluar-expresion default env)`. Esto cubre correctamente el caso `else`.

**Caso 3: Una condición no es booleana**

Cuando `(boolean? test-value)` es `#false`, se lanza `eopl:error` con el valor recibido. Esto impide evaluaciones incorrectas por condiciones no booleanas.

### Terminación

Cada llamada recursiva a `evaluar-cond` opera sobre `(cdr conds)`, que tiene estrictamente un elemento menos. La lista es finita, por lo tanto el proceso termina en el caso base `(null? conds)`.

---

## 5.2 Corrección de `evaluar-expresion` para `let*-exp`

### Diferencia semántica entre `let` y `let*`

**Con `let`** las ligaduras se evalúan **en paralelo** en el ambiente exterior:

```
let p = 3
    q = +(p, 5)   ; p no existe en este punto → error
in r
```

**Con `let*`** las ligaduras se evalúan **secuencialmente**; cada una extiende el ambiente antes de evaluar la siguiente:

```
let* p = 3
     q = +(p, 5)  ; p = 3 ya disponible → q = 8
     r = *(q, p)  ; q = 8, p = 3 → r = 24
in r              ; resultado: 24
```

### Invariante del ambiente

Sea $\text{env}_0$ el ambiente exterior antes del `let*`. Definimos:

$$\text{env}_k = [\text{id}_k = v_k\ |\ \text{env}_{k-1}], \quad v_k = \text{value-of}(\text{rand}_k,\ \text{env}_{k-1})$$

**Invariante:** en el paso $k$, el ambiente disponible es $\text{env}_k$ que contiene todas las ligaduras $\text{id}_1, \ldots, \text{id}_k$ más las del ambiente exterior.

### Demostración

La función `extender-secuencial` implementa exactamente esta invariante:

- **Caso base** `(null? ids)`: evalúa el cuerpo en `env-actual` que contiene todas las ligaduras procesadas. Correcto por invariante.
- **Caso recursivo**: evalúa `(car rands)` en `env-actual`, crea un nuevo ambiente extendido con `(car ids)` ligado al valor obtenido, y llama recursivamente. La invariante se preserva en cada paso.

**Terminación:** cada llamada recursiva consume `(cdr ids)` y `(cdr rands)`, listas de igual longitud que decrecen hasta `null`.

---

## 5.3 Corrección de `apply-env`

### Propiedad de shadowing

Sea un ambiente $\text{env} = [\text{ids}_n = \text{vals}_n\ |\ \cdots\ |\ \text{ids}_1 = \text{vals}_1\ |\ \text{env}_\emptyset]$.

Si la misma variable `id` aparece en marcos diferentes, `apply-env` retorna el valor del **marco más reciente** (el primero en la cadena).

**Demostración:** La función `buscar` recorre `ids` del marco actual de izquierda a derecha. Si encuentra `id`, retorna inmediatamente el valor correspondiente sin explorar marcos anteriores. Solo cuando `(null? ids)` llama recursivamente a `(apply-env old-env id)`, propagando la búsqueda al marco anterior. Por tanto, la ligadura más reciente siempre tiene prioridad.

### Caso de error

Cuando se llega a `(ambiente-vacio)`, significa que `id` no está ligado en ningún marco. Se lanza:

```racket
(eopl:error 'apply-env "Variable no ligada: ~s" id)
```

Esto garantiza que nunca se retorna un valor incorrecto ni `#f` silencioso.

### Terminación

Cada llamada recursiva pasa a `old-env`, que es el ambiente anterior en la cadena. Como los ambientes son finitos y se construyen empilando marcos sobre `ambiente-vacio`, la cadena tiene longitud finita. El proceso termina necesariamente en `ambiente-vacio`.

**Conclusión formal:**

$$\forall\ \text{env},\ \forall\ \text{id} : \text{apply-env}(\text{env}, \text{id}) = \begin{cases} v & \text{si } \text{id} \in \text{env} \text{ (valor de la ligadura más reciente)} \\ \text{error} & \text{si } \text{id} \notin \text{env} \end{cases}$$
