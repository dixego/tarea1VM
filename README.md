Estructura del proyecto:

- `Kripke.hs` contiene el módulo que define modelos de Kripke
- `CTL.hs` contiene el módulo que define fórmulas de CTL
- `SExpr.hs` contiene el módulo que define las S-Expressions y su parser
- `Verification.hs` contiene el módulo que define la función de verificación de modelos
   para un estado.

Para utilizar el verificador: cargue `Verification.hs` en el interprete interactivo de
Haskell `ghci`. Utilice la función `verifyString` para resolver un problema de
verificación escrito directamente en una cadena o la función `verifyFile` con un nombre de
archivo para extraer el problema de dicho archivo. Para aprender el formato de entrada del
problema, consultar la documentación de cada módulo o revisar los archivos de ejemplo
`ex1` y `ex2`. A continuación se muestra un modelo de ejemplo simple:

```
(problem
  (model
    (0 (p) (1 2))
    (1 (q) (0 2))
    (2 (p q r) ()))
  (state 0)
  (formula (eg (not (and p (or q r))))))
```
