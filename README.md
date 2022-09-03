# star-chart
A Haskell program which reads the HYG Star Database and uses the Diagrams module to generate a star chart.

![chart](https://user-images.githubusercontent.com/33742833/56004301-881e9200-5c98-11e9-9e94-cb030ef79596.png)

This project uses nix to pull in the necessary star-data dependencies and feed them into the compiled Haskell program. To build everything and produce an SVG, run
```ShellSession
$ nix-build release.nix
```
