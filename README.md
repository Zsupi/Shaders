# Shaders

> [!TIP]
>
> * Use the [Shader languages support for VS Code](https://marketplace.visualstudio.com/items?itemName=slevesque.shader) extension for syntax highlighting.
> * Use the [GLSL Lint](https://marketplace.visualstudio.com/items?itemName=dtoplak.vscode-glsllint) extension for linting.
> * Install the [glslCanvas](https://marketplace.visualstudio.com/items?itemName=circledev.glsl-canvas) visual studio code extension tp see the results of the shaders.

## Projects

### 01_Blob 🔵

* Distorted spheres are rendered using signed distance functions and ray marching. 
* The spheres are blend together with smooth min function. You can find more information about smooth min functions in this article: [link](https://iquilezles.org/articles/smin/)
* The amount of steps the ray went before hitting the blobs is used to brighten the edges.

![image](https://github.com/Zsupi/Shader/assets/73688652/da71211e-6c39-4b55-a9d5-1cad2be6c95b)
