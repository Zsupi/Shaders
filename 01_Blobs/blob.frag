/* Main function, uniforms & utils */
#ifdef GL_ES
    precision mediump float;
#endif

#define PI_TWO			1.570796326794897
#define PI				3.141592653589793
#define TWO_PI			6.283185307179586
#define EPSILON			0.0001
#define N_SPHEARE       5

#define rx 1.0 / min(u_resolution.x, u_resolution.y)
#define uv gl_FragCoord.xy / u_resolution.xy
#define st coord(gl_FragCoord.xy)
#define mx coord(u_mouse)

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform vec3 u_camera;
uniform float u_time;

struct Sphere {
    vec3 position;
    float radius;  
};

/* Coordinate and unit utils */
vec2 coord(in vec2 p) {
    p = p / u_resolution.xy;
    // correct aspect ratio
    if (u_resolution.x > u_resolution.y) {
        p.x *= u_resolution.x / u_resolution.y;
        p.x += (u_resolution.y - u_resolution.x) / u_resolution.y / 2.0;
    } else {
        p.y *= u_resolution.y / u_resolution.x;
        p.y += (u_resolution.x - u_resolution.y) / u_resolution.x / 2.0;
    }
    // centering
    p -= 0.5;
    p *= vec2(-1.0, 1.0);
    return p;
}

// https://www.shadertoy.com/view/Xds3zN : line 606
mat3 setCamera(vec3 cameraPos, in vec3 lookat, float cr )
{
	vec3 cw = normalize(lookat-cameraPos);
	vec3 cp = vec3(sin(cr), cos(cr),0.0);
	vec3 cu = normalize( cross(cw,cp) );
	vec3 cv =          ( cross(cu,cw) );
    return mat3( cu, cv, cw );
}

vec3 rayDir(out vec3 rayOrigin) {
    vec3 lookat = vec3(0.25, 0.25, 0.25);
    rayOrigin = lookat + vec3(15.5 * cos(0.1 * u_time + 2.3), 4.2, 15.5 * sin(0.1 * u_time + 2.3));
    mat3 camera = setCamera(rayOrigin, lookat, 0.0);
    return camera * normalize(vec3(st, 2.5));
}

void setupSpheres(inout Sphere spheres[N_SPHEARE]) {
    spheres[0] = Sphere(vec3(1.0, sin(u_time) / 2.0, -cos(u_time / 1.5)), 1.0);
    spheres[1] = Sphere(vec3(0.0, 1.0, cos(u_time)), 0.8);
    spheres[2] = Sphere(vec3(sin(u_time) * 0.11, cos(u_time / 1.3), 0.2), 0.5);
    spheres[3] = Sphere(vec3(0.12, sin(u_time / 8.2) * 1.2, 0.43), 0.6);
    spheres[4] = Sphere(vec3(sin(u_time) * 0.2, 0.4, cos(u_time / 2.1)), 0.45);
}

//https://iquilezles.org/articles/distfunctions/
float sdSphere( vec3 p, float radius, float noise) {
    float displacement = sin(8.1 * p.x) * sin(6.2 * p.y) * sin(5.1 * p.z) * abs((sin(u_time * noise))/ 7.0);
    return length(p) - radius + displacement;
}

//https://iquilezles.org/articles/smin/
float smin( float a, float b, float k)
{
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return mix( b, a, h ) - k*h*(1.0-h);
}

float calcMin(Sphere spheres[N_SPHEARE], vec3 pos) {
    float dist1 = sdSphere(spheres[0].position - pos, spheres[0].radius, 0.62);
    float dist2 = sdSphere(spheres[1].position - pos, spheres[1].radius, 0.41);

    float result;
    result = smin(dist1, dist2, 0.3);
    for (int i = 2; i < N_SPHEARE; i++) {
        float disti = sdSphere(spheres[i].position - pos, spheres[i].radius, 1.2);
        result = smin(result, disti, 0.3);
    }
    return result;
}

vec4 marchStep(vec3 rayOrigin) {
    Sphere spheres[N_SPHEARE];
    setupSpheres(spheres);
    
    vec3 pos = rayOrigin;

    float dist = calcMin(spheres, pos);
    vec3 color = vec3(0.0, 0.1412, 0.1412);
    return vec4(color, dist);
}

vec3 rayMarch(vec3 rayOrigin, vec3 rayDir, out bool hit) {
    float dist;
    hit = false;
    const int maxStep = 200;
    int nStep = 0;
    vec3 color;

    vec3 pos = rayOrigin;

    for (int i = 0; i < maxStep; i++) {
        vec4 result = marchStep(pos);
        color = result.xyz;
        dist += result.w;

        pos = pos + normalize(rayDir) * result.w;
        nStep++;

        if (result.w < EPSILON) {
            float g = mix(color.g, 1.0, float(nStep*2) / float(maxStep));
            float b = mix(color.b, 1.0, float(nStep*2) / float(maxStep));
            color = vec3(color.r, g, b);
            hit = true;
            break;
        }
        if (dist > 1000.0) {
            color = vec3(0.0, 0.1137, 0.1137);
            break;
        }
    }
    return color;
}

float random2d(vec2 seed) {
    return fract(sin(dot(seed.xy, vec2(12.9898, 78.233))) * 43758.5453);
}



void main() {
    vec3 rayOrigin;
    vec3 rayDir = rayDir(rayOrigin);
    vec3 color;
    bool hit;
    color += rayMarch(rayOrigin, rayDir, hit);

    // Simple antialias
    if (hit) {
        const int nSample = 10;
        for (int i = 0; i < nSample; i++) {
            float value = random2d(vec2(cos(float(i)), sin(float(i)))) * 0.0007;
            color += rayMarch(rayOrigin, rayDir + vec3(value, 0, 0), hit);
            color += rayMarch(rayOrigin, rayDir + vec3(0, value, 0), hit);
            color += rayMarch(rayOrigin, rayDir + vec3(value, value, 0), hit);
        }

        color /= float(nSample*3 + 1);
    }
    gl_FragColor = vec4(color, 1.0);
}