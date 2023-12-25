#ifdef GL_ES
    precision mediump float;
#endif

#define PI_TWO			1.570796326794897
#define PI				3.141592653589793
#define TWO_PI			6.283185307179586
#define EPSILON			0.0001
#define N_SPHEARE       5
#define MAX_STEP        1000

#define rx 1.0 / min(u_resolution.x, u_resolution.y)
#define uv gl_FragCoord.xy / u_resolution.xy
#define st coord(gl_FragCoord.xy)
#define mx coord(u_mouse)

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

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
    vec3 lookat = vec3(0.1);
    rayOrigin = lookat + vec3(15.5 * cos(0.1 * u_time + 10.0 * mx.x), 4.2, 15.5 * sin(0.1 * u_time + 10.0 * mx.x));
    mat3 camera = setCamera(rayOrigin, lookat, 0.0);
    return camera * normalize(vec3(st, 2.5));
}

float fresnel (float n1, float n2, vec3 normal, vec3 incident)
{
  float r0 = (n1 - n2) / (n1 + n2);
  r0 *= r0;
  float cosX = -dot(normal, incident);
  if (n1 > n2)
  {
    float n = n1 / n2;
    float sinT2 = pow(n, 2.0) * (1.0-cosX*cosX);
    if (sinT2 > 1.0) {
      return 1.0;
    }
    cosX = sqrt(1.0 - sinT2);
  }
  float x = 1.0 - cosX;
  float ret = r0 + (1.0 - r0) * pow(x, 5.0);

  ret = (max(n1, n2) + (1.0 - max(n1, n2)) * ret);
  return ret;
}

//https://iquilezles.org/articles/distfunctions/
float sdSphere( vec3 p, float radius) {
    return length(p) - radius;
}

//https://iquilezles.org/articles/smin/
float smin( float a, float b, float k)
{
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return mix( b, a, h ) - k*h*(1.0-h);
}

//https://iquilezles.org/articles/distfunctions/
float opOnion( float sdf, float thickness )
{
    return abs(sdf)-thickness;
}

vec3 coreNormal(vec3 p){
  return normalize(vec3(
    sdSphere(p + vec3(+0.05, 0.0, 0.0), 1.0) -
    sdSphere(p + vec3(-0.05, 0.0, 0.0), 1.0) ,
    sdSphere(p + vec3(0.0, +0.05, 0.0), 1.0) -
    sdSphere(p + vec3(0.0, -0.05, 0.0), 1.0) ,
    sdSphere(p + vec3(0.0, 0.0, +0.05), 1.0) -
    sdSphere(p + vec3(0.0, 0.0, -0.05), 1.0)
  ));
}

vec4 innerCore(vec3 pos, vec3 center) {
    float radius = 0.5;
    float dist = sdSphere(center - pos, radius);
    vec3 color = vec3(0.8392, 0.4627, 0.3686);
    return vec4(color, dist);
}

// vec4 core(vec3 pos, out vec3 rayDir) {
//     float radius = 1.0;
//     vec3 center = vec3(0, 0, 0);
//     float outterDist = sdSphere(center - pos, radius);
//     vec3 outterColor = vec3(0.84, 0.68, 0.37);

//     if (outterDist < EPSILON) {
//         float ior = 0.9;
//         vec3 normal = coreNormal(pos);

//         if(dot(normal, rayDir.xyz) > 0.0){
//             normal = -normal;
//             ior = 1.0 / ior;
//   	    }

//         float fresnel = fresnel(ior, 1.0 / ior, normal, rayDir.xyz);
//         vec3 innerDir = refract(rayDir.xyz, normal, fresnel);
//         vec3 innerPos = pos;
//         vec3 intersectPos = pos;

//         for (int i = 0; i < 100; i++) {
//             if (sdSphere(center - innerPos, radius) > EPSILON) break;
//             vec4 result = innerCore(innerPos, center);

//             innerPos = innerPos + normalize(innerDir) * result.w;

//             if (result.w < EPSILON) {
//                 float v = 200.0;
//                 float s = length(innerPos - intersectPos)/300.0;
//                 float attenuation = exp(-v * s);
//                 outterColor = result.xyz * attenuation + (outterColor * (1.0 - attenuation)) / v;
                
//                 break;
//             }
//         }
//     } 

//     return vec4(outterColor, outterDist);
// }

vec4 core(vec3 pos, out vec3 rayDir) {
    vec3 center = vec3(0, 0, 0);
    float outterDist = sdSphere(center - pos, 1.0);
    vec3 outterColor = vec3(0.84, 0.68, 0.37);

    if (outterDist < 0.0) {
        float ior = 0.9;
        vec3 normal = coreNormal(pos);

        if(dot(normal, rayDir.xyz) > 0.0){
            normal = -normal;
            ior = 1.0 / ior;
  	    }
        float fresnel = fresnel(ior, 1.0 / ior, normal, rayDir.xyz);
        rayDir = refract(rayDir.xyz, normal, fresnel);
    }

    float innerDist = sdSphere(center - pos, 0.75);
    vec3 innerColor = vec3(0.8392, 0.4627, 0.3686);
    return vec4(outterColor,opOnion(outterDist, 0.001));
}

vec3 rayMarch(vec3 rayOrigin, vec3 rayDir) {
    float dist;
    int nStep = 0;
    vec3 color;

    vec3 pos = rayOrigin;

    for (int i = 0; i < MAX_STEP; i++) {
        vec4 result = core(pos, rayDir);

        pos = pos + normalize(rayDir) * result.w;
        nStep++;

        if (result.w < EPSILON) {
            return result.xyz;
        }
    }
    return vec3(
        abs(cos(st.x + mx.x)), 
        abs(sin(st.y + mx.y)), 
        abs(sin(u_time))
    );;
}

void main() {
    vec3 rayOrigin;
    vec3 rayDir = rayDir(rayOrigin);

    vec3 color = rayMarch(rayOrigin, rayDir);

    gl_FragColor = vec4(color, 1.0);
}