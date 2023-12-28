#ifdef GL_ES
    precision mediump float;
#endif

#define PI_TWO			1.570796326794897
#define PI				3.141592653589793
#define TWO_PI			6.283185307179586
#define EPSILON			0.0001
#define N_SPHEARE       5
#define MAX_STEP        1000
#define MAX_BOUNCE      5

#define REFLECT         1
#define REFRACT         2
#define SOLID           3

#define rx 1.0 / min(u_resolution.x, u_resolution.y)
#define uv gl_FragCoord.xy / u_resolution.xy
#define st coord(gl_FragCoord.xy)
#define mx coord(u_mouse)
#define sdfnormal(func, p, size) normalize(vec3( func(p + vec3(+0.05, 0.0, 0.0), size) - func(p + vec3(-0.05, 0.0, 0.0), size), func(p + vec3(0.0, +0.05, 0.0), size) - func(p + vec3(0.0, -0.05, 0.0), size), func(p + vec3(0.0, 0.0, +0.05), size) - func(p + vec3(0.0, 0.0, -0.05), size)))

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

struct SdfObject {
    int type;
    float dist;
    vec3 color;
    vec3 normal;
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
    vec3 lookat = vec3(0.1);
    rayOrigin = lookat + vec3(15.5 * cos(0.1 * u_time + 10.0 * mx.x), 4.2, 15.5 * sin(0.1 * u_time + 10.0 * mx.x));
    mat3 camera = setCamera(rayOrigin, lookat, 0.0);
    return camera * normalize(vec3(st, 2.5));
}

float snoise(vec3 p) {
    return sin(dot(p, vec3(12.9898, 78.233, 45.543))) * 43758.5453;
}

float schlick(vec3 rayDir, vec3 normal, float r1, float r2) {
    float r = (r1-r2) / (r1+r2);
    r *= r;
    return r+(1.0-r)*pow(1.0-abs(dot(normal, rayDir)), 5.0);
}

//https://iquilezles.org/articles/distfunctions/
float sdSphere( vec3 p, float radius) {
    return length(p) - radius;
}

float sdBox( vec3 p, vec3 b )
{
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

//https://iquilezles.org/articles/smin/
float opUnion( float a, float b, float k)
{
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return mix( b, a, h ) - k*h*(1.0-h);
}

//https://iquilezles.org/articles/distfunctions/
SdfObject opUnion( SdfObject a, SdfObject b)
{
    if (a.dist < b.dist) {
        return a;
    }
    return b;
}

SdfObject opSubtraction( SdfObject a, SdfObject b )
{
    if (a.dist > -b.dist) {
        return a;
    }
    return b;
}

//https://iquilezles.org/articles/distfunctions/
float opOnion( float sdf, float thickness )
{
    return abs(sdf)-thickness;
}

float opDistort(vec3 p) {
    return sin(p.x * 3.3 + u_time) * 0.03 + cos(p.y * 3.3 + u_time)*0.03;
}

float sdCutout(vec3 p, float size) {
    float d1 = sdBox(p, vec3(0.2, 0.4, 0.3));
    float d2 = sdBox(p+vec3(0.3, -0.34, -0.4), vec3(0.3, 0.65, 0.23));
    float d3 = sdBox(p+vec3(-0.4, 0.4, -0.3), vec3(0.34, 0.22, 0.53));
    float d4 = sdBox(p+vec3(0.4, -0.3, 0.4), vec3(0.25, 0.32, 0.22));
    return opUnion(opUnion(opUnion(d1, d2, 0.2), d3, 0.2), d4, 0.2) * size;
}

vec3 opFog(vec3 color, vec3 fogColor, float dist) {
    float v = 1.2; 
    float s = dist;
    float attenuation = exp(-v * s);
    vec3 radiance = color * attenuation + (fogColor * (1.0 - attenuation)) / v;
    return radiance;
}

SdfObject outerCore(vec3 center, vec3 pos) {
    
    float radius = 1.0;
    float dist = sdSphere(center - pos, radius) + opDistort(center -pos);
    vec3 color = vec3(0.84, 0.68, 0.37);
    vec3 normal = sdfnormal(sdSphere, center - pos, radius);
    SdfObject object;
    object.type = REFRACT;
    object.dist = opOnion(dist, 0.0);
    object.color = color;
    object.normal = normal;
    return object;
}

SdfObject innerCore(vec3 center, vec3 pos) {
    float radius = 0.72;
    float dist = sdSphere(center - pos, radius) + opDistort(center - pos);
    vec3 color = vec3(0.702, 0.2863, 0.0471);
    vec3 normal = sdfnormal(sdSphere, center - pos, radius);
    SdfObject object;
    object.type = SOLID;
    object.dist = dist;
    object.color = color;
    object.normal = normal;
    return object;
}

SdfObject core(vec3 center, vec3 pos) {
    SdfObject outerCore = outerCore(center, pos);
    SdfObject innerCore = innerCore(center, pos);
    if (outerCore.dist < innerCore.dist) 
          return outerCore;
    return innerCore;
}

SdfObject cutoutPlanet(vec3 center, vec3 pos) {
    float radius = 1.2;
    float dist = sdSphere(center - pos, radius);
    vec3 color = vec3(0.0706, 0.7451, 0.4667);
    vec3 normal = sdfnormal(sdSphere, center - pos, radius);
    SdfObject object;
    object.type = SOLID;
    object.dist = dist;
    object.color = color;
    object.normal = normal;
    return object;
}

SdfObject basePlanet(vec3 center, vec3 pos, float radius) {
    float dist = sdSphere(center - pos, radius);
    vec3 color = vec3(0.0706, 0.7451, 0.4667);
    vec3 normal = sdfnormal(sdSphere, center - pos, radius);
    SdfObject object;
    object.type = SOLID;
    object.dist = dist;
    object.color = color;
    object.normal = normal;
    return object;
}

SdfObject planet(vec3 center, vec3 pos) {
    SdfObject base = basePlanet(center, pos, 1.5);
    SdfObject insideCutout = basePlanet(center, pos, 1.4);
    SdfObject outsideCutout = cutoutPlanet(pos, vec3(0.2, 1.2, -0.9));
    //SdfObject result = opSubtraction(base, insideCutout);
    SdfObject result = opSubtraction(base, outsideCutout);
    result.dist = opOnion(result.dist, 0.02);
    return result;
}

SdfObject scene(vec3 pos) {
    vec3 center = vec3(0);
    SdfObject core = core(center, pos);
    SdfObject planet = planet(center, pos);
    //return core;
    return opUnion(core, planet);
}

SdfObject rayMarch(vec3 rayOrigin, vec3 rayDir) {
    float dist = 0.01;
    vec3 color;

    vec3 pos = rayOrigin;

    for (int i = 0; i < MAX_STEP; i++) {
        pos = rayOrigin + normalize(rayDir) * dist;
        
        SdfObject hit = scene(pos);
        dist += hit.dist;

        if (abs(hit.dist) < EPSILON) {
            hit.dist = dist;
            return hit;
        }
    }
    vec3 bgColor = vec3(
        abs(cos(st.x + mx.x)), 
        abs(sin(st.y + mx.y)), 
        abs(sin(u_time))
    );
    vec3 w = vec3(0.4392, 0.4392, 0.4392);
    return SdfObject(SOLID, dist, w, vec3(0.0));
}

vec3 shade(vec3 powerDensity, vec3 normal, vec3 lightPos, vec3 e, vec3 hit, vec3 materialColor) {
  
  float ks = 0.8;
  float shininess = 1000.0;

  vec3 lightDir = normalize(lightPos - hit);
  vec3 viewDir = normalize(hit.xyz - e);

  float cosa = clamp( dot(lightDir, normal), 0.0, 1.0);

  return powerDensity * cosa * materialColor + powerDensity * pow(clamp(dot(normalize(viewDir + lightDir), normal), 0.0, 1.0), shininess) * ks;
}

void main() {
    vec3 rayOrigin;
    vec3 rayDir = rayDir(rayOrigin);
    vec3 color;
    vec3 pos = rayOrigin;

    // for refraction & fog
    vec3 outerColor;
    bool applyFog = false;
    float v = 1.2;
    float IOR = 1.33;

    for (int i = 0; i < MAX_BOUNCE; i++) {
        SdfObject hit = rayMarch(pos, rayDir);

        if (hit.type == SOLID) {
            if (applyFog) {
                color += opFog(hit.color, outerColor, 0.3);
                break;
            }
            color += hit.color;
            break;
        }
        if (hit.type == REFRACT) {
            vec3 normal = hit.normal;
            if (dot(rayDir, normal) > 0.0) {
                normal = -normal;
                IOR = 1.0 / IOR;
            }

            float fresnel = schlick(rayDir, normal, IOR, 1.0);
            color += hit.color * fresnel;
            outerColor = hit.color;

            pos = pos + rayDir * hit.dist;
            pos = pos - normal * EPSILON * 3.0;
            //rayDir = refract(rayDir, normal, 1.0 / IOR);
            applyFog = true;
        }
        
    }
    gl_FragColor = vec4(color, 1.0);
}