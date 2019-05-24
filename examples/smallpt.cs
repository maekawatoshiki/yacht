using System;

public class Random {  
  private uint x;  
  private uint y;  
  private uint z;  
  private uint w;  

  public Random(uint seed) {  
    SetSeed(seed);  
  }  

  public void SetSeed(uint seed) {  
    x = 521288629;  
    y = 341235113;  
    z = seed;  
    w = x ^ z;  
  }  

  public uint Next() {  
    uint t = x ^ (x << 11);  
    x = y;  
    y = z;  
    z = w;  
    w = (w ^ (w >> 19)) ^ (t ^ (t >> 8));  
    return w;
  } 

  public double NextDouble() {
    return (double)Next() / 0xfffffff0;
  }
}  

public class Vec {
  public double x, y, z;              // position, also color (r,g,b)
  public Vec(double x_, double y_, double z_) { x = x_; y = y_; z = z_; }
  public static Vec operator +(Vec a, Vec b) { return new Vec(a.x + b.x, a.y + b.y, a.z + b.z); }
  public static Vec operator -(Vec a, Vec b) { return new Vec(a.x - b.x, a.y - b.y, a.z - b.z); }
  public static Vec operator *(Vec a, double b) { return new Vec(a.x * b, a.y * b, a.z * b); }
  public Vec mult(Vec b) { return new Vec(x * b.x, y * b.y, z * b.z); }
  public Vec norm() { return this * (1 / Math.Sqrt(x * x + y * y + z * z)); }
  public double dot(Vec b) { return x * b.x + y * b.y + z * b.z; } // cross:
  public static Vec operator %(Vec a, Vec b) { return new Vec(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x); }
}

public class Ray { 
  public Sphere s;
  public double t;
  public Vec o, d; 
  public Ray(Vec o_, Vec d_) { o = o_; d = d_; } 
  public bool intersect(Sphere[] spheres) {
    double inf = t = 1e20, d;
    for (int i = spheres.Length - 1; i >= 0; i--) { d = spheres[i].intersect(this); if (d > 0 && d < t) { t = d; s = spheres[i]; } }
    return t < inf;
  }
}

public class Sphere {
  public double rad;       // radius
  public Vec p, e, c;      // position, emission, color
  public int refl;      // reflection type (DIFFuse, SPECular, REFRactive)
  public Sphere(double rad_, Vec p_, Vec e_, Vec c_, int refl_) {
    rad = rad_; p = p_; e = e_; c = c_; refl = refl_;
  }
  public double intersect(Ray r) { // returns distance, 0 if nohit
    Vec op = p - r.o; // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
    double t, eps = 1e-4, b = op.dot(r.d), det = b * b - op.dot(op) + rad * rad;
    if (det < 0) return 0; else det = Math.Sqrt(det);
    return (t = b - det) > eps ? t : ((t = b + det) > eps ? t : 0);
  }
}

internal static class Program {
  static double clamp(double x) { return x < 0 ? 0 : x > 1 ? 1 : x; }
  static int toInt(double x) { return (int)(Math.Pow(clamp(x), 1 / 2.2) * 255 + .5); }
  static Vec radiance(Sphere[] spheres, Ray r, int depth, Random random) {
    if (!r.intersect(spheres)) return new Vec(0, 0, 0); // if miss, return black
    Sphere obj = r.s;             // the hit object
    Vec x = r.o + r.d * r.t, n = (x - obj.p).norm(), nl = n.dot(r.d) < 0 ? n : n * -1, f = obj.c;
    double p = f.x > f.y && f.x > f.z ? f.x : f.y > f.z ? f.y : f.z; // max refl
    if (depth > 100) {
      return obj.e; // *** Added to prevent stack overflow
    }
    if (++depth > 5) if (random.NextDouble() < p) f = f * (1 / p); else return obj.e; //R.R.
    if (obj.refl == 0) {         // Ideal DIFFUSE reflection
      double r1 = 2 * Math.PI * random.NextDouble(), r2 = random.NextDouble(), r2s = Math.Sqrt(r2);
      Vec w = nl, u = ((Math.Abs(w.x) > .1 ? new Vec(0, 1, 0) : new Vec(1, 0, 0)) % w).norm(), v = w % u;
      Vec d = (u * Math.Cos(r1) * r2s + v * Math.Sin(r1) * r2s + w * Math.Sqrt(1 - r2)).norm();
      return obj.e + f.mult(radiance(spheres, new Ray(x, d), depth, random));
    }
    else if (obj.refl == 1) // Ideal SPECULAR reflection
      return obj.e + f.mult(radiance(spheres, new Ray(x, r.d - n * 2 * n.dot(r.d)), depth, random));
    Ray reflRay = new Ray(x, r.d - n * 2 * n.dot(r.d));// Ideal dielectric REFRACTION
    bool into = n.dot(nl) > 0;                   // Ray from outside going in?
    double nc = 1, nt = 1.5, nnt = into ? nc / nt : nt / nc, ddn = r.d.dot(nl), cos2t;
    if ((cos2t = 1 - nnt * nnt * (1 - ddn * ddn)) < 0)        // Total internal reflection
      return obj.e + f.mult(radiance(spheres, reflRay, depth, random));
    Vec tdir = (r.d * nnt - n * ((into ? 1 : -1) * (ddn * nnt + Math.Sqrt(cos2t)))).norm();
    double a = nt - nc, b = nt + nc, R0 = a * a / (b * b), c = 1 - (into ? -ddn : tdir.dot(n));
    double Re = R0 + (1 - R0) * c * c * c * c * c, Tr = 1 - Re, P = .25 + .5 * Re, RP = Re / P, TP = Tr / (1 - P);
    return obj.e + f.mult(depth > 2 ? (random.NextDouble() < P ?   // Russian roulette
          radiance(spheres, reflRay, depth, random) * RP 
          : radiance(spheres, new Ray(x, tdir), depth, random) * TP) 
        : radiance(spheres, reflRay, depth, random) * Re + radiance(spheres, new Ray(x, tdir), depth, random) * Tr);
  }
  private static void Main() {
    int w = 200, h = 200, samps = 25; 
    Ray cam = new Ray(new Vec(50, 52, 295.6), new Vec(0, -0.042612, -1).norm()); // cam pos, dir
    Vec cx = new Vec(w * .5135 / h, 0, 0), cy = (cx % cam.d).norm() * .5135; 
    var c = new Vec[w*h];
    var random = new Random(12345);
    
    Sphere[] spheres = new Sphere[9];
    spheres[0] = new Sphere(1e5, new Vec( 1e5+1,40.8,81.6), new Vec(0,0,0),new Vec(.75,.25,.25),0); //Left
    spheres[1] = new Sphere(1e5, new Vec(-1e5+99,40.8,81.6),new Vec(0,0,0),new Vec(.25,.25,.75),0); //Rght
    spheres[2] = new Sphere(1e5, new Vec(50,40.8, 1e5),     new Vec(0,0,0),new Vec(.75,.75,.75),0); //Back
    spheres[3] = new Sphere(1e5, new Vec(50,40.8,-1e5+170), new Vec(0,0,0),new Vec(0,0,0),      0); //Frnt
    spheres[4] = new Sphere(1e5, new Vec(50, 1e5, 81.6),    new Vec(0,0,0),new Vec(.75,.75,.75),0); //Botm
    spheres[5] = new Sphere(1e5, new Vec(50,-1e5+81.6,81.6),new Vec(0,0,0),new Vec(.75,.75,.75),0); //Top
    spheres[6] = new Sphere(16.5,new Vec(27,16.5,47),       new Vec(0,0,0),new Vec(1,1,1)*.999, 1); //Mirr
    spheres[7] = new Sphere(16.5,new Vec(73,16.5,78),       new Vec(0,0,0),new Vec(1,1,1)*.999, 2); //Glas
    spheres[8] = new Sphere(600, new Vec(50,681.6-.27,81.6),new Vec(12,12,12),  new Vec(0,0,0), 0); //Lite

    for (int i = 0; i < (w * h); i++) {
      var x = i % w;
      var y = h - i / w - 1;
      var color = new Vec(0,0,0);
      for (int sy = 0; sy < 2; sy++) { // 2x2 subpixel rows
        for (int sx = 0; sx < 2; sx++) { // 2x2 subpixel cols
          Vec r = new Vec(0,0,0);
          for (int s = 0; s < samps; s++) {
            double r1 = 2 * random.NextDouble(), dx = r1 < 1 ? Math.Sqrt(r1) - 1 : 1 - Math.Sqrt(2 - r1);
            double r2 = 2 * random.NextDouble(), dy = r2 < 1 ? Math.Sqrt(r2) - 1 : 1 - Math.Sqrt(2 - r2);
            Vec d = cx * (((sx + .5 + dx) / 2 + x) / w - .5) + cy * (((sy + .5 + dy) / 2 + y) / h - .5) + cam.d;
            d = d.norm();
            r = r + radiance(spheres, new Ray(cam.o + d * 140, d), 0, random) * (1.0 / samps);
          } 
          color = color + new Vec(clamp(r.x), clamp(r.y), clamp(r.z)) * .25;
        }
      }
      c[i] = color;
    }
    
    Console.WriteLine("P3");
    Console.Write(w); Console.Write(' ');
    Console.Write(h); Console.WriteLine(" 255");
    for (int i = 0; i < w * h; i++) {
      Console.Write(toInt(c[i].x)); Console.Write(' ');
      Console.Write(toInt(c[i].y)); Console.Write(' ');
      Console.Write(toInt(c[i].z)); Console.WriteLine(' ');
    }
  }
}

