class Geometry {
  static EPSILL = 1e-7;
  static isPointInsideCircle(x, y, r, px, py) {
    let spec = { x, y, r };
    let pt = { x: px, y: py };
    const distancesquared = (pt.x - spec.x) * (pt.x - spec.x) + (pt.y - spec.y) * (pt.y - spec.y);
    return distancesquared <= spec.r * spec.r;
  }
  static isPointInsidePolygon(poly, x, y) {
    const pt = { x, y };
    for (var c = false, i = -1, l = poly.length, j = l - 1; ++i < l; j = i) ((poly[i][1] <= pt.y && pt.y < poly[j][1]) || (poly[j][1] <= pt.y && pt.y < poly[i][1])) && pt.x < ((poly[j][0] - poly[i][0]) * (pt.y - poly[i][1])) / (poly[j][1] - poly[i][1]) + poly[i][0] && (c = !c);
    return c;
  }
  static getPointByRadian(x, y, radian, length) {
    const nX = x + Math.cos(radian) * length;
    const nY = y + Math.sin(radian) * length;
    return [nX, nY];
  }
  static getRadianBetweenPoints(x, y, xe, ye) {
    return Math.atan2(ye - y, xe - x);
  }
  static areCirclesOverlapped(x0, y0, r0, x1, y1, r1) {
    const x = x0 - x1;
    const y = y0 - y1;
    return r0 + r1 > Math.sqrt(x * x + y * y);
  }
  static areLinesOverlapped(a, b, c, d, p, q, r, s) {
    let det = (c - a) * (s - q) - (r - p) * (d - b);
    if (det === 0) return false;
    let lambda = ((s - q) * (r - a) + (p - r) * (s - b)) / det;
    let gamma = ((b - d) * (r - a) + (c - a) * (s - b)) / det;
    return 0 < lambda && lambda < 1 && 0 < gamma && gamma < 1;
  }
  static getPointOnArc(cx, cy, cr, dx, dy) {
    let circle = { x: cx, y: cy, r: cr };
    let dot = { x: dx, y: dy };
    let loop = true;
    let pll = circle.r;
    let angle = Math.atan2(circle.y - dot.y, circle.x - dot.x) + Math.PI;
    while (loop) {
      dot.x = circle.x + Math.cos(angle) * pll;
      dot.y = circle.y + Math.sin(angle) * pll;
      let a = circle.x - dot.x;
      let b = circle.y - dot.y;
      if (Math.sqrt(a * a + b * b) >= circle.r) {
        // console.log(dot)
        return [dot.x, dot.y];
        return dot; //
      } else {
        pll += Geometry.EPSILL;
      }
    }
  }
  static doesLineInterceptCircle(Ax, Ay, Bx, By, Cx, Cy, radius) {
    let dist;
    const A = { x: Ax, y: Ay };
    const B = { x: Bx, y: By };
    const C = { x: Cx, y: Cy };
    const v1x = B.x - A.x;
    const v1y = B.y - A.y;
    const v2x = C.x - A.x;
    const v2y = C.y - A.y;
    const u = (v2x * v1x + v2y * v1y) / (v1y * v1y + v1x * v1x);
    if (u >= 0 && u <= 1) {
      dist = (A.x + v1x * u - C.x) ** 2 + (A.y + v1y * u - C.y) ** 2;
    } else {
      dist = u < 0 ? (A.x - C.x) ** 2 + (A.y - C.y) ** 2 : (B.x - C.x) ** 2 + (B.y - C.y) ** 2;
    }
    return dist < radius * radius;
  }
  static distanceBetweenPoints(point1x, point1y, point2x, point2y) {
    const a = point1x - point2x;
    const b = point1y - point2y;
    return Math.sqrt(a * a + b * b);
  }
  static Intersect = class {
    static lineCircle(sx, sy, eex, eey, x, y, r) {
      const getPointOnArc = Geometry.getPointOnArc;
      let line = { first: { x: sx, y: sy }, second: { x: eex, y: eey } };
      let circle = { x, y, r };
      let distance_1, distance_2;
      let msqrt = Math.sqrt;
      let mdrs = null;
      let lx = line.second.x - line.first.x,
        ly = line.second.y - line.first.y;
      let len = msqrt(lx * lx + ly * ly);
      let dx = lx / len,
        dy = ly / len;
      let t = dx * (circle.x - line.first.x) + dy * (circle.y - line.first.y);
      let ex = t * dx + line.first.x,
        ey = t * dy + line.first.y;
      let lec = msqrt((ex - circle.x) * (ex - circle.x) + (ey - circle.y) * (ey - circle.y));
      for (let bomo = 0; bomo < 3; bomo++) {
        if (bomo > 1) {
          bomo = -1;
        }
        if (true) {
          let lf1 = line.first.x - circle.x,
            lf2 = line.first.y - circle.y,
            ls1 = line.second.x - circle.x,
            ls2 = line.second.y - circle.y,
            boel = Geometry.EPSILL * bomo;
          distance_1 = msqrt(lf1 * lf1 + lf2 * lf2) + boel;
          distance_2 = msqrt(ls1 * ls1 + ls2 * ls2) + boel;
        }
        let compare_1 = distance_1 >= circle.r;
        let compare_2 = distance_2 >= circle.r;
        let compare_data = (compare_1 && distance_2 < circle.r) || (compare_2 && distance_1 < circle.r);
        if (lec < circle.r) {
          let dt = msqrt(circle.r * circle.r - lec * lec) - Geometry.EPSILL;
          let t_m_dt = t - dt;
          let t_p_dt = t + dt;
          //---
          let te = dx * lx + dy * ly;
          let t_m_dt_lt_0_or_t_m_dt_gt_te = t_m_dt < 0 || t_m_dt > te;
          let t_p_dt_lt_0_or_t_p_dt_gt_te = t_p_dt < 0 || t_p_dt > te;
          if (!compare_data && t_m_dt_lt_0_or_t_m_dt_gt_te && t_p_dt_lt_0_or_t_p_dt_gt_te) {
            mdrs = [];
          } else if (compare_data && t_m_dt_lt_0_or_t_m_dt_gt_te) {
            mdrs = [getPointOnArc(circle.x, circle.y, circle.r, t_p_dt * dx + line.first.x, t_p_dt * dy + line.first.y)];
          } else if (compare_data && t_p_dt_lt_0_or_t_p_dt_gt_te) {
            mdrs = [getPointOnArc(circle.x, circle.y, circle.r, t_m_dt * dx + line.first.x, t_m_dt * dy + line.first.y)];
          }
          //---
          if (!mdrs && compare_1 && compare_2) {
            mdrs = [getPointOnArc(circle.x, circle.y, circle.r, t_m_dt * dx + line.first.x, t_m_dt * dy + line.first.y), getPointOnArc(circle.x, circle.y, circle.r, t_p_dt * dx + line.first.x, t_p_dt * dy + line.first.y)];
          }
        } else if (lec == circle.r) {
          let result1 = { x: ex, y: ey };
          let result2 = { x: ex, y: ey };
          let angle__forward = Math.atan2(circle.y - result1.y, circle.x - result1.x);
          let distance22 = Geometry.distanceBetweenPoints(circle.x, circle.y, result1.x, result1.y) + Geometry.EPSILL;
          let lll = Geometry.getPointByRadian(circle.x, circle.y, angle__forward, distance22);
          let int = Geometry.Intersect.lines(circle.x, circle.y, lll.x, lll.y, line.first.x, line.first.y, line.second.x, line.second.y);
          if (int) {
            result1 = getPointOnArc(circle.x, circle.y, circle.r, result1.x, result1.y);
            result2 = getPointOnArc(circle.x, circle.y, circle.r, result2.x, result2.y);
            mdrs = [result1, result2];
          }
        }
        if (!mdrs && !compare_data) {
          mdrs = [];
        }
        if (mdrs !== null || bomo == -1) {
          return mdrs;
          return mdrs.map((dot) => [dot.x, dot.y]);
        }
      }
      return [];
      // };
    }
    static lines(line1StartX, line1StartY, line1EndX, line1EndY, line2StartX, line2StartY, line2EndX, line2EndY) {
      let denominator, a, b, numerator1, numerator2;
      denominator = (line2EndY - line2StartY) * (line1EndX - line1StartX) - (line2EndX - line2StartX) * (line1EndY - line1StartY);
      if (denominator == 0) return;
      a = line1StartY - line2StartY;
      b = line1StartX - line2StartX;
      numerator1 = (line2EndX - line2StartX) * a - (line2EndY - line2StartY) * b;
      numerator2 = (line1EndX - line1StartX) * a - (line1EndY - line1StartY) * b;
      a = numerator1 / denominator;
      b = numerator2 / denominator;
      if (a > 0 && a < 1 && b > 0 && b < 1) {
        return [line1StartX + a * (line1EndX - line1StartX), line1StartY + a * (line1EndY - line1StartY)];
      }
      return;
    }
    static circles(x0, y0, r0, x1, y1, r1) {
      let a, dx, dy, d, h, rx, ry;
      let x2, y2;
      dx = x1 - x0;
      dy = y1 - y0;
      d = Math.sqrt(dy * dy + dx * dx);
      if (d > r0 + r1) return false;
      if (d < Math.abs(r0 - r1)) return false;
      a = (r0 * r0 - r1 * r1 + d * d) / (2.0 * d);
      x2 = x0 + (dx * a) / d;
      y2 = y0 + (dy * a) / d;
      h = Math.sqrt(r0 * r0 - a * a);
      rx = -dy * (h / d);
      ry = dx * (h / d);
      let xi = x2 + rx;
      let xi_prime = x2 - rx;
      let yi = y2 + ry;
      let yi_prime = y2 - ry;
      return [
        [xi, yi],
        [xi_prime, yi_prime],
      ];
    }
  };
}
