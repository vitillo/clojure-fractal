(ns fractal.core
  (:use quil.core))

(defn square [x]
  (* x x))

(defn eu-distance [p]
  (let [x (first p)
        y (second p)
        z (last p)]
    (sqrt (+ (square x)
             (square y)
             (square z)))))

(def max-iteration 100)

(def color-table
  (let [mix (rand-nth [[255 144 30]
                       [30 244 144]
                       [144 30 255]])
        average #(/ (+ %1 %2) 2)
        average-color #(map average %1 %2)]
    (vec (sort-by eu-distance
                  (for [x (range (* max-iteration))]
                    (let [rand-color [(rand 255) (rand 255) (rand 255)]]
                      (average-color mix rand-color)))))))

(defn setup []
  (smooth)
  (frame-rate 60)
  (background 200))

(defn relative-x [value]
  (- (* value 
        (/ 2.7 (width)))
     1.7))

(defn relative-y [value]
  (- (* value
        (/ 2.0 (height)))
     1.0))

(defn interpolate [t c0 c1]
  (+ (* (- 1 t) c0)
     (* t c1)))

(defn mandelbrot [x0 y0]
  (loop [x 0
         y 0
         iteration 0]
    (if (and (< iteration (dec max-iteration))
             (< (+ (* x x)
                   (* y y))
                4))
      (recur (+ x0
                (* x x)
                (- (* y y)))
             (+ y0
                (* 2 x y))
             (inc iteration))
      (do
        (if (< iteration (dec max-iteration))
          (let [zn (sqrt (+ (* x x) (* y y)))
                nu (/ (log (/ (log zn)
                              (log 2)))
                      (log 2))
                color1 (color-table iteration)
                color2 (color-table (inc iteration))
                interpolate (partial interpolate (- 1 nu))]
            (map interpolate color1 color2))
          '(0 0 0))))))

(defn draw []
  (background 0)
  (dorun (for [x (range (- (width) 1))
               y (range (- (height) 1))]
           (let [x0 (relative-x x)
                 y0 (relative-y y)
                 c (mandelbrot x0 y0)]
             (set-pixel x
                        y
                        (apply color c))))))


(defsketch mandel
  :title "MandelBrot"
  :setup setup
  :draw draw
  :size [640 480])
