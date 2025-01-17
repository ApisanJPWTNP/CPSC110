;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname f-p4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(@assignment exams/2024w1-f/f-p4) ;Do not edit or remove this tag

(@cwl csaps)   ;fill in your CWL here

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line
(@problem 4) ;do not edit or delete this line


(define THICKNESS 2)

#|

Carefully study the explanation in f-p4-figure.pdf, then complete the design
of the function below by writing appropriate tests, the template origin tag,
and the function definition.  

NOTE: This problem will be autograded, and ALL OF THE FOLLOWING ARE ESSENTIAL
      IN YOUR SOLUTION.  Failure to follow these requirements may result in
      receiving zero marks for this problem.

 - The function you design MUST BE CALLED ray-star.

 - You MUST USE the provided THICKNESS constant.
 
 - You MUST NOT COMMENT out any @ metadata tags.
 
 - You MUST FOLLOW all applicable design rules.
 
 - The file MUST NOT have any errors when the Check Syntax button is pressed.

 - We are providing one check-expect, which you MUST NOT EDIT OR COMMENT OUT.

 - You must add more tests.

 - The function definition MUST call one or more built-in abstract functions.

 - You must define a single top-level function with the given name. You are
   permitted to define helpers, but they must be defined within the top-level
   function using local.

 - The function definition and any helper functions you design MUST NOT be
   recursive.

 - The result of the function must directly be the result of one of the
   built-in abstract functions. So, for example, the following would not
   be a valid function body:

       (define (foo x)
         (empty? (filter ...)))

   This would be a valid function body:

       (define (foo x)
         (local [(define (helper y) (foldr ... ... ...))]
           (helper ...)))

|#

;; 360/num of line

(@htdf ray-star)
(@signature Number Natural Color -> Image)
;; produce n lines overlaid on centers, rotated 360/n each from the previous
(check-expect (ray-star 100 3 "red")
              (overlay (rotate   0 (rectangle 100 THICKNESS "solid" "red"))
                       (rotate 120 (rectangle 100 THICKNESS "solid" "red"))
                       (rotate 240 (rectangle 100 THICKNESS "solid" "red"))))
(check-expect (ray-star 200 3 "blue")
              (overlay (rotate   0 (rectangle 200 THICKNESS "solid" "blue"))
                       (rotate 120 (rectangle 200 THICKNESS "solid" "blue"))
                       (rotate 240 (rectangle 200 THICKNESS "solid" "blue"))))
(check-expect (ray-star 100 4 "blue")
              (overlay (rotate  0 (rectangle 100 THICKNESS "solid" "blue"))
                       (rotate 90 (rectangle 100 THICKNESS "solid" "blue"))
                       (rotate 180 (rectangle 100 THICKNESS "solid" "blue"))
                       (rotate 270 (rectangle 100 THICKNESS "solid" "blue"))
                       (rotate 360 (rectangle 100 THICKNESS "solid" "blue"))))
(check-expect (ray-star 1000 5 "green")
              (overlay (rotate  0 (rectangle 1000 THICKNESS "solid" "green"))
                       (rotate 72 (rectangle 1000 THICKNESS "solid" "green"))
                       (rotate 144 (rectangle 1000 THICKNESS "solid" "green"))
                       (rotate (* 72 3)
                               (rectangle 1000 THICKNESS "solid" "green"))
                       (rotate (* 72 4)
                               (rectangle 1000 THICKNESS "solid" "green"))))
(check-expect (ray-star 50 10 "purple")
              (overlay (rotate  0 (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 1)
                               (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 2)
                               (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 3)
                               (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 4)
                               (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 5)
                               (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 6)
                               (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 7)
                               (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 8)
                               (rectangle 50 THICKNESS "solid" "purple"))
                       (rotate (* 36 9)
                               (rectangle 50 THICKNESS "solid" "purple"))))
(check-expect (ray-star 100 0 "red") empty-image)

;(define (ray-star diameter n-lines color) empty-image) ;stub

(@template-origin use-abstract-fn)

(define (ray-star diameter n-lines color)
  (foldr overlay empty-image
         (build-list n-lines
                     (λ (n) (rotate (* n (/ 360 n-lines))
                                    (rectangle diameter THICKNESS "solid"
                                               color))))))
