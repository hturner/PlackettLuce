# output of print.PlackettLuce is correct

    Call: PlackettLuce(rankings = R)
    
    Coefficients:
      apple   banana   orange     tie2  
     0.0000   0.2868  -0.4637  -1.0701  

# output of print.coef.PlackettLuce is correct

         apple     banana     orange       tie2 
     0.0000000  0.2867987 -0.4636687 -1.0701129 

# output of print.summary.PlackettLuce is correct

    Call: PlackettLuce(rankings = R)
    
    Coefficients:
           Estimate Std. Error z value Pr(>|z|)
    apple    0.0000         NA      NA       NA
    banana   0.2868     1.0129   0.283    0.777
    orange  -0.4637     1.0280  -0.451    0.652
    tie2    -1.0701     0.9043  -1.183    0.237
    
    Residual deviance:  21.61 on 25 degrees of freedom
    AIC:  27.61 
    Number of iterations: 5

# output of fitted.PlackettLuce is correct

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["choices", "alternatives", "ranking", "fitted", "n"]
        },
        "nchoices": {
          "type": "integer",
          "attributes": {},
          "value": [14]
        },
        "objects": {
          "type": "character",
          "attributes": {},
          "value": ["apple", "banana", "orange"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["aggregated_choices", "data.frame"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 3]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [3, 5]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [4]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [4]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [6]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.36656777, 0.33785994, 0.33086831, 0.07962156, 0.2536189, 0.58554515, 0.52604625]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 2, 1, 1, 1]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["choices", "alternatives", "ranking", "fitted", "n"]
        },
        "nchoices": {
          "type": "integer",
          "attributes": {},
          "value": [14]
        },
        "objects": {
          "type": "character",
          "attributes": {},
          "value": ["apple", "banana", "orange"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 3]
            }
          ]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 2, 3, 4, 4, 5, 6]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.36656777, 0.33785994, 0.33086831, 0.07962156, 0.2536189, 0.58554515, 0.07962156, 0.52604625]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["choices", "alternatives", "ranking", "fitted", "n"]
        },
        "nchoices": {
          "type": "integer",
          "attributes": {},
          "value": [14]
        },
        "objects": {
          "type": "character",
          "attributes": {},
          "value": ["apple", "banana", "orange"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["aggregated_choices", "data.frame"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2, 3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [3]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 3]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [2, 3, 5]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [3, 5]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [4]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [4]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [4, 6]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [6]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.36656777, 1, 0.33785994, 0.33086831, 1, 0.07962156, 0.2536189, 0.58554515, 1, 0.52604625]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 3, 2, 1, 1, 2, 1]
        }
      ]
    }

