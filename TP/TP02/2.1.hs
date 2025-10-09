classify :: Int -> String
classify n
    | n <= 9    = "failed"
    | n <= 12   = "passed"
    | n <= 15   = "good"
    | n <= 18   = "very good"
    | n <= 20   = "excellent"