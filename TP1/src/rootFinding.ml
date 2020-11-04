let rec rootDeriv df a b =
    let dfa = df a and dfb = df b in
        if dfa > 0. && dfb < 0. then
            Root1D.brent df a b
        else
            let mid = ((a +. b) /. 2.) in
                let dfm = df mid in
                    if dfm = 0. then
                        mid
                    else if dfm > 0. then
                        rootDeriv df mid b
                    else
                        rootDeriv df a mid;;

let rootFinding f df a b =
    let fa = f a and fb = f b in
        if fa > 0. && fb > 0. then
            []
        else if fa = 0. && fb > 0. then
            [a]
        else if fa > 0. && fb = 0. then
            [b]
        else
            let m = rootDeriv df a b in
                if fa > 0. && fb <= 0. then
                    let x = Root1D.brent f m b in
                        [x]
                else if fa <= 0. && fb > 0. then
                    let x = Root1D.brent f a m in
                        [x]
                else
                    let fm = f m in
                        if fm > 0. then
                            let x = Root1D.brent f a m and
                                y = Root1D.brent f m b in
                                [x; y]
                        else if fm = 0. then
                            [m]
                        else
                            [];;