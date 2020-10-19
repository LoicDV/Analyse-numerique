let rootFinding f df a b =
    (* Rappel : nous ne pouvons avoir que MAX 2 racines car unimodale *)
    let fa = f a and fb = f b and dfa = df a and dfb = df b in

        if (fa < 0. && fb > 0.) || (fa < 0. && fb = 0. && dfb > 0.) then
            let x = Root1D.brent f a b in
                [x]

        else if (fa > 0. && fb < 0.) || (fa = 0. && fb < 0. && dfa < 0.) then
            let x = Root1D.brent f a b in
                [x]

        else if fa = 0. && fb = 0. then
            [a; b]

        else if fa = 0. && fb > 0. then
            [a]

        else if fa > 0. && fb = 0. then
            [b]

        else if fa < 0. && fb = 0. && dfb < 0. then
            let m = Root1D.brent df a b in
                let x = Root1D.brent f a m in
                    [x; m]

        else if fa = 0. && fb < 0. && dfa > 0. then
            let m = Root1D.brent df a b in
                let x = Root1D.brent f m b in
                    [a; x]

        else if fa < 0. && fb < 0. && (dfa *. dfb) < 0. then
            let m = Root1D.brent df a b in
                let x = Root1D.brent f a m and y = Root1D.brent f m b in
                    [x; y]

        else
	    []