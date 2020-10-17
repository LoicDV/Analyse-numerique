open Root1d;;

let rootFinding f df a b =
    (* Rappel : nous ne pouvons avoir que MAX 2 racines car unimodale *)
    let fa = f a and fb = f b and dfa = df a and dfb = df b in

        if (fa < 0. && fb > 0.) || (fa < 0. && fb = 0. && dfb > 0.) then
            let x = Root1d.brent f a b in
                x;

        if (fa > 0. && fb < 0.) || (fa = 0. && fb < 0. && dfa < 0.) then
            let x = Root1d.brent f a b in
                x;

        if fa = 0. && fb = 0. then
            a; b;

        if fa = 0. && fb > 0. then
            a;

        if fa > 0. && fb = 0. then
            b;

        if fa < 0. && fb = 0. && dfb < 0. then
            let m = Root1d.brent df a b in
                let x = Root1d.brent f a m in
                    x; m;

        if fa = 0. && fb < 0. && dfa > 0. then
            let m = Root1d.brent df a b in
                let x = Root1d.brent f m b in
                    a; x;

        if fa < 0. && fb < 0. && (dfa *. dfb) < 0. then
            let m = Root1d.brent df a b in
                let x = Root1d.brent f a m and y = Root1d.brent f m b in
                    x; y;

        None;;