let reply_pkt rxtime txtime qp curtime = (* for NTP server functionality *)
    let leap = NoWarning in
    let version = 4 in
    let mode = Server in
    let stratum = Primary in
    let poll = qp.poll in
    let precision = -6 in
    let root_delay = {seconds = 1; fraction = 0} in
    let root_dispersion = {seconds = 1; fraction = 0} in
    let refid = Int32.of_string "0x43414d4c" in

    let reference_ts = curtime in
    let origin_ts = qp.trans_ts in
    let recv_ts = rxtime in
    let trans_ts = txtime in
    {leap;version;mode; stratum; poll; precision; root_delay; root_dispersion; refid; reference_ts; origin_ts; recv_ts; trans_ts}

