type counter = Cstruct.uint64

type stamp = {
    ta:     counter;
    tb:     float;
    te:     float;
    tf:     counter;
}

type quality = NG | OK

type sample = {
    quality:    quality;
    uuid:       Cstruct.uint64;
    rank:       Cstruct.uint64;
    ip:         Ipaddr.V6.t;
    ttl:        int;
    stratum:    int;
    leap:       Wire.leap;
    refid:      Cstruct.uint32;
    rootdelay:  float;
    rootdisp:   float;
    stamp:      stamp;
}


