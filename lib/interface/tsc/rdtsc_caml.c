#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

CAMLprim value caml_rdtsc( )
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return Val_long( ((unsigned long long)lo)|( ((unsigned long long)hi)<<32 ));
}
