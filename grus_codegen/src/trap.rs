/*!
  Tooling to print the general purpose registers when an int3 happens.
*/
use libc::{c_int, c_void};
use log::debug;

#[no_mangle]
extern "C" fn sigtrap_handler(_sig: c_int, info: *mut libc::siginfo_t, context: *mut c_void) {
    unsafe {
        debug!("Sigtrap!");

        if (*info).si_signo != libc::SIGTRAP {
            panic!(
                "Expected SIGTRAP error in handler, got {}.",
                (*info).si_signo
            );
        }

        // Cast the context
        let ucontext = std::mem::transmute::<_, *mut libc::ucontext_t>(context);
        let mcontext = &mut (*ucontext).uc_mcontext;

        // The general purpose reigsters.
        // EAX   ECX   EDX   EBX   ESP   EBP   ESI   EDI
        debug!("REG_RAX 0x{:0>8x}", mcontext.gregs[libc::REG_RAX as usize]);
        debug!("REG_RCX 0x{:0>8x}", mcontext.gregs[libc::REG_RCX as usize]);
        debug!("REG_RDX 0x{:0>8x}", mcontext.gregs[libc::REG_RDX as usize]);
        debug!("REG_RBX 0x{:0>8x}", mcontext.gregs[libc::REG_RBX as usize]);
        debug!("REG_RSP 0x{:0>8x}", mcontext.gregs[libc::REG_RSP as usize]);
        debug!("REG_RSI 0x{:0>8x}", mcontext.gregs[libc::REG_RSI as usize]);
        debug!("REG_RDI 0x{:0>8x}", mcontext.gregs[libc::REG_RDI as usize]);

        // The instruction position
        debug!("REG_RIP 0x{:0>8x}", mcontext.gregs[libc::REG_RIP as usize]);
    }
    // Return from the sigtrap handler allows continuing from where it was triggered, in this case after the int3.
}

pub fn setup_int3() {
    unsafe {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction =
            std::mem::transmute::<_, usize>(sigtrap_handler as extern "C" fn(_, _, _));
        action.sa_flags = libc::SA_SIGINFO;

        let r = libc::sigaction(libc::SIGTRAP, &action, std::ptr::null_mut());
        println!("sigaction register: {r:?}");
    }
}
