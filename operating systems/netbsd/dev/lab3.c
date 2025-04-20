#include <sys/module.h>
#include <sys/kernel.h>
#include <sys/proc.h>

MODULE(MODULE_CLASS_MISC, lab3, NULL);

static int lab3_modcmd(modcmd_t cmd, void* arg) {
    struct proc* p;

    printf("Process list:\n");

    PROCLIST_FOREACH(p, &allproc) {
        const pid_t parent_pid = p->p_pptr ? p->p_pptr->p_pid : -1;
        printf("%d: %s (%d)\n", p->p_pid, p->p_comm, parent_pid);
    }

    return 0;
}
