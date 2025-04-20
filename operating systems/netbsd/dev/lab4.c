#include <sys/module.h>
#include <sys/kernel.h>
#include <uvm/uvm.h>

typedef uint16_t u16;

MODULE(MODULE_CLASS_MISC, lab4, NULL);

static void lab4_modload(void) {
    vaddr_t addr = uvm_km_alloc(kernel_map, 10*PAGE_SIZE, UVM_KMF_PAGEABLE);

    if (addr == 0) {
        printf("uvm_km_alloc failed\n");
        return;
    }

    struct pglist list;

    uvm_pglistalloc(5*PAGE_SIZE, 0, (paddr_t)(-1), 0, 0, &list, 5, 0);

    {
        struct vm_page* page = TAILQ_FIRST(&list);

        for (int i = 0; i < 5; ++i) {
            vaddr_t page_va = addr + i * PAGE_SiZE;

            pmap_kenter_pa(page_va, page->phys_addr, VM_PROT_READ | VM_PROT_WRITE, PMAP_NOCACHE);

            page = TAILQ_NEXT(page, pageq.queue);
        }
        
        pmap_update(pmap_kernel());
    }

    for (int i = 0; i < 10; ++i) {
        vaddr_t page_va = addr + i * PAGE_SIZE;
        paddr_t page_pa;

        bool valid = pmap_extract(pmap_kernel(), page_va, &page_pa);
        bool accessed = valid;

        if (valid) {
            // against CPU caching
            const u16 prev = *(u16*)(page_va);
            *(u16*)(page_va) = 0xDEAD;

            pmap_update(pmap_kernel());

            paddr_t new_page_pa;
            bool still_valid = pmap_extract(pmap_kernel(), page_va, &new_page_pa);

            accessed = still_valid && new_page_pa == page_pa;

            *(u16*)(page_pa) = prev;
        }

        printf("Page %d\n", i+1);
        printf("Physical address: 0xlx\n", page_pa);
        printf("Valid: %s\n", (valid ? "Yes" : "No"));
        printf("Accessed: %s\n", (accessed ? "Yes" : "No"));
    }

    {
        for (int i = 0; i < 5; ++i) {
            vaddr_t page_va = addr + i * PAGE_SIZE;
            pmap_kremove(page_va, PAGE_SIZE);
        }

        pmap_update(pmap_kernel());
    }

    uvm_pglistfree(&list);
    uvm_km_free(kernel_map, addr, 10 * PAGE_SIZE, UVM_KMF_PAGEABLE);
}

static int lab4_modcmd(modcmd_t cmd, void* arg) {
    switch (cmd) {
        case MODULE_CMD_INIT:
            lab4_modload();
            break;
        case MODULE_CMD_FINI:
            printf("sad\n");
            break;
        default:
            break;
    }

    return 0;
}
