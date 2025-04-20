add_definitions(-D_LAB2CLASS_)

add_library(lab2 MODULE lab2.c lab2.rc)

set_module_type(lab2 kernelmodedriver)
add_importlibs(lab2 ntoskrnl)
add_cd_file(TARGET lab2 DESTINATION reactos/system32/drivers FOR all)
