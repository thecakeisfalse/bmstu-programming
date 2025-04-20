add_definitions(-D_LAB4CLASS_)

add_library(lab4 MODULE lab4.c lab4.rc)

set_module_type(lab4 kernelmodedriver)
add_importlibs(lab4 ntoskrnl)
add_cd_file(TARGET lab4 DESTINATION reactos/system32/drivers FOR all)
