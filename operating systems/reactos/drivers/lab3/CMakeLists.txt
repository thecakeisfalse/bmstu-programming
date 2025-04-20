add_definitions(-D_LAB3CLASS_)

add_library(lab3 MODULE lab3.c lab3.rc)

set_module_type(lab3 kernelmodedriver)
add_importlibs(lab3 ntoskrnl)
add_cd_file(TARGET lab3 DESTINATION reactos/system32/drivers FOR all)
