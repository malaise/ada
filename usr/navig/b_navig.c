int  __main_priority = -1;
int  __time_slice_val = -1;
char __locking_policy = ' ';
char __queuing_policy = ' ';
char __task_dispatching_policy = ' ';

extern int gnat_argc;
extern char **gnat_argv;
extern char **gnat_envp;
extern int gnat_exit_status;
void adainit ()
{
   system___elabs ();
   interfaces__c_streams___elabs ();
   system__powten_table___elabs ();
   system__exception_table___elabb ();
   ada__calendar___elabs ();
   ada__io_exceptions___elabs ();
   ada__numerics___elabs ();
   interfaces__c___elabs ();
   system__interrupt_management___elabs ();
   system__interrupt_management___elabb ();
   system__interrupt_management__operations___elabb ();
   ada__tags___elabs ();
   ada__tags___elabb ();
   ada__streams___elabs ();
   ada__exceptions___elabs ();
   system__task_info___elabs ();
   system__tasking_soft_links___elabb ();
   system__secondary_stack___elabb ();
   system__time_operations___elabb ();
   system__finalization_root___elabs ();
   system__finalization_implementation___elabs ();
   ada__finalization___elabs ();
   ada__finalization__list_controller___elabs ();
   system__file_control_block___elabs ();
   ada__text_io___elabs ();
   system__file_io___elabb ();
   ada__text_io___elabb ();
   system__tasking___elabs ();
   system__task_primitives__operations___elabb ();
   system__tasking__initialization___elabb ();
   system__tasking__utilities___elabs ();
   system__tasking__rendezvous___elabb ();
   system__task_timer___elabs ();
   ada__calendar__delays___elabs ();
   ada__real_time__delays___elabs ();
   system__task_timer___elabb ();
   system__interrupts___elabb ();
   con_io___elabs ();
   math___elabs ();
   math___elabb ();
   nav_data___elabs ();
   nav_format___elabs ();
   nav_screen___elabs ();
   nav_screen___elabb ();
   nav_dialog___elabb ();
   nav_data___elabb ();
   text_handler___elabs ();
   argument___elabs ();
   x_mng___elabs ();
   x_mng___elabb ();
   con_io___elabb ();
}
void adafinal () {
   system__tasking__stages__finalize_global_tasks ();
}
int main (argc, argv, envp)
    int argc;
    char **argv;
    char **envp;
{
   gnat_argc = argc;
   gnat_argv = argv;
   gnat_envp = envp;
 
   __gnat_initialize();
   adainit();
 
   _ada_navig ();
 
   adafinal();
   __gnat_finalize();
   exit (gnat_exit_status);
}
unsigned navigB = 0x57EF3862;
unsigned system__standard_libraryB = 0x4B7932E9;
unsigned system__standard_libraryS = 0x79B018CE;
unsigned nav_dataB = 0x2FD4E207;
unsigned nav_dataS = 0x2FD6474F;
unsigned mathB = 0x18112D8F;
unsigned mathS = 0x345425DB;
unsigned adaS = 0x2359F9ED;
unsigned ada__numericsS = 0x0443320F;
unsigned systemS = 0x08FBDA7E;
unsigned system__exception_tableB = 0x2066F787;
unsigned system__exception_tableS = 0x19C2AE08;
unsigned gnatS = 0x156A40CF;
unsigned gnat__htableB = 0x0C2B72B8;
unsigned gnat__htableS = 0x599BF48E;
unsigned ada__numerics__auxS = 0x64310BE0;
unsigned ada__text_ioB = 0x05C8666D;
unsigned ada__text_ioS = 0x7CA0EE43;
unsigned ada__streamsS = 0x7C25DE96;
unsigned ada__tagsB = 0x4AA39140;
unsigned ada__tagsS = 0x0A72F2E2;
unsigned system__secondary_stackB = 0x7D09B545;
unsigned system__secondary_stackS = 0x36DDFD40;
unsigned system__task_specific_dataB = 0x714B839E;
unsigned system__task_specific_dataS = 0x47178527;
unsigned system__tasking_soft_linksB = 0x10094824;
unsigned system__tasking_soft_linksS = 0x7A49706B;
unsigned system__storage_elementsB = 0x6FD7DF62;
unsigned system__storage_elementsS = 0x5B2FF7B1;
unsigned interfacesS = 0x0357E00A;
unsigned interfaces__c_streamsB = 0x29A89FE2;
unsigned interfaces__c_streamsS = 0x4DDCB504;
unsigned system__file_ioB = 0x02DB4CE6;
unsigned system__file_ioS = 0x66141777;
unsigned ada__finalizationB = 0x4F0184F2;
unsigned ada__finalizationS = 0x0A0669D8;
unsigned system__finalization_rootB = 0x26610831;
unsigned system__finalization_rootS = 0x1E9694A4;
unsigned system__stream_attributesB = 0x3E43967C;
unsigned system__stream_attributesS = 0x55C81A60;
unsigned ada__io_exceptionsS = 0x34054F96;
unsigned system__unsigned_typesS = 0x362290AA;
unsigned system__finalization_implementationB = 0x2D05109B;
unsigned system__finalization_implementationS = 0x2C2D53BC;
unsigned ada__exceptionsB = 0x4BE10F9C;
unsigned ada__exceptionsS = 0x4CED7A40;
unsigned system__string_opsB = 0x6E258F4E;
unsigned system__string_opsS = 0x260A1D23;
unsigned system__file_control_blockS = 0x7B3BF0FA;
unsigned ada__finalization__list_controllerB = 0x144D9D8E;
unsigned ada__finalization__list_controllerS = 0x05F2A9BF;
unsigned system__parametersB = 0x5AE99D6B;
unsigned system__parametersS = 0x47327F4D;
unsigned ada__text_io__float_auxB = 0x09BCEF1E;
unsigned ada__text_io__float_auxS = 0x1569EDF5;
unsigned ada__text_io__generic_auxB = 0x78A37038;
unsigned ada__text_io__generic_auxS = 0x77134B76;
unsigned system__img_realB = 0x4874ABD1;
unsigned system__img_realS = 0x7207087A;
unsigned system__fat_llfS = 0x34C0D34E;
unsigned system__img_lluB = 0x327658F4;
unsigned system__img_lluS = 0x365A4C95;
unsigned system__img_unsB = 0x04FCDB0C;
unsigned system__img_unsS = 0x0E07D0DF;
unsigned system__powten_tableS = 0x7893525A;
unsigned system__val_realB = 0x6E7AABE6;
unsigned system__val_realS = 0x4F1238F4;
unsigned system__exn_llfS = 0x670FF1D2;
unsigned system__exn_genB = 0x72152961;
unsigned system__exn_genS = 0x325B6B9E;
unsigned system__val_utilB = 0x43F8A78C;
unsigned system__val_utilS = 0x6B7B6F1B;
unsigned gnat__case_utilB = 0x50DFD047;
unsigned gnat__case_utilS = 0x240BBC41;
unsigned calendarS = 0x4F3F7603;
unsigned ada__calendarB = 0x7C642A5A;
unsigned ada__calendarS = 0x2D46F565;
unsigned system__arith_64B = 0x2A62AD9A;
unsigned system__arith_64S = 0x4E72F5BE;
unsigned system__time_operationsB = 0x1604F321;
unsigned system__time_operationsS = 0x45706DC1;
unsigned interfaces__cB = 0x5359610B;
unsigned interfaces__cS = 0x42FE8D9A;
unsigned system__error_reportingB = 0x388666EE;
unsigned system__error_reportingS = 0x027342F4;
unsigned system__os_interfaceB = 0x451547BD;
unsigned system__os_interfaceS = 0x49500CC8;
unsigned system__val_intB = 0x720C563A;
unsigned system__val_intS = 0x45FF83FC;
unsigned system__val_unsB = 0x1B64BB05;
unsigned system__val_unsS = 0x6CBA7A61;
unsigned text_ioS = 0x04039200;
unsigned nbres_cB = 0x43373861;
unsigned nbres_cS = 0x37452EC9;
unsigned nav_typesB = 0x10DB7923;
unsigned nav_typesS = 0x48D1D723;
unsigned nav_dialogB = 0x776CA363;
unsigned nav_dialogS = 0x45B8F1C0;
unsigned con_ioB = 0x4797CE2C;
unsigned con_ioS = 0x5FDCEE9F;
unsigned argumentB = 0x7B1D4623;
unsigned argumentS = 0x6665D7EF;
unsigned loc_argB = 0x45691FFD;
unsigned loc_argS = 0x7F6A1E62;
unsigned ada__command_lineB = 0x2490234E;
unsigned ada__command_lineS = 0x010A1D94;
unsigned text_handlerB = 0x2321DFCD;
unsigned text_handlerS = 0x3C4E4F5E;
unsigned sys_callsB = 0x66315511;
unsigned sys_callsS = 0x425BC704;
unsigned x_mngB = 0x7F7F55CC;
unsigned x_mngS = 0x64727DAD;
unsigned nav_formatB = 0x39919EED;
unsigned nav_formatS = 0x09D11BB2;
unsigned normalB = 0x76EEACD4;
unsigned system__exn_lfltS = 0x7A982D1F;
unsigned system__img_intB = 0x79CE2327;
unsigned system__img_intS = 0x294E114F;
unsigned nav_screenB = 0x2D2751C5;
unsigned nav_screenS = 0x3F6B717B;
unsigned ada__calendar__delaysB = 0x159649CF;
unsigned ada__calendar__delaysS = 0x149C924C;
unsigned system__compiler_exceptionsB = 0x29FFDD03;
unsigned system__compiler_exceptionsS = 0x3C0D3AEF;
unsigned system__task_primitivesS = 0x41573C90;
unsigned system__task_primitives__operationsB = 0x52C41ADE;
unsigned system__task_primitives__operationsS = 0x40156BBC;
unsigned system__interrupt_managementB = 0x0C370B28;
unsigned system__interrupt_managementS = 0x4F7820DA;
unsigned system__taskingB = 0x4BACBE52;
unsigned system__taskingS = 0x0435CB29;
unsigned system__task_infoS = 0x116FEDBA;
unsigned system__task_timerB = 0x0C972780;
unsigned system__task_timerS = 0x4B0CA56D;
unsigned ada__real_timeB = 0x6FB35142;
unsigned ada__real_timeS = 0x6D3CE549;
unsigned ada__real_time__delaysB = 0x0F020599;
unsigned ada__real_time__delaysS = 0x2ABCD4EC;
unsigned system__tasking__stagesB = 0x33DE9D81;
unsigned system__tasking__stagesS = 0x57E9B39F;
unsigned system__tasking__initializationB = 0x49199C97;
unsigned system__tasking__initializationS = 0x7C03C1E5;
unsigned system__tasking__utilitiesB = 0x04516D5E;
unsigned system__tasking__utilitiesS = 0x3BB5A6F4;
unsigned system__interruptsB = 0x02E7B26C;
unsigned system__interruptsS = 0x7EE63C44;
unsigned ada__task_identificationB = 0x7B8B9A1B;
unsigned ada__task_identificationS = 0x48FBA2BA;
unsigned system__address_imageB = 0x0AFE69D5;
unsigned system__address_imageS = 0x14A37527;
unsigned system__tasking__abortionB = 0x6D4270A3;
unsigned system__tasking__abortionS = 0x409231AA;
unsigned system__tasking__rendezvousB = 0x7053FC4C;
unsigned system__tasking__rendezvousS = 0x44A44BEA;
unsigned system__tasking__entry_callsB = 0x5A06AA84;
unsigned system__tasking__entry_callsS = 0x6A015D0F;
unsigned system__tasking__protected_objectsB = 0x36F466A3;
unsigned system__tasking__protected_objectsS = 0x4FDD919B;
unsigned system__tasking__queuingB = 0x3E812CD9;
unsigned system__tasking__queuingS = 0x247FAC01;
unsigned system__interrupt_management__operationsB = 0x412376A1;
unsigned system__interrupt_management__operationsS = 0x5ACA7DD0;
unsigned day_mngB = 0x54B1BBFC;
unsigned day_mngS = 0x02AE433A;
/* BEGIN Object file/option list
/users/malaise/ada/reposit/loc_arg.o
/users/malaise/ada/reposit/math.o
/users/malaise/ada/reposit/day_mng.o
/users/malaise/ada/reposit/normal.o
./nav_types.o
./nav_format.o
./nav_screen.o
./nav_dialog.o
./navig.o
/users/malaise/ada/reposit/nbres_c.o
./nav_data.o
/users/malaise/ada/reposit/sys_calls.o
/users/malaise/ada/reposit/text_handler.o
/users/malaise/ada/reposit/argument.o
/users/malaise/ada/reposit/x_mng.o
/users/malaise/ada/reposit/con_io.o
-lm
-lpthreads
   END Object file/option list */
