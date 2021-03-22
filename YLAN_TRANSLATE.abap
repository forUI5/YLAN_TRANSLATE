*&---------------------------------------------------------------------*
*& Report YLAN_TRANSLATE
*&---------------------------------------------------------------------*
*&BY 小懒
*&---------------------------------------------------------------------*
report ylan_translate.

tables:tadir.

*&---------------------------------------------------------------------*
* TYPES
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* CONSTANTS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* GLOBAL DATA
*&---------------------------------------------------------------------*
* ALV变量
*CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
data:
  gv_container type ref to cl_gui_docking_container, "容器类
*  GO_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER,
  go_alvgrid   type ref to  cl_gui_alv_grid.

data:
  gt_fieldcat      type lvc_t_fcat,
  gt_excluding     type ui_functions,
  gs_layout        type lvc_s_layo,
  gs_styl          type lvc_s_styl,
  gt_styl          type lvc_t_styl,
  gt_events        type slis_t_event with header line,
  gt_event_exit    type slis_t_event_exit with header line,
  gs_grid_settings type lvc_s_glay.

data: begin of gt_alv occurs 0,
        sel,
        trkorr type trkorr,
        rtype  type bapi_mtype,
        rtmsg  type bapi_msg.
        include structure tadir.
      data:
            end of gt_alv.

*全局变量
data:
  gv_msg(255),
  gv_program        like sy-repid value sy-repid, "程序名
  gv_package        type devclass,
  gv_request        type trkorr,
  gv_xml            type string,
  gv_filename       like ibipparms-path,
  gv_state, "程序执行状态
  gv_rtype,
  gv_rtmsg(255),
  gs_rfcsi          type rfcsi,
  gv_codepage       type cpcodepage,
  gv_namespace(250),
  gv_init,
  gv_notif_time(30), "通知时间
  gv_name_c(30). "当前资源库对象的所属用户

selection-screen begin of block b1.
select-options:
s_pack for tadir-devclass,
s_obj for tadir-obj_name,
s_korr for tadir-korrnum,
s_auth for tadir-author.
selection-screen end of block b1.

start-of-selection.
  perform frm_data_fetch.
  perform frm_data_output.
*&---------------------------------------------------------------------*
*& Form FRM_DATA_FETCH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_data_fetch .
  ranges:
  lr_object for tadir-object.
  lr_object-sign = 'I'.
  lr_object-option = 'EQ'.
  lr_object-low = 'PROG'.
  append lr_object.
*  lr_object-low = 'CLAS'.
*  append lr_object.
*  lr_object-low = 'FUNC'.
*  append lr_object.

  select *
    into corresponding fields of table @gt_alv
    from tadir
    where devclass in @s_pack and obj_name in @s_obj and korrnum in @s_korr and author in @s_auth
    and object in @lr_object.

endform.

*&---------------------------------------------------------------------*
*& FORM FRM_DATA_OUTPUT
*&---------------------------------------------------------------------*
*& TEXT
*&---------------------------------------------------------------------*
*& -->  P1        TEXT
*& <--  P2        TEXT
*&---------------------------------------------------------------------*
form frm_data_output .
  perform frm_alv_layout.
  perform frm_alv_fieldcat.
  perform frm_alv_call.
endform.

form frm_alv_layout .
  gs_layout-cwidth_opt = 'X'.
  gs_layout-sel_mode = 'A'.
  gs_layout-zebra = 'X'.
  gs_layout-box_fname = 'SEL'.
*  GS_LAYOUT-STYLEFNAME = 'TSTYL'.
*  GS_LAYOUT-EXCP_FNAME = 'LED'.
endform.                    "FRM_ALV_LAYOUT

form frm_alv_call .
  data lv_title(70).

  gs_grid_settings-edt_cll_cb = 'X'.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_grid_title             = lv_title
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_ALV_STATUS_SET'
      i_callback_user_command  = 'FRM_ALV_USER_COMMAND'
      i_grid_settings          = gs_grid_settings
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat[]
      i_save                   = 'A'
      it_events                = gt_events[]
      it_event_exit            = gt_event_exit[]
    tables
      t_outtab                 = gt_alv.

endform.

form frm_alv_fieldcat .
  perform frm_build_fieldcat tables gt_fieldcat using 'TRKORR' '请求' space space space space space space space.
  perform frm_build_fieldcat tables gt_fieldcat using 'RTYPE' '类型' space space space space space space space.
  perform frm_build_fieldcat tables gt_fieldcat using 'RTMSG' '消息' space space space space space space space.
  perform frm_fieldcatalog_merge tables gt_fieldcat using 'TADIR'.


endform.

form frm_build_fieldcat tables lt_fieldcat structure lvc_s_fcat
using fieldname coltext ref_tab ref_field convexit drop_down_handle f4availabl checktable edit.
  clear lt_fieldcat.
  lt_fieldcat-fieldname = fieldname.
  lt_fieldcat-coltext = coltext.
  lt_fieldcat-ref_table = ref_tab.
  lt_fieldcat-ref_field = ref_field.
  lt_fieldcat-convexit  = convexit.
  lt_fieldcat-drdn_field = drop_down_handle.
  lt_fieldcat-f4availabl = f4availabl.
  lt_fieldcat-checktable = checktable.
  lt_fieldcat-edit = edit.

  append lt_fieldcat.
endform.                    "BUILD_FIELDCAT

form frm_alv_status_set using rt_extab type slis_t_extab.
  data: begin of lt_excltab occurs 0,
          fcode like sy-ucomm,
        end of lt_excltab.

*  IF P_CREATE = 'X' OR P_CREAT2 = 'X'.
*    LT_EXCLTAB-FCODE = 'CHANGE'.
*    APPEND LT_EXCLTAB.
*  ENDIF.

  set pf-status 'STATUS_ALV' excluding lt_excltab.
endform.                    "PF_STATUS_SET

form frm_alv_user_command using l_ucomm like sy-ucomm
      ls_selfield type slis_selfield.

  case l_ucomm.
    when 'CODE_DIS'.
      perform frm_text_create using 'X'.
    when 'CREATE'.
      perform frm_text_create using ''.
    when 'DOWNLOAD'.
      perform frm_text_download.
    when 'UPLOAD'.
      perform frm_text_upload.
    when 'REQUEST'.
      perform frm_request.
  endcase.

  perform frm_alv_refresh.
endform.

*&---------------------------------------------------------------------*
*& FORM FRM_ALV_REFRESH
*&---------------------------------------------------------------------*
*& TEXT
*&---------------------------------------------------------------------*
*& -->  P1        TEXT
*& <--  P2        TEXT
*&---------------------------------------------------------------------*

form frm_alv_refresh.
  data l_grid type ref to cl_gui_alv_grid.

  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = go_alvgrid.

  data:
    ls_stable     type lvc_s_stbl,
    ls_layout     type lvc_s_layo,
    lt_columns    type lvc_t_col,
    lt_index_rows type  lvc_t_row,
    lt_row_no     type  lvc_t_roid.

  ls_stable-row = 'X'.
  ls_stable-col = 'X'.

  clear: lt_columns,ls_layout.

  call method go_alvgrid->get_frontend_layout
    importing
      es_layout = ls_layout.

  ls_layout-cwidth_opt = 'X'.

  call method go_alvgrid->set_frontend_layout
    exporting
      is_layout = ls_layout.

  call method go_alvgrid->get_selected_columns "获取选择列
    importing
      et_index_columns = lt_columns.

  call method go_alvgrid->get_selected_rows "获取选择行
    importing
      et_index_rows = lt_index_rows
      et_row_no     = lt_row_no.



  call method go_alvgrid->refresh_table_display
    exporting
      is_stable      = ls_stable
      i_soft_refresh = 'X'
    exceptions
      finished       = 1
      others         = 2.
*
  call method go_alvgrid->set_selected_columns "设置回刷新前的列
    exporting
      it_col_table = lt_columns.

  call method go_alvgrid->set_selected_rows "设置回刷新前的行
    exporting
      it_index_rows = lt_index_rows
      it_row_no     = lt_row_no.


endform.                    " FRM_ALV_REFRESH

form frm_fieldcatalog_merge  tables t_fieldcat structure lvc_s_fcat
                             using uv_structure.
  data lt_fieldcat like table of lvc_s_fcat with header line.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = uv_structure
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    changing
      ct_fieldcat            = lt_fieldcat[]
    exceptions
      inconsistent_interface = 1
      program_error          = 2.

  append lines of lt_fieldcat to t_fieldcat.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_TEXT_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_text_create using uv_display.
  data:begin of lt_code occurs 0,
         line(720),
       end of lt_code.
  data:
    lt_textpool_main like table of textpool with default key with header line,
    lt_textpool      like table of textpool with default key with header line.

  data:
    lt_str        like table of lt_code with header line,
    lv_count      type i,
    lv_mod        type i,
    lv_numc3      type n length 3,
    lv_trans,
    lv_transed,
    lv_serial(10),
    lv_lines      type i.

*语法检查
  data: itab type standard table of string,
        mess type string,
        lin  type i,
        wrd  type string,
        dir  type trdir.

  loop at gt_alv where sel = 'X'.
    clear:lt_textpool_main[],lt_textpool[].

*分析代码
    read report gt_alv-obj_name into lt_code.
    loop at lt_code.
      replace all occurrences of `'` in lt_code-line with `'@#`. "占位符
      split lt_code-line at `'` into table lt_str.
      clear lt_code-line.
      describe table lt_str lines lv_lines.
      loop at lt_str.
        data(lv_tabix) = sy-tabix - 1.
        find '@#' in lt_str-line.
        if sy-subrc = 0.
          lv_count = lv_count + 1.
        endif.
        if lv_trans = 'X'.
*判断是否已经翻译过
          find '@#(' in lt_str-line.
          if sy-subrc ne 0.
            lv_numc3 = lv_numc3 + 1.
            lv_serial = `'` && `(` && lv_numc3 && ')'.
*收集文本
            read table lt_str into data(ls_str_l) index lv_tabix.
            lt_textpool-id = 'I'.
*            lt_textpool-key = lv_numc3.
            lt_textpool-entry =  ls_str_l-line .
            replace all occurrences of `@#` in lt_textpool-entry with ''.
            lt_textpool-length = 132.
            append lt_textpool.
*替换代码
            replace '@#' in lt_str-line with lv_serial.
          else.
            replace '@#' in lt_str-line with `'`.
          endif.
          clear lv_trans.
          lv_transed = 'X'.
        else.
          lv_mod = lv_count mod 2.
          if  lv_mod = 1..
            data(lv_left) = 'X'. "左边的引号
          else.
            data(lv_right) = 'X'.
          endif.
        endif.

*        data(lv_pattern) = '^['  && p_regul && '@#]+$'.
        if lv_transed = 'X'.
          clear lv_transed.
        elseif lv_lines > 1 "没有引号就不判断了
        and lt_str-line(1) ne '*' "排除注释行
        and lv_left = 'X'.
*          if lt_str-line is not initial and cl_abap_matcher=>matches( pattern = lv_pattern
*                                             text    = lt_str-line )
*                   = abap_false.
*            lv_trans = 'X'.
*          endif.
          call method cl_abap_matcher=>matches
            exporting
              pattern = '[^[:unicode:]]+$' " 中文及中文字符
              text    = lt_str-line
            receiving
              success = lv_trans. "包含中文返回空
          if lv_trans = 'X'.
            lv_trans = ''.
          else.
            lv_trans = 'X'.
          endif.
        endif.
        if lv_left = 'X'.
          replace '@#' in lt_str-line with ` '`.
        endif.
        lt_code-line = lt_code-line && lt_str-line.
        clear:lv_left,lv_right.
      endloop.
      replace all occurrences of '@#' in lt_code-line with `'`.
      modify lt_code.
    endloop.

    if uv_display = 'X'. "展示程序代码
      cl_demo_output=>display( lt_code[] ).
    else.
*找主程序
      data lv_obj_name_main like gt_alv-obj_name.
      perform frm_main_prog_get using gt_alv-obj_name gt_alv-object changing lv_obj_name_main.
      if lv_obj_name_main is initial.
        gt_alv-rtype = 'E'.
        gt_alv-rtmsg = gt_alv-rtmsg && 'INLCUDE找不到主程序或有多个主程序'.
      else.
*更新代码
        data(lv_i) = ''.
*语法检查
        select single *
               from trdir
               where name = @gt_alv-obj_name
               into @dir.
        dir-uccheck = 'X'. "不检查unicode
        syntax-check for lt_code message mess line lin word wrd
                     directory entry dir.
        if sy-subrc = 0
          or mess = '缺少 REPORT/PROGRAM 语句，或程序类型是 INCLUDE。or the program type is INCLUDE.'. "include让过
          insert report gt_alv-obj_name from lt_code.
          if sy-subrc = 0.
            gt_alv-rtype = 'S'.
            gt_alv-rtmsg = '更新程序代码成功；'.
          endif.
*更新文本池
          read textpool lv_obj_name_main into lt_textpool_main language '1'.
*读到最大的Key开始编号
          sort lt_textpool_main by key descending.
          read table lt_textpool_main with key id = 'I'.
          if sy-subrc = 0.
            lv_numc3 = lt_textpool_main-key.
          else.
            clear lv_numc3.
          endif.
          loop at lt_textpool.
            lv_numc3 = lv_numc3 + 1.
            lt_textpool-key = lv_numc3.
            append lt_textpool to lt_textpool_main.
          endloop.
          sort lt_textpool_main by key.
          insert textpool lv_obj_name_main from lt_textpool_main language '1'.
          if sy-subrc = 0.
            gt_alv-rtype = 'S'.
            gt_alv-rtmsg = gt_alv-rtmsg && '更新文本池成功'.
          endif.
*包含请求
          perform frm_request_set using  'PROG' gt_alv-obj_name  changing gv_package gv_request gv_rtype gv_rtmsg. "程序包入请求 以SE03为准
          perform frm_request_set using  'REPT' gt_alv-obj_name  changing gv_package gv_request gv_rtype gv_rtmsg. "程序包入请求 以SE03为准
          gt_alv-trkorr = gv_request.
        else.
          gt_alv-rtype = 'E'.
          gt_alv-rtmsg = '有语法错误，未更新源码'.
        endif.
      endif.
    endif.
    modify gt_alv.
  endloop.
  if sy-subrc ne 0.
    message e001(00) with '至少选择一行'.
  endif.

endform.

form frm_request_set  using  p_type p_program
                           changing p_gv_package ordernum rtype rtmsg.

  data:
    lv_ordernum    like  e070-trkorr,
    lv_devclass    like tadir-devclass,
    lv_global_lock.

  clear:rtype,rtmsg.

  if p_type = 'FUGR' "不加锁在包函数组时会报错："这一语法是不能用于对象名称"
    or p_type = 'ENQU'
    or p_type = 'NROB'.
    lv_global_lock = 'X'.
  endif.

  free memory id 'RESUL1'. "重复包函数时会返回空请求号，debug找到缓存位置
  lv_ordernum = ordernum.
  call function 'RS_CORR_INSERT'
    exporting
      korrnum             = lv_ordernum
      global_lock         = lv_global_lock
      object              = p_program
      object_class        = p_type
      devclass            = p_gv_package
      master_language     = '1'
*     mode                = 'INSERT' "如果是insert每次都会创建对象条目目录
*     object_class_supports_ma = 'X'
    importing
      ordernum            = lv_ordernum
      devclass            = lv_devclass
    exceptions
      cancelled           = 1
      permission_failure  = 2
      unknown_objectclass = 3
      others              = 4.
  if sy-subrc ne 0.
    rtype = 'E'.
*    perform msg_sys_into changing rtmsg.
  else.

    "如果函数组在请求中，则函数跟随函数组（且请求中只有函数组没有函数，不返回请求号算了）
*    if lv_devclass is initial.
*      lv_devclass = '$TMP'.
*      lv_ordernum = lv_devclass.
*    endif.

    p_gv_package = lv_devclass.
    ordernum = lv_ordernum.

    rtype = 'S'.
    rtmsg = '创建请求成功'.
  endif.
endform.                    " frm_request_set
*&---------------------------------------------------------------------*
*& Form FRM_TEXT_DOWNLOAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_text_download .
  data:
    lt_textpool      like table of textpool with default key with header line,
    lv_langu         like sy-langu,
    lv_fieldname(40).
  data:begin of ls_export,
         obj_name type char255,
         id       type char255,
         key      type char255,
         entry_1  type char255,
         entry_e  type char255,
         length   type char255,
       end of ls_export.
  data:
    lt_export       like sorted  table of ls_export with header line with unique  key obj_name id key,
    lt_export_excel like table of lt_export with header line,
    lt_alv_c        like table of gt_alv with header line.

  loop at gt_alv where sel = 'X'.
    perform frm_main_prog_get using gt_alv-obj_name gt_alv-object changing lt_alv_c-obj_name.
    if lt_alv_c-obj_name is not initial.
      collect lt_alv_c.
    endif.
  endloop.
  if sy-subrc ne 0.
    message e001(00) with '至少选择一行'.
  endif.

  loop at lt_alv_c.
    clear:lt_export,lt_export[].
    do 2 times.
      case sy-index.
        when 1.
          lv_langu = '1'.
        when 2.
          lv_langu = 'E'.
        when others.
      endcase.
      clear:lt_textpool,lt_textpool[].
      read textpool lt_alv_c-obj_name into lt_textpool language  lv_langu. "测试可以不指定语言
      loop at lt_textpool where entry ne 'D       .'. "从数据字典带出来的
        clear lt_export.
        move-corresponding lt_textpool to lt_export.
        lv_fieldname = 'ENTRY_' && lv_langu.
        assign component lv_fieldname of structure lt_export to  field-symbol(<lv_entry>).
        <lv_entry> = lt_textpool-entry.
        if sy-index = 1.
          insert lt_export into table lt_export.
        else.
          read table lt_export into data(ls_export2) with key obj_name = '' id = lt_textpool-id key = lt_textpool-key binary search.
          if sy-subrc = 0.
            assign component lv_fieldname of structure ls_export2 to <lv_entry>.
            <lv_entry> = lt_textpool-entry.
            modify lt_export from ls_export2 index sy-tabix.
          endif.
        endif.
      endloop.
    enddo.
    loop at lt_export.
      move-corresponding lt_export to lt_export_excel.
      lt_export_excel-obj_name = lt_alv_c-obj_name.
      append lt_export_excel.
    endloop.
  endloop.

*抬头
*  lt_export_excel[] = lt_export[].
  lt_export_excel-obj_name = '对象目录中的对象名称'.
  lt_export_excel-id = 'ABAP 文本池标识（选择文本/编号文本）'.
  lt_export_excel-key = '文本元素关键字 （序号/选择名）'.
  lt_export_excel-entry_1 = '中文文本'.
  lt_export_excel-entry_e = '英文文本'.
  lt_export_excel-length = '文本保留长度（含语言保留）'.
  insert lt_export_excel index 1.

*下载
  data:lv_filename type string.
*pop
  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = 'EXPORT.XLSX'
      def_path         = 'C:'
      mask             = ' EXCEL Files (*.XLSX)|*.XLSX||'
      mode             = 'S'
    importing
      filename         = lv_filename
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.
  if sy-subrc <> 0.
  endif.
  data:
        lv_fname type rlgrap-filename.
  lv_fname = lv_filename.
  call function 'SAP_CONVERT_TO_XLS_FORMAT'
    exporting
      i_filename        = lv_fname
    tables
      i_tab_sap_data    = lt_export_excel
    exceptions
      conversion_failed = 1
      others            = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form FRM_TEXT_UPLOAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_text_upload .
  data:
    lt_textpool     like table of textpool with default key with header line,
    lt_textpool_old like table of textpool with default key with header line, "更新前的文本池
    lv_langu        like sy-langu,
    lv_fname(40),
    lv_n3           type n length 3.
  data:begin of lt_export occurs 0,
         obj_name like tadir-obj_name,
         id       type char255,
         key      type char255,
         entry_1  type char255,
         entry_e  type char255,
         length   type char255,
       end of lt_export.

*上传文件地址对话框
  data:
    lo_obj       type ref to cl_gui_frontend_services,
    lt_file_list type filetable with header line,
    lv_rc        type i.

*上传EXCEL
  data:
    lt_file   type standard table of alsmex_tabline,
    ls_file   type alsmex_tabline,
    lv_filenm like rlgrap-filename.



  field-symbols:
    <fs>.

*  call function 'F4_FILENAME'
*    importing
*      file_name = lv_file_name.

  create object lo_obj.
  call method lo_obj->file_open_dialog
    exporting
      file_filter       = '*.xlsx'
      initial_directory = 'C:\'
    changing
      file_table        = lt_file_list[]
      rc                = lv_rc.
  read table lt_file_list index 1.
  if sy-subrc ne 0.
    return.
  endif.
  lv_filenm = lt_file_list-filename.

* File
  perform frm_excel_upload tables lt_export using lv_filenm.
  loop at lt_export.
    if lt_export-id = 'I'.
      lv_n3 = lt_export-key.
      lt_export-key = lv_n3.
    endif.
    modify lt_export.
  endloop.
  sort lt_export by obj_name id key.

  do 2 times.
    if sy-index = 1.
      lv_langu ='1'.
    elseif sy-index = 2.
      lv_langu ='E'.
    endif.
    lv_fname = 'ENTRY_' && lv_langu.
    loop at lt_export.
      data(lv_tabix) = sy-tabix.
      at new obj_name.
        clear:lt_textpool[].
      endat.
      move-corresponding lt_export to lt_textpool.
      assign component lv_fname of structure lt_export to field-symbol(<lv_entry>).
      lt_textpool-entry = <lv_entry>.
      append lt_textpool.
      at end of obj_name.
        read table lt_export index lv_tabix.
*保留参考数据字典的屏幕字段
        clear:lt_textpool_old[].
        read textpool lt_export-obj_name into lt_textpool_old.
        loop at lt_textpool_old where entry = 'D       .'.
          append lt_textpool_old to lt_textpool.
        endloop.
        insert textpool lt_export-obj_name from lt_textpool language lv_langu.
        if sy-subrc ne 0.
          data(lv_err) = 'X'.
        endif.
      endat.
    endloop.

  enddo.

  if lv_err is initial.
    message s001(00) with '更新文本池成功'.
  else.
    message s001(00) with '更新文本池出错' display like 'E'.
  endif.

endform.

form frm_excel_upload  tables   p_lt_upload_string
                       using    p_lv_filename.

  data:
    lt_file   type standard table of alsmex_tabline,
    ls_file   type alsmex_tabline,
    lv_filenm like rlgrap-filename.

  field-symbols:
    <fs>.

* File
  lv_filenm = p_lv_filename.

  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = lv_filenm
      i_begin_col             = 1
      i_begin_row             = 2 "
      i_end_col               = 100 " 最大列数
      i_end_row               = 10000  "最大行数
    tables
      intern                  = lt_file
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  catch system-exceptions convt_no_number = 1.
    loop at lt_file into ls_file.
      on change of ls_file-row.
        if sy-tabix ne 1.
          append p_lt_upload_string to p_lt_upload_string.
          clear  p_lt_upload_string.
        endif.
      endon.
      assign component ls_file-col of structure p_lt_upload_string to <fs>.  "动态方法将值传到相应的内表
      if <fs> is assigned.
        <fs> = ls_file-value.
      endif.
    endloop.

    append p_lt_upload_string to p_lt_upload_string.

  endcatch.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_REQUEST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_request .
  data:
        lv_obj_name_main like gt_alv-obj_name.
  loop at gt_alv where sel = 'X'.
    perform frm_request_set using  'PROG' gt_alv-obj_name  changing gv_package gv_request gv_rtype gv_rtmsg. "程序包入请求 以SE03为准
    perform frm_main_prog_get using gt_alv-obj_name gt_alv-object changing lv_obj_name_main.
    if lv_obj_name_main is not initial.
      perform frm_request_set using  'REPT' lv_obj_name_main  changing gv_package gv_request gv_rtype gv_rtmsg. "程序包入请求 以SE03为准
    endif.
    gt_alv-trkorr = gv_request.
    modify gt_alv.
  endloop.
  if sy-subrc ne 0.
    message e001(00) with '至少选择一行'.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_MAIN_PROG_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_ALV_OBJ_NAME
*&      <-- GT_ALV_OBJ_NAME_MAIN
*&---------------------------------------------------------------------*
form frm_main_prog_get  using    uv_obj_name uv_object
                        changing cv_obj_name_main.

  data: begin of lt_mainprograms occurs 0,
          mainprogram type char40,
        end of lt_mainprograms .

  data: lv_number_of_mainprograms type i.

  clear cv_obj_name_main.

  if uv_object = 'PROG'.
    call function 'RS_GET_MAINPROGRAMS'
      exporting
        name                   = uv_obj_name
      importing
        number_of_mainprograms = lv_number_of_mainprograms
      tables
        mainprograms           = lt_mainprograms
      exceptions
        cancelled              = 1.

    if lv_number_of_mainprograms > 1.
    else.
      read table lt_mainprograms index 1.
      cv_obj_name_main = lt_mainprograms-mainprogram.
    endif.
  else. "函数或者CLASS
*DATA FUGR_GROUP            TYPE RS38L-AREA.
*DATA FUGR_INCLUDE_NUMBER   TYPE TFDIR-INCLUDE.
*DATA SLDB_NAME             TYPE LDBD-LDBNAME.
*DATA MENU_NAME             TYPE TSTC-TCODE.
*DATA TYPE_NAME             TYPE TRDIR-NAME.
*DATA MST_NAME              TYPE DD02L-TABNAME.
*DATA CNTX_NAME             TYPE TRDIR-NAME.
    data clas_name             type seoclass-clsname.
*DATA INTF_NAME             TYPE SEOCLASS-CLSNAME.
*DATA FUGR_PROGNAME_GROUP   TYPE TRDIR-NAME.
*DATA FUGR_PROGNAME_INCLUDE TYPE TRDIR-NAME.
*DATA FUGR_PROGNAME_TOP     TYPE TRDIR-NAME.
*DATA FUGR_PROGNAME_UXX     TYPE TRDIR-NAME.
*DATA SLDB_PROGNAME_DB      TYPE TRDIR-NAME.
*DATA SLDB_PROGNAME_SEL     TYPE TRDIR-NAME.
*DATA MENU_PROGNAME         TYPE TRDIR-NAME.
*DATA TYPE_PROGNAME         TYPE TRDIR-NAME.
*DATA MST_PROGNAME          TYPE TRDIR-NAME.
*DATA CNTX_PROGNAME         TYPE TRDIR-NAME.
*DATA INTF_PROGNAME         TYPE TRDIR-NAME.
    data clas_progname         type trdir-name.

    clas_name = uv_obj_name.

    call function 'RS_PROGNAME_CONCATENATE'
      exporting
*       FUGR_GROUP      = FUGR_GROUP
*       FUGR_INCLUDE_NUMBER         = FUGR_INCLUDE_NUMBER
*       SLDB_NAME       = SLDB_NAME
*       MENU_NAME       = MENU_NAME
*       TYPE_NAME       = TYPE_NAME
*       MST_NAME        = MST_NAME
*       CNTX_NAME       = CNTX_NAME
        clas_name       = clas_name
*       INTF_NAME       = INTF_NAME
* IMPORTING
*       FUGR_PROGNAME_GROUP         = FUGR_PROGNAME_GROUP
*       FUGR_PROGNAME_INCLUDE       = FUGR_PROGNAME_INCLUDE
*       FUGR_PROGNAME_TOP           = FUGR_PROGNAME_TOP
*       FUGR_PROGNAME_UXX           = FUGR_PROGNAME_UXX
*       SLDB_PROGNAME_DB            = SLDB_PROGNAME_DB
*       SLDB_PROGNAME_SEL           = SLDB_PROGNAME_SEL
*       MENU_PROGNAME   = MENU_PROGNAME
*       TYPE_PROGNAME   = TYPE_PROGNAME
*       MST_PROGNAME    = MST_PROGNAME
*       CNTX_PROGNAME   = CNTX_PROGNAME
*       INTF_PROGNAME   = INTF_PROGNAME
        clas_progname   = clas_progname
      exceptions
        delimiter_error = 1.
    cv_obj_name_main = clas_progname.
  endif.
endform.
