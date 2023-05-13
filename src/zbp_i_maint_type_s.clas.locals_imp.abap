CLASS lhc_rap_tdat_cts DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      get
        RETURNING
          VALUE(result) TYPE REF TO if_mbc_cp_rap_table_cts.

ENDCLASS.

CLASS lhc_rap_tdat_cts IMPLEMENTATION.
  METHOD get.
    result = mbc_cp_api=>rap_table_cts( table_entity_relations = VALUE #(
                                         ( entity = 'MaintType' table = 'ZMAINT_TYPE' )
                                       ) ) ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.
CLASS lhc_zi_mainttype_s DEFINITION FINAL INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING
                  keys   REQUEST requested_features FOR MaintTypeAll
        RESULT    result,
      selectcustomizingtransptreq FOR MODIFY
        IMPORTING
                  keys   FOR ACTION MaintTypeAll~SelectCustomizingTransptReq
        RESULT    result,
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR MaintTypeAll
        RESULT result.
ENDCLASS.

CLASS lhc_zi_mainttype_s IMPLEMENTATION.
  METHOD get_instance_features.
    DATA: selecttransport_flag TYPE abp_behv_flag VALUE if_abap_behv=>fc-o-enabled,
          edit_flag            TYPE abp_behv_flag VALUE if_abap_behv=>fc-o-enabled.

    IF cl_bcfg_cd_reuse_api_factory=>get_cust_obj_service_instance(
        iv_objectname = 'ZMAINT_TYPE'
        iv_objecttype = cl_bcfg_cd_reuse_api_factory=>simple_table )->is_editable( ) = abap_false.
      edit_flag = if_abap_behv=>fc-o-disabled.
    ENDIF.
    DATA(transport_service) = cl_bcfg_cd_reuse_api_factory=>get_transport_service_instance(
                                iv_objectname = 'ZMAINT_TYPE'
                                iv_objecttype = cl_bcfg_cd_reuse_api_factory=>simple_table ).
    IF transport_service->is_transport_allowed( ) = abap_false.
      selecttransport_flag = if_abap_behv=>fc-o-disabled.
    ENDIF.
    READ ENTITIES OF ZI_MaintType_S IN LOCAL MODE
    ENTITY MaintTypeAll
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(all).
    IF all[ 1 ]-%is_draft = if_abap_behv=>mk-off.
      selecttransport_flag = if_abap_behv=>fc-o-disabled.
    ENDIF.
    result = VALUE #( (
               %tky = all[ 1 ]-%tky
               %action-edit = edit_flag
               %assoc-_MaintType = edit_flag
               %action-SelectCustomizingTransptReq = selecttransport_flag ) ).
  ENDMETHOD.
  METHOD selectcustomizingtransptreq.
    MODIFY ENTITIES OF ZI_MaintType_S IN LOCAL MODE
      ENTITY MaintTypeAll
        UPDATE FIELDS ( TransportRequestID HideTransport )
        WITH VALUE #( FOR key IN keys
                        ( %tky               = key-%tky
                          TransportRequestID = key-%param-transportrequestid
                          HideTransport      = abap_false ) ).

    READ ENTITIES OF ZI_MaintType_S IN LOCAL MODE
      ENTITY MaintTypeAll
        ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(entities).
    result = VALUE #( FOR entity IN entities
                        ( %tky   = entity-%tky
                          %param = entity ) ).
  ENDMETHOD.
  METHOD get_global_authorizations.
*    AUTHORITY-CHECK OBJECT 'S_TABU_NAM' ID 'TABLE' FIELD 'ZI_MAINTTYPE' ID 'ACTVT' FIELD '02'.
*    DATA(is_authorized) = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed
*                                  ELSE if_abap_behv=>auth-unauthorized ).
    DATA(is_authorized) = abap_true.
    result-%update      = is_authorized.
    result-%action-Edit = is_authorized.
    result-%action-SelectCustomizingTransptReq = is_authorized.
  ENDMETHOD.
ENDCLASS.
CLASS lsc_zi_mainttype_s DEFINITION FINAL INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS:
      save_modified REDEFINITION,
      cleanup_finalize REDEFINITION.
ENDCLASS.

CLASS lsc_zi_mainttype_s IMPLEMENTATION.
  METHOD save_modified.
*    READ TABLE update-MaintTypeAll INDEX 1 INTO DATA(all).
*    IF all-TransportRequestID IS NOT INITIAL.
*      lhc_rap_tdat_cts=>get( )->record_changes(
*                                  transport_request = all-TransportRequestID
*                                  create            = REF #( create )
*                                  update            = REF #( update )
*                                  delete            = REF #( delete ) ).
*    ENDIF.
  ENDMETHOD.
  METHOD cleanup_finalize ##NEEDED.
  ENDMETHOD.
ENDCLASS.
CLASS lhc_zi_mainttype DEFINITION FINAL INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      validatetransportrequest FOR VALIDATE ON SAVE
        IMPORTING
          keys FOR MaintType~ValidateTransportRequest,
      get_global_features FOR GLOBAL FEATURES
        IMPORTING
        REQUEST requested_features FOR MaintType
        RESULT result,
      validateExtRef FOR VALIDATE ON SAVE
        IMPORTING keys FOR MaintType~validateExtRef,
      get_global_authorizations FOR GLOBAL AUTHORIZATION
            IMPORTING REQUEST requested_authorizations FOR MaintType RESULT result.
ENDCLASS.

CLASS lhc_zi_mainttype IMPLEMENTATION.
  METHOD validatetransportrequest.
*    DATA change TYPE REQUEST FOR CHANGE ZI_MaintType_S.
*    SELECT SINGLE TransportRequestID
*      FROM ZMAINT_TYPE_D_S
*      WHERE SingletonID = 1
*      INTO @DATA(TransportRequestID).
*    lhc_rap_tdat_cts=>get( )->validate_changes(
*                                transport_request = TransportRequestID
*                                table             = 'ZMAINT_TYPE'
*                                keys              = REF #( keys )
*                                reported          = REF #( reported )
*                                failed            = REF #( failed )
*                                change            = REF #( change-MaintType ) ).
  ENDMETHOD.
  METHOD get_global_features.
    DATA edit_flag TYPE abp_behv_flag VALUE if_abap_behv=>fc-o-enabled.
*    IF cl_bcfg_cd_reuse_api_factory=>get_cust_obj_service_instance(
*         iv_objectname = 'ZMAINT_TYPE'
*         iv_objecttype = cl_bcfg_cd_reuse_api_factory=>simple_table )->is_editable( ) = abap_false.
*      edit_flag = if_abap_behv=>fc-o-disabled.
*    ENDIF.
    result-%update = edit_flag.
    result-%delete = edit_flag.
  ENDMETHOD.

  METHOD validateExtRef.

    READ ENTITIES OF ZI_MaintType_S IN LOCAL MODE
      ENTITY MaintType
        FIELDS ( ExternalReference )
        WITH CORRESPONDING #( keys )
        RESULT DATA(MaintTypes).

    LOOP AT MaintTypes INTO DATA(MaintType).

      IF MaintType-ExternalReference IS NOT INITIAL AND match( val = MaintType-ExternalReference pcre = '^(\d\d-\d\d\d)$' occ = 1 ) = space.
        APPEND VALUE #( %tky = MaintType-%tky ) TO failed-MaintType.
        APPEND VALUE #( %tky = MaintType-%tky
                        %msg = new_message( id       = 'ZMAINT_TYPE'
                                            number   = 001
                                            severity = if_abap_behv_message=>severity-error
                                            v1       = MaintType-ExternalReference )
                        %element-ExternalReference = if_abap_behv=>mk-on
                      ) TO reported-MaintType.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

ENDCLASS.
