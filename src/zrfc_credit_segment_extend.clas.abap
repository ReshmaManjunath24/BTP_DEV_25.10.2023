CLASS zrfc_credit_segment_extend DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_aco_proxy .

    TYPES:
      partner                        TYPE c LENGTH 000017 ##TYPSHADOW .
    TYPES:
      ukm_credit_limit               TYPE p LENGTH 000008 DECIMALS 000002 ##TYPSHADOW .
    TYPES:
      ukm_credit_sgmnt               TYPE c LENGTH 000010 ##TYPSHADOW .
    TYPES:
      ukm_valid_date                 TYPE d ##TYPSHADOW .

    METHODS constructor
      IMPORTING
        !destination TYPE REF TO if_rfc_dest
      RAISING
        cx_rfc_dest_provider_error .
    METHODS z_iffco_extend_credit
      IMPORTING
        !iv_bp       TYPE partner
        !iv_limit    TYPE ukm_credit_limit
        !iv_segment  TYPE ukm_credit_sgmnt
        !iv_validity TYPE ukm_valid_date
      EXPORTING
        !ev_message  TYPE string
      RAISING
        cx_aco_application_exception
        cx_aco_communication_failure
        cx_aco_system_failure .
  PROTECTED SECTION.

    DATA destination TYPE rfcdest .
  PRIVATE SECTION.
ENDCLASS.



CLASS zrfc_credit_segment_extend IMPLEMENTATION.


  METHOD constructor.
    me->destination = destination->get_destination_name( ).
  ENDMETHOD.


  METHOD z_iffco_extend_credit.
    DATA: _rfc_message_ TYPE aco_proxy_msg_type.
    CALL FUNCTION 'Z_IFFCO_EXTEND_CREDIT' DESTINATION me->destination
      EXPORTING
        iv_bp                 = iv_bp
        iv_limit              = iv_limit
        iv_segment            = iv_segment
        iv_validity           = iv_validity
      IMPORTING
        ev_message            = ev_message
      EXCEPTIONS
        communication_failure = 1 MESSAGE _rfc_message_
        system_failure        = 2 MESSAGE _rfc_message_
        OTHERS                = 3.
    IF sy-subrc NE 0.
      DATA __sysubrc TYPE sy-subrc.
      DATA __textid TYPE aco_proxy_textid_type.
      __sysubrc = sy-subrc.
      __textid-msgid = sy-msgid.
      __textid-msgno = sy-msgno.
      __textid-attr1 = sy-msgv1.
      __textid-attr2 = sy-msgv2.
      __textid-attr3 = sy-msgv3.
      __textid-attr4 = sy-msgv4.
      CASE __sysubrc.
        WHEN 1 .
          RAISE EXCEPTION TYPE cx_aco_communication_failure
            EXPORTING
              rfc_msg = _rfc_message_.
        WHEN 2 .
          RAISE EXCEPTION TYPE cx_aco_system_failure
            EXPORTING
              rfc_msg = _rfc_message_.
        WHEN 3 .
          RAISE EXCEPTION TYPE cx_aco_application_exception
            EXPORTING
              exception_id = 'OTHERS'
              textid       = __textid.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
