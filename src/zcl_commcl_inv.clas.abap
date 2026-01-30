CLASS zcl_commcl_inv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_pdf_64
      IMPORTING
                VALUE(io_billingdocument) TYPE  i_billingdocument-billingdocument    "<-write your input name and type
      RETURNING VALUE(pdf_64)             TYPE string..

 METHODS num2words
      IMPORTING
        iv_num          TYPE string
        iv_major        TYPE string
        iv_minor        TYPE string
        iv_top_call     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_words) TYPE string.

    METHODS escape_xml
      IMPORTING
        iv_in         TYPE any
      RETURNING
        VALUE(rv_out) TYPE string.


  PRIVATE SECTION.

    METHODS build_xml
      IMPORTING
        VALUE(io_billingdocument) TYPE  i_billingdocument-billingdocument  "<-write your input name and type
      RETURNING
        VALUE(rv_xml)             TYPE string.
ENDCLASS.



CLASS ZCL_COMMCL_INV IMPLEMENTATION.


  METHOD get_pdf_64.

    DATA(lv_xml) = build_xml(
                      io_billingdocument   = io_billingdocument ).    "<-write your input name

    IF lv_xml IS INITIAL.
      RETURN.
    ENDIF.

    CALL METHOD zadobe_ads_class=>getpdf
      EXPORTING
        template = 'ZSD_COMM_INV/ZSD_COMM_INV'     "<-write your template and schema name
        xmldata  = lv_xml
      RECEIVING
        result   = DATA(lv_result).

    IF lv_result IS NOT INITIAL.
      pdf_64 = lv_result.
    ENDIF.

  ENDMETHOD.


  METHOD build_xml.

    DATA: lv_inv    TYPE I_BillingDocument-BillingDocument,
          lv_inv_dt TYPE i_billingdocument-BillingDocumentDate,
          lv_ref    TYPE I_salesorder-ReferenceSDDocument,
          lv_buy    TYPE i_SalesOrder-PurchaseOrderByCustomer,
          lv_po_dt  TYPE i_SalesOrder-CustomerPurchaseOrderDate,
          lv_items  TYPE string.

    SELECT SINGLE
       a~BillingDocument,
       a~BillingDocumentDate,
       b~ReferenceSDDocument,
       b~PurchaseOrderByCustomer,
       b~CustomerPurchaseOrderDate
     FROM I_BillingDocument AS a
     LEFT OUTER JOIN I_SalesOrder AS b
          ON b~SalesOrder = b~ReferenceSDDocument
     WHERE a~BillingDocument = @io_billingdocument
     INTO  @DATA(ls_matdoc).


    DATA:lv_sr_no             TYPE i VALUE 0,
         lv_des               TYPE i_SalesOrderitem-SalesOrderItemText,
         lv_item              TYPE i_SalesOrderitem-SalesOrderItem,
         exp_date             TYPE I_DeliveryDocumentItem-ShelfLifeExpirationDate,
         mfg_no               TYPE string,
         lv_custpur           TYPE i_SalesOrder-CustomerPurchaseOrderDate,
         lv_billdocdate       TYPE I_BillingDocument-BillingDocumentDate,
         lv_salesdate         TYPE i_SalesOrder-SalesOrderDate,
         lv_text(1000)        TYPE c,
         sperson(1000)        TYPE c,
         lv_hsn               TYPE i_productplantbasic-consumptiontaxctrlcode,
         saddress1(1000)      TYPE c,
         bperson(1000)        TYPE c,
         batch                TYPE I_DeliveryDocumentItem-Batch,
         expiry               TYPE I_DeliveryDocumentItem-ShelfLifeExpirationDate,
         baddress1(1000)      TYPE c,
         rate                 TYPE i_billingdocitemprcgelmntbasic-ConditionRateAmount,
         lv_description(1000) TYPE c.

    DATA: lv_sgst_total   TYPE decfloat34,
          lv_cgst_total   TYPE decfloat34,
          lv_igst_total   TYPE decfloat34,
          lv_discount_tot TYPE decfloat34,
          lv_charges_tot  TYPE decfloat34.

    CLEAR: lv_sgst_total, lv_cgst_total, lv_igst_total,
           lv_discount_tot, lv_charges_tot.



    "--------------------------------------------------------
    "query to fetch objects from i_billingdocumentitem
    "--------------------------------------------------------

    DATA: it_billdoc_item TYPE TABLE OF i_billingdocumentitem,
          wa_billdoc_item TYPE i_billingdocumentitem.
    SELECT *
    FROM i_billingdocumentitem
    WHERE billingdocument = @io_billingdocument
     INTO TABLE @it_billdoc_item.


    "--------------------------------------------------------
    "query to fetch objects from i_billingdocument
    "--------------------------------------------------------
    READ TABLE it_billdoc_item INTO wa_billdoc_item INDEX 1.
    SELECT SINGLE *
      FROM i_billingdocument
      WHERE BillingDocument = @io_billingdocument
       INTO  @DATA(wa_billdoc).

    "--------------------------------------------------------
    "query to fetch objects from i_billingdocumenttp

    "this query is added to fetch custom fields
    "--------------------------------------------------------

    SELECT SINGLE *
      FROM i_billingdocumenttp
      WHERE BillingDocument = @io_billingdocument
       INTO  @DATA(wa_billdoctp).

    "--------------------------------------------------------
    "query to fetch objects from I_BILLINGDOCUMENTITEMTP

    "this query is added to fetch custom fields
    "--------------------------------------------------------

    DATA: it_billdoc_itemtp TYPE TABLE OF i_billingdocumentitemtp,
          wa_billdoc_itemtp TYPE i_billingdocumentitemtp.
    SELECT *
    FROM i_billingdocumentitemtp
    WHERE billingdocument = @io_billingdocument
     INTO TABLE @it_billdoc_itemtp.
    READ TABLE it_billdoc_itemtp INTO wa_billdoc_itemtp INDEX 1.

    "--------------------------------------------------------
    "query to fetch objects from i_billingdocitemprcgelmntbasic
    "--------------------------------------------------------

    DATA: it_billdoc_price TYPE TABLE OF i_billingdocitemprcgelmntbasic,
          wa_billdoc_price TYPE i_billingdocitemprcgelmntbasic.
    SELECT *
    FROM i_billingdocitemprcgelmntbasic
    WHERE billingdocument = @io_billingdocument
     INTO TABLE @it_billdoc_price.
    "--------------------------------------------------------
    "query to fetch objectsi_deliveryitem
    "--------------------------------------------------------

    DATA: it_del_item TYPE TABLE OF I_DeliveryDocumentItem,
          wa_del_item TYPE I_DeliveryDocumentItem.

    SELECT *
      FROM I_DeliveryDocumentItem
      WHERE DeliveryDocument          = @wa_billdoc_item-ReferenceSDDocument
        AND ReferenceSDDocumentItem   = @wa_billdoc_item-ReferenceSDDocumentItem
      INTO TABLE @it_del_item.

    DATA: lv_batch     TYPE string,
          lv_mnf_dt    TYPE string,
          lv_exp_dt    TYPE string,
          lv_gross_wt  TYPE string,
          lv_gross_sum TYPE p DECIMALS 3,
          lv_net_wt    TYPE string,
          lv_net_sum   TYPE p DECIMALS 3.

    DATA: lt_mnf_dates TYPE SORTED TABLE OF sy-datum WITH UNIQUE KEY table_line,
          lt_exp_dates TYPE SORTED TABLE OF sy-datum WITH UNIQUE KEY table_line.

    CLEAR: lv_gross_sum,
           lv_net_sum.


    LOOP AT it_del_item INTO wa_del_item.

      " BATCH (comma separated)
      IF wa_del_item-Batch IS NOT INITIAL.
        IF lv_batch IS INITIAL.
          lv_batch = wa_del_item-Batch.
        ELSE.
          lv_batch = |{ lv_batch }, { wa_del_item-Batch }|. "#EC CI_NOORDER
        ENDIF.
      ENDIF.

      " MANUFACTURE DATE (skip 00000000, remove duplicates)
      IF wa_del_item-ManufactureDate IS NOT INITIAL.
        INSERT wa_del_item-ManufactureDate INTO TABLE lt_mnf_dates.
      ENDIF.

      " EXPIRY DATE (skip 00000000, remove duplicates)
      IF wa_del_item-ShelfLifeExpirationDate IS NOT INITIAL.
        INSERT wa_del_item-ShelfLifeExpirationDate INTO TABLE lt_exp_dates.
      ENDIF.

      " GROSS WEIGHT (SUM)
      lv_gross_sum += wa_del_item-itemgrossweight.
      lv_net_sum += wa_del_item-ItemNetWeight.


    ENDLOOP.

    LOOP AT lt_mnf_dates INTO DATA(lv_date).
      DATA(lv_fmt_mnf) =
        |{ lv_date+6(2) }/|
        && |{ lv_date+4(2) }/|
        && |{ lv_date+2(2) }|.

      IF lv_mnf_dt IS INITIAL.
        lv_mnf_dt = lv_fmt_mnf.
      ELSE.
        lv_mnf_dt = |{ lv_mnf_dt }, { lv_fmt_mnf }|.
      ENDIF.
    ENDLOOP.


    LOOP AT lt_exp_dates INTO DATA(lv_exp).
      DATA(lv_fmt_exp) =
        |{ lv_exp+6(2) }/|
        && |{ lv_exp+4(2) }/|
        && |{ lv_exp+2(2) }|.

      IF lv_exp_dt IS INITIAL.
        lv_exp_dt = lv_fmt_exp.
      ELSE.
        lv_exp_dt = |{ lv_exp_dt }, { lv_fmt_exp }|.
      ENDIF.
    ENDLOOP.

    lv_gross_wt = lv_gross_sum.
    lv_net_wt = lv_net_sum.


    "--------------------------------------------------------
    "query to fetch objectsi_deliverydocument
    "--------------------------------------------------------
    DATA: wa_del_header TYPE I_DeliveryDocument.
    SELECT SINGLE * FROM I_DeliveryDocument
    WHERE DeliveryDocument = @wa_billdoc_item-ReferenceSDDocument
    INTO @wa_del_header.


    "--------------------------------------------------------
    "query to fetch objects from i_SalesOrder
    "--------------------------------------------------------

    SELECT SINGLE * FROM i_SalesOrder
      WHERE SalesOrder = @wa_billdoc_item-SalesDocument
           INTO @DATA(wa_saleshead).


    SELECT SINGLE * FROM I_SalesOrderTP
WHERE SalesOrder = @wa_billdoc_item-SalesDocument
    INTO @DATA(wa_salesheadtp).

    "--------------------------------------------------------
    "query to fetch objects from i_SalesOrderitem
    "--------------------------------------------------------

    DATA: it_sales_item TYPE TABLE OF i_SalesOrderitem,
          wa_sales_item TYPE i_SalesOrderitem.
    SELECT *
    FROM i_SalesOrderitem
    WHERE SalesOrder = @wa_billdoc_item-SalesDocument
     INTO TABLE @it_sales_item.
    READ TABLE it_sales_item INTO wa_sales_item INDEX 1.

    DATA: it_sales_itemtp TYPE TABLE OF i_SalesOrderitemtp,
          wa_sales_itemtp TYPE i_SalesOrderitemtp.
    SELECT *
    FROM I_SalesOrderItemTP
    WHERE SalesOrder = @wa_billdoc_item-SalesDocument
     INTO TABLE @it_sales_itemtp.
    READ TABLE it_sales_itemtp INTO wa_sales_itemtp INDEX 1.

    "--------------------------------------------------------
    "query to fetch payment term description
    "--------------------------------------------------------


    SELECT SINGLE *
FROM I_PaymentTermsConditionsText
WHERE PaymentTerms = @wa_saleshead-CustomerPaymentTerms
INTO @DATA(wa_payment).
    "--------------------------------------------------------
    "query to fetch objects from ship to address
    "--------------------------------------------------------

    DATA: it_vbpa TYPE TABLE OF i_salesorderpartner.
    SELECT *
             FROM i_salesorderpartner "i_salesorderitempartner "
       WHERE salesorder = @wa_billdoc_item-SalesDocument
       INTO TABLE @it_vbpa.


    READ TABLE it_vbpa INTO DATA(wa_vbpa) WITH  KEY partnerfunction = 'WE'. "SHIP TO PARTY
    IF sy-subrc = 0.

      SELECT SINGLE customer, addressid, customername, taxnumber3, country, region, bpcustomerfullname
       FROM i_customer
        WHERE customer = @wa_vbpa-customer
        INTO @DATA(wa_kna1_s).

      SELECT SINGLE * FROM i_address_2
       WITH PRIVILEGED ACCESS
       WHERE addressid = @wa_kna1_s-addressid
       INTO @DATA(wa_address_ship).

      SELECT SINGLE * FROM i_regiontext WHERE country = @wa_kna1_s-country
      AND region = @wa_kna1_s-region
      AND language = @sy-langu
     INTO @DATA(wa_region_ship).

      SELECT SINGLE * FROM i_countrytext WHERE country = @wa_kna1_s-country
      AND language = @sy-langu
      INTO @DATA(wa_country_ship).

      IF wa_address_ship-careofname IS NOT INITIAL.
        sperson = wa_address_ship-careofname.
      ELSE.
        sperson = wa_address_ship-OrganizationName1.
      ENDIF.

      saddress1 = |{ wa_address_ship-HouseNumber } { wa_address_ship-streetprefixname1 } { wa_address_ship-streetname } { wa_address_ship-streetsuffixname1 } { wa_address_ship-streetsuffixname2 }  { wa_address_ship-cityname }| .
      saddress1 = |{ saddress1 },{ wa_address_ship-postalcode },{ wa_region_ship-regionname },{ wa_country_ship-countryname }| .

    ENDIF.

    DATA(lv_shipto) = |{ sperson }\n{ saddress1 }|.

    "--------------------------------------------------------
    "query to fetch objects from Bill to address
    "--------------------------------------------------------



    READ TABLE it_vbpa INTO DATA(wa_vbpab) WITH  KEY partnerfunction = 'AG'. "SHIP TO PARTY
    IF sy-subrc = 0.

      SELECT SINGLE customer, addressid, customername, taxnumber3, country, region, bpcustomerfullname
       FROM i_customer
        WHERE customer = @wa_vbpab-customer
        INTO @DATA(wa_kna1_b).

      SELECT SINGLE * FROM i_address_2
       WITH PRIVILEGED ACCESS
       WHERE addressid = @wa_kna1_b-addressid
       INTO @DATA(wa_address_bill).

      SELECT SINGLE * FROM i_regiontext WHERE country = @wa_kna1_b-country
      AND region = @wa_kna1_b-region
      AND language = @sy-langu
     INTO @DATA(wa_region_bill).

      SELECT SINGLE * FROM i_countrytext WHERE country = @wa_kna1_b-country
      AND language = @sy-langu
      INTO @DATA(wa_country_bill).

      IF wa_address_bill-careofname IS NOT INITIAL.
        bperson = wa_address_bill-careofname.
      ELSE.
        bperson = wa_address_bill-OrganizationName1.
      ENDIF.

      baddress1 = |{ wa_address_ship-HouseNumber } { wa_address_bill-streetprefixname1 } { wa_address_bill-streetname } { wa_address_bill-streetsuffixname1 } { wa_address_bill-streetsuffixname2 }  { wa_address_bill-cityname }| .
      baddress1 = |{ baddress1 },{ wa_address_bill-postalcode },{ wa_region_bill-regionname },{ wa_country_bill-countryname }| .

    ENDIF.

    DATA(lv_billto) = |{ bperson }\n{ baddress1 }|.

    "--------------------------------------------------------
    "hardcoded sender address
    "--------------------------------------------------------
    DATA : sender_nm(1000)    TYPE c,
           sender_addr(1000)  TYPE c,
           sender_addr1(1000) TYPE c,
           country(1000)      TYPE c,
           gst(1000)          TYPE c,
           endusercode(1000)  TYPE c.




    sender_nm = ' KOPRAN RESEARCH LABORATORIES LIMITED - 25-26'.
    sender_addr = 'ADD PARIJAT HOUSE 1076, DR. E MOSES ROAD WORLI MUMBAI 400018 INDIA'.
    country = 'India'.
    gst = '27AAACK3198E1ZJ'.
    endusercode = 'DCX 200'.

    DATA(lv_sender) = |{ sender_nm }\n{ sender_addr }|.

    lv_description =  'We hereby declare that we will avail RoDTEP Scheme against this invoice'.

    DATA(lv_incoterm) = |{ wa_saleshead-IncotermsClassification },{ wa_saleshead-incotermslocation1 }|.

 "--------------------------------------------------------
    "hardcoded bank details
 "--------------------------------------------------------
DATA: lv_key        TYPE string,
      lv_bank_dlts  TYPE string,
      lv_nl         TYPE string VALUE cl_abap_char_utilities=>newline.

lv_key = |{ wa_billdoctp-YY1_Bank_BDH }_{ wa_saleshead-TransactionCurrency }|.


CLEAR lv_bank_dlts.

CASE lv_key.

  WHEN 'SBI_USD'.

    lv_bank_dlts =
      |State Bank of India, 460, Park Avenue, New York, NY 10022, USA| && lv_nl &&
      |A/c No. 77600121220001,| && lv_nl &&
      |by CHIPS - UID 034282,| && lv_nl &&
      |ABA Routing No. 026009140,| && lv_nl &&
      |SWIFT: SBINUS33,| && lv_nl &&
      |Credit to :| && lv_nl &&
      |State Bank of India, Commercial Branch, International Banking Division, Mumbai, India,| && lv_nl &&
      |SWIFT Code : SBININBB101,| && lv_nl &&
      |A/C Kopran Research Laboratories Ltd,| && lv_nl &&
      |A/C No. 35812957128|.

WHEN 'SBI_EURO'.
lv_bank_dlts =
  |State Bank of India,| && lv_nl &&
  |Frankfurt Germany, Swift : SBINDEFF| && lv_nl &&
  |IBAN: DE78 5033 0000 0021 017108| && lv_nl &&
  |Euro A/c No. 52608101720001| && lv_nl &&
  |Credit to : OF "STATE BANK OF INDIA"| && lv_nl &&
  |Commercial Branch, Intl Banking Division,| && lv_nl &&
  |Mumbai, India, SWIFT Code : SBININBB101| && lv_nl &&
  |A/C Kopran Research Laboratories Ltd| && lv_nl &&
  |A/C No. SBI : 35812957128|.

WHEN 'SBI_INR'.
  lv_bank_dlts =
  |NAME OF THE BANK: STATE BANK OF INDIA| && lv_nl &&
  |ADDRESS OF THE BANK: COMMERCIAL BRANCH, HORNIMAN CIRCLE, MUMBAI – 400023| && lv_nl &&
  |ACCOUNT NUMBER: 35812957128, BANK IFSC CODE: SBIN0006070| && lv_nl &&
  |ACCOUNT TYPE: CC, BRANCH CODE: 6070,| && lv_nl &&
  |NAME OF BENEFICIARY: KOPRAN RESEARCH LABORATORIES LIMITED,| && lv_nl &&
  |ADDRESS OF BENEFICIARY: PARIJAT HOUSE, 1076, DR. E. MOSES ROAD, WORLI, MUMBAI - 400018|.

WHEN 'RBL_USD'.
  lv_bank_dlts =
  |OUR BANK DETAILS:| && lv_nl &&
  |RBL BANK LTD, Mumbai| && lv_nl &&
  |(SWIFT: RATNINBBXXX) (CCIL ID: CCBPRABL0129)| && lv_nl &&
  |Beneficiary Name : Kopran Research Laboratories Ltd, Account No. 609000455232| && lv_nl &&
  |Advisory Bank / Intermediary Bank: Standard Chartered Bank, New York| && lv_nl &&
  |CHIPS 0256 / Fed Wire ABA 026002561,| && lv_nl &&
  |SWIFT Code: SCBLUS33, Account No. 3582-026450-001|.

  WHEN 'RBL_INR'.
  lv_bank_dlts =
  |OUR BANK DETAILS:| && lv_nl &&
  |RBL BANK LTD, Mumbai| && lv_nl &&
  |(SWIFT: RATNINBBXXX) (CCIL ID: CCBPRABL0129)| && lv_nl &&
  |Beneficiary Name : Kopran Research Laboratories Ltd, Account No. 609000455232| && lv_nl &&
  |Advisory Bank / Intermediary Bank: ICICI Bank, Hong Kong| && lv_nl &&
  |SWIFT Code: ICICHKHHCLR, Nostro A/c: 852045762|.

  WHEN 'RBL_EURO'.
  lv_bank_dlts =
  |OUR BANK DETAILS:| && lv_nl &&
  |RBL BANK LTD, Mumbai| && lv_nl &&
  |(SWIFT: RATNINBBXXX) (CCIL ID: CCBPRABL0129)| && lv_nl &&
  |Beneficiary Name : Kopran Research Laboratories Ltd, Account No. 609000455232| && lv_nl &&
  |Advisory Bank / Intermediary Bank: Standard Chartered Bank,| && lv_nl &&
  |Frankfurt, Germany, Franklinstrasse 46-48, Frankfurt am Main,| && lv_nl &&
  |Germany - 60486, SWIFT Code: SCBLDEFXXXX,| && lv_nl &&
  |Nostro A/c: 018301701|.


  WHEN 'YES BANK_USD'.
  lv_bank_dlts =
  |OUR BANK DETAILS:| && lv_nl &&
  |YES Bank, Mumbai| && lv_nl &&
  |A/C No. 000181300005969| && lv_nl &&
  |Swift Code : YESBINBB| && lv_nl &&
  |Beneficiary Name : Kopran Research Laboratories Ltd| && lv_nl &&
  |Advisory Bank / Intermediary Bank: Wells Fargo Bank, N.A.| && lv_nl &&
  |A/C Number : 2000193004500 (ABA Routing No: 026005092)| && lv_nl &&
  |SWIFT Code : PNBPUS3NNYC|.

    WHEN 'ICICI_USD'.
    lv_bank_dlts =
  |OUR BANK DETAILS:| && lv_nl &&
  |ICICI Bank Ltd| && lv_nl &&
  |Beneficiary Name : Kopran Research Lab Ltd, A/C No. 149051000001| && lv_nl &&
  |Intermediary Bank: Account Number: 36329377, Bank Name: CITI Bank N.A.| && lv_nl &&
  |Swift Code: CITIUS33XXX,| && lv_nl &&
  |Bank Clearing Code: FED ABA 021000089,| && lv_nl &&
  |ICICI Bank Swift Code: ICICINBBCTS,| && lv_nl &&
  |ABA FED Number: 021000089|.

   WHEN 'ICICI_EURO'.
   lv_bank_dlts =
  |OUR BANK DETAILS:| && lv_nl &&
  |ICICI Bank Ltd| && lv_nl &&
  |Beneficiary Name : Kopran Research Lab Ltd, A/C No. 149051000001| && lv_nl &&
  |Intermediary Bank: Account Number: 0000484637,| && lv_nl &&
  |Bank Name: ICICI Bank UK PLC, Germany Branch| && lv_nl &&
  |Swift Code: ICICDEFFXXX,| && lv_nl &&
  |IBAN No: DE56 5012 0100 0000 4846 37,| && lv_nl &&
  |ICICI Bank Swift Code: ICICINBBCTS|.

    WHEN 'SBER BANK'.
    lv_bank_dlts =
  |Bank Name: SBERBANK.| && lv_nl &&
  |Account Name : Kopran Research Laboratories Ltd.| && lv_nl &&
  |Account No : 45000356600000019000.| && lv_nl &&
  |CIF ID : CIF000001889.| && lv_nl &&
  |Bank Name : SBERBANK.| && lv_nl &&
  |IFSC Code : SABR0000001.| && lv_nl &&
  |Swift Code : SABRINDD|.

      ENDCASE.




    "--------------------------------------------------------
    "query to fetch objects from i_productplantbasic
    "--------------------------------------------------------

    DATA: productplantbasic    TYPE TABLE OF i_productplantbasic,
          wa_productplantbasic TYPE i_productplantbasic.
    SELECT * FROM i_productplantbasic
        FOR ALL ENTRIES IN @it_billdoc_item
        WHERE product  = @it_billdoc_item-product
         AND plant = @it_billdoc_item-plant
*AND plant = '1200'
          INTO TABLE @productplantbasic.
    READ TABLE productplantbasic INTO wa_productplantbasic INDEX 1.



    DATA: lv_rate        TYPE p DECIMALS 2 VALUE 0,
          lv_amount      TYPE p DECIMALS 2,
          lv_total       TYPE p DECIMALS 2,
          amt_mul        TYPE p DECIMALS 2,

          currency(1000) TYPE c,

          lv_comm1       TYPE p DECIMALS 2 VALUE 0,
          lv_comm2       TYPE p DECIMALS 2 VALUE 0,
          lv_comm3       TYPE p DECIMALS 2 VALUE 0,
          lv_comm4       TYPE p DECIMALS 2 VALUE 0,

          lv_freight     TYPE p DECIMALS 2 VALUE 0,
          lv_insurance   TYPE p DECIMALS 2 VALUE 0,
          lv_fob         TYPE p DECIMALS 2 VALUE 0,

          lv_packing     TYPE I_BillingDocumentItemTP-YY1_PackingSize1_BDI,
          description    TYPE I_BillingDocumentItem-BillingDocumentItemText,
          quantity       TYPE I_BillingDocumentItem-BillingQuantity.

    LOOP AT it_billdoc_price INTO wa_billdoc_price
      WHERE BillingDocument     = wa_billdoc_item-BillingDocument
        AND BillingDocumentItem = wa_billdoc_item-BillingDocumentItem.

      CASE wa_billdoc_price-ConditionType.

        WHEN 'ZCIF'.         "Base price
          lv_rate += wa_billdoc_price-ConditionRateAmount.
          wa_saleshead-TransactionCurrency = wa_billdoc_price-ConditionCurrency.

        WHEN 'ZCOM'.         "Commission 1
          lv_comm1 += wa_billdoc_price-ConditionRateRatio.

        WHEN 'ZECM'.         "Commission 2
          lv_comm2 += wa_billdoc_price-ConditionRateRatio.

        WHEN 'ZCQT'.         "Commission 3
          lv_comm3 += wa_billdoc_price-ConditionRateRatio.

        WHEN 'ZCQ4'.         "Commission 4
          lv_comm4 += wa_billdoc_price-ConditionRateRatio.

        WHEN 'ZFRE'.         "Freight
          lv_freight += abs( wa_billdoc_price-ConditionRateAmount ).

        WHEN 'ZINS'.         "Insurance
          lv_insurance += abs( wa_billdoc_price-ConditionRateAmount ).

        WHEN 'ZFOB'.         "FOB
          lv_fob += wa_billdoc_price-ConditionAmount.

      ENDCASE.

    ENDLOOP.


    DATA: lv_comm_pct  TYPE p DECIMALS 2,
          lv_comm_kilo TYPE p DECIMALS 2.

    IF lv_comm1 IS NOT INITIAL.
      lv_comm_pct  = lv_comm1.
    ELSEIF lv_comm2 IS NOT INITIAL.
      lv_comm_pct  = lv_comm2.
    ENDIF.

    IF lv_comm3 IS NOT INITIAL.
      lv_comm_pct  = lv_comm3.
    ELSEIF lv_comm4 IS NOT INITIAL.
      lv_comm_pct  = lv_comm4.
    ENDIF.

    DATA lv_comm_final TYPE p DECIMALS 2.

    IF lv_comm_pct IS NOT INITIAL.
      lv_comm_final = lv_comm_pct.
    ELSE.
      lv_comm_final = lv_comm_kilo.
    ENDIF.


    DATA: lv_es_rs     TYPE i_billingdocument-AccountingExchangeRate,
          lv_rs_frg    TYPE p DECIMALS 2,
          lv_rs_insure TYPE p DECIMALS 2,
          lv_rs_fob    TYPE p DECIMALS 2.


    lv_es_rs = wa_billdoc-AccountingExchangeRate.

    lv_rs_frg = lv_freight * lv_es_rs.
    lv_rs_insure = lv_insurance * lv_es_rs.
    lv_rs_fob = lv_fob * lv_es_rs.


    "--------------------------------------------------------
    " star of  xml binding
    "--------------------------------------------------------
    DATA: lv_xml TYPE string VALUE ''.


    lv_custpur = |{ wa_saleshead-CustomerPurchaseOrderDate+6(2) }/{ wa_saleshead-CustomerPurchaseOrderDate+4(2) }/{ wa_saleshead-CustomerPurchaseOrderDate+2(4) }|.
    lv_billdocdate = |{ wa_billdoc-BillingDocumentDate+6(2) }/{ wa_billdoc-BillingDocumentDate+4(2) }/{ wa_billdoc-BillingDocumentDate+2(4) }|.
    lv_salesdate = |{ wa_saleshead-SalesOrderDate+6(2) }/{ wa_saleshead-SalesOrderDate+4(2) }/{ wa_saleshead-SalesOrderDate+2(4) }|.
    lv_hsn = wa_productplantbasic-consumptiontaxctrlcode.
    exp_date = |{ wa_del_item-ShelfLifeExpirationDate+6(2) }/{ wa_del_item-ShelfLifeExpirationDate+4(2) }/{ wa_del_item-ShelfLifeExpirationDate+2(4) }|.
    mfg_no = |{ lv_mnf_dt+6(2) }/{ lv_mnf_dt+4(2) }/{ lv_mnf_dt+2(4) }|.


        lv_sr_no = lv_sr_no + 1.
    IF lv_rate IS INITIAL.
      lv_rate = wa_billdoc_item-NetAmount.
    ENDIF.
    lv_packing =  wa_billdoc_itemtp-YY1_PackingSize1_BDI.
    description =
      |Pharmaceutical raw Material :\n{ wa_sales_item-SalesOrderItemText }\n| &&
      |HSN Code : { lv_hsn }\n| &&
      |GST No : { gst }\n| &&
      |IGST  LUT ARN No :\n| &&
      |End used Code : { endusercode }|.

    quantity = wa_billdoc_item-BillingQuantity.
    lv_amount = lv_rate * wa_billdoc_item-BillingQuantity.
    lv_total = lv_total + lv_amount.
    amt_mul =  wa_billdoc-TotalNetAmount  *  wa_billdoc-AccountingExchangeRate .

    DATA : lv_conditiontext_xml TYPE I_DeliveryDocument-YY1_NoKindofPacking_DLH..

    lv_conditiontext_xml = me->escape_xml( wa_del_header-YY1_NoKindofPacking_DLH ).
        "--------------------------------------------------------
    " Amount to words
    "--------------------------------------------------------

        DATA: lv_amount_string TYPE string,
          lv_amt_inword    TYPE string.

    lv_amount_string = |{  lv_total }|.
    CONDENSE lv_amount_string.
*      REPLACE '.00' IN lv_amount_string WITH ''.
*      REPLACE '.0'  IN lv_amount_string WITH ''.



    DATA: lv_major TYPE string,
          lv_minor TYPE string.

    CLEAR: lv_major, lv_minor.
    CLEAR: lv_major, lv_minor.

    CASE wa_billdoc-transactioncurrency.

        " -------- RUPEE FAMILY --------
      WHEN 'INR'. lv_major = 'Rupee'.   lv_minor = 'Paise'.
      WHEN 'PKR'. lv_major = 'Rupee'.   lv_minor = 'Paisa'.
      WHEN 'NPR'. lv_major = 'Rupee'.   lv_minor = 'Paisa'.
      WHEN 'LKR'. lv_major = 'Rupee'.   lv_minor = 'Cent'.
      WHEN 'SCR'. lv_major = 'Rupee'.   lv_minor = 'Cent'.

        " -------- DOLLAR FAMILY --------
      WHEN 'USD'. lv_major = 'Dollar'.  lv_minor = 'Cent'.
      WHEN 'AUD'. lv_major = 'Dollar'.  lv_minor = 'Cent'.
      WHEN 'CAD'. lv_major = 'Dollar'.  lv_minor = 'Cent'.
      WHEN 'NZD'. lv_major = 'Dollar'.  lv_minor = 'Cent'.
      WHEN 'SGD'. lv_major = 'Dollar'.  lv_minor = 'Cent'.
      WHEN 'HKD'. lv_major = 'Dollar'.  lv_minor = 'Cent'.

        " -------- EURO --------
      WHEN 'EUR'. lv_major = 'Euro'.    lv_minor = 'Cent'.

        " -------- POUND --------
      WHEN 'GBP'. lv_major = 'Pound'.   lv_minor = 'Penny'.

        " -------- YEN / WON (NO MINOR) --------
      WHEN 'JPY'. lv_major = 'Yen'.     lv_minor = ''.
      WHEN 'KRW'. lv_major = 'Won'.     lv_minor = ''.

        " -------- MIDDLE EAST --------
      WHEN 'AED'. lv_major = 'Dirham'.  lv_minor = 'Fils'.
      WHEN 'SAR'. lv_major = 'Riyal'.   lv_minor = 'Halala'.
      WHEN 'QAR'. lv_major = 'Riyal'.   lv_minor = 'Dirham'.
      WHEN 'OMR'. lv_major = 'Rial'.    lv_minor = 'Baisa'.
      WHEN 'KWD'. lv_major = 'Dinar'.   lv_minor = 'Fils'.
      WHEN 'BHD'. lv_major = 'Dinar'.   lv_minor = 'Fils'.

        " -------- ASIA --------
      WHEN 'CNY'. lv_major = 'Yuan'.    lv_minor = 'Fen'.
      WHEN 'THB'. lv_major = 'Baht'.    lv_minor = 'Satang'.
      WHEN 'MYR'. lv_major = 'Ringgit'. lv_minor = 'Sen'.
      WHEN 'IDR'. lv_major = 'Rupiah'.  lv_minor = 'Sen'.
      WHEN 'PHP'. lv_major = 'Peso'.    lv_minor = 'Centavo'.

        " -------- AFRICA --------
      WHEN 'ZAR'. lv_major = 'Rand'.    lv_minor = 'Cent'.
      WHEN 'NGN'. lv_major = 'Naira'.   lv_minor = 'Kobo'.

        " -------- OTHERS / FALLBACK --------
      WHEN OTHERS.
        lv_major = wa_billdoc-transactioncurrency.
        lv_minor = ''.

    ENDCASE.


*    lv_amt_inword = num2words( iv_num = lv_amount_string ).

       DATA: lv_level      TYPE i.

    CLEAR lv_level.


    lv_amt_inword = me->num2words(
      iv_num   = lv_amount_string
      iv_major = lv_major
      iv_minor = lv_minor
    ).

DATA: lv_awb_label TYPE string,
      lv_awb_value TYPE string,
      lv_awb_date type I_BillingDocument-YY1_BLAWBDate_BDH.

IF wa_billdoc-YY1_BLNOAWBNo_BDH IS NOT INITIAL.
  lv_awb_label = 'B/L No.'.
  lv_awb_value = wa_billdoc-YY1_BLNOAWBNo_BDH.
ELSEIF wa_billdoc-YY1_AWBNo_BDH IS NOT INITIAL.
  lv_awb_label = 'AWB No.'.
  lv_awb_value = wa_billdoc-YY1_AWBNo_BDH.
ENDIF.

lv_awb_date =  |{ wa_billdoc-YY1_BLAWBDate_BDH+6(2) }/{ wa_billdoc-YY1_BLAWBDate_BDH+4(2) }/{ wa_billdoc-YY1_BLAWBDate_BDH+2(4) }|.

select single * from zc_port_code
where SapPortCode = @wa_saleshead-YY1_Portofloading_SDH
into @data(wa_port_dtls).





   DATA(lv_header) =
 |<form1>| &&
 |  <Subform2>| &&
 |    <SUBFORM3>| &&
 |      <SUBFORM4>| &&
 |        <EXPT></EXPT>| &&
 |        <EXPT1>{ lv_sender }</EXPT1>| &&
 |      </SUBFORM4>| &&
 |      <SUBFORM5>| &&
 |        <INV_NO>{ wa_billdoc-BillingDocument }</INV_NO>| &&
 |        <INV_DT>{ lv_billdocdate }</INV_DT>| &&
 |        <BUY_ORD>{ wa_saleshead-PurchaseOrderByCustomer }</BUY_ORD>| &&
 |        <PO_DT>{ lv_custpur }</PO_DT>| &&
 |        <OT_REF>{ wa_saleshead-SalesOrder }</OT_REF>| &&
 |        <EOPO_DT>{ lv_salesdate }</EOPO_DT>| &&
 |      </SUBFORM5>| &&
 |    </SUBFORM3>| &&
 |    <SUUBFORM6>| &&
 |      <SUBFORM7>| &&
 |        <CONSIGEE></CONSIGEE>| &&
 |        <EXPT>{ lv_shipto }</EXPT>| &&
 |      </SUBFORM7>| &&
 |      <SUBFORM8>| &&
 |        <BUY_BILL> </BUY_BILL>| &&
 |        <COUN_GD>{ country }</COUN_GD>| &&
 |        <COUN_DES>{ wa_country_ship-countryname }</COUN_DES>| &&
 |        <BUY_BILL2>{ lv_billto }</BUY_BILL2>| &&
 |      </SUBFORM8>| &&
 |    </SUUBFORM6>| &&
 |    <SUBFORM9>| &&
 |      <SUBFORM17>| &&
 |        <TextField></TextField>| &&      "no binding in reference
 |        <TD_PY> </TD_PY>| &&              "no binding
 |        <INCO_TM>{ lv_incoterm }</INCO_TM>| &&
 |        <PY_TM>{ wa_payment-PaymentTermsConditionDesc }</PY_TM>| &&              "no binding
 |      </SUBFORM17>| &&
 |      <PT_LD>{ wa_port_dtls-name1 }</PT_LD>| &&
 |      <FI_DS>{ wa_salesheadtp-YY1_PortofDischarge_SDH  }</FI_DS>| &&                "no binding
 |      <PT_DIS>{ wa_salesheadtp-YY1_PortofDischarge_SDH }</PT_DIS>| &&              "no binding
 |    </SUBFORM9>| &&
 |    <SUBFORM10>| &&
 |      <Table2>| &&
 |        <HeaderRow>| &&
 |          <Cell4>| &&
 |            <qty_unit>{ wa_sales_item-BaseUnit }</qty_unit>| &&
 |          </Cell4>| &&
 |          <Cell5>| &&
 |            <curry0>{ wa_saleshead-TransactionCurrency }</curry0>| &&
 |          </Cell5>| &&
 |          <Cell6>| &&
 |            <curry1>{ wa_saleshead-TransactionCurrency }</curry1>| &&
 |          </Cell6>| &&
 |        </HeaderRow>|.


DATA(lv_item_row) = ``.

  "-------------------------------------------------


    lv_item_row &&=
   |        <ROW>| &&
   |          <MK_NOS>{ wa_billdoc-YY1_MarksNosContainerN_BDH }</MK_NOS>| &&
   |          <NO_PK>{ lv_conditiontext_xml }</NO_PK>| &&
   |          <licenc_des>{ wa_billdoc-YY1_LicenceDescription_BDH }</licenc_des>| &&
   |          <QN_TY>{ quantity }</QN_TY>| &&
   |          <RATE>{ lv_rate }</RATE>| &&
   |          <AMT>{ lv_amount }</AMT>| &&
   |        </ROW>|.


  "-------------------------------------------------
  " ITEM ROW 2
  "-------------------------------------------------
  lv_item_row &&=
|        <ROW2>| &&
|          <BT_NO>{ lv_batch }</BT_NO>| &&
|          <MFG_NO>{ lv_mnf_dt }</MFG_NO>| &&
|          <EXP_DATE>{ lv_exp_dt }</EXP_DATE>| &&
|          <GROSS_WT>{ lv_gross_wt }</GROSS_WT>| &&
|          <NET_WT>{ lv_net_wt }</NET_WT>| &&
|        </ROW2>|.


     "AWB row
  lv_item_row &&=
   |        <Row1>| &&
   |          <awb_text>{ lv_awb_label }</awb_text>| &&
   |          <awb>{ lv_awb_value }</awb>| &&
   |          <awb_date>{ lv_awb_date }</awb_date>| &&
   |        </Row1>|.
  lv_item_row &&=
   |        <Row1>| &&
   |          <TextField>{ lv_amt_inword }</TextField>| &&
   |          <tot_amt>{ lv_total }</tot_amt>| &&
   |          <curry>{ wa_saleshead-TransactionCurrency }</curry>| &&
   |        </Row1>|.
   lv_items = lv_items && lv_item_row.
   clear : lv_item_row.
*  ENDLOOP.
*--------------------------------------
* XML FOOTER
*--------------------------------------

DATA(lv_footer) =
 |      </Table2>| &&
 |      <Subform4>| &&
 |        <bank_details>{ lv_bank_dlts }</bank_details>| &&
 |      </Subform4>| &&
 |    </SUBFORM10>| &&
 |  </Subform2>| &&
 |</form1>|.

rv_xml = |{ lv_header } { lv_items } { lv_footer }|.



  ENDMETHOD.


  METHOD escape_xml.

    rv_out = CONV string( iv_in ).

    REPLACE ALL OCCURRENCES OF '&'  IN rv_out WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<'  IN rv_out WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>'  IN rv_out WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"'  IN rv_out WITH '&quot;'.
    REPLACE ALL OCCURRENCES OF '''' IN rv_out WITH '&apos;'.

  ENDMETHOD.


METHOD num2words.

    TYPES: BEGIN OF ty_map,
             num  TYPE i,
             word TYPE string,
           END OF ty_map.

    DATA: lt_map TYPE STANDARD TABLE OF ty_map,
          ls_map TYPE ty_map.

    DATA: lv_int  TYPE i,
          lv_dec  TYPE i,
          lv_inp1 TYPE string,
          lv_inp2 TYPE string.

    DATA: lv_result TYPE string,
          lv_decres TYPE string.

    IF iv_num IS INITIAL.
      RETURN.
    ENDIF.

    lt_map = VALUE #(
      ( num = 0  word = 'Zero' )
      ( num = 1  word = 'One' )
      ( num = 2  word = 'Two' )
      ( num = 3  word = 'Three' )
      ( num = 4  word = 'Four' )
      ( num = 5  word = 'Five' )
      ( num = 6  word = 'Six' )
      ( num = 7  word = 'Seven' )
      ( num = 8  word = 'Eight' )
      ( num = 9  word = 'Nine' )
      ( num = 10 word = 'Ten' )
      ( num = 11 word = 'Eleven' )
      ( num = 12 word = 'Twelve' )
      ( num = 13 word = 'Thirteen' )
      ( num = 14 word = 'Fourteen' )
      ( num = 15 word = 'Fifteen' )
      ( num = 16 word = 'Sixteen' )
      ( num = 17 word = 'Seventeen' )
      ( num = 18 word = 'Eighteen' )
      ( num = 19 word = 'Nineteen' )
      ( num = 20 word = 'Twenty' )
      ( num = 30 word = 'Thirty' )
      ( num = 40 word = 'Forty' )
      ( num = 50 word = 'Fifty' )
      ( num = 60 word = 'Sixty' )
      ( num = 70 word = 'Seventy' )
      ( num = 80 word = 'Eighty' )
      ( num = 90 word = 'Ninety' )
    ).

    SPLIT iv_num AT '.' INTO lv_inp1 lv_inp2.
    lv_int = lv_inp1.
    IF lv_inp2 IS NOT INITIAL.
      lv_dec = lv_inp2.
    ENDIF.

    " ---- INTEGER PART ----
    IF lv_int < 20.
      READ TABLE lt_map INTO ls_map WITH KEY num = lv_int.
      lv_result = ls_map-word.

    ELSEIF lv_int < 100.
      READ TABLE lt_map INTO ls_map WITH KEY num = ( lv_int DIV 10 ) * 10.
      lv_result = ls_map-word.
      IF lv_int MOD 10 > 0.
        READ TABLE lt_map INTO ls_map WITH KEY num = lv_int MOD 10.
        lv_result = |{ lv_result } { ls_map-word }|.
      ENDIF.

    ELSEIF lv_int < 1000.
      lv_result =
        num2words( iv_num = |{ lv_int DIV 100 }|
                   iv_major = iv_major
                   iv_minor = iv_minor
                   iv_top_call = abap_false )
        && ' Hundred'.

      IF lv_int MOD 100 > 0.
        lv_result = |{ lv_result } |
          && num2words( iv_num = |{ lv_int MOD 100 }|
                        iv_major = iv_major
                        iv_minor = iv_minor
                        iv_top_call = abap_false ).
      ENDIF.

    ELSEIF lv_int < 100000.
      lv_result =
        num2words( iv_num = |{ lv_int DIV 1000 }|
                   iv_major = iv_major
                   iv_minor = iv_minor
                   iv_top_call = abap_false )
        && ' Thousand'.

      IF lv_int MOD 1000 > 0.
        lv_result = |{ lv_result } |
          && num2words( iv_num = |{ lv_int MOD 1000 }|
                        iv_major = iv_major
                        iv_minor = iv_minor
                        iv_top_call = abap_false ).
      ENDIF.

    ELSE.
      lv_result =
        num2words( iv_num = |{ lv_int DIV 100000 }|
                   iv_major = iv_major
                   iv_minor = iv_minor
                   iv_top_call = abap_false )
        && ' Lakh'.

      IF lv_int MOD 100000 > 0.
        lv_result = |{ lv_result } |
          && num2words( iv_num = |{ lv_int MOD 100000 }|
                        iv_major = iv_major
                        iv_minor = iv_minor
                        iv_top_call = abap_false ).
      ENDIF.
    ENDIF.

    " ---- APPEND CURRENCY ONLY ONCE ----
    rv_words = lv_result.

    IF iv_top_call = abap_true.
      IF lv_dec > 0.
        lv_decres =
          num2words(
            iv_num      = |{ lv_dec }|
            iv_major    = iv_major
            iv_minor    = iv_minor
            iv_top_call = abap_false
          ).
        rv_words = |{ rv_words } { iv_major } and { lv_decres } { iv_minor } Only|.
      ELSE.
        rv_words = |{ rv_words } { iv_major } Only|.
      ENDIF.
    ENDIF.

    CONDENSE rv_words.
    TRANSLATE rv_words TO UPPER CASE.

  ENDMETHOD.

ENDCLASS.
