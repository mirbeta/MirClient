{
  @html(<b>)
  Information Objects
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This unit defines easy-to-use helper classes for handling user-defined data
  structures (native Values, Arrays, Records, DataSets, Linked Objects, etc).
}
unit rtcInfo;

{$INCLUDE rtcDefs.inc}

interface

uses
  {$IFDEF RTC_FMI}
    FMX_Types, {$DEFINE RTC_FMXOBJECT}
  {$ELSE}
    {$IFDEF RTC_FMX}
      FMX.Types, {$DEFINE RTC_FMXOBJECT}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WINDOWS}
    Windows, // GetTickCount() and file access API
  {$ELSE}
    {$IFDEF POSIX}
      {$IFDEF MACOSX}
        Macapi.CoreServices, // used for getTickTime
      {$ELSE}
        Posix.SysTime,
      {$ENDIF}
      Posix.Pthread,
      Posix.Unistd,
      Posix.Stdio,
    {$ELSE}
      {$IFDEF FPC}
        BaseUnix, // used for GetTickTime
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}


  {$IFDEF NEXTGEN}
  // needed for TObjectList = TList<TObject>;
  System.Generics.Collections,
  {$ENDIF}

  Classes,
  SysUtils,

  {$IFNDEF IDE_1}
  Variants,
  {$ELSE}
  FileCtrl,
  {$ENDIF}

  rtcTypes,
  rtcLog,
  rtcFastStrings,
  rtcSyncObjs,

  memXList,
  memObjList,
  memStringObjList,
  memStringPtrList;

var
  // Log errors raised from RTC Value object classes?
  LOG_INFO_ERRORS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

  // Full Name of the application or extension in which RTC is running.
  AppFileName:String='';

  { Default memory cache size when accepting FormPost data (Param property).
    When cache memory is full, data will start being written to a temporary file,
    to reduce memory usage. This temporary file is created and deleted automaticaly by RTC,
    to keep the usage of the Param property simple. }
  RTC_FORMPOST_CACHESIZE:integer=16000;

{$IFDEF COMPRESS}
  { Minimum data size to be compressed when using automatic compression/decompression.
    Data smaller than this size will not be compressed, to reduce the CPU usage.
    Default value is 256. Values smaller than 32 bytes will only increase the CPU usage
    without having any positive effects, because very small data chunks can NOT be compressed
    (there are at least 10 bytes of header information needed in compressed data). }
  RTC_MIN_COMPRESS_SIZE:integer=256;
{$ENDIF}

  { JSON Generator:
    Should we escape the "/" character in JSON, converting it into "\/"?

    If False, "/" will NOT be changed into "\/" when generating a JSON String.
    This would make the output String shorter, but it will also make it
    *impossible* for plain Strings to have a "\/" anywhere, because
    "/" will remain "/", but "\/" will be converted to "\\/".

    If True, all "/" characters in a string or name will be sent as "\/". }
  RTC_JSON_GenEscapeSlash:boolean=True;

  { JSON Generator:
    Encode DateTime type as "\/Date(<milliseconds-since-1970-1-1>)\/" in JSON?
    When False, plain ISO DateTime format will be used when generating JSON. }
  RTC_JSON_GenTypedDateTime:boolean=False;

  { JSON Generator:
    Encode Exception type with a "\/error\/" prefix in front of the message string?
    When False, plain message String will be generated for Exception types in JSON. }
  RTC_JSON_GenTypedException:boolean=False;

  { JSON Generator:
    Encode ByteStram data with a "\/base64\/" prefix in front of the Mime_Encoded stream?
    When False, there will be no special prefix, only the ByteStream will be encoded. }
  RTC_JSON_GenTypedByteStream:boolean=False;

  { JSON Generator:
    Include FunctionName as "\/method" parameter when encoding a Function object to JSON?
    When False, FunctionName will NOT be stored when generating JSON for a Function object. }
  RTC_JSON_GenTypedFunctions:boolean=False;

  {JSON Parser:
    Check for typed DateTime values in "\/date\/" and "\/Date(..)\/" and store them as DateTime fields? }
  RTC_JSON_ParseTypedDateTime:boolean=False;

  {JSON Parser:
    Check for typed ByteStream (starts with "\/base64\/") and store them as ByteStream objects? }
  RTC_JSON_ParseTypedByteStream:boolean=False;

  {JSON Parser:
    Check for typed Functions (FunctionName in "\/method") and store them as Function objects? }
  RTC_JSON_ParseTypedFunctions:boolean=False;

  {JSON Parser:
    Check for typed DataSets ("\/dsfields" and "\/dsrows" in an record) and store them as DataSet objects? }
  RTC_JSON_ParseTypedDataSet:boolean=False;

  {JSON Parser:
    Check for typed Exceptions (starts with "\/error\/") and store them as Exception type? }
  RTC_JSON_ParseTypedException:boolean=False;

const
  // Member name used in XML-RPC <struct> to enumerate DataSet Fields
  RTC_XMLRPC_DataSetFieldsName='RTC.DATASET.FIELDS';
  // Member name used in XML-RPC <struct> to enumerate DataSet Rows
  RTC_XMLRPC_DataSetRowsName='RTC.DATASET.ROWS';
  // XML-RPC field name used when sending params to a function as an array rather than a record
  RTC_XMLRPC_ParamsAsArrayName='PARAMS';

  // JSON-RPC field name used when sending params to a function as an array rather than a record
  RTC_JSON_ParamsAsArrayName='params';

  // Reserved parameter name for keeping Field definitions when generating JSON for a DataSet object
  RTC_JSON_DataSetFieldsName = '"\/dsfields"';
  // Reserved parameter name for keeping Table rows when generating JSON for a DataSet object
  RTC_JSON_DataSetRowsName   = '"\/dsrows"';
  // Reserved parameter name for "FunctionName" when generating JSON for a FunctionInfo object
  RTC_JSON_FunctionName      = '"\/method"';

  // Opening String for DateTime values encoded as milliseconds since 1970-01-01
  RTC_JSON_DateTimeStr       = '"\/Date(';
  // Opening String for DateTime values encoded using the ISO format
  RTC_JSON_DateTimeISOStr    = '"\/date\/';
  // Opening String for ByteStream objects encoded as Base64
  RTC_JSON_ByteStreamStr     = '"\/base64\/';
  // Opening String for Exception values
  RTC_JSON_ExceptionStr      = '"\/error\/';

  {$IFDEF IDE_XE2up}
    RTC_INVALID_FILE_HDL = INVALID_HANDLE_VALUE;
  {$ELSE}
    RTC_INVALID_FILE_HDL = -1;
  {$ENDIF}

  RTC_NIL_OBJECT_ID = 0;
  RTC_NIL_OBJECT = nil;

type
  {$IFDEF NEXTGEN}
    TObjectList = TList<TObject>;
  {$ELSE}
    TObjectList = TList;
  {$ENDIF}

  {$IFDEF IDE_XE2up}
    TRtcFileHdl=THandle;
  {$ELSE}
    TRtcFileHdl=integer;
  {$ENDIF}

  // @exclude
  RtcIntPtr = rtcTypes.RtcIntPtr;

  // Unicode String
  RtcWideString = rtcTypes.RtcWideString;
  // Unicode character
  RtcWideChar = rtcTypes.RtcWideChar;

  // 8-bit Character String (NOT UNICODE!)
  RtcString = rtcTypes.RtcString;
  // 8-bit Character (NOT UNICODE!)
  RtcChar = rtcTypes.RtcChar;

  // @exclude
  RtcPtrAnsiChar = rtcTypes.RtcPtrAnsiChar;
  // @exclude
  RtcPtrWideChar = rtcTypes.RtcPtrWideChar;

  // @exclude
  PRtcByte = ^Byte;

  // Array of Bytes
  RtcByteArray = rtcTypes.RtcByteArray;

  // RTC Info exception
  ERtcInfo = class(Exception);

  // RTC Compression Level
  TRtcCompressLevel = (// no compression
                       cNone,
                       // fastest compression (low compress rate)
                       cFast,
                       // default compression
                       cDefault,
                       // maximum compression (slow)
                       cMax);

  // SSL Certificate Store Type
  TRtcCertStoreType = (// Accept any certificate (Ignore certificate errors)
                       certAny,
                       // Do not use any certificates
                       certNone,
                       // Certification Authority certificates
                       certCA,
                       // A certificate store that holds certificates with associated private keys
                       certMY,
                       // Root Certificates
                       certROOT,
                       // Software Publisher Certificates
                       certSPC);

  // File Access Mode
  TRtcFileAccessMode= (// Allow other apps to read and write to the same file
                       rtc_ShareDenyNone,
                       // Allow other apps to read, but not to write to the file
                       rtc_ShareDenyWrite,
                       // Deny all access to the file - exclusive access mode!
                       rtc_ShareExclusive);

  // RTC Value Types
  TRtcValueTypes = (// No value assigned
                    rtc_Null,
                    // variable name: Check local or Session variables for a result
                    rtc_Variable,
                    // Function call (Function will be called to get a result)
                    rtc_Function,
                    // Exception message (returned as a Result from a function call which ended in an exception being raised)
                    rtc_Exception,
                    // Array (starting from index 0)
                    rtc_Array,
                    // Record
                    rtc_Record,
                    // DataSet
                    rtc_DataSet,
                    // Unicode / Text String (Unicode String stored and transported using UTF-8 encoding)
                    rtc_Text,
                    // "Ansi" String value (strings up to 2GB)
                    rtc_String,
                    // Wide String value (wide strings up to 2GB)
                    rtc_WideString,
                    // Boolean value
                    rtc_Boolean,
                    // Integer value
                    rtc_Integer,
                    // Cardinal (LongWord) value
                    rtc_Cardinal,
                    // Large Integer value (int64)
                    rtc_LargeInt,
                    // Floating-point value (double)
                    rtc_Float,
                    // Currency value
                    rtc_Currency,
                    // Date and Time value
                    rtc_DateTime,
                    // Byte Stream
                    rtc_ByteStream,
                    // RTC Linked Object ID
                    rtc_OID,
                    // Byte Array
                    rtc_ByteArray,
                    // Variant / Any Type
                    rtc_Variant);

const
  RTC_BOOLEAN_TYPES=[rtc_Boolean, rtc_Variant];
  RTC_EXCEPTION_TYPES=[rtc_Exception, rtc_Variant];
  RTC_VARIABLE_TYPES=[rtc_Variable, rtc_Variant];

  RTC_BYTESTREAM_TYPES=[rtc_ByteStream, rtc_ByteArray, rtc_String, rtc_Variant];
  RTC_STRING_TYPES=[rtc_String, rtc_Text, rtc_WideString, rtc_Variant];

  RTC_INTEGER_TYPES=[rtc_OID, rtc_Integer, rtc_LargeInt, rtc_Cardinal, rtc_Variant];
  RTC_FLOAT_TYPES=[rtc_DateTime, rtc_Float, rtc_Currency, rtc_Variant];

  RTC_RECORD_TYPES=[rtc_Record];
  RTC_ARRAY_TYPES=[rtc_Array];
  RTC_FUNCTION_TYPES=[rtc_Function];
  RTC_DATASET_TYPES=[rtc_DataSet];

type
  TRtcDataFormat = ( // The best format for communicating between RTC Clients and Servers
                     fmt_RTC,
                     // XML-RPC format: makes it possible to communicate with non-RTC Clients and Servers
                     fmt_XMLRPC);

  TRtcDataFormatSupport = set of TRtcDataFormat;

  TRtcFieldTypes = ( ft_Unknown, ft_String, ft_Smallint, ft_Integer, ft_Word,
                     ft_Boolean, ft_Float, ft_Currency, ft_BCD, ft_Date, ft_Time, ft_DateTime,
                     ft_Bytes, ft_VarBytes, ft_AutoInc, ft_Blob, ft_Memo, ft_Graphic, ft_FmtMemo,
                     ft_ParadoxOle, ft_DBaseOle, ft_TypedBinary, ft_Cursor, ft_FixedChar, ft_WideString,
                     ft_Largeint, ft_ADT, ft_Array, ft_Reference, ft_DataSet, ft_OraBlob, ft_OraClob,
                     ft_Variant, ft_Interface, ft_IDispatch, ft_Guid, ft_TimeStamp, ft_FMTBcd,
                     ft_FixedWideChar, ft_WideMemo, ft_OraTimeStamp, ft_OraInterval,
                     ft_LongWord, ft_Shortint, ft_Byte, ft_Extended, ft_Connection, ft_Params, ft_Stream,
                     ft_TimeStampOffset, ft_Object, ft_Single
                     );

  { @abstract(Session lock type) }
  TRtcSessionLockType=({ Allow access to anyone. No client data will be used for identifying clients,
                         which means that any client knowing which Session IDs are open,
                         will have access to those sessions, regardless of its IP address or other header values.
                         @html(<br><br>)
                         This setting is not recommended for Servers available on the Internet. }
                       sesNoLock,
                       { Allow access to an opened Session only to Clients coming from
                         the same IP as the client which has created the session.
                         @html(<br><br>)
                         This setting is not recommended for Web Applications which need to be accessible
                         by anyone, since people behind proxy servers with changing IP addresses will not
                         be able to "stay logged in" (when their IP changes, they will lose access to their Session). }
                       sesIPLock,
                       { This is the default Session Lock setting, which should work for all clients.
                         It will provide maximum security for clients which are NOT behind a proxy server,
                         while still allowing access to clients behind proxy servers with changing IP addresses
                         (the "X-FORWARDED-FOR" header has to be set by the proxy forwarding client requests).
                         @html(<br><br>)
                         If the client opening the Session had the "X-FORWARDED-FOR" header set,
                         any client with the same "X-FORWARDED-FOR" header will be allowed access to his Session
                         (it just has to use the same Session ID as the client which has opened/created the session).
                         If "X-FORWARDER-FOR" header was not set by the client creating the session,
                         Peer IP address will be used for client identification. }
                       sesFwdLock,
                       { Session will always be locked to the Peer IP address,
                         *plus* to the "X-FORWARDED-FOR" request header,
                         if it was set for the client which has opened the session.
                         @html(<br><br>)
                         This setting is not recommended for Web Applications which need to be accessible
                         to a wide public audience, since people behind proxy servers with changing IP addresses
                         will NOT be able to "stay logged in". When their IP address changes,
                         they will lose access to their Session data and need to log in again. }
                       sesIPFwdLock);

const
  // @exclude
  RTC_FIELD2VALUE_TYPES: array[TRtcFieldTypes] of TRtcValueTypes =
                   ( rtc_NULL, // ft_Unknown
                     {$IFDEF Unicode} rtc_Text {$ELSE} rtc_String {$ENDIF}, // ft_String
                     rtc_Integer, // ft_Smallint
                     rtc_Integer, // ft_Integer
                     rtc_Integer, // ft_Word
                     rtc_Boolean, // ft_Boolean
                     rtc_Float, // ft_Float
                     rtc_Currency, // ft_Currency
                     rtc_Currency, // ft_BCD
                     rtc_DateTime, // ft_Date
                     rtc_DateTime, // ft_Time
                     rtc_DateTime, // ft_DateTime
                     rtc_String, // ft_Bytes
                     rtc_String, // ft_VarBytes,
                     rtc_Integer, // ft_AutoInc
                     rtc_String, // ft_Blob
                     {$IFDEF Unicode} rtc_Text {$ELSE} rtc_String {$ENDIF}, // ft_Memo
                     rtc_String, // ft_Graphic
                     {$IFDEF Unicode} rtc_Text {$ELSE} rtc_String {$ENDIF}, // ft_FmtMemo
                     rtc_String, // ft_ParadoxOle
                     rtc_String, // ft_DBaseOle
                     rtc_String, // ft_TypedBinary
                     rtc_NULL, // ft_Cursor
                     {$IFDEF Unicode} rtc_Text {$ELSE} rtc_String {$ENDIF}, // ft_FixedChar
                     {$IFDEF Unicode} rtc_Text {$ELSE} rtc_WideString {$ENDIF}, // ft_WideString
                     rtc_LargeInt, // ft_Largeint
                     rtc_Record, // ft_ADT
                     rtc_Array, // ft_Array
                     rtc_Record, // ft_Reference
                     rtc_DataSet, // ft_DataSet
                     rtc_String, // ft_OraBlob,
                     rtc_String, // ft_OraClob,
                     rtc_String, // ft_Variant
                     rtc_String, // ft_Interface
                     rtc_String, // ft_IDispatch,
                     rtc_String, // ft_Guid,
                     rtc_DateTime, // ft_TimeStamp
                     rtc_Currency, // ft_FMTBcd
                     {$IFDEF Unicode} rtc_Text {$ELSE} rtc_WideString {$ENDIF}, // ft_FixedWideChar
                     {$IFDEF Unicode} rtc_Text {$ELSE} rtc_WideString {$ENDIF}, // ft_WideMemo
                     rtc_DateTime, // ft_OraTimeStamp
                     rtc_String, // ft_OraInterval
                     rtc_Cardinal, // ft_LongWord
                     rtc_Integer, // ft_Shortint
                     rtc_Integer, // ft_Byte
                     rtc_Float, // ft_Extended
                     rtc_NULL, // ft_Connection
                     rtc_Record, // ft_Params
                     rtc_ByteStream, // ft_Stream
                     rtc_String, // ft_TimeStampOffset
                     rtc_String, // ft_Object
                     rtc_Integer); // ft_Single

  // @exclude
  RTC_TYPE2STR_CONV: array[TRtcValueTypes] of RtcString =
                  ( 'X', // rtc_Null,
                    'V', // rtc_Variable,
                    'FC', // rtc_Function,
                    'E', // rtc_Exception,
                    'AR', // rtc_Array,
                    'RE', // rtc_Record,
                    'DS', // rtc_DataSet,
                    'T', // rtc_Text,
                    'S', // rtc_String,
                    'W', // rtc_WideString,
                    'B', // rtc_Boolean,
                    'I', // rtc_Integer,
                    'K', // rtc_Cardinal,
                    'L', // rtc_LargeInt,
                    'F', // rtc_Float,
                    'C', // rtc_Currency,
                    'D', // rtc_DateTime,
                    'BS',// rtc_ByteStream
                    'O', // rtc_OID
                    'BA',// rtc_ByteArray
                    ''); // Variant is NOT sent nor received

  RTC_TYPE2FULLNAME_CONV: array[TRtcValueTypes] of RtcString =
                  ( 'Null', // rtc_Null,
                    'Variable', // rtc_Variable,
                    'FunctionCall', // rtc_Function,
                    'Exception', // rtc_Exception,
                    'Array', // rtc_Array,
                    'Record', // rtc_Record,
                    'DataSet', // rtc_DataSet,
                    'Text', // rtc_Text,
                    'String', // rtc_String,
                    'WideString', // rtc_WideString,
                    'Boolean', // rtc_Boolean,
                    'Integer', // rtc_Integer,
                    'Cardinal', // rtc_Cardinal,
                    'LargeInt', // rtc_LargeInt,
                    'Float', // rtc_Float,
                    'Currency', // rtc_Currency,
                    'DateTime', // rtc_DateTime,
                    'ByteStream', //rtc_ByteStream
                    'OID', // rtc_OID
                    'ByteArray', // rtc_ByteArray
                    'Variant'); // rtc_Variant

  // @exclude
  RTC_FIELD2STR_CONV: array[TRtcFieldTypes] of RtcString =
                   ('U', // ft_Unknown
                    'S', // ft_String
                    'SI', // ft_Smallint
                    'I', // ft_Integer
                    'WI', // ft_Word
                    'B', // ft_Boolean
                    'F', // ft_Float
                    'C', // ft_Currency
                    'BC', // ft_BCD
                    'DD', // ft_Date
                    'T', // ft_Time
                    'D', // ft_DateTime
                    'BY', // ft_Bytes
                    'VB', // ft_VarBytes
                    'AI', // ft_AutoInc
                    'O', // ft_Blob
                    'M', // ft_Memo
                    'G', // ft_Graphic
                    'FM', // ft_FmtMemo
                    'PO', // ft_ParadoxOle
                    'DO', // ft_DBaseOle
                    'TB', // ft_TypedBinary
                    'CU', // ft_Cursor
                    'FC', // ft_FixedChar
                    'W', // ft_WideString
                    'L', // ft_Largeint
                    'AD', // ft_ADT
                    'AR', // ft_Array
                    'RF', // ft_Reference
                    'DS', // ft_DataSet
                    'OB', // ft_OraBlob
                    'OC', // ft_OraClob
                    'V', // ft_Variant
                    'IT', // ft_Interface
                    'ID', // ft_IDispatch
                    'GU', // ft_Guid
                    'DT', // ft_TimeStamp
                    'FB', // ft_FMTBcd
                    'WC', // ft_WideFixChar
                    'WM', // ft_WideMemo
                    'OT', // ft_OraTimeStamp
                    'OI', // ft_OraInterval
                    'WL', // ft_LongWord
                    'SH', // ft_Shortint
                    'BB', // ft_Byte
                    'FE', // ft_Extended
                    'CO', // ft_Connection
                    'PA', // ft_Params
                    'ST', // ft_Stream
                    'TO', // ft_TimeStampOffset
                    'OJ', // ft_Object
                    'SN'); // ft_Single

  // @exclude
  RTC_VALUE2FIELD_TYPES: array[TRtcValueTypes] of TRtcFieldTypes  =
                   (ft_Unknown, // rtc_Null
                    ft_Unknown, // rtc_Variable
                    ft_Unknown, // rtc_Function
                    ft_String, // rtc_Exception
                    ft_Array, // rtc_Array
                    ft_ADT, // rtc_Record
                    ft_DataSet, // rtc_DataSet
                    ft_String, // rtc_Text
                    ft_String, // rtc_String
                    ft_WideString, // rtc_WideString
                    ft_Boolean, // rtc_Boolean
                    ft_Integer, // rtc_Integer
                    ft_Largeint, // rtc_Cardinal
                    ft_Largeint, // rtc_LargeInt
                    ft_Float, // rtc_Float
                    ft_Currency, // rtc_Currency
                    ft_DateTime, // rtc_DateTime
                    ft_Blob, // rtc_ByteStream
                    ft_Largeint, // rtc_OID
                    ft_Blob, // rtc_ByteArray
                    ft_Unknown); // rtc_Variant

type
  { All RTC components use at least one class declared in this unit.

    For Delphi to add this unit to uses clause,
    all RTC components inherit from this class.

    @exclude }
{$IFDEF RTC_FMXOBJECT}
  TRtc_Component = class(TFmxObject);
{$ELSE}
  TRtc_Component = class(TComponent);
{$ENDIF}

  { @exclude }
  TRtcObjectID = int64;

  { @exclude }
  ERtcObjectLinks = class(Exception);

  // Type to use to store Floating-point values (default:double; could be changed to "extended" for maximum precision)
  rtcFloat = double;

  // Type to use to store Integer values (default:LongInt; could be changed to "int64" for maximum precision)
  rtcInteger = longint;

  // Type to use to store Cardinal values (default:LongWord)
  rtcCardinal = longword;

  // Type to use to store LargeInt values (default:int64)
  rtcLargeInt = int64;

  // Type to use to store ByteStream data (default:TMemoryStream);
  rtcByteStream = TMemoryStream;

  // @exclude
  rtcClosingTagsType = array of RtcString;

  { @abstract(Basic class for 'storable' objects)

    This is the object class which has to be extended
    with your object information if you want to pass
    objects to RTC components. }
  TRtcObject = class(TObject)
  public
    { Implement the Kill method so it releases the object from memory.
      Calling 'Free' from anyone else than the class creator (you)
      could result in freeing objects more than once, which is "unhealthy".
      The Kill method will be called on all objects that still
      remain in the list (info list, job queue, etc) when the
      list is being cleared, for whatever reason. }
    procedure Kill; virtual; abstract;
    end;

  TRtcFunctionInfo = class; // forward

  TRtcValue = class; // forward
  TRtcValueObject = class; // forward
  TRtcDataSet = class; // forward
  TRtcRecord = class; // forward
  TRtcArray = class; // forward
  TRtcVariableName = class; // forward
  TRtcExceptionValue = class; // forward
  TRtcOIDValue = class; // forward
  TRtcByteStream = class; // forward
  TRtcByteArray = class; // forward

  { @abstract(All RTC Value Objects extend this class) }
  TRtcValueObject = class(TRtcObject)
  protected
    // @exclude
    procedure CopyFrom(Value:TRtcValueObject); virtual; abstract;

    // Object has been extracted from its parent
    procedure Extracted; virtual;

    { Check object type
      @exclude}
    function GetType:TRtcValueTypes; virtual; abstract;

    { Check if Value can be Read as Type "typ"
      @exclude }
    function TypeCheck(typ:TRtcValueTypes):boolean; virtual; abstract;

    // @exclude
    class function ObjectFromVariant(const v:Variant):TRtcValueObject;

    // @exclude
    class function ObjectFromType(const typ:TRtcValueTypes):TRtcValueObject;

    {*** RTC format serializing and deserializing functions ***}

    // @exclude
    class function code_toLongString(const typ, s:RtcString):RtcString;
    // @exclude
    class function code_toByteStream(const typ:RtcString; bs:TStream):RtcString;
    // @exclude
    class function code_toByteArray(const typ:RtcString; const ba:RtcByteArray):RtcString;
    // @exclude
    class function code_toShortString(const typ, s:RtcString):RtcString;
    class function code_toShortNameString(const typ:RtcString; const s:RtcWideString):RtcString;
    // @exclude
    class function code_toNameString(const s:RtcWideString):RtcString;
    // @exclude
    class function code_toMidString(const s:RtcString):RtcString;
    // @exclude
    class function code_toEndString(const s:RtcString):RtcString;

    // @exclude
    class function code_checkStrType(const s:RtcString; const at:integer):TRtcValueTypes;
    // @exclude
    class function code_fromLongString(const typ:RtcString; const s:RtcString; var at:integer):RtcString;
    // @exclude
    class procedure code_fromByteStream(const typ:RtcString; const s:RtcString; var at:integer; const bs:TStream);
    // @exclude
    class function code_fromByteArray(const typ:RtcString; const s:RtcString; var at:integer):RtcByteArray;
    // @exclude
    class function code_fromShortString(const typ:RtcString; const s:RtcString; var at:integer):RtcString;
    class function code_fromShortNameString(const typ:RtcString; const s:RtcString; var at:integer):RtcWideString;
    // @exclude
    class function code_fromNameString(const s: RtcString; var at:integer):RtcWideString;
    // @exclude
    class function code_fromMidString(const s: RtcString; var at:integer):RtcString;
    // @exclude
    class function code_fromEndString(const s: RtcString; var at:integer):RtcString;

    // @exclude
    class function ObjectFromCode(const s:RtcString; var at:integer):TRtcValueObject; overload;
    // @exclude
    class function ObjectFromCode(const s:RtcString):TRtcValueObject; overload;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); overload; virtual; abstract;
    { Fill object information from RtcString.
      @exclude }
    procedure from_Code(const s:RtcString); overload;

    {*** XML RPC serilizing and deserializing functions ***}

    // @exclude
    class function xmlrpc_FirstCloseTag(const closing_tags:rtcClosingTagsType):RtcString;
    // @exclude
    class procedure xmlrpc_OpenTag(const tag:RtcString; var closing_tags:rtcClosingTagsType);
    // @exclude
    class function xmlrpc_CloseTag(const tag:RtcString; var closing_tags:rtcClosingTagsType):boolean;
    // @exclude
    class function xmlrpc_TagsToXML(const closing:rtcClosingTagsType):RtcString;

    // @exclude
    class function xmlrpc_checkStrType(const s:RtcString; const at:integer):TRtcValueTypes;
    // @exclude
    class procedure xmlrpc_skipWhitespace(const s:RtcString; var at:integer);
    // @exclude
    class function xmlrpc_checkTag(const s:RtcString; at:integer):RtcString;
    // @exclude
    class procedure xmlrpc_skipTag(const s:RtcString; var at:integer; skipWhitespace:boolean=True);
    // @exclude
    class function xmlrpc_readTag(const s:RtcString; var at:integer; const tag_want:RtcString=''; skipWhitespace:boolean=True):RtcString;
    // @exclude
    class function xmlrpc_readValue(const s:RtcString; var at:integer):RtcString;
    // @exclude
    class function xmlrpc_readTrimValue(const s:RtcString; var at:integer):RtcString;
    class function xmlrpc_readTrimNameValue(const s:RtcString; var at:integer):RtcWideString;
    // @exclude
    class procedure xmlrpc_skipNull(const s: RtcString; var at: integer);

    // @exclude
    class procedure xmlrpc_readByteStream(const s:RtcString; var at:integer; const bs:TStream);
    // @exclude
    class function xmlrpc_readByteArray(const s:RtcString; var at:integer):RtcByteArray;
    // @exclude
    class function xmlrpc_writeByteStream(bs:TStream):RtcString;
    // @exclude
    class function xmlrpc_writeByteArray(const ba:RtcByteArray):RtcString;
    // @exclude
    class function xmlrpc_readString(const s:RtcString; var at:integer):RtcString;
    class function xmlrpc_readNameString(const s:RtcString; var at:integer):RtcWideString;
    // @exclude
    class function xmlrpc_writeString(const s:RtcString):RtcString;
    class function xmlrpc_writeNameString(const s:RtcWideString):RtcString;

    // @exclude
    class procedure xmlrpc_skipValueOpen(const tag:RtcString; const s: RtcString; var at: integer; var closing_tags:rtcClosingTagsType);
    // @exclude
    class procedure xmlrpc_skipValueClose(const s:RtcString; var at:integer; var closing_tags:rtcClosingTagsType);

    // @exclude
    class function ObjectFromXMLrpc(const s:RtcString; var at:integer):TRtcValueObject; overload;
    // @exclude
    class function ObjectFromXMLrpc(const s:RtcString):TRtcValueObject; overload;

    { Fill object information from XML RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); overload; virtual; abstract;
    { Fill object information from RtcString.
      @exclude }
    procedure from_XMLrpc(const s:RtcString); overload;

    {**** JSON serializing and deserializing functions ****}

    // @exclude
    class function json_checkStrType(const s:RtcWideString; const at:integer):TRtcValueTypes;
    // @exclude
    class procedure json_skipWhitespace(const s:RtcWideString; var at:integer);
    // @exclude
    class function json_readString(const s:RtcWideString; var at:integer; skippedQuote:boolean=False):RtcWideString;
    // @exclude
    class function json_readNumber(const s:RtcWideString; var at:integer):RtcWideString;
    // @exclude
    class procedure json_readByteStream(const s:RtcWideString; var at:integer; const bs:TStream);
    // @exclude
    class function json_readByteArray(const s:RtcWideString; var at:integer):RtcByteArray;

    // @exclude
    class procedure json_skipNull(const s: RtcWideString; var at: integer);
    // @exclude
    class procedure json_skipTag(const tag:RtcWideString; const s: RtcWideString; var at: integer);
    // @exclude
    class function json_checkTag(const tag:RtcWideString; const s: RtcWideString; var at: integer; autoSkip:boolean=False):boolean;

    // @exclude
    class function json_writeByteStream(bs:TStream):RtcString;
    // @exclude
    class function json_writeByteArray(const ba:RtcByteArray):RtcString;

    // @exclude
    class function ObjectFromJSON(const s:RtcWideString; var at:integer):TRtcValueObject; overload;
    // @exclude
    class function ObjectFromJSON(const s:RtcWideString):TRtcValueObject; overload;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); overload; virtual; abstract;
    { Fill object information from JSON String.
      @exclude }
    procedure from_JSON(const s:RtcWideString); overload;

  public
    // @exclude
    procedure Kill; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; virtual; abstract;

    { Create a RtcString containing all object information,
      serialized using a packed RTC-coded format. }
    procedure to_Code(const Result:TRtcHugeString); virtual; abstract;

    { Create a RtcString containing all object information,
      serialized using a packed RTC-coded format. }
    function toCode:RtcString;

    { Create a RtcByteArray containing all object information,
      serialized using a packed RTC-coded format. }
    function toCodeEx:RtcByteArray;

    { Create the XML-RPC RtcString containing all object information,
      serialized using the XML RPC standard format. }
    procedure to_XMLRPC(const Result:TRtcHugeString); virtual; abstract;

    { Create the XML-RPC RtcString containing all object information,
      serialized using the XML RPC standard format. }
    function toXMLrpc:RtcString;

    { Create an XML-RPC Byte Array containing all object information,
      serialized using the XML RPC standard format. }
    function toXMLrpcEx:RtcByteArray;

    { Create the XML-RPC RtcString containing all object information,
      packed in the XML-RPC request header and footer. }
    function toXMLrpcRequest:RtcString;

    { Create the XML-RPC Byte Array containing all object information,
      packed in the XML-RPC request header and footer. }
    function toXMLrpcRequestEx:RtcByteArray;

    { Create the XML-RPC RtcString containing all object information,
      packed in the XML-RPC response header and footer. }
    function toXMLrpcResponse:RtcString;

    { Create the XML-RPC Byte Array containing all object information,
      packed in the XML-RPC response header and footer. }
    function toXMLrpcResponseEx:RtcByteArray;

    { Create a JSON Ansi String containing all object information,
      serialized using the JSON standard format. }
    procedure to_JSON(const Result:TRtcHugeString); virtual; abstract;

    { Create the JSON Ansi String containing all object information,
      serialized using the JSON standard format. }
    function toJSON:RtcString;

    { Create the JSON Byte Array containing all object information,
      serialized using the JSON standard format. }
    function toJSONEx:RtcByteArray;
    end;

  // @exclude
  TRtcSimpleValue = class(TRtcValueObject)
  protected
    procedure Extracted; override;

  public
    function GetBoolean: boolean; virtual;
    function GetCurrency: Currency; virtual;
    function GetDateTime: TDateTime; virtual;
    function GetException: RtcWideString; virtual;
    function GetVarName: RtcWideString; virtual;
    function GetInteger: rtcInteger; virtual;
    function GetCardinal: rtcCardinal; virtual;
    function GetLargeInt: rtcLargeInt; virtual;
    function GetFloat: rtcFloat; virtual;
    function GetString: RtcString; virtual;
    function GetWideString: RtcWideString; virtual;
    function GetText: RtcWideString; virtual;
    function GetByteArray: RtcByteArray; virtual;
    function GetByteStream: TStream; virtual;
    function GetOID: TRtcObjectID; virtual;
    function GetLinkedObject: TObject; virtual;

    procedure SetNull(const Value: boolean); virtual;

    procedure SetBoolean(const Value: boolean); virtual;
    procedure SetCurrency(const Value: Currency); virtual;
    procedure SetDateTime(const Value: TDateTime); virtual;
    procedure SetException(const Value: RtcWideString); virtual;
    procedure SetVarName(const Value: RtcWideString); virtual;
    procedure SetInteger(const Value: rtcInteger); virtual;
    procedure SetCardinal(const Value: rtcCardinal); virtual;
    procedure SetLargeInt(const Value: rtcLargeInt); virtual;
    procedure SetFloat(const Value: rtcFloat); virtual;
    procedure SetString(const Value: RtcString); virtual;
    procedure SetWideString(const Value: RtcWideString); virtual;
    procedure SetText(const Value: RtcWideString); virtual;
    procedure SetByteArray(const Value: RtcByteArray); virtual;
    procedure SetByteStream(const Value: TStream); virtual;
    procedure SetOID(const Value: TRtcObjectID); virtual;
    procedure SetLinkedObject(const Value: TObject); virtual;

    function GetVariant: Variant; virtual;
    function SetVariant(const Value: Variant):boolean; virtual;
    end;

  // @exclude
  TRtcByteStream = class(TRtcSimpleValue)
  private
    FValue:TStream;

  protected
    class function NullValue:TStream;
    procedure CopyFrom(Value:TRtcValueObject); override;

    procedure Extracted; override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:TStream); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetByteStream: TStream; override;
    procedure SetByteStream(const Value: TStream); override;

    function GetByteArray: RtcByteArray; override;
    procedure SetByteArray(const Value: RtcByteArray); override;

    function GetText: RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetVariant: Variant; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcByteArray = class(TRtcSimpleValue)
  private
    FValue:RtcByteArray;

  protected
    class function NullValue:RtcByteArray;
    procedure CopyFrom(Value:TRtcValueObject); override;

    procedure Extracted; override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:RtcByteArray); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function NewByteArray(NewSize:Integer): RtcByteArray;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetByteArray: RtcByteArray; override;
    procedure SetByteArray(const Value: RtcByteArray); override;

    function GetText: RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetVariant: Variant; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcOIDValue = class(TRtcSimpleValue)
  private
    FValue:TRtcObjectID;

  protected
    class function NullValue:TRtcObjectID;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:TRtcObjectID); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetFloat: rtcFloat; override;

    function GetText: RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetInteger: rtcInteger; override;
    procedure SetInteger(const Value: rtcInteger); override;

    function GetCardinal: rtcCardinal; override;
    procedure SetCardinal(const Value: rtcCardinal); override;

    function GetLargeInt: rtcLargeInt; override;
    procedure SetLargeInt(const Value: rtcLargeInt); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    function GetOID: TRtcObjectID; override;
    procedure SetOID(const Value: TRtcObjectID); override;

    function GetLinkedObject: TObject; override;
    procedure SetLinkedObject(const Value: TObject); override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcExceptionValue = class(TRtcSimpleValue)
  private
    FValue:RtcWideString;

  protected
    class function NullValue:RtcWideString;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:RtcWideString); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetText: RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;
    function GetByteArray: RtcByteArray; override;

    function GetException: RtcWideString; override;
    procedure SetException(const Value: RtcWideString); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcVariableName = class(TRtcSimpleValue)
  private
    FValue:RtcWideString;

  protected
    class function NullValue:RtcWideString;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:RtcWideString); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetText: RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;
    function GetByteArray: RtcByteArray; override;

    function GetVarName: RtcWideString; override;
    procedure SetVarName(const Value: RtcWideString); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcBooleanValue=class(TRtcSimpleValue)
  private
    FValue:boolean;

  protected
    class function NullValue:boolean;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s). }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s). }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s). }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:boolean); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetCurrency: Currency; override;
    function GetInteger: rtcInteger; override;
    function GetCardinal: rtcCardinal; override;
    function GetFloat: rtcFloat; override;

    function GetText: RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;
    function GetByteArray: RtcByteArray; override;

    function GetBoolean: boolean; override;
    procedure SetBoolean(const Value: boolean); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcStringValue=class(TRtcSimpleValue)
  private
    FValue:RtcString;

  protected
    class function NullValue:RtcString;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:RtcString); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetCardinal: rtcCardinal; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetText: RtcWideString; override;
    function GetWideString: RtcWideString; override;
    function GetByteArray: RtcByteArray; override;

    function GetString: RtcString; override;
    procedure SetString(const Value: RtcString); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcWideStringValue=class(TRtcSimpleValue)
  private
    FValue:RtcWideString;

  protected
    class function NullValue:RtcWideString;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:RtcWideString); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetCardinal: rtcCardinal; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetText: RtcWideString; override;
    function GetByteArray: RtcByteArray; override;

    function GetString: RtcString; override;
    procedure SetString(const Value: RtcString); override;

    function GetWideString: RtcWideString; override;
    procedure SetWideString(const Value: RtcWideString); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcTextValue=class(TRtcSimpleValue)
  private
    FValue:RtcWideString;

  protected
    class function NullValue:RtcWideString;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:RtcWideString); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetCardinal: rtcCardinal; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetText: RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;
    function GetByteArray: RtcByteArray; override;

    procedure SetText(const Value:RtcWideString); override;
    procedure SetString(const Value: RtcString); override;
    procedure SetWideString(const Value: RtcWideString); override;
    procedure SetByteArray(const Value: RtcByteArray); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcIntegerValue=class(TRtcSimpleValue)
  private
    FValue:rtcInteger;

  protected
    class function NullValue:rtcInteger;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:rtcInteger); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetFloat: rtcFloat; override;
    function GetCardinal: rtcCardinal; override;
    function GetLargeInt: rtcLargeInt; override;

    function GetText:RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetInteger: rtcInteger; override;
    procedure SetInteger(const Value: rtcInteger); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcCardinalValue=class(TRtcSimpleValue)
  private
    FValue:rtcCardinal;

  protected
    class function NullValue:rtcCardinal;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:rtcCardinal); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetFloat: rtcFloat; override;
    function GetLargeInt: rtcLargeInt; override;

    function GetText:RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetInteger: rtcInteger; override;
    procedure SetInteger(const Value: rtcInteger); override;

    function GetCardinal: rtcCardinal; override;
    procedure SetCardinal(const Value: rtcCardinal); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcLargeIntValue=class(TRtcSimpleValue)
  private
    FValue:rtcLargeInt;

  protected
    class function NullValue:rtcLargeInt;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:rtcLargeInt); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetFloat: rtcFloat; override;

    function GetText:RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetInteger: rtcInteger; override;
    procedure SetInteger(const Value: rtcInteger); override;

    function GetCardinal: rtcCardinal; override;
    procedure SetCardinal(const Value: rtcCardinal); override;

    function GetLargeInt: rtcLargeInt; override;
    procedure SetLargeInt(const Value: rtcLargeInt); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcFloatValue=class(TRtcSimpleValue)
  private
    FValue:rtcFloat;

  protected
    class function NullValue:rtcFloat;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:rtcFloat); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetCardinal: rtcCardinal; override;
    function GetLargeInt: rtcLargeInt; override;

    function GetText:RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetFloat: rtcFloat; override;
    procedure SetFloat(const Value: rtcFloat); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcCurrencyValue=class(TRtcSimpleValue)
  private
    FValue:Currency;

  protected
    class function NullValue:Currency;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:Currency); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetCardinal: rtcCardinal; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetText:RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetCurrency: Currency; override;
    procedure SetCurrency(const Value: Currency); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcDateTimeValue=class(TRtcSimpleValue)
  private
    FValue:TDateTime;

  protected
    class function NullValue:TDateTime;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:TDateTime); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetInteger: rtcInteger; override;
    function GetCardinal: rtcCardinal; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetText:RtcWideString; override;
    function GetString: RtcString; override;
    function GetWideString: RtcWideString; override;

    function GetDateTime: TDateTime; override;
    procedure SetDateTime(const Value: TDateTime); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  { @abstract(Abstract Value Object)
    There are two ways to work with TRtcAbsValue objects.
    One is to let the stored object be auto-created on first read access
    to the asStream/asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
    the other is to use NewStream/NewArray/NewRecord/NewFunction/NewDataSet functions
    to create objects before first usage (AutoCreate=FALSE). You control the behavior
    by setting the AutoCreate property to TRUE or FALSE. Any child object created
    using the New... function will inherit this property state. You can change this
    property at any time, on any object. Default AutoCreate state for objects
    created by using the Create and FromCode methods is always FALSE. }
  TRtcAbsValue = class(TRtcValueObject)
  protected
    // @exclude
    FAutoCreate:boolean;

    // @exclude
    procedure SetObject(const Value:TRtcValueObject; asCopy:boolean=False); virtual; abstract;
    // @exclude
    procedure SetAsObject(const Value:TRtcValueObject);
    // @exclude
    function GetObject:TRtcValueObject; virtual; abstract;

    // @exclude
    function GetArray: TRtcArray;
    // @exclude
    function GetRecord: TRtcRecord;
    // @exclude
    function GetDataSet: TRtcDataSet;
    // @exclude
    function GetFunctionInfo: TRtcFunctionInfo;

    // @exclude
    function GetBoolean: boolean;
    // @exclude
    function GetCurrency: Currency;
    // @exclude
    function GetDateTime: TDateTime;
    // @exclude
    function GetException:RtcWideString;
    // @exclude
    function GetVarName:RtcWideString;
    // @exclude
    function GetInteger: rtcInteger;
    // @exclude
    function GetCardinal: rtcCardinal;
    // @exclude
    function GetLargeInt: rtcLargeInt;
    // @exclude
    function GetNull: boolean;
    // @exclude
    function GetFloat: rtcFloat;
    // @exclude
    function GetString: RtcString;
    // @exclude
    function GetWideString: RtcWideString;
    // @exclude
    function GetText:RtcWideString;
    // @exclude
    function GetByteArray: RtcByteArray;
    // @exclude
    function GetByteStream: TStream;
    // @exclude
    function GetOID: TRtcObjectID;
    // @exclude
    function GetLinkedObject: TObject;

    // @exclude
    function GetVariant: Variant;
    // @exclude
    procedure SetVariant(const Value: Variant);

    // @exclude
    procedure SetArray(const Value: TRtcArray);
    // @exclude
    procedure SetRecord(const Value: TRtcRecord);
    // @exclude
    procedure SetDataSet(const Value: TRtcDataSet);
    // @exclude
    procedure SetFunctionInfo(const Value: TRtcFunctionInfo);

    // @exclude
    procedure SetBoolean(const Value: boolean);
    // @exclude
    procedure SetCurrency(const Value: Currency);
    // @exclude
    procedure SetDateTime(const Value: TDateTime);
    // @exclude
    procedure SetException(const Value:RtcWideString);
    // @exclude
    procedure SetVarName(const Value:RtcWideString);
    // @exclude
    procedure SetInteger(const Value: rtcInteger);
    // @exclude
    procedure SetCardinal(const Value: rtcCardinal);
    // @exclude
    procedure SetLargeInt(const Value: rtcLargeInt);
    // @exclude
    procedure SetNull(const Value: boolean);
    // @exclude
    procedure SetFloat(const Value: rtcFloat);
    // @exclude
    procedure SetString(const Value: RtcString);
    // @exclude
    procedure SetWideString(const Value: RtcWideString);
    // @exclude
    procedure SetText(const Value:RtcWideString);
    // @exclude
    procedure SetByteArray(const Value: RtcByteArray);
    // @exclude
    procedure SetByteStream(const Value: TStream);
    // @exclude
    procedure SetOID(const Value: TRtcObjectID);
    // @exclude
    procedure SetLinkedObject(const Value: TObject);

    // @exclude
    function GetCode: RtcString;
    // @exclude
    procedure SetCode(const Value: RtcString);

    // @exclude
    function GetXMLrpc: RtcString;
    // @exclude
    procedure SetXMLrpc(const Value: RtcString);

    // @exclude
    function GetJSON:RtcWideString;
    // @exclude
    procedure SetJSON(const Value:RtcWideString);

  public
    { Clear this object (initialize: Free all values and objects assigned) }
    procedure Clear; virtual; abstract;

    { Assign a new, fresh Array.
      To turn this Value object into an array of values,
      you have to call "newArray" or assign already created TRtcArray
      using asArray:= (assigns a copy of the array) or
      asObject:= (assigns existing array instance). }
    function NewArray: TRtcArray;
    { Assign a new, fresh Record.
      To turn this Value object into a record of values,
      you have to call "newRecord" or assign already created TRtcRecord
      using asRecord:= (assigns a copy of the record) or
      asObject:= (assigns existing record instance). }
    function NewRecord: TRtcRecord;
    { Assign a new, fresh DataSet.
      To turn this Value object into a dataset of values,
      you have to call "newDataSet" or assign already created TRtcDataSet
     using asDataSet:= (assigns a copy of the dataset) or
      asObject:= (assigns existing dataset instance). }
    function NewDataSet: TRtcDataSet;
    { Assign a new, fresh FunctionInfo.
      To turn this Value object into a function call,
      you have to call this "newFunction" method, or: @html(<br><br>)
      assign already created TRtcFunctionInfo using asFunction:= (assigns a copy of the Function object) or @html(<br><br>)
      asObject:= (assigns existing TRtcFunctionInfo instance). }
    function NewFunction(const func_name:RtcWideString=''): TRtcFunctionInfo; overload;

    { Assign a new, fresh Byte Stream.
      To turn this Value object into a Byte Stream,
      you have to call this "newByteStream" method, or: @html(<br><br>)
      assign already created TStream using asByteStream:= (assigns a copy of the Stream data). }
    function NewByteStream: TStream;

    { Assigns a new, fresh ByteArray, containing "InitialSize" items (filled with zero). }
    function NewByteArray(InitialSize:Integer=0): RtcByteArray;

    { Assign a new, fresh Boolean value.
      Only sets the type to Boolean.
      You DO NOT have to use this function before accessing as Boolean value. }
    function NewBoolean: boolean;
    { Assign a new, fresh Currency value.
      Only sets the type to Currency.
      You DO NOT have to use this function before accessing as Currency value. }
    function NewCurrency: Currency;
    { Assign a new, fresh DateTime value.
      Only sets the type to DateTime.
      You DO NOT have to use this function before accessing as DateTime value. }
    function NewDateTime: TDateTime;
    { Assign a new, fresh Exception value.
      Only sets the type to Exception.
      You DO NOT have to use this function before accessing as Exception value. }
    function NewException:RtcWideString;
    { Assign a new, fresh VariableName value.
      Only sets the type to VariableName.
      You DO NOT have to use this function before accessing as VariableName value. }
    function NewVariable:RtcWideString;
    { Assign a new, fresh Integer value.
      Only sets the type to Integer.
      You DO NOT have to use this function before accessing as Integer value. }
    function NewInteger: rtcInteger;
    { Assign a new, fresh Cardinal value.
      Only sets the type to Cardinal.
      You DO NOT have to use this function before accessing as Cardinal value. }
    function NewCardinal: rtcCardinal;
    { Assign a new, fresh Large Integer value.
      Only sets the type to Large Integer.
      You DO NOT have to use this function before accessing as Large Integer value. }
    function NewLargeInt: rtcLargeInt;
    { Assign a new, fresh Floating-point value.
      Only sets the type to Floating-point.
      You DO NOT have to use this function before accessing as Floating-point value. }
    function NewFloat: rtcFloat;
    { Assign a new, fresh RtcString value.
      Only sets the type to RtcString.
      You DO NOT have to use this function before accessing as RtcString value. }
    function NewString: RtcString;
    { Assign a new, fresh RtcWideString value.
      Only sets the type to RtcWideString.
      You DO NOT have to use this function before accessing as RtcWideString value. }
    function NewWideString: RtcWideString;
    { Assign a new, fresh Text value.
      Only sets the type to Text.
      You DO NOT have to use this function before accessing as Text value. }
    function NewText:RtcWideString;

    { Check if Value can be accessed (read) using Type "typ" }
    function CheckType(typ:TRtcValueTypes):boolean;

    { read: Is the Value NULL (not assigned) ? /
      write: TRUE = destroy any object stored here and set value to null. }
    property isNull:boolean read GetNull write SetNull;
    { Check Value type. NULL values always return rtc_NULL }
    property isType:TRtcValueTypes read GetType;

    { Read/Write as native value (Variant). @html(<br>)
      You can use this property to get/set all native types:
      Boolean,Integer,Float,Currency,DateTime,RtcString,Text. @html(<br>)
      You CAN NOT use this property to get or set complex structures like
      ByteArray, ByteStream, Record, Array, DataSet or FunctionInfo. }
    property Value:Variant read GetVariant write SetVariant;
    { alias for @Link(TRtcAbsValue.Value) }
    property asValue:Variant read GetVariant write SetVariant;

    { Read/Write as Boolean value }
    property asBoolean:boolean read GetBoolean write SetBoolean;
    { Read/Write as Integer value }
    property asInteger:rtcInteger read GetInteger write SetInteger;
    { Read/Write as Cardinal value }
    property asCardinal:rtcCardinal read GetCardinal write SetCardinal;
    { Read/Write as Large Integer value }
    property asLargeInt:rtcLargeInt read GetLargeInt write SetLargeInt;
    { Read/Write as Float value }
    property asFloat:rtcFloat read GetFloat write SetFloat;
    { Read/Write as Currency value }
    property asCurrency:Currency read GetCurrency write SetCurrency;
    { Read/Write as DataTime value }
    property asDateTime:TDateTime read GetDateTime write SetDateTime;
    { Read/Write as Exception value }
    property asException:RtcWideString read GetException write SetException;
    { Read/Write as VariableName }
    property asVarName:RtcWideString read GetVarName write SetVarName;
    { Read/Write as RtcString value (access as raw RtcString) }
    property asString:RtcString read GetString write SetString;
    { Read/Write as RtcWideString value (access as raw RtcWideString) }
    property asWideString:RtcWideString read GetWideString write SetWideString;
    { Read/Write as Unicode Text string (uses UTF-8 encoding to write and UTF-8 decode to read Strings) }
    property asText:RtcWideString read GetText write SetText;

    { Read/Write as Linked Object ID }
    property asOID:TRtcObjectID read GetOID write SetOID;
    { Wherever possible, use "asOID" instead of "asLinkedObject" because
      "asLinkedObject" requires an active "Object Manager" for the current thread. @html(<br>)
      Reading "asLinkedObject" is identical to "GetRtcObjectManager.FindObject(asOID);" @html(<br>)
      Writing to "asLinkedObject" is identical to "asOID:=GetRtcObjectManager.FindOID(<TObject>);"  }
    property asLinkedObject:TObject read GetLinkedObject write SetLinkedObject;

    { Read: return this object coded as a RtcString, from which the object can be reconstructed anytime. /
      Write: reconstruct object from assigned RtcString. }
    property asCode:RtcString read GetCode write SetCode;
    { Read: return this object coded as a XML-RPC RtcString,
            from which the object can be reconstructed. /
      Write: reconstruct object from assigned XML-RPC RtcString. }
    property asXMLrpc:RtcString read GetXMLrpc write SetXMLrpc;
    { Read: return this object coded as a JSON Ansi String,
            from which the object can be reconstructed. /
      Write: reconstruct object from assigned JSON String. }
    property asJSON:RtcWideString read GetJSON write SetJSON;

    { read: Access stored ByteArray / write: assign a copy of the source Byte Array }
    property asByteArray:RtcByteArray read GetByteArray write SetByteArray;

    { read: Access stored ByteStream / write: assign a copy of the source Stream }
    property asByteStream:TStream read GetByteStream write SetByteStream;

    { read: Access stored array / write: assign a copy of the source TRtcArray }
    property asArray:TRtcArray read GetArray write SetArray;
    { read: Access stored record / write: assign a copy of the source TRtcRecord }
    property asRecord:TRtcRecord read GetRecord write SetRecord;
    { read: Access stored DataSet / write: assign a copy of the source TRtcDataset }
    property asDataSet:TRtcDataSet read GetDataSet write SetDataSet;
    { read: Access stored FunctionInfo / write: assign a copy of the source TRtcFunctionInfo }
    property asFunction:TRtcFunctionInfo read GetFunctionInfo write SetFunctionInfo;

    { Read: Access stored information as TRtcValueObject / @html(<br>)
      Write: NIL = remove object without destroying it / @html(<br>)
      Write: object = Assign object to this structure. It will be maintained and destroyed
      with this structure, as long as it isn't removed by calling asObject:=nil. @html(<br><br>)

      WARNING: NEVER! use asObject to assign an object which you did not create yourself,
      or to assign an object to more than one TRtcValueObject structure.
      If the same object instance is assigned to more than one structure,
      you WILL end up with Access Violation Errors when those objects are
      released by the connection component. To assign a copy of an object,
      allways use the object.CopyOf function or assign the object using
      specific 'as...' property (which doesn't assign the object itself,
      but rather creates a copy of the object). }
    property asObject:TRtcValueObject read GetObject write SetAsObject;

    { There are two ways to work with TRtcValue objects.
      One is to let the stored object be auto-created on first read access
      to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
      the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
      to create objects before first usage (AutoCreate=FALSE). You control the behavior
      by setting the AutoCreate property to TRUE or FALSE. Any child object created
      using the New... function will inherit this property state. You can change this
      property at any time, on any object. Default AutoCreate state for objects
      created by using the Create and FromCode methods is always FALSE. }
    property AutoCreate:boolean read FAutoCreate write FAutoCreate;
    end;

  { @abstract(Abstract Record Object)
    There are two ways to work with TRtcAbsRecord objects. @html(<br>)
    One is to let the stored object be auto-created on first read access
    to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
    the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
    to create objects before first usage (AutoCreate=FALSE). You control the behavior
    by setting the AutoCreate property to TRUE or FALSE. Any child object created
    using the New... function will inherit this property state. You can change this
    property at any time, on any object. Default AutoCreate state for objects
    created by using the Create and FromCode methods is always FALSE. }
  TRtcAbsRecord = class(TRtcValueObject)
  protected
    // @exclude
    FAutoCreate:boolean;

    // @exclude
    function GetObject(const index:RtcWideString): TRtcValueObject; virtual; abstract;
    // @exclude
    function Get_Object(const index:RtcString): TRtcValueObject;

    // @exclude
    procedure SetObject(const index:RtcWideString; Value:TRtcValueObject; asCopy:boolean=False); virtual; abstract;
    // @exclude
    procedure Set_Object(const index:RtcString; Value:TRtcValueObject; asCopy:boolean=False);

    // @exclude
    procedure SetAsObject(const index:RtcWideString; Value:TRtcValueObject);
    // @exclude
    procedure Set_AsObject(const index:RtcString; Value:TRtcValueObject);

    // @exclude
    function GetValueType(const index:RtcWideString): TRtcValueTypes;
    // @exclude
    function Get_ValueType(const index:RtcString): TRtcValueTypes;

    // @exclude
    function GetArray(const index:RtcWideString): TRtcArray;
    // @exclude
    function Get_Array(const index:RtcString): TRtcArray;

    // @exclude
    function GetRecord(const index:RtcWideString): TRtcRecord;
    // @exclude
    function Get_Record(const index:RtcString): TRtcRecord;

    // @exclude
    function GetDataSet(const index:RtcWideString): TRtcDataSet;
    // @exclude
    function Get_DataSet(const index:RtcString): TRtcDataSet;

    // @exclude
    function GetFunctionInfo(const index:RtcWideString): TRtcFunctionInfo;
    // @exclude
    function Get_FunctionInfo(const index:RtcString): TRtcFunctionInfo;

    // @exclude
    function GetByteStream(const index:RtcWideString): TStream;
    // @exclude
    function Get_ByteStream(const index:RtcString): TStream;

    // @exclude
    function GetNull(const index:RtcWideString): boolean;
    // @exclude
    function Get_Null(const index:RtcString): boolean;

    // @exclude
    function GetBoolean(const index:RtcWideString): boolean;
    // @exclude
    function Get_Boolean(const index:RtcString): boolean;

    // @exclude
    function GetCurrency(const index:RtcWideString): Currency;
    // @exclude
    function Get_Currency(const index:RtcString): Currency;

    // @exclude
    function GetDateTime(const index:RtcWideString): TDateTime;
    // @exclude
    function Get_DateTime(const index:RtcString): TDateTime;

    // @exclude
    function GetException(const index:RtcWideString):RtcWideString;
    // @exclude
    function Get_Exception(const index:RtcString):RtcWideString;

    // @exclude
    function GetVarName(const index:RtcWideString):RtcWideString;
    // @exclude
    function Get_VarName(const index:RtcString):RtcWideString;

    // @exclude
    function GetInteger(const index:RtcWideString): rtcInteger;
    // @exclude
    function Get_Integer(const index:RtcString): rtcInteger;

    // @exclude
    function GetCardinal(const index:RtcWideString): rtcCardinal;
    // @exclude
    function Get_Cardinal(const index:RtcString): rtcCardinal;

    // @exclude
    function GetLargeInt(const index:RtcWideString): rtcLargeInt;
    // @exclude
    function Get_LargeInt(const index:RtcString): rtcLargeInt;

    // @exclude
    function GetFloat(const index:RtcWideString): rtcFloat;
    // @exclude
    function Get_Float(const index:RtcString): rtcFloat;

    // @exclude
    function GetString(const index:RtcWideString): RtcString;
    // @exclude
    function Get_String(const index:RtcString): RtcString;

    // @exclude
    function GetByteArray(const index:RtcWideString): RtcByteArray;
    // @exclude
    function Get_ByteArray(const index:RtcString): RtcByteArray;

    // @exclude
    function GetWideString(const index:RtcWideString): RtcWideString;
    // @exclude
    function Get_WideString(const index:RtcString): RtcWideString;

    // @exclude
    function GetText(const index:RtcWideString):RtcWideString;
    // @exclude
    function Get_Text(const index:RtcString):RtcWideString;

    // @exclude
    function GetVariant(const index:RtcWideString): Variant;
    // @exclude
    function Get_Variant(const index:RtcString): Variant;

    // @exclude
    function GetOID(const index:RtcWideString): TRtcObjectID;
    // @exclude
    function Get_OID(const index:RtcString): TRtcObjectID;

    // @exclude
    function GetLinkedObject(const index:RtcWideString): TObject;
    // @exclude
    function Get_LinkedObject(const index:RtcString): TObject;

    // @exclude
    procedure SetVariant(const index:RtcWideString; const Value: Variant);
    // @exclude
    procedure Set_Variant(const index:RtcString; const Value: Variant);

    // @exclude
    procedure SetArray(const index:RtcWideString; const Value: TRtcArray);
    // @exclude
    procedure Set_Array(const index:RtcString; const Value: TRtcArray);

    // @exclude
    procedure SetRecord(const index:RtcWideString; const Value: TRtcRecord);
    // @exclude
    procedure Set_Record(const index:RtcString; const Value: TRtcRecord);

    // @exclude
    procedure SetDataSet(const index:RtcWideString; const Value: TRtcDataSet);
    // @exclude
    procedure Set_DataSet(const index:RtcString; const Value: TRtcDataSet);

    // @exclude
    procedure SetFunctionInfo(const index:RtcWideString; const Value: TRtcFunctionInfo);
    // @exclude
    procedure Set_FunctionInfo(const index:RtcString; const Value: TRtcFunctionInfo);

    // @exclude
    procedure SetByteStream(const index:RtcWideString; const Value: TStream);
    // @exclude
    procedure Set_ByteStream(const index:RtcString; const Value: TStream);

    // @exclude
    procedure SetNull(const index:RtcWideString; const Value: boolean);
    // @exclude
    procedure Set_Null(const index:RtcString; const Value: boolean);

    // @exclude
    procedure SetBoolean(const index:RtcWideString; const Value: boolean);
    // @exclude
    procedure Set_Boolean(const index:RtcString; const Value: boolean);

    // @exclude
    procedure SetCurrency(const index:RtcWideString; const Value: Currency);
    // @exclude
    procedure Set_Currency(const index:RtcString; const Value: Currency);

    // @exclude
    procedure SetDateTime(const index:RtcWideString; const Value: TDateTime);
    // @exclude
    procedure Set_DateTime(const index:RtcString; const Value: TDateTime);

    // @exclude
    procedure SetException(const index:RtcWideString; const Value:RtcWideString);
    // @exclude
    procedure Set_Exception(const index:RtcString; const Value:RtcWideString);

    // @exclude
    procedure SetVarName(const index:RtcWideString; const Value:RtcWideString);
    // @exclude
    procedure Set_VarName(const index:RtcString; const Value:RtcWideString);

    // @exclude
    procedure SetInteger(const index:RtcWideString; const Value: rtcInteger);
    // @exclude
    procedure Set_Integer(const index:RtcString; const Value: rtcInteger);

    // @exclude
    procedure SetCardinal(const index:RtcWideString; const Value: rtcCardinal);
    // @exclude
    procedure Set_Cardinal(const index:RtcString; const Value: rtcCardinal);

    // @exclude
    procedure SetLargeInt(const index:RtcWideString; const Value: rtcLargeInt);
    // @exclude
    procedure Set_LargeInt(const index:RtcString; const Value: rtcLargeInt);

    // @exclude
    procedure SetFloat(const index:RtcWideString; const Value: rtcFloat);
    // @exclude
    procedure Set_Float(const index:RtcString; const Value: rtcFloat);

    // @exclude
    procedure SetString(const index:RtcWideString; const Value: RtcString);
    // @exclude
    procedure Set_String(const index:RtcString; const Value: RtcString);

    // @exclude
    procedure SetWideString(const index:RtcWideString; const Value: RtcWideString);
    // @exclude
    procedure Set_WideString(const index:RtcString; const Value: RtcWideString);

    // @exclude
    procedure SetText(const index:RtcWideString; const Value:RtcWideString);
    // @exclude
    procedure Set_Text(const index:RtcString; const Value:RtcWideString);

    // @exclude
    procedure SetByteArray(const index:RtcWideString; const Value: RtcByteArray);
    // @exclude
    procedure Set_ByteArray(const index:RtcString; const Value: RtcByteArray);

    // @exclude
    procedure SetOID(const index:RtcWideString; const Value: TRtcObjectID);
    // @exclude
    procedure Set_OID(const index:RtcString; const Value: TRtcObjectID);

    // @exclude
    procedure SetLinkedObject(const index:RtcWideString; const Value: TObject);
    // @exclude
    procedure Set_LinkedObject(const index:RtcString; const Value: TObject);

    // @exclude
    function GetCode(const index:RtcWideString): RtcString;
    // @exclude
    function Get_Code(const index:RtcString): RtcString;

    // @exclude
    procedure SetCode(const index:RtcWideString; const Value: RtcString);
    // @exclude
    procedure Set_Code(const index:RtcString; const Value: RtcString);

    // @exclude
    function GetXMLrpc(const index:RtcWideString): RtcString;
    // @exclude
    function Get_XMLrpc(const index:RtcString): RtcString;

    // @exclude
    procedure SetXMLrpc(const index:RtcWideString; const Value: RtcString);
    // @exclude
    procedure Set_XMLrpc(const index:RtcString; const Value: RtcString);

    // @exclude
    function GetJSON(const index:RtcWideString):RtcWideString;
    // @exclude
    function Get_JSON(const index:RtcString):RtcWideString;

    // @exclude
    procedure SetJSON(const index:RtcWideString; const Value:RtcWideString);
    // @exclude
    procedure Set_JSON(const index:RtcString; const Value:RtcWideString);

  public
    { Clear this object (initialize: Free all values and objects assigned) }
    procedure Clear; virtual; abstract;

    { Assign a new, fresh Array to the 'index' field.
      To turn the 'index' field into an array of values,
      you have to call "newArray" or assign already created TRtcArray
      using asArray[]:= (assigns a copy of the array) or
      asObject[]:= (assigns existing array instance). }
    function NewArray(const index:RtcWideString): TRtcArray;
    { (RtcString) Assign a new, fresh Array to the 'index' field.
      To turn the 'index' field into an array of values,
      you have to call "newArray" or assign already created TRtcArray
      using asArray[]:= (assigns a copy of the array) or
      asObject[]:= (assigns existing array instance). }
    function New_Array(const index:RtcString): TRtcArray;

    { Assign a new, fresh Record to the 'index' field.
      To turn the 'index' field into a record of values,
      you have to call "newRecord" or assign already created TRtcRecord
      using asRecord[]:= (assigns a copy of the record) or
      asObject[]:= (assigns existing record instance). }
    function NewRecord(const index:RtcWideString): TRtcRecord;
    { (RtcString) Assign a new, fresh Record to the 'index' field.
      To turn the 'index' field into a record of values,
      you have to call "newRecord" or assign already created TRtcRecord
      using asRecord[]:= (assigns a copy of the record) or
      asObject[]:= (assigns existing record instance). }
    function New_Record(const index:RtcString): TRtcRecord;

    { Assign a new, fresh DataSet to the 'index' field.
      To turn the 'index' field into a dataset of values,
      you have to call "newDataSet" or assign already created TRtcDataSet
      using asDataSet[]:= (assigns a copy of the dataset) or
      asObject[]:= (assigns existing dataset instance). }
    function NewDataSet(const index:RtcWideString): TRtcDataSet;
    { (RtcString) Assign a new, fresh DataSet to the 'index' field.
      To turn the 'index' field into a dataset of values,
      you have to call "newDataSet" or assign already created TRtcDataSet
      using asDataSet[]:= (assigns a copy of the dataset) or
      asObject[]:= (assigns existing dataset instance). }
    function New_DataSet(const index:RtcString): TRtcDataSet;

    { Assign a new, fresh FunctionInfo to the 'index' field.
      To turn the 'index' field into a function call,
      you have to call "newFunction" or assign already created TRtcFunctionInfo
      using asFunction[]:= (assigns a copy of the FunctionInfo object) or
      asObject[]:= (assigns existing FunctionInfo instance). }
    function NewFunction(const index:RtcWideString; const func_name:RtcWideString=''): TRtcFunctionInfo;
    { (RtcString) Assign a new, fresh FunctionInfo to the 'index' field.
      To turn the 'index' field into a function call,
      you have to call "newFunction" or assign already created TRtcFunctionInfo
      using asFunction[]:= (assigns a copy of the FunctionInfo object) or
      asObject[]:= (assigns existing FunctionInfo instance). }
    function New_Function(const index:RtcString; const func_name:RtcWideString=''): TRtcFunctionInfo;

    { Assign a new, fresh Byte Stream to the 'index' field.
      To turn the 'index' field into a Byte Stream,
      you have to call "newByteStream" or assign already created TStream
      using asByteStream[]:= (assigns a copy of the Stream data). }
    function NewByteStream(const index:RtcWideString): TStream;
    { (RtcString) Assign a new, fresh Byte Stream to the 'index' field.
      To turn the 'index' field into a Byte Stream,
      you have to call "newByteStream" or assign already created TStream
      using asByteStream[]:= (assigns a copy of the Stream data). }
    function New_ByteStream(const index:RtcString): TStream;

    { Assigns a new, fresh Byte Array, containing "InitialSize" items (filled with zero). }
    function NewByteArray(const index:RtcWideString; InitialSize:Integer=0): RtcByteArray;
    { (RtcString) Assigns a new, fresh Byte Array, containing "InitialSize" items (filled with zero). }
    function New_ByteArray(const index:RtcString; InitialSize:Integer=0): RtcByteArray;

    { Assign a new, fresh Boolean value to the 'index' field.
      Only sets the type to Boolean.
      You DO NOT have to use this function before accessing as Boolean value. }
    function NewBoolean(const index:RtcWideString): boolean;
    { (RtcString) Assign a new, fresh Boolean value to the 'index' field.
      Only sets the type to Boolean.
      You DO NOT have to use this function before accessing as Boolean value. }
    function New_Boolean(const index:RtcString): boolean;

    { Assign a new, fresh Currency value to the 'index' field.
      Only sets the type to Currency.
      You DO NOT have to use this function before accessing as Currency value. }
    function NewCurrency(const index:RtcWideString): Currency;
    { (RtcString) Assign a new, fresh Currency value to the 'index' field.
      Only sets the type to Currency.
      You DO NOT have to use this function before accessing as Currency value. }
    function New_Currency(const index:RtcString): Currency;

    { Assign a new, fresh DateTime value to the 'index' field.
      Only sets the type to DateTime.
      You DO NOT have to use this function before accessing as DateTime value. }
    function NewDateTime(const index:RtcWideString): TDateTime;
    { (RtcString) Assign a new, fresh DateTime value to the 'index' field.
      Only sets the type to DateTime.
      You DO NOT have to use this function before accessing as DateTime value. }
    function New_DateTime(const index:RtcString): TDateTime;

    { Assign a new, fresh Exception value to the 'index' field.
      Only sets the type to Exception.
      You DO NOT have to use this function before accessing as Exception value. }
    function NewException(const index:RtcWideString):RtcWideString;
    { (RtcString) Assign a new, fresh Exception value to the 'index' field.
      Only sets the type to Exception.
      You DO NOT have to use this function before accessing as Exception value. }
    function New_Exception(const index:RtcString):RtcWideString;

    { Assign a new, fresh VariableName value to the 'index' field.
      Only sets the type to VariableName.
      You DO NOT have to use this function before accessing as VariableName. }
    function NewVariable(const index:RtcWideString):RtcWideString;
    { (RtcString) Assign a new, fresh VariableName value to the 'index' field.
      Only sets the type to VariableName.
      You DO NOT have to use this function before accessing as VariableName. }
    function New_Variable(const index:RtcString):RtcWideString;

    { Assign a new, fresh Integer value to the 'index' field.
      Only sets the type to Integer.
      You DO NOT have to use this function before accessing as Integer value. }
    function NewInteger(const index:RtcWideString): rtcInteger;
    { (RtcString) Assign a new, fresh Integer value to the 'index' field.
      Only sets the type to Integer.
      You DO NOT have to use this function before accessing as Integer value. }
    function New_Integer(const index:RtcString): rtcInteger;

    { Assign a new, fresh Cardinal value to the 'index' field.
      Only sets the type to Cardinal.
      You DO NOT have to use this function before accessing as Cardinal value. }
    function NewCardinal(const index:RtcWideString): rtcCardinal;
    { (RtcString) Assign a new, fresh Cardinal value to the 'index' field.
      Only sets the type to Cardinal.
      You DO NOT have to use this function before accessing as Cardinal value. }
    function New_Cardinal(const index:RtcString): rtcCardinal;

    { Assign a new, fresh Large Integer value to the 'index' field.
      Only sets the type to Large Integer.
      You DO NOT have to use this function before accessing as Large Integer value. }
    function NewLargeInt(const index:RtcWideString): rtcLargeInt;
    { (RtcString) Assign a new, fresh Large Integer value to the 'index' field.
      Only sets the type to Large Integer.
      You DO NOT have to use this function before accessing as Large Integer value. }
    function New_LargeInt(const index:RtcString): rtcLargeInt;

    { Assign a new, fresh Floating-point value to the 'index' field.
      Only sets the type to Floating-point.
      You DO NOT have to use this function before accessing as Floating-point value. }
    function NewFloat(const index:RtcWideString): rtcFloat;
    { (RtcString) Assign a new, fresh Floating-point value to the 'index' field.
      Only sets the type to Floating-point.
      You DO NOT have to use this function before accessing as Floating-point value. }
    function New_Float(const index:RtcString): rtcFloat;

    { Assign a new, fresh RtcString value to the 'index' field.
      Only sets the type to RtcString.
      You DO NOT have to use this function before accessing as RtcString value. }
    function NewString(const index:RtcWideString): RtcString;
    { (RtcString) Assign a new, fresh RtcString value to the 'index' field.
      Only sets the type to RtcString.
      You DO NOT have to use this function before accessing as RtcString value. }
    function New_String(const index:RtcString): RtcString;

    { Assign a new, fresh RtcWideString value to the 'index' field.
      Only sets the type to RtcWideString.
      You DO NOT have to use this function before accessing as RtcWideString value. }
    function NewWideString(const index:RtcWideString): RtcWideString;
    { (RtcString) Assign a new, fresh RtcWideString value to the 'index' field.
      Only sets the type to RtcWideString.
      You DO NOT have to use this function before accessing as RtcWideString value. }
    function New_WideString(const index:RtcString): RtcWideString;

    { Assign a new, fresh Text value to the 'index' field.
      Only sets the type to Text.
      You DO NOT have to use this function before accessing as Text value. }
    function NewText(const index:RtcWideString):RtcWideString;
    { (RtcString) Assign a new, fresh Text value to the 'index' field.
      Only sets the type to Text.
      You DO NOT have to use this function before accessing as Text value. }
    function New_Text(const index:RtcString):RtcWideString;

    { Check if 'index' field Value can be accessed (read) using Type "typ" }
    function CheckType(const index:RtcWideString; typ:TRtcValueTypes):boolean;
    { Check if 'index' field Value can be accessed (read) using Type "typ" }
    function Check_Type(const index:RtcString; typ:TRtcValueTypes):boolean;

    { read: Is the 'index' field value NULL (not assigned) ? /
      write: TRUE = set 'index' field value to null (this will destroy any object stored there) }
    property isNull[const index:RtcWideString]:boolean read GetNull write SetNull;
    { (RtcString) /
      read: Is the 'index' field value NULL (not assigned) ? /
      write: TRUE = set 'index' field value to null (this will destroy any object stored there) }
    property is_Null[const index:RtcString]:boolean read Get_Null write Set_Null;

    { Check 'index' field Value type. NULL values always return rtc_NULL }
    property isType[const index:RtcWideString]:TRtcValueTypes read GetValueType;
    { (RtcString) Check 'index' field Value type. NULL values always return rtc_NULL }
    property is_Type[const index:RtcString]:TRtcValueTypes read Get_ValueType;

    { Read/Write as native value (Variant). @html(<br>)
      You can use this property to get/set all native types:
      Boolean,Integer,Float,Currency,DateTime,RtcString,RtcWideString. @html(<br>)
      You CAN NOT use this property to get or set complex structures like
      Record, Array, DataSet or FunctionInfo. }
    property Value[const index:RtcWideString]:Variant read GetVariant write SetVariant; default;
    { (RtcString) Read/Write as native field value (Variant). @html(<br>)
      You can use this property to get/set all native types:
      Boolean,Integer,Float,Currency,DateTime,RtcString,RtcWideString. @html(<br>)
      You CAN NOT use this property to get or set complex structures like
      Record, Array, DataSet or FunctionInfo. }
    property _Value[const index:RtcString]:Variant read Get_Variant write Set_Variant;

    { alias for @Link(TRtcAbsValue.Value) }
    property asValue[const index:RtcWideString]:Variant read GetVariant write SetVariant;
    { (RtcString) alias for @Link(TRtcAbsValue._Value) }
    property as_Value[const index:RtcString]:Variant read Get_Variant write Set_Variant;

    { Read/Write 'index' field as Boolean value }
    property asBoolean[const index:RtcWideString]:boolean read GetBoolean write SetBoolean;
    { (RtcString) Read/Write 'index' field as Boolean value }
    property as_Boolean[const index:RtcString]:boolean read Get_Boolean write Set_Boolean;

    { Read/Write 'index' field as Integer value }
    property asInteger[const index:RtcWideString]:rtcInteger read GetInteger write SetInteger;
    { (RtcString) Read/Write 'index' field as Integer value }
    property as_Integer[const index:RtcString]:rtcInteger read Get_Integer write Set_Integer;

    { Read/Write 'index' field as Cardinal value }
    property asCardinal[const index:RtcWideString]:rtcCardinal read GetCardinal write SetCardinal;
    { (RtcString) Read/Write 'index' field as Cardinal value }
    property as_Cardinal[const index:RtcString]:rtcCardinal read Get_Cardinal write Set_Cardinal;

    { Read/Write 'index' field as Large Integer value }
    property asLargeInt[const index:RtcWideString]:rtcLargeInt read GetLargeInt write SetLargeInt;
    { (RtcString) Read/Write 'index' field as Large Integer value }
    property as_LargeInt[const index:RtcString]:rtcLargeInt read Get_LargeInt write Set_LargeInt;

    { Read/Write 'index' field as Float value }
    property asFloat[const index:RtcWideString]:rtcFloat read GetFloat write SetFloat;
    { (RtcString) Read/Write 'index' field as Float value }
    property as_Float[const index:RtcString]:rtcFloat read Get_Float write Set_Float;

    { Read/Write 'index' field as Currency value }
    property asCurrency[const index:RtcWideString]:Currency read GetCurrency write SetCurrency;
    { (RtcString) Read/Write 'index' field as Currency value }
    property as_Currency[const index:RtcString]:Currency read Get_Currency write Set_Currency;

    { Read/Write 'index' field as DataTime value }
    property asDateTime[const index:RtcWideString]:TDateTime read GetDateTime write SetDateTime;
    { (RtcString) Read/Write 'index' field as DataTime value }
    property as_DateTime[const index:RtcString]:TDateTime read Get_DateTime write Set_DateTime;

    { Read/Write 'index' field as Exception value }
    property asException[const index:RtcWideString]:RtcWideString read GetException write SetException;
    { (RtcString) Read/Write 'index' field as Exception value }
    property as_Exception[const index:RtcString]:RtcWideString read Get_Exception write Set_Exception;

    { Read/Write 'index' field as VariableName }
    property asVarName[const index:RtcWideString]:RtcWideString read GetVarName write SetVarName;
    { (RtcString) Read/Write 'index' field as VariableName }
    property as_VarName[const index:RtcString]:RtcWideString read Get_VarName write Set_VarName;

    { Read/Write 'index' field as RtcString (raw RtcString access) value }
    property asString[const index:RtcWideString]:RtcString read GetString write SetString;
    { (RtcString) Read/Write 'index' field as RtcString (raw RtcString access) value }
    property as_String[const index:RtcString]:RtcString read Get_String write Set_String;

    { Read/Write 'index' field as RtcWideString (raw RtcWideString access) value }
    property asWideString[const index:RtcWideString]:RtcWideString read GetWideString write SetWideString;
    { (RtcString) Read/Write 'index' field as RtcWideString (raw RtcWideString access) value }
    property as_WideString[const index:RtcString]:RtcWideString read Get_WideString write Set_WideString;

    { Read/Write 'index' field as Text value (UTF-8 encoded on write, decoded on read) }
    property asText[const index:RtcWideString]:RtcWideString read GetText write SetText;
    { (RtcString) Read/Write 'index' field as Text value (UTF-8 encoded on write, decoded on read) }
    property as_Text[const index:RtcString]:RtcWideString read Get_Text write Set_Text;

    { read: Access Byte Array stored in the 'index' field /
      write: assign a copy of the source Byte Array to the 'index' field }
    property asByteArray[const index:RtcWideString]:RtcByteArray read GetByteArray write SetByteArray;
    { (RtcString)
      read: Access Byte Array stored in the 'index' field /
      write: assign a copy of the source Byte Array to the 'index' field }
    property as_ByteArray[const index:RtcString]:RtcByteArray read Get_ByteArray write Set_ByteArray;

    { Read/Write 'index' field as Linked Object ID }
    property asOID[const index:RtcWideString]:TRtcObjectID read GetOID write SetOID;
    { (RtcString) Read/Write 'index' field as Linked Object ID }
    property as_OID[const index:RtcString]:TRtcObjectID read Get_OID write Set_OID;

    { Wherever possible, use "asOID" instead of "asLinkedObject" because
      "asLinkedObject" requires an active "Object Manager" for the current thread. @html(<br>)
      Reading "asLinkedObject" is identical to "GetRtcObjectManager.FindObject(asOID[..]);" @html(<br>)
      Writing to "asLinkedObject" is identical to "asOID[..]:=GetRtcObjectManager.FindOID(<TObject>);"  }
    property asLinkedObject[const index:RtcWideString]:TObject read GetLinkedObject write SetLinkedObject;
    { Wherever possible, use "as_OID" instead of "as_LinkedObject" because
      "as_LinkedObject" requires an active "Object Manager" for the current thread. @html(<br>)
      Reading "as_LinkedObject" is identical to "GetRtcObjectManager.FindObject(as_OID[..]);" @html(<br>)
      Writing to "as_LinkedObject" is identical to "as_OID[..]:=GetRtcObjectManager.FindOID(<TObject>);"  }
    property as_LinkedObject[const index:RtcString]:TObject read Get_LinkedObject write Set_LinkedObject;

    { Read: return 'index' field coded as a RtcString,
      from which the object can be reconstructed anytime. /
      Write: reconstruct object from RtcString and assign to 'index' field. }
    property asCode[const index:RtcWideString]:RtcString read GetCode write SetCode;
    { (RtcString) /
      Read: return 'index' field coded as a RtcString,
      from which the object can be reconstructed anytime. /
      Write: reconstruct object from RtcString and assign to 'index' field. }
    property as_Code[const index:RtcString]:RtcString read Get_Code write Set_Code;

    { Read: return 'index' field coded as a XML-RPC RtcString,
            from which the object can be reconstructed. /
      Write: reconstruct object from RtcString and assign to 'index' field. }
    property asXMLrpc[const index:RtcWideString]:RtcString read GetXMLrpc write SetXMLrpc;
    { (RtcString) /
      Read: return 'index' field coded as a XML-RPC RtcString,
            from which the object can be reconstructed. /
      Write: reconstruct object from RtcString and assign to 'index' field. }
    property as_XMLrpc[const index:RtcString]:RtcString read Get_XMLrpc write Set_XMLrpc;

    { Read: return 'index' field coded as a JSON Ansi String,
            from which the object can be reconstructed. /
      Write: reconstruct object from String and assign to 'index' field. }
    property asJSON[const index:RtcWideString]:RtcWideString read GetJSON write SetJSON;
    { (RtcString) /
      Read: return 'index' field coded as a JSON Ansi String,
            from which the object can be reconstructed. /
      Write: reconstruct object from String and assign to 'index' field. }
    property as_JSON[const index:RtcString]:RtcWideString read Get_JSON write Set_JSON;

    { read: Access Stream stored in the 'index' field /
      write: assign a copy of the source Stream to the 'index' field }
    property asByteStream[const index:RtcWideString]:TStream read GetByteStream write SetByteStream;
    { (RtcString) /
      read: Access Stream stored in the 'index' field /
      write: assign a copy of the source Stream to the 'index' field }
    property as_ByteStream[const index:RtcString]:TStream read Get_ByteStream write Set_ByteStream;

    { read: Access array stored in the 'index' field /
      write: assign a copy of the source TRtcArray to the 'index' field }
    property asArray[const index:RtcWideString]:TRtcArray read GetArray write SetArray;
    { (RtcString) /
      read: Access array stored in the 'index' field /
      write: assign a copy of the source TRtcArray to the 'index' field }
    property as_Array[const index:RtcString]:TRtcArray read Get_Array write Set_Array;

    { read: Access record stored in the 'index' field /
      write: assign a copy of the source TRtcRecord to the 'index' field }
    property asRecord[const index:RtcWideString]:TRtcRecord read GetRecord write SetRecord;
    { (RtcString) /
      read: Access record stored in the 'index' field /
      write: assign a copy of the source TRtcRecord to the 'index' field }
    property as_Record[const index:RtcString]:TRtcRecord read Get_Record write Set_Record;

    { read: Access DataSet stored in the 'index' field /
      write: assign a copy of the source TRtcDataset to the 'index' field }
    property asDataSet[const index:RtcWideString]:TRtcDataSet read GetDataSet write SetDataSet;
    { (RtcString) /
      read: Access DataSet stored in the 'index' field /
      write: assign a copy of the source TRtcDataset to the 'index' field }
    property as_DataSet[const index:RtcString]:TRtcDataSet read Get_DataSet write Set_DataSet;

    { read: Access FunctionInfo stored in the 'index' field /
      write: assign a copy of the source TRtcFunctionInfo to the 'index' field }
    property asFunction[const index:RtcWideString]:TRtcFunctionInfo read GetFunctionInfo write SetFunctionInfo;
    { (RtcString) /
      read: Access FunctionInfo stored in the 'index' field /
      write: assign a copy of the source TRtcFunctionInfo to the 'index' field }
    property as_Function[const index:RtcString]:TRtcFunctionInfo read Get_FunctionInfo write Set_FunctionInfo;

    { Read: Access information in the 'index' field as TRtcValueObject / @html(<br>)
      Write: NIL = remove object from the 'index' field without destroying it / @html(<br>)
      Write: object = Assign object to the 'index' field in this structure.
      It will be maintained and destroyed with this structure,
      as long as it isn't removed by calling asObject[index]:=nil. @html(<br><br>)

      WARNING: NEVER! use asObject to assign an object which you did not create yourself,
      or to assign an object to more than one TRtcValueObject structure.
      If the same object instance is assigned to more than one structure,
      you WILL end up with Access Violation Errors when those objects are
      released by the connection component. To assign a copy of an object,
      allways use the object.CopyOf function or assign the object using
      specific 'as...' property (which doesn't assign the object itself,
      but rather creates a copy of the object). }
    property asObject[const index:RtcWideString]:TRtcValueObject read GetObject write SetAsObject;
    { (RtcString) /
      Read: Access information in the 'index' field as TRtcValueObject / @html(<br>)
      Write: NIL = remove object from the 'index' field without destroying it / @html(<br>)
      Write: object = Assign object to the 'index' field in this structure.
      It will be maintained and destroyed with this structure,
      as long as it isn't removed by calling asObject[index]:=nil. @html(<br><br>)

      WARNING: NEVER! use asObject to assign an object which you did not create yourself,
      or to assign an object to more than one TRtcValueObject structure.
      If the same object instance is assigned to more than one structure,
      you WILL end up with Access Violation Errors when those objects are
      released by the connection component. To assign a copy of an object,
      allways use the object.CopyOf function or assign the object using
      specific 'as...' property (which doesn't assign the object itself,
      but rather creates a copy of the object). }
    property as_Object[const index:RtcString]:TRtcValueObject read Get_Object write Set_AsObject;

    { There are two ways to work with TRtcAbsRecord objects.
      One is to let the stored object be auto-created on first read access
      to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
      the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
      to create objects before first usage (AutoCreate=FALSE). You control the behavior
      by setting the AutoCreate property to TRUE or FALSE. Any child object created
      using the New... function will inherit this property state. You can change this
      property at any time, on any object. Default AutoCreate state for objects
      created by using the Create and FromCode methods is always FALSE. }
    property AutoCreate:boolean read FAutoCreate write FAutoCreate;
    end;

  { @abstract(Abstract Array Object)
    There are two ways to work with TRtcAbsArray objects.
    One is to let the stored object be auto-created on first read access
    to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
    the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
    to create objects before first usage (AutoCreate=FALSE). You control the behavior
    by setting the AutoCreate property to TRUE or FALSE. Any child object created
    using the New... function will inherit this property state. You can change this
    property at any time, on any object. Default AutoCreate state for objects
    created by using the Create and FromCode methods is always FALSE. }
  TRtcAbsArray = class(TRtcValueObject)
  protected
    // @exclude
    FAutoCreate:boolean;

    // @exclude
    function GetObject(index: integer): TRtcValueObject; virtual; abstract;
    // @exclude
    procedure SetObject(index: integer; Value:TRtcValueObject; asCopy:boolean=False); virtual; abstract;
    // @exclude
    procedure SetAsObject(index: integer; Value:TRtcValueObject);

    // @exclude
    function GetValueType(index: integer): TRtcValueTypes;

    // @exclude
    function GetArray(index: integer): TRtcArray;
    // @exclude
    function GetRecord(index: integer): TRtcRecord;
    // @exclude
    function GetDataSet(index: integer): TRtcDataSet;
    // @exclude
    function GetFunctionInfo(index: integer): TRtcFunctionInfo;

    // @exclude
    function GetByteStream(index: integer): TStream;

    // @exclude
    function GetNull(index: integer): boolean;
    // @exclude
    function GetBoolean(index: integer): boolean;
    // @exclude
    function GetCurrency(index: integer): Currency;
    // @exclude
    function GetDateTime(index: integer): TDateTime;
    // @exclude
    function GetException(index: integer):RtcWideString;
    // @exclude
    function GetVarName(index: integer):RtcWideString;
    // @exclude
    function GetInteger(index: integer): rtcInteger;
    // @exclude
    function GetCardinal(index: integer): rtcCardinal;
    // @exclude
    function GetLargeInt(index: integer): rtcLargeInt;
    // @exclude
    function GetFloat(index: integer): rtcFloat;
    // @exclude
    function GetString(index: integer): RtcString;
    // @exclude
    function GetWideString(index: integer): RtcWideString;
    // @exclude
    function GetText(index: integer):RtcWideString;
    // @exclude
    function GetByteArray(index: integer): RtcByteArray;
    // @exclude
    function GetOID(index: integer): TRtcObjectID;
    // @exclude
    function GetLinkedObject(index: integer): TObject;

    // @exclude
    function GetVariant(index: integer): Variant;
    // @exclude
    procedure SetVariant(index: integer; const Value: Variant);

    // @exclude
    procedure SetArray(index: integer; const Value: TRtcArray);
    // @exclude
    procedure SetRecord(index: integer; const Value: TRtcRecord);
    // @exclude
    procedure SetDataSet(index: integer; const Value: TRtcDataSet);
    // @exclude
    procedure SetFunctionInfo(index: integer; const Value: TRtcFunctionInfo);

    // @exclude
    procedure SetByteStream(index: integer; const Value: TStream);

    // @exclude
    procedure SetNull(index: integer; const Value: boolean);
    // @exclude
    procedure SetBoolean(index: integer; const Value: boolean);
    // @exclude
    procedure SetCurrency(index: integer; const Value: Currency);
    // @exclude
    procedure SetDateTime(index: integer; const Value: TDateTime);
    // @exclude
    procedure SetException(index: integer; const Value:RtcWideString);
    // @exclude
    procedure SetVarName(index: integer; const Value:RtcWideString);
    // @exclude
    procedure SetInteger(index: integer; const Value: rtcInteger);
    // @exclude
    procedure SetCardinal(index: integer; const Value: rtcCardinal);
    // @exclude
    procedure SetLargeInt(index: integer; const Value: rtcLargeInt);
    // @exclude
    procedure SetFloat(index: integer; const Value: rtcFloat);
    // @exclude
    procedure SetString(index: integer; const Value: RtcString);
    // @exclude
    procedure SetWideString(index: integer; const Value: RtcWideString);
    // @exclude
    procedure SetText(index: integer; const Value:RtcWideString);
    // @exclude
    procedure SetByteArray(index: integer; const Value: RtcByteArray);
    // @exclude
    procedure SetOID(index: integer; const Value: TRtcObjectID);
    // @exclude
    procedure SetLinkedObject(index: integer; const Value: TObject);

    // @exclude
    function GetCode(index: integer): RtcString;
    // @exclude
    procedure SetCode(index: integer; const Value: RtcString);

    // @exclude
    function GetXMLrpc(index: integer): RtcString;
    // @exclude
    procedure SetXMLrpc(index: integer; const Value: RtcString);

    // @exclude
    function GetJSON(index: integer):RtcWideString;
    // @exclude
    procedure SetJSON(index: integer; const Value:RtcWideString);

  public
    { Clear this object (initialize: Free all values and objects assigned) }
    procedure Clear; virtual; abstract;

    { Assign a new, fresh Array to the 'index' field.
      To turn the 'index' field into an array of values,
      you have to call "newArray" or assign already created TRtcArray
      using asArray[]:= (assigns a copy of the array) or
      asObject[]:= (assigns existing array instance). }
    function NewArray(index: integer): TRtcArray;
    { Assign a new, fresh Record to the 'index' field.
      To turn the 'index' field into a record of values,
      you have to call "newRecord" or assign already created TRtcRecord
      using asRecord[]:= (assigns a copy of the record) or
      asObject[]:= (assigns existing record instance). }
    function NewRecord(index: integer): TRtcRecord;
    { Assign a new, fresh DataSet to the 'index' field.
      To turn the 'index' field into a dataset of values,
      you have to call "newDataSet" or assign already created TRtcDataSet
      using asDataSet[]:= (assigns a copy of the dataset) or
      asObject[]:= (assigns existing dataset instance). }
    function NewDataSet(index: integer): TRtcDataSet;
    { Assign a new, fresh FunctionInfo to the 'index' field.
      To turn the 'index' field into a function call,
      you have to call "newFunction" or assign already created TRtcFunctionInfo
      using asFunction[]:= (assigns a copy of the FunctionInfo object) or
      asObject[]:= (assigns existing FunctionInfo instance). }
    function NewFunction(index: integer; const func_name:RtcWideString=''): TRtcFunctionInfo;

    { Assign a new, fresh Byte Stream to the 'index' field.
      To turn the 'index' field into a Byte Stream,
      you have to call "newByteStream" or assign already created TStream
      using asByteStream[]:= (assigns a copy of the Stream data). }
    function NewByteStream(index: integer): TStream;

    { Assigns a new, fresh ByteArray to the 'index' field, containing "InitialSize" items (filled with zero). }
    function NewByteArray(index: integer; InitialSize:Integer=0): RtcByteArray;

    { Assign a new, fresh Boolean value to the 'index' field.
      Only sets the type to Boolean.
      You DO NOT have to use this function before accessing as Boolean value. }
    function NewBoolean(index: integer): boolean;
    { Assign a new, fresh Currency value to the 'index' field.
      Only sets the type to Currency.
      You DO NOT have to use this function before accessing as Currency value. }
    function NewCurrency(index: integer): Currency;
    { Assign a new, fresh DateTime value to the 'index' field.
      Only sets the type to DateTime.
      You DO NOT have to use this function before accessing as DateTime value. }
    function NewDateTime(index: integer): TDateTime;
    { Assign a new, fresh Exception value to the 'index' field.
      Only sets the type to Exception.
      You DO NOT have to use this function before accessing as Exception value. }
    function NewException(index: integer):RtcWideString;
    { Assign a new, fresh VariableName value to the 'index' field.
      Only sets the type to VariableName.
      You DO NOT have to use this function before accessing as VariableName. }
    function NewVariable(index: integer):RtcWideString;
    { Assign a new, fresh Integer value to the 'index' field.
      Only sets the type to Integer.
      You DO NOT have to use this function before accessing as Integer value. }
    function NewInteger(index: integer): rtcInteger;
    { Assign a new, fresh Cardinal value to the 'index' field.
      Only sets the type to Cardinal.
      You DO NOT have to use this function before accessing as Cardinal value. }
    function NewCardinal(index: integer): rtcCardinal;
    { Assign a new, fresh Large Integer value to the 'index' field.
      Only sets the type to Large Integer.
      You DO NOT have to use this function before accessing as Large Integer value. }
    function NewLargeInt(index: integer): rtcLargeInt;
    { Assign a new, fresh Floating-point value to the 'index' field.
      Only sets the type to Floating-point.
      You DO NOT have to use this function before accessing as Floating-point value. }
    function NewFloat(index: integer): rtcFloat;
    { Assign a new, fresh RtcString value to the 'index' field.
      Only sets the type to RtcString.
      You DO NOT have to use this function before accessing as RtcString value. }
    function NewString(index: integer): RtcString;
    { Assign a new, fresh RtcWideString value to the 'index' field.
      Only sets the type to RtcWideString.
      You DO NOT have to use this function before accessing as RtcWideString value. }
    function NewWideString(index: integer): RtcWideString;
    { Assign a new, fresh Text value to the 'index' field.
      Only sets the type to Text.
      You DO NOT have to use this function before accessing as Text value. }
    function NewText(index: integer):RtcWideString;

    { Check if 'index' field Value can be accessed (read) using Type "typ" }
    function CheckType(const index:integer; typ:TRtcValueTypes):boolean;

    { read: Is the 'index' field value NULL (not assigned) ? /
      write: TRUE = set 'index' field value to null (this will destroy any object stored there) }
    property isNull[index:integer]:boolean read GetNull write SetNull;
    { Check 'index' field Value type. NULL values always return rtc_NULL }
    property isType[index:integer]:TRtcValueTypes read GetValueType;

    { Read/Write as native value (Variant). @html(<br>)
      You can use this property to get/set all native types:
      Boolean,Integer,Float,Currency,DateTime,RtcString. @html(<br>)
      You CAN NOT use this property to get or set complex structures like
      Record, Array, DataSet or FunctionInfo. }
    property Value[index:integer]:Variant read GetVariant write SetVariant; default;
    { alias for @Link(TRtcAbsValue.Value) }
    property asValue[index:integer]:Variant read GetVariant write SetVariant;

    { Read/Write 'index' field as Boolean value }
    property asBoolean[index:integer]:boolean read GetBoolean write SetBoolean;
    { Read/Write 'index' field as Integer value }
    property asInteger[index:integer]:rtcInteger read GetInteger write SetInteger;
    { Read/Write 'index' field as Cardinal value }
    property asCardinal[index:integer]:rtcCardinal read GetCardinal write SetCardinal;
    { Read/Write 'index' field as Large Integer value }
    property asLargeInt[index:integer]:rtcLargeInt read GetLargeInt write SetLargeInt;
    { Read/Write 'index' field as Float value }
    property asFloat[index:integer]:rtcFloat read GetFloat write SetFloat;
    { Read/Write 'index' field as Currency value }
    property asCurrency[index:integer]:Currency read GetCurrency write SetCurrency;
    { Read/Write 'index' field as DataTime value }
    property asDateTime[index:integer]:TDateTime read GetDateTime write SetDateTime;
    { Read/Write 'index' field as Exception value (integer) }
    property asException[index:integer]:RtcWideString read GetException write SetException;
    { Read/Write 'index' field as VariableName (integer) }
    property asVarName[index:integer]:RtcWideString read GetVarName write SetVarName;

    { Read/Write 'index' field as Linked Object ID }
    property asOID[index:integer]:TRtcObjectID read GetOID write SetOID;
    { Wherever possible, use "asOID" instead of "asLinkedObject" because
      "asLinkedObject" requires an active "Object Manager" for the current thread. @html(<br>)
      Reading "asLinkedObject" is identical to "GetRtcObjectManager.FindObject(asOID[..]);" @html(<br>)
      Writing to "asLinkedObject" is identical to "asOID[..]:=GetRtcObjectManager.FindOID(<TObject>);"  }
    property asLinkedObject[index:integer]:TObject read GetLinkedObject write SetLinkedObject;

    { Read/Write 'index' field as RtcString value (raw access) }
    property asString[index:integer]:RtcString read GetString write SetString;
    { Read/Write 'index' field as RtcWideString value (raw access) }
    property asWideString[index:integer]:RtcWideString read GetWideString write SetWideString;
    { Read/Write 'index' field as Text value (UTF-8 encode on write, decode on read) }
    property asText[index:integer]:RtcWideString read GetText write SetText;

    { read: Access Byte Array stored in the 'index' field /
      write: assign a copy of the source Byte Array to the 'index' field }
    property asByteArray[index:integer]:RtcByteArray read GetByteArray write SetByteArray;

    { Read: return 'index' field coded as a RtcString,
      from which the object can be reconstructed anytime. /
      Write: reconstruct object from RtcString and assign to 'index' field. }
    property asCode[index:integer]:RtcString read GetCode write SetCode;
    { Read: return 'index' field coded as an XML-RPC RtcString,
      from which the object can be reconstructed. /
      Write: reconstruct object from XML-RPC RtcString and assign to 'index' field. }
    property asXMLrpc[index:integer]:RtcString read GetXMLrpc write SetXMLrpc;
    { Read: return 'index' field coded as an JSON Ansi String,
      from which the object can be reconstructed. /
      Write: reconstruct object from JSON String and assign to 'index' field. }
    property asJSON[index:integer]:RtcWideString read GetJSON write SetJSON;

    { read: Access Stream stored in the 'index' field /
      write: assign a copy of the source Stream to the 'index' field }
    property asByteStream[index:integer]:TStream read GetByteStream write SetByteStream;

    { read: Access array stored in the 'index' field /
      write: assign a copy of the source TRtcArray to the 'index' field }
    property asArray[index:integer]:TRtcArray read GetArray write SetArray;
    { read: Access record stored in the 'index' field /
      write: assign a copy of the source TRtcRecord to the 'index' field }
    property asRecord[index:integer]:TRtcRecord read GetRecord write SetRecord;
    { read: Access DataSet stored in the 'index' field /
      write: assign a copy of the source TRtcDataset to the 'index' field }
    property asDataSet[index:integer]:TRtcDataSet read GetDataSet write SetDataSet;
    { read: Access FunctionInfo stored in the 'index' field /
      write: assign a copy of the source TRtcFunctionInfo to the 'index' field }
    property asFunction[index:integer]:TRtcFunctionInfo read GetFunctionInfo write SetFunctionInfo;

    { Read: Access information in the 'index' field as TRtcValueObject / @html(<br>)
      Write: NIL = remove object from the 'index' field without destroying it / @html(<br>)
      Write: object = Assign object to the 'index' field in this structure.
      It will be maintained and destroyed with this structure,
      as long as it isn't removed by calling asObject[index]:=nil. @html(<br><br>)

      WARNING: NEVER! use asObject to assign an object which you did not create yourself,
      or to assign an object to more than one TRtcValueObject structure.
      If the same object instance is assigned to more than one structure,
      you WILL end up with Access Violation Errors when those objects are
      released by the connection component. To assign a copy of an object,
      allways use the object.CopyOf function or assign the object using
      specific 'as...' property (which doesn't assign the object itself,
      but rather creates a copy of the object). }
    property asObject[index:integer]:TRtcValueObject read GetObject write SetAsObject;

    { There are two ways to work with TRtcAbsArray objects.
      One is to let the stored object be auto-created on first read access
      to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
      the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
      to create objects before first usage (AutoCreate=FALSE). You control the behavior
      by setting the AutoCreate property to TRUE or FALSE. Any child object created
      using the New... function will inherit this property state. You can change this
      property at any time, on any object. Default AutoCreate state for objects
      created by using the Create and FromCode methods is always FALSE. }
    property AutoCreate:boolean read FAutoCreate write FAutoCreate;
    end;

  { @abstract(Object to store and transport a single value of any kind)
    This value can be anything from a static value, to an array,
    a named-list or a function call. }
  TRtcValue = class(TRtcAbsValue)
  protected
    // @exclude
    FValue:TRtcValueObject;

    procedure Extracted; override;

    // @exclude
    function GetType:TRtcValueTypes; override;
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    // @exclude
    procedure SetObject(const pValue:TRtcValueObject; asCopy:boolean=False); override;
    // @exclude
    function GetObject:TRtcValueObject; override;

    // @exclude
    procedure CopyFrom(pValue:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    // @exclude
    constructor Create;

    // @exclude
    destructor Destroy; override;

    { Clear this object (initialize: Free all values and objects assigned) }
    procedure Clear; override;

    { You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure Extract;

    { Create TRtcValue object from 'data' RtcString, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      Since TRtcValue encapsulates one entity of any kind, you can use
      TRtcValue to reconstruct objects where you do not know which object type
      is stored after the 'at' chacacter inside your 'data' RtcString. }
    class function FromCode(const data:RtcString; var at:integer):TRtcValue; overload;

    { Create TRtcValue object from 'data' RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the RtcString.
      @html(<br><br>)
      Since TRtcValue encapsulates one entity of any kind, you can use
      TRtcValue to reconstruct objects where you do not know which object type
      is stored after the 'at' chacacter inside your 'data' RtcString. }
    class function FromCode(const data:RtcString):TRtcValue; overload;

    { Create TRtcValue object from 'data' XML-RPC RtcString, starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromXMLrpc,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:RtcString; var at:integer):TRtcValue; overload;

    { Create TRtcValue object from 'data' XML-RPC RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the XML-RPC RtcString. }
    class function FromXMLrpc(const data:RtcString):TRtcValue; overload;

    { Create TRtcValue object from 'data' JSON String, starting after character at position 'at'.
      Before the first call to FromJSON, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromJSON,
      'at' should equal to length(data). }
    class function FromJSON(const data:RtcWideString; var at:integer):TRtcValue; overload;

    { Create TRtcValue object from 'data' JSON String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromJSON with 'at' parameter
      if there are more objects stored in the JSON String. }
    class function FromJSON(const data:RtcWideString):TRtcValue; overload;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  { @abstract(Object to store and transport data in record) }
  TRtcRecord = class(TRtcAbsRecord)
  protected
    // @exclude
    FValues:tRtcFastStringObjList;

    // @exclude
    class function NullValue:TRtcRecord;
    // @exclude
    function GetType:TRtcValueTypes; override;
    // @exclude
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    // @exclude
    function GetObject(const index:RtcWideString): TRtcValueObject; override;
    // @exclude
    procedure SetObject(const index:RtcWideString; pValue:TRtcValueObject; asCopy:boolean=False); override;

    // @exclude
    function GetFieldCount: integer;

    // @exclude
    function GetFieldName(index: integer):RtcWideString;
    // @exclude
    function Get_FieldName(index: integer): RtcString;

    // @exclude
    procedure SetFieldName(index: integer; const pValue:RtcWideString);
    // @exclude
    procedure Set_FieldName(index: integer; const pValue: RtcString);

    // @exclude
    procedure CopyFrom(pValue:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    // @exclude
    constructor Create; virtual;
    // @exclude
    destructor Destroy; override;

    { Clear this object (Free all values and objects assigned) }
    procedure Clear; override;

    { You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure Extract(const index:RtcWideString);
    { (RtcString) You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure _Extract(const index:RtcString);

    { Create TRtcRecord object from 'data' RtcString, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' RtcString after 'at'
      position was prepared using the TRtcRecord.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:RtcString; var at:integer):TRtcRecord; overload;

    { Create TRtcRecord object from 'data' RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the RtcString.
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data'
      was prepared using the TRtcRecord.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:RtcString):TRtcRecord; overload;

    { Create TRtcRecord object from 'data' as XML-RPC RtcString, starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromXMLrpc,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:RtcString; var at:integer):TRtcRecord; overload;

    { Create TRtcRecord object from 'data' as XML-RPC RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the RtcString. }
    class function FromXMLrpc(const data:RtcString):TRtcRecord; overload;

    { Create TRtcRecord object from 'data' as JSON String, starting after character at position 'at'.
      Before the first call to FromJSON, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromJSON,
      'at' should equal to length(data). }
    class function FromJSON(const data:RtcWideString; var at:integer):TRtcRecord; overload;

    { Create TRtcRecord object from 'data' as JSON String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromJSON with 'at' parameter
      if there are more objects stored in the String. }
    class function FromJSON(const data:RtcWideString):TRtcRecord; overload;

    { Get all Record elements as one continuous RtcString }
    function GetAsString: RtcString;
    { Get all Record elements as one continuous Unicode String }
    function GetAsText:RtcWideString;
    { Get all Record elements as one continuous RtcWideString }
    function GetAsWideString: RtcWideString;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;

    // Count Fields in Record (same as FieldCount)
    function Count:integer; virtual;

    // Count Fields in Record
    property FieldCount:integer read GetFieldCount default 0;

    // Get/Set Name of the field at position 'index'
    property FieldName[index:integer]:RtcWideString read GetFieldName write SetFieldName;
    // (RtcString) Get/Set Name of the field at position 'index'
    property Field_Name[index:integer]:RtcString read Get_FieldName write Set_FieldName;
    end;

  { @abstract(Object to store and transport data in an array) }
  TRtcArray = class(TRtcAbsArray)
  protected
    // @exclude
    FValues:TObjectList;
    // @exclude
    class function NullValue:TRtcArray;
    // @exclude
    function GetType:TRtcValueTypes; override;
    // @exclude
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    // @exclude
    function GetObject(index: integer): TRtcValueObject; override;
    // @exclude
    procedure SetObject(index: integer; pValue:TRtcValueObject; asCopy:boolean=False); override;

    // @exclude
    function GetFieldCount: integer;

    // @exclude
    procedure CopyFrom(pValue:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    // @exclude
    constructor Create; virtual;
    // @exclude
    destructor Destroy; override;

    { Clear this object (Free all values and objects assigned) }
    procedure Clear; override;

    { You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure Extract(const index: integer);

    { Create TRtcArray object from 'data' RtcString, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' RtcString after 'at'
      position was prepared using the TRtcArray.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:RtcString; var at:integer):TRtcArray; overload;

    { Create TRtcArray object from 'data' RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the RtcString.
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' RtcString
      was prepared using the TRtcArray.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:RtcString):TRtcArray; overload;

    { Create TRtcArray object from 'data' as XML-RPC RtcString, starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromXMLrpc,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:RtcString; var at:integer):TRtcArray; overload;

    { Create TRtcArray object from 'data' as XML-RPC RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the RtcString. }
    class function FromXMLrpc(const data:RtcString):TRtcArray; overload;

    { Create TRtcArray object from 'data' as JSON String, starting after character at position 'at'.
      Before the first call to FromJSON, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromJSON,
      'at' should equal to length(data). }
    class function FromJSON(const data:RtcWideString; var at:integer):TRtcArray; overload;

    { Create TRtcArray object from 'data' as JSON String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromJSON with 'at' parameter
      if there are more objects stored in the RtcString. }
    class function FromJSON(const data:RtcWideString):TRtcArray; overload;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;

    { Combine all array elements into one continuous RtcString }
    function GetAsString: RtcString;
    { Combine all array elements into one continuous Unicode String }
    function GetAsText:RtcWideString;
    { Combine all array elements into one continuous RtcWideString }
    function GetAsWideString: RtcWideString;

    // Count fields in the Array (same as FieldCount)
    function Count:integer; virtual;

    // Count fields in the Array
    property FieldCount:integer read GetFieldCount default 0;
    end;

  // @exclude
  TRtcFieldTypesArray=array of TRtcFieldTypes;
  // @exclude
  TRtcIntegerArray=array of integer;
  // @exclude
  TRtcBooleanArray=array of boolean;

 { @abstract(Access to a single Row from a TRtcDataSet)
   This class provides access to fields and data of a single ROW inside a TRtcDataSet,
   without giving up access to methods for moving to other rows, deleting or inserting rows.

   TRtcDataRow class makes it possible to pass a single but complete ROW from a
   TRtcDataSet to user functions and events, without giving the receiving functions
   and events access to methods for moving between ROWs, deleting ROWs or inserting
   ROWs of the TRtcDataSet. Only methods and properties required for accessing and
   modifying fields (field type/size/required) and data inside a single ROW are made
   available to the functions and events receiving the TRtcDataRow as parameter.

   By using the "TRtcDataRow.CreateFromDataSet(Source:TRtcDataSet)" constructor,
   it is also possible to give functions and events access to a select set of ROWs,
   each through a separate TRtcDataRow instance, all inside the same TRtcDataSet. }
  TRtcDataRow = class(TRtcAbsRecord)
  protected
    // @exclude
    FNames:tRtcFastStringObjList;

    // @exclude
    FTypes:^TRtcFieldTypesArray;
    // @exclude
    FSizes:^TRtcIntegerArray;
    // @exclude
    FRequired:^TRtcBooleanArray;
    // @exclude
    FData:TObjectList;
    // @exclude
    FRow:integer;

    // @exclude (not to be used -> throws exception)
    procedure CopyFrom(Value:TRtcValueObject); override;
    // @exclude (not to be used -> throws exception)
    function GetType:TRtcValueTypes; override;
    // @exclude (not to be used -> throws exception)
    function TypeCheck(typ:TRtcValueTypes):boolean; override;
    // @exclude (not to be used -> throws exception)
    procedure from_Code(const s:RtcString; var at:integer); overload; override;
    // @exclude (not to be used -> throws exception)
    procedure from_XMLrpc(const s:RtcString; var at:integer); overload; override;
    // @exclude (not to be used -> throws exception)
    procedure from_JSON(const s:RtcWideString; var at:integer); overload; override;

    // @exclude
    function GetObject(const index:RtcWideString): TRtcValueObject; override;
    // @exclude
    procedure SetObject(const index:RtcWideString; pValue:TRtcValueObject; asCopy:boolean=False); override;

    // @exclude
    function GetFieldIndex(const index:RtcWideString): integer;
    // @exclude
    function Get_FieldIndex(const index:RtcString): integer;

    // @exclude
    function GetFieldCount: integer;

    // @exclude
    function GetFieldName(index: integer):RtcWideString;
    // @exclude
    function Get_FieldName(index: integer): RtcString;

    // @exclude
    procedure SetFieldName(index: integer; const pValue:RtcWideString);
    // @exclude
    procedure Set_FieldName(index: integer; const pValue: RtcString);

    // @exclude
    function GetFieldSize(const index:RtcWideString): Integer;
    // @exclude
    function Get_FieldSize(const index:RtcString): Integer;

    // @exclude
    procedure SetFieldSize(const index:RtcWideString; const pValue: Integer);
    // @exclude
    procedure Set_FieldSize(const index:RtcString; const pValue: Integer);

    // @exclude
    function GetFieldType(const index:RtcWideString): TRtcFieldTypes;
    // @exclude
    function Get_FieldType(const index:RtcString): TRtcFieldTypes;

    // @exclude
    procedure SetFieldType(const index:RtcWideString; const pValue: TRtcFieldTypes);
    // @exclude
    procedure Set_FieldType(const index:RtcString; const pValue: TRtcFieldTypes);

    // @exclude
    function GetFieldRequired(const index:RtcWideString): boolean;
    // @exclude
    function Get_FieldRequired(const index:RtcString): boolean;

    // @exclude
    procedure SetFieldRequired(const index:RtcWideString; const pValue: boolean);
    // @exclude
    procedure Set_FieldRequired(const index:RtcString; const pValue: boolean);

  public
    // @exclude
    destructor Destroy; override;

    // @exclude (not to be used -> throws exception)
    procedure Clear; override;
    // @exclude (not to be used -> throws exception)
    function copyOf:TRtcValueObject; override;
    // @exclude (not to be used -> throws exception)
    procedure to_Code(const Result:TRtcHugeString); override;
    // @exclude (not to be used -> throws exception)
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    // @exclude (not to be used -> throws exception)
    procedure to_JSON(const Result:TRtcHugeString); override;

    { Create a new TRtcDataRow object fixed at the current ROW of the Source TRtcDataSet object.
      A physical link will be created to the current row inside the Source TRtcDataSet without making
      a copy of the Source. This new TRtcDataRow instance will give you direct access to the ROW
      which was active on the Source:TRtcDataSet when calling the CreateFromRtcDataSet constructor,
      even if the ROW position on the Source:TRtcDataSet is changed afterwards. }
    constructor CreateFromRtcDataSet(Source:TRtcDataSet);

    { You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure Extract(const index:RtcWideString);

    { (RtcString) You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure _Extract(const index:RtcString);

    // You can use this method to create a new field and set all its properties in one call
    procedure SetField(const FldName:RtcWideString; FldType:TRtcFieldTypes; FldSize:integer=0; FldRequired:boolean=False);
    // (RtcString) You can use this method to create a new field and set all its properties in one call
    procedure Set_Field(const FldName:RtcString; FldType:TRtcFieldTypes; FldSize:integer=0; FldRequired:boolean=False);

    { This function was added only to make field access similar to TDataSet.FieldByName() possible.
      FieldByName will always return a TRtcValue object (never NIL), which you can use to read data
      inside this specific field in this specific row. If nothing was assigned to that row and field,
      a new NULL value will automatically be created and assigned on first access.

      When using FieldByName, a container object will be created for each field and row,
      which takes more memory and makes field access slower. Unless you absolutely
      NEED to use FieldByName to access field data, it would be much better to use
      asString[], asInteger[], asDateTime[] and other "as..." properties, which work
      faster and require less memory, since the TRtcValue container is not needed. }
    function FieldByName(const index:RtcWideString):TRtcValue;
    { (RtcString) This function was added only to make field access similar to TDataSet.FieldByName() possible.
      FieldByName will always return a TRtcValue object (never NIL), which you can use to read data
      inside this specific field in this specific row. If nothing was assigned to that row and field,
      a new NULL value will automatically be created and assigned on first access.

      When using FieldByName, a container object will be created for each field and row,
      which takes more memory and makes field access slower. Unless you absolutely
      NEED to use FieldByName to access field data, it would be much better to use
      asString[], asInteger[], asDateTime[] and other "as..." properties, which work
      faster and require less memory, since the TRtcValue container is not needed. }
    function Field_ByName(const index:RtcString):TRtcValue;

    // Count Fields in the record (all records in a dataset have the same fields)
    property FieldCount:integer read GetFieldCount;

    // Get/Set name for the field at position 'index'
    property FieldName[index:integer]:RtcWideString read GetFieldName write SetFieldName;
    // (RtcString) Get/Set name for the field at position 'index'
    property Field_Name[index:integer]:RtcString read Get_FieldName write Set_FieldName;

    // Get index (position) for the Field with name 'index'
    property FieldIndex[const index:RtcWideString]:integer read GetFieldIndex;
    // (RtcString) Get index (position) for the Field with name 'index'
    property Field_Index[const index:RtcString]:integer read Get_FieldIndex;

    { Get/Set "FieldType" property for the field with name 'index'.
      Actual data type can varry from row to row.
      Value Type of data stored in the field in a specific row
      can be obtained using the isType[] property. }
    property FieldType[const index:RtcWideString]:TRtcFieldTypes read GetFieldType write SetFieldType;
    { (RtcString) Get/Set "FieldType" property for the field with name 'index'.
      Actual data type can varry from row to row.
      Value Type of data stored in the field in a specific row
      can be obtained using the isType[] property. }
    property Field_Type[const index:RtcString]:TRtcFieldTypes read Get_FieldType write Set_FieldType;

    // Get/Set "DataSize" property for the field with name 'index'
    property FieldSize[const index:RtcWideString]:Integer read GetFieldSize write SetFieldSize;
    // (RtcString) Get/Set "DataSize" property for the field with name 'index'
    property Field_Size[const index:RtcString]:Integer read Get_FieldSize write Set_FieldSize;

    // Get/Set "required" property for the field with name 'index'
    property FieldRequired[const index:RtcWideString]:boolean read GetFieldRequired write SetFieldRequired;
    // (RtcString) Get/Set "required" property for the field with name 'index'
    property Field_Required[const index:RtcString]:boolean read Get_FieldRequired write Set_FieldRequired;
    end;

  { @abstract(Object to store and transport a DataSet) }
  TRtcDataSet = class(TRtcDataRow)
  protected
    // @exclude
    class function NullValue:TRtcDataSet;
    // @exclude
    function GetType:TRtcValueTypes; override;
    // @exclude
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    // @exclude
    function GetRowData: TRtcArray;
    // @exclude
    procedure SetRowData(const pValue: TRtcArray);

    // @exclude
    function GetRowCount: integer;
    // @exclude
    function GetRow: integer;
    // @exclude
    procedure SetRow(const pValue: integer);

    // @exclude
    procedure CopyFrom(pValue:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    // @exclude
    constructor Create; virtual;
    // @exclude
    destructor Destroy; override;

    { Clear this object (Free all values and objects assigned) }
    procedure Clear; override;

    { Create TRtcDataSet object from 'data' RtcString, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' RtcString after 'at'
      position was prepared using the TRtcDataSet.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:RtcString; var at:integer):TRtcDataSet; overload;

    { Create TRtcDataSet object from 'data' RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the RtcString.
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' RtcString
      was prepared using the TRtcDataSet.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:RtcString):TRtcDataSet; overload;

    { Create TRtcDataSet object from 'data' as XML-RPC RtcString, starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromXMLrpc,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:RtcString; var at:integer):TRtcDataSet; overload;

    { Create TRtcDataSet object from 'data' as XML-RPC RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the RtcString. }
    class function FromXMLrpc(const data:RtcString):TRtcDataSet; overload;

    { Create TRtcDataSet object from 'data' as JSON String, starting after character at position 'at'.
      Before the first call to FromJSON, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromJSON,
      'at' should equal to length(data). }
    class function FromJSON(const data:RtcWideString; var at:integer):TRtcDataSet; overload;

    { Create TRtcDataSet object from 'data' as JSON String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromJSON with 'at' parameter
      if there are more objects stored in the String. }
    class function FromJSON(const data:RtcWideString):TRtcDataSet; overload;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;

    // Jump to first row
    procedure First;
    // Jump to last row
    procedure Last;
    // Jump to a prior row
    procedure Prior;
    { Jump to the next row. "Next" will move you *behind* the last Row
      if you are on the last, but it wil not move further than that.
      Eof will become TRUE if you were on the last row before calling Next. }
    procedure Next;
    // Insert a row here
    procedure Insert;
    // Append a row after last row
    procedure Append;
    // Delete current Row
    procedure Delete;
    // Behind the last row (Row>=RowCount)?
    function Eof:boolean;
    // Before the first row (Row<0)?
    function Bof:boolean;
    // No rows in dataset?
    function Empty:boolean;

    { Combine all DatSet elements as one continuous RtcString }
    function GetAsString: RtcString;
    { Combine all DatSet elements as one continuous Unicode String }
    function GetAsText:RtcWideString;
    { Combine all DatSet elements as one continuous RtcWideString }
    function GetAsWideString: RtcWideString;

    // Count Rows in Dataset
    property RowCount:integer read GetRowCount;
    // Check current Row position / Set row position
    property Row:integer read GetRow write SetRow;
    { Get/Set data for the complete current row. @html(<br>)
      Once a Row is assigned, it can not be removed without being destroyed.
      By assigning NIL to RowData, existing row will be destroyed. }
    property RowData:TRtcArray read GetRowData write SetRowData;
    end;

  { Actions applied to a DataSet (Insert / Update / Delete) }
  TRtcDataSetAction=(rds_None, rds_Insert, rds_Update, rds_Delete);

  { @abstract(DataSet Changes)
    Create an instance of "TRtcDataSetChanges" by using the RTC Value Object received from the
    "ExtractChanges" method on "TRtcMemDataSet", "TRtcDataSetMonitor" or "TRtcDataSetRecorder".
    This class is used to read Client-side DataSet changes, so they can be executed on the Server. }
  TRtcDataSetChanges=class(TObject)
  private
    FChanges:TRtcValue;
    FChangeSet:TRtcDataSet;
    FChangeList:TRtcArray;

    FPosition:integer;
    FTmpRow:TRtcDataRow;

  public
    { Creates a TRtcDataSetChanges instance from a RTC Value Object received using the "ExtractChanges" method.
      "Changes" object passed here as parameter will NOT be destroyed by the TRtcDataSetChanges instance! }
    constructor Create(Changes:TRtcValueObject);
    // @exclude
    destructor Destroy; override;

    { Number of Actions }
    function Count:integer;

    { Move to the 1st Action }
    procedure First;
    { Move to the last Action }
    procedure Last;
    { Move to the previous Action }
    procedure Prior;
    { Move to the next Action }
    procedure Next;

    { Before the first action.
      No more actions in "reverse" direction (Prior). }
    function Bof:boolean;
    { Behind the last action.
      No more actions in "forward" direction (Next). }
    function Eof:boolean;

    { Get example SQL statement for executing the current Action on Table "TableName".
      This method only uses the "Action", "NewRow" and "OldRow" methods (all available below). }
    function GetActionSQL(const TableName:RtcWideString):RtcWideString;

    { Get example SQL statement for executing the reverse of the current Action on Table "TableName".
      This method only uses the "Action", "NewRow" and "OldRow" methods (all available below). }
    function GetReverseSQL(const TableName:RtcWideString):RtcWideString;

    { Get "Action" at the current position (rds_Insert, rds_Update or rds_Delete).
      case Action of
        rds_Insert: Insert Row ( NewRow );
        rds_Update: Update Row ( OldRow, NewRow );
        rds_Delete: Delete Row ( OldRow );
        end; }
    function Action:TRtcDataSetAction;

    { Get "OLD DataSet Row" at the current position.
      This function will return a valid TRtcDataRow ONLY for rds_Update and rds_Delete actions.
      Calling this function when positioned on a rds_Insert action will always return NIL.
      NOTE: Do NOT destroy the instance received here, it will be destroyed automatically after
      moving away from the current position or destroying the TRtcDataSetChanges instance. }
    function OldRow:TRtcDataRow;

    { Get "NEW DataSet Row" at the current position.
      This function will return a valid TRtcDataRow ONLY for rds_Update and rds_Insert actions.
      Calling this function when positioned on a rds_Delete action will always return NIL.
      NOTE: Do NOT destroy the instance received here, it will be destroyed automatically after
      moving away from the current position or destroying the TRtcDataSetChanges instance. }
    function NewRow:TRtcDataRow;
    end;

  { @abstract(Object to store and transport information for a remote function call) }
  TRtcFunctionInfo = class(TRtcRecord)
  protected
    // @exclude
    class function NullValue:TRtcFunctionInfo;
    // @exclude
    function GetType:TRtcValueTypes; override;
    // @exclude
    function TypeCheck(typ:TRtcValueTypes):boolean; override;

    // @exclude
    procedure CopyFrom(pValue:TRtcValueObject); override;

    { Fill object information from RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:RtcString; var at:integer); override;

    { Fill object information from XML-RPC RtcString, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole RtcString was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:RtcString; var at:integer); override;

    { Fill object information from JSON String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_JSON, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_JSON(const s:RtcWideString; var at:integer); override;

  public
    // Name of the Function
    FunctionName:RtcWideString;

    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    { Clear this object (Free all values and objects assigned) }
    procedure Clear; override;

    { Create TRtcFunctionInfo object from 'data' RtcString, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' RtcString after 'at'
      position was prepared using the TRtcDataSet.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:RtcString; var at:integer):TRtcFunctionInfo; overload;

    { Create TRtcFunctionInfo object from 'data' RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the RtcString.
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' RtcString
      was prepared using the TRtcDataSet.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:RtcString):TRtcFunctionInfo; overload;

    { Create TRtcFunctionInfo object from 'data' as XML-RPC RtcString,
      starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from RtcString
      while recreating the object. After the last call to FromXMLrpc,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:RtcString; var at:integer):TRtcFunctionInfo; overload;

    { Create TRtcFunctionInfo object from 'data' as XML-RPC RtcString.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the RtcString. }
    class function FromXMLrpc(const data:RtcString):TRtcFunctionInfo; overload;

    { Create TRtcFunctionInfo object from 'data' as JSON String,
      starting after character at position 'at'.
      Before the first call to FromJSON, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromJSON,
      'at' should equal to length(data). }
    class function FromJSON(const data:RtcWideString; var at:integer):TRtcFunctionInfo; overload;

    { Create TRtcFunctionInfo object from 'data' as JSON String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromJSON with 'at' parameter
      if there are more objects stored in the String. }
    class function FromJSON(const data:RtcWideString):TRtcFunctionInfo; overload;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a RtcString containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC RtcString containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    { Create a JSON Ansi String containing all object information. }
    procedure to_JSON(const Result:TRtcHugeString); override;
    end;

  { @abstract(Advanced Information Object, used for storing multiple objects and values for local usage)

    TRtcInfo extends TRtcRecord with Obj and Child properties,
    allowing you to store pointers to local TObject instances,
    which will NOT intervene with any other field or property
    values defined by TRtcRecord. @html(<br><br>)

    Objects stored using Obj and/or Child properties are kept in a separate list,
    so they can NOT be accessed by TRtcRecord and therefor will NOT be used by TRtcRecord. }
  TRtcInfo = class(TRtcRecord)
  private
    ObjList:tStringObjList;

    function Get_Object(const index:RtcWideString):TObject;
    procedure Set_Object(const index:RtcWideString; pObj:TObject);
    function Get_ChildInfo(const index:RtcWideString): TRtcInfo;
    procedure Set_ChildInfo(const index:RtcWideString; const pValue: TRtcInfo);

    function _Get_Object(const index:RtcString):TObject;
    procedure _Set_Object(const index:RtcString; pObj:TObject);
    function _Get_ChildInfo(const index:RtcString): TRtcInfo;
    procedure _Set_ChildInfo(const index:RtcString; const pValue: TRtcInfo);

  public
    { @exclude }
    constructor Create; override;

    { Number of properties assigned to this TRtcInfo,
      including objects assigned to Obj and/or Child properties. }
    function Count:integer; override;

    { Number of objects assigned to Obj and/or Child properties. }
    function ObjCount:integer;

    { Clear this info: this will also 'Kill' all TRtcObjects assigned to Obj. }
    procedure Clear; override;

    { Create a new Child-Info object and return its pointer }
    function NewChild(const index:RtcWideString):TRtcInfo;
    { (RtcString) Create a new Child-Info object and return its pointer }
    function _NewChild(const index:RtcString):TRtcInfo;

    { Get and set an Object (TRtcObject or TObject) for a given name (String, NOT case-sensitive).
      Seting it to NIL will remove the object from list,
      *without* calling the 'Kill' method for TRtcObject. }
    property Obj[const index:RtcWideString]:TObject read Get_Object write Set_Object;
    { (RtcString) Get and set an Object (TRtcObject or TObject) for a given name (RtcString, NOT case-sensitive).
      Seting it to NIL will remove the object from list,
      *without* calling the 'Kill' method for TRtcObject. }
    property _Obj[const index:RtcString]:TObject read _Get_Object write _Set_Object;

    { Get and set child-info Object for a given name (String, NOT case-sensitive).
      Seting it to NIL will remove the object from list,
      *without* calling the 'Kill' method for TRtcInfo.
      If AutoCreate is TRUE, read access to Child will auto-create a new instance of TRtcInfo. }
    property Child[const index:RtcWideString]:TRtcInfo read Get_ChildInfo write Set_ChildInfo;
    { (RtcString) Get and set child-info Object for a given name (RtcString, NOT case-sensitive).
      Seting it to NIL will remove the object from list,
      *without* calling the 'Kill' method for TRtcInfo.
      If AutoCreate is TRUE, read access to Child will auto-create a new instance of TRtcInfo. }
    property _Child[const index:RtcString]:TRtcInfo read _Get_ChildInfo write _Set_ChildInfo;

    // Kill Object or child-info object with name "index" and set this pointer to NIL
    procedure SetNil(const index:RtcWideString);
    // (RtcString) Kill Object or child-info object with name "index" and set this pointer to NIL
    procedure _SetNil(const index:RtcString);
    end;

  { @abstract(Basic Session Information object)
    TRtcSession extends TRtcInfo with a few Session propertis,
    which will NOT be used by TRtcRecord's toCode and FromCode functions.
    This means that you can pack all the session variables into
    a transportable object, without the information which is not
    part of your session data (ID, Created, PeerAddr, etc). }
  TRtcSession=class(TRtcInfo)
  protected
    // @exclude
    FLockType:TRtcSessionLockType;
    // @exclude
    FForwardedFor:RtcString;
    // @exclude
    FID,FPeerAddr:RtcString;
    // @exclude
    FCreated:TDateTime;

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    // Session ID
    property ID:RtcString read FID;
    // Session Create Time
    property Created:TDateTime read FCreated;
    // Session Peer Address
    property PeerAddr:RtcString read FPeerAddr;
    // Session "X-FORWARDER-FOR" value
    property ForwardedFor:RtcString read FForwardedFor;
    // Session Lock Type
    property LockType:TRtcSessionLockType read FLockType;
    end;

  { @abstract(HTTP Values: Reading and writing HTTP Values) }
  TRtcHttpValues = class(TObject)
  private
    FValChange,
    FTxtChange:boolean;

    FValues:tRtcFastStrObjList;

    FDelimiter:RtcString;
    FOrigQuery:RtcString;
    FCacheSize:integer;
    FTempFileSize:int64;
    FTempFileName:RtcWideString;

    procedure PrepareValues;
    procedure PrepareText;

    function GetItemCount: integer;
    function GetItemName(index: integer): RtcString;
    function GetItemValue(index: integer): RtcString;
    procedure SetItemName(index: integer; const Value: RtcString);
    procedure SetItemValue(index: integer; const Value: RtcString);

    function GetValue(const index: RtcString): RtcString;
    procedure SetValue(const index: RtcString; const Value: RtcString);

    function GetDelimiter: RtcString;
    procedure SetDelimiter(const Value: RtcString);

    function GetElementCount(const index: RtcString):integer;

    { Get element stored as "index" at location "loc" (starting from 0). }
    function GetElement(const index: RtcString; loc: integer):RtcString;
    procedure SetElement(const index: RtcString; loc: integer; const Value:RtcString);
    function GetValueCS(const index: RtcString): RtcString;
    procedure SetValueCS(const index, Value: RtcString);

  protected
    { @exclude }
    function GetAsText:RtcString; virtual;
    { @exclude }
    procedure SetText(const pValue: RtcString); virtual;

  public
    { @exclude }
    constructor Create; virtual;
    { @exclude }
    destructor Destroy; override;

    { Clear this parameters object (prepare for new parameters information) }
    procedure Clear; virtual;

    // Add more text (used with "Read" to append more data)
    procedure AddText(const s:RtcString); overload;

    { Return TRUE if a File was uploaded using <input type="file">.
      To retieve File content, use the GetFile method. }
    function IsFile(const index: RtcString):boolean; overload;

    { Get file uploaded using <input type="file"> and store its content in a "Stream".
      Return TRUE if successful, FALSE if file not found or variable name not recognized. }
    function GetFile(const index: RtcString; stream:TStream):boolean; overload;

    { Get file uploaded using <input type="file"> and store its content in a local file "LocalFileName".
      Return TRUE if successful, FALSE if file not found or variable name not recognized. }
    function GetFile(const index: RtcString; const LocalFileName:RtcWideString):boolean; overload;

    { Maximum size of memory cache which may be used for storing data.
      If this cache size is filled, AddText() and Text will write all data to
      a temporary file and continue storing data in that temp file
      instead of storing all data in memory. This makes this object
      suitable for accepting file uploads. @html(<br><br>)
      -1 = "store everything in memory", @html(<br>)
      0 = use default value (RTC_FORMPOST_CACHESIZE) @html(<br>) }
    property CacheSize:integer read FCacheSize write FCacheSize;

    // Number of Items (all items with the same name count as 1 item)
    property ItemCount:integer read GetItemCount default 0;
    // Name of the 'index'-th item (attribute), starting from 0
    property ItemName[index:integer]:RtcString read GetItemName write SetItemName;
    { Value of the 'index'-th item (attribute), starting from 0.
      If there are multiple elements stored at 'index', the value of the last element will be returned/changed. }
    property ItemValue[index:integer]:RtcString read GetItemValue write SetItemValue;

    { Value of the attribute with the name 'index'.
      If there are multiple elements with the name 'index',
      the value of the last element in the array will be returned/changed.
      To work with arrays (multiple elements with the same name), use the ElementCount and Element properties. }
    property Value[const index: RtcString]:RtcString read GetValue write SetValue; default;
    // same as "Value[]"
    property asString[const index: RtcString]:RtcString read GetValue write SetValue;

    { Return the number of elements received with the name "index".
      If more than one element was received with the same name (it is an array),
      use the Element property to get and set individual elements. }
    property ElementCount[const index: RtcString]:integer read GetElementCount;
    { Get/Set "loc"-th element (starting from 0) of the attribute with the name "index". }
    property Element[const index: RtcString; loc:integer]:RtcString read GetElement write SetElement;

    { Faster case-sensitive version of "Value[]" which
      should ONLY be used with "index" in all UPPERCASE!
      ValueCS property presumes that "index" is all UPPERCASE
      and will NOT work correctly if "index" is NOT UPPERCASE.
      This method is used internally by other RTC SDK components for faster access to values. }
    property ValueCS[const index: RtcString]:RtcString read GetValueCS write SetValueCS;

    { Items delimiter, default = '&' @html(<br>)
      On the server-side, delimiter will in most cases be recognized automaticaly,
      so you just have to assign complete Content Body to the Text property
      and you can access all variables by name using Value[]. }
    property Delimiter:RtcString read GetDelimiter write SetDelimiter;

    { Use this property if you need to set the exact Text as
      unmodified RtcString or read exact Text as one RtcString. }
    property Text:RtcString read GetAsText write SetText;
    end;

  { @abstract(HTTP Header: Basic Request/Response wrapper) }
  TRtcHttpHeader = class(TObject)
  private
    FValues:tRtcFastStrObjList;

    FCookie:TRtcHttpValues;

  protected
    { @exclude }
    function GetHeaderCount: integer;
    { @exclude }
    function GetIHeader(index: integer): RtcString;
    { @exclude }
    procedure SetIHeader(index: integer; const Value: RtcString);
    { @exclude }
    function GetHeader(const index: RtcString): RtcString;
    { @exclude }
    procedure SetHeader(const index: RtcString; const Value: RtcString);
    { @exclude }
    function GetIHeaderName(index: integer): RtcString;
    { @exclude }
    procedure SetIHeaderName(index: integer; const Value: RtcString);

    { @exclude }
    function GetContentLength: int64;
    { @exclude }
    function GetChunkedTransferEncoding: boolean;
    { @exclude }
    function GetContentType: RtcString;
    { @exclude }
    procedure SetContentLength(const pValue: int64);
    { @exclude }
    procedure SetChunkedTransferEncoding(const pValue: boolean);
    { @exclude }
    procedure SetContentType(const pValue: RtcString);

    { @exclude }
    function GetCookie: TRtcHttpValues;
    { @exclude }
    function GetHeaderCS(const index: RtcString): RtcString;
    { @exclude }
    procedure SetHeaderCS(const index, Value: RtcString);

    { @exclude }
    function GetHeaderText: RtcString; virtual;
    { @exclude }
    procedure SetHeaderText(const pValue: RtcString); virtual;
    { @exclude }
    function GetCookieName:RtcString; virtual; abstract;
    { @exclude }
    function isCookieName(const Value:RtcString):boolean; virtual; abstract;
    { @exclude }
    function isCookieNameCS(const Value:RtcString):boolean; virtual; abstract;

  public
    { @exclude }
    constructor Create; virtual;
    { @exclude }
    destructor Destroy; override;

    { Clear this request object (prepare for new request information) }
    procedure Clear; virtual;

    { Cookie: same as TRtcRequest.Value['COKIE'] or TRtcResponse.Value['SET-COOKIE'].
      You can use this property to access Cookie values by name. }
    property Cookie:TRtcHttpValues read GetCookie;

    // Number of Items in the HTTP Response Header
    property ItemCount:integer read GetHeaderCount default 0;
    // Name of the 'index'-th item (attribute) for the HTTP Response Header (starting from 0)
    property ItemName[index:integer]:RtcString read GetIHeaderName write SetIHeaderName;
    // Value of the 'index'-th item (attribute) for the HTTP Response Header (starting from 0)
    property ItemValue[index:integer]:RtcString read GetIHeader write SetIHeader;

    // Value of the attribute with name 'index' from the HTTP Response Header.
    property Value[const index: RtcString]:RtcString read GetHeader write SetHeader; default;
    // same as "Value[]"
    property asString[const index: RtcString]:RtcString read GetHeader write SetHeader;

    { Faster case-sensitive version of the "Value" property
      which requires that the "index" parameter is all UPPERCASE.
      Do NOT use ValueCS if you are not 100% certain that "index" is in all UPPERCASE.
      This method is used internally by other RTC SDK components for faster access to values. }
    property ValueCS[const index: RtcString]:RtcString read GetHeaderCS write SetHeaderCS;

    { Content-Type value. If not set, will be left blank.
      @html(<br>)
      This property maps to Value['CONTENT-TYPE'] }
    property ContentType:RtcString read GetContentType write SetContentType;
    { Content Length to be sent out after Header.
      If you are generating one small response, which doesn't need to
      be split in multiple packages, content length will be automatically
      set after your event handler finishes execution. Otherwise,
      you need to set this to the exact number of characters (bytes) you
      want to send out for this request, before your first call to Write().
      @html(<br>)
      This property maps to Value['CONTENT-LENGTH'] }
    property ContentLength:int64 read GetContentLength write SetContentLength;
    // Synonim for ContentLength
    property DataSize:int64 read GetContentLength write SetContentLength;

    // Using HTTP "Transfer-Encoding: chunked" ?
    property ChunkedTransferEncoding:boolean read GetChunkedTransferEncoding write SetChunkedTransferEncoding;

    { You can use this property to set multiple header values using HTTP formatting,
      or get a preformated HTTP header, with all values set for this response. }
    property HeaderText:RtcString read GetHeaderText write SetHeaderText;
    end;

  // @exclude
  TRtcRequest=class;

  { Container for accessing the FileName element-wise }
  TRtcRequestFilePath=class
  private
    FRequest:TRtcRequest;
    FValues:array of RtcString;
    FUpdating:integer;

    procedure UpdateFilePath;

    function GetValue(index: integer): RtcString;
    procedure SetValue(index: integer; const pValue: RtcString);

    function GetCount: integer;
    procedure SetCount(const pValue: integer);

  public
    constructor Create(Parent:TRtcRequest); // created by TRtcRequest
    destructor Destroy; override; // destroyed by TRtcRequest

    { Case-insensitive compare of element at position "index" with the string "pValue" }
    function Equal(index:integer; const pValue:RtcString):boolean;

    { Returns the Number of elements in FilePath (read).
      Can also be used to set the element count to a specific number (write). }
    property Count:integer read GetCount write SetCount;

    { Returns the element at "index" (staring at 0) - without any trailing or pending "/".
      Also used to set the element at "index" (starting at 0) to a specific value.
      Never include "/" when setting values. If "/" is included, an exception will be raised. }
    property Value[index:integer]:RtcString read GetValue write SetValue; default;
    end;

  { @abstract(Basic Http Request wrapper) }
  TRtcRequest=class(TRtcHttpHeader)
  private
    FInfo:TRtcInfo;
    FQuery:TRtcHttpValues;
    FParams:TRtcHttpValues;
    FFilePath:TRtcRequestFilePath;

    FMethod:RtcString;
    FFileName:RtcString;
    FFullName:RtcString;
    FClose:boolean;

    procedure SetURI(const pValue: RtcString);
    function GetMethod: RtcString;
    procedure SetMethod(const pValue: RtcString);
    function GetParams: TRtcHttpValues;
    function GetQuery: TRtcHttpValues;
    function GetInfo: TRtcInfo;
    procedure SetFileName(const pValue: RtcString);
    function GetForwardedFor: RtcString;
    procedure SetForwardedFor(const pValue: RtcString);

    function GetFilePath: TRtcRequestFilePath;

  protected

    procedure UpdateFileName;

    { @exclude }
    // function GetHeaderText: RtcString; override;

    { @exclude }
    procedure SetHeaderText(const pValue: RtcString); override;

    { @exclude }
    function GetCookieName:RtcString; override;
    { @exclude }
    function isCookieName(const Value:RtcString):boolean; override;
    { @exclude }
    function isCookieNameCS(const Value:RtcString):boolean; override;

    { @exclude }
    function GetURI: RtcString;
    { @exclude }
    function GetURL: RtcString;
    { @exclude }
    function GetRequestAgent: RtcString;
    { @exclude }
    function GetRequestHost: RtcString;
    { @exclude }
    function GetRequestReferer: RtcString;
    { @exclude }
    procedure SetRequestAgent(const pValue: RtcString);
    { @exclude }
    procedure SetRequestHost(const pValue: RtcString);
    { @exclude }
    procedure SetRequestReferer(const pValue: RtcString);

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;
    // @exclude
    procedure Clear; override;

    // Request method (GET, POST, ...)
    property Method:RtcString read GetMethod write SetMethod;
    // Request File Name (as written in the URL, without the part after '?')
    property FileName:RtcString read FFileName write SetFileName;

    // Request File Path is the FileName property split into elements, separated by the "/" element separator.
    // ".Count" returns the number of elements, with the first element starting at [0].
    // setting ".Count" to a specific value will either trim elements or add empty elements.
    // "/" is the element separator and should NOT be used when setting an element,
    // nor will it be returned when asking for element value by using [index] or ".Value[index]".
    property FilePath:TRtcRequestFilePath read GetFilePath;

    { Request Query (the part after '?' in the URL).
      Use this to prepare the query on the client-side before sending
      and to read the query on the server-side (when received). }
    property Query:TRtcHttpValues read GetQuery;

    { Params gives you an easy-to-use way to prepare FORM parameters. @html(<br>)
      This property supports "application/x-www-form-urlencoded" and
      "multipart/form-data" decoding and encoding of Form data. @html(<br>)
      Delimiter (one of Params properties) determines what kind of formatting
      will be used when accessing Values and Text in Params. If Delmiter is
      undefined or contains a single character, simple "URLencoding" is used
      (same as the Query property), and if Delimiter contains 2 or more characters,
      "multipart/form-data" encoding is used. @html(<br><br>)

      This property is *NOT* used by the Client, nor the Server automatically.
      Server ONLY sets the Delimiter property, in case Content-Type is
      "multipart/form-data", so you don't have to extract the delimiter manualy
      (simply call "Params.AddText(Read)" on every "OnDataReceived" event). @html(<br><br>)

      This property has a built-in cache limit, which will be used to create a temporary file
      on disk and store data to that temporary file when cache size is exceeded, which makes
      it capable of accepting large file uploads from a Web Browser using the
      "multipart/form-data" format with input type "file". When a file was uploaded to
      your server and you used Params.AddText(Read) to store all data received,
      you can use the Params.Value[myFormVariable] property to access the name of the
      file uploaded from the Browser and GetFile(myFormVariable,MyLocalFileName) to write the
      file into a specific file on your local drive. @html(<br><br>)

      Client can also use this property to prepare the parameters and then use the "Text"
      property to call Write(Params.Text), so it doesn't have to manualy prepare form data.
      But, this property is *NOT* suited for the Client to upload files to a Web Server.
      Only server-side code for accepting uploaded files from a Web Browser is now supported. }
    property Params:TRtcHttpValues read GetParams;

    // Will the connection be closed after result has been sent out?
    property Close:boolean read FClose write FClose default false;

    // 'Host' value from the HTTP Header (mirrored for easier access)
    property Host:RtcString read GetRequestHost write SetRequestHost;
    // 'User-agent' value from HTTP Header (mirrored for easier access)
    property Agent:RtcString read GetRequestAgent write SetRequestAgent;
    // 'Referer' value from HTTP Header (mirrored for easier access)
    property Referer:RtcString read GetRequestReferer write SetRequestReferer;
    // 'X-FORWARDED-FOR' value from HTTP Header (mirrored for easier access)
    property ForwardedFor:RtcString read GetForwardedFor write SetForwardedFor;

    { URI = fully qualified URI.
      in stand-alone EXE, this is same as FileName + Query.Text,
      in a ISAPI DLL extension, this is a fully qualified URI, including DLL name and path }
    property URI:RtcString read GetURI write SetURI;
    // URL = Host + URI
    property URL:RtcString read GetURL;

    { User-defined additional request info. }
    property Info:TRtcInfo read GetInfo;
    end;

  { @abstract(Basic Http Response wrapper) }
  TRtcResponse=class(TRtcHttpHeader)
  private
    FStatusCode:integer;
    FStatusText:RtcString;

  protected
    { @exclude }
    // function GetHeaderText: RtcString; override;
    { @exclude }
    procedure SetHeaderText(const pValue: RtcString); override;

    { @exclude }
    function GetCookieName:RtcString; override;
    { @exclude }
    function isCookieName(const Value:RtcString):boolean; override;
    { @exclude }
    function isCookieNameCS(const Value:RtcString):boolean; override;

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;

    // Status code to be sent back (default = 200)
    property StatusCode:integer read FStatusCode write FStatusCode;
    // Status text to be sent back (default = OK)
    property StatusText:RtcString read FStatusText write FStatusText;
    end;

  { @abstract(RTC File Stream implementation) }
  TRtcFileStream = class(TRtcObject)
    private
      f:TRtcFileHdl;
      l:int64;

    public
      destructor Destroy; override;
      procedure Kill; override;

      procedure Open(const fname:RtcWideString);
      procedure Seek(loc:int64);

      function ReadEx(size:int64):RtcByteArray;
      function Read(size:int64):RtcString;

      procedure Close;
    end;

  { @abstract(RTC Byte Array Stream implementation) }
  TRtcByteArrayStream = class(TStream)
  private
    FData: RtcByteArray;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AData: RtcByteArray);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function ReadBytes(Count: Longint): RtcByteArray;
    procedure WriteBytes(const AData: RtcByteArray);
    property GetBytes: RtcByteArray read FData;
  end;

  { TRtcCustomEvent is the event handler type for custom-defined events. }
  TRtcCustomEvent = procedure(Sender:TObject; Obj:TObject) of object;

  { TRtcCustomDataEvent is the event handler type for custom-defined data events. }
  TRtcCustomDataEvent = procedure(Sender:TObject; Obj:TObject; Data:TRtcValue) of object;

(* RTC Linked Object types as well as their implementations could have been placed
   into the "rtcLink.pas" unit, but doing that would require adding "rtcLink" to the
   "uses" clause of older units if we wanted to extend them with "Linked Objects", which
   is why all types required for using RTC Linked Objects are defined in this unit.
   Helper types and functions which are NOT required by someone *using* "RTC Linked Objects"
   (used only for implementing new Linked Objects) are a separate "rtcLink.pas" unit. *)

type
  TRtcObjectLink = class; // forward

  TRtcObjectManager = class; // forward

  { @abstract(TRtcValue with "xName" and "Manager" properties)
    This class is only used locally for passing "Linked Object Call" parameters. }
  TRtcObjectCall=class(TRtcValue)
  private
    FName:RtcWideString;
    FManager:TRtcObjectManager;
  public
    { xManager = Object Manager responsible for this Object Call }
    constructor Create(xManager:TRtcObjectManager);
    destructor Destroy; override;
    { xEventName, xPropName, xMethodName or xClassName - depending on the event called }
    property xName:RtcWideString read FName write FName;
    { Object Manager where this Call is comming from, or going to }
    property Manager:TRtcObjectManager read FManager;
    end;

  TRtcObjectCallEvent = procedure(Sender:TObject; Param:TRtcObjectCall) of object;

  { @abstract(RTC Object Manager definition / abstract class)
    All methods with a "_" prefix can be used to send data to *remote* Linked Objects. }
  TRtcObjectManager = class(TRtcObject)
  private
    FLinks,FObjs:TObjList;
    FNextObjectID:TRtcObjectID;
    FIsServer:boolean;
    FUpdating:integer;
    FInfo:TRtcInfo;
    FRemoteDestroyed:boolean;
    FCreatingObjectID: TRtcObjectID;

    function GetIsUpdating: boolean;

  protected
    function GetNextObjectID:TRtcObjectID; virtual;

    procedure AddObject(const xOID:TRtcObjectID; const xLink:TRtcObjectLink; const xObject:TObject);
    procedure RemoveObject(const xOID:TRtcObjectID; const xObject:TObject);

    // Called automatically from TRtcObjectLink destroctor
    procedure _RemoteDestroy(const xLink:TRtcObjectLink); virtual; abstract;

    // Called from TRtcObjectLink's "Subscribe" method.
    // Subscribe Linked Object "xLink" to receive *local* Broadcasts on channel "xChannel" }
    procedure _Subscribe(const xLink:TRtcObjectLink; const xChannel:RtcWideString); virtual; abstract;

    // Called from TRtcObjectLink's "Unsubscribe" method.
    // Unsubscribe Linked Object "xLink" from receiving *local* Broadcasts on channel "xChannel".
    // All Linked Object subscriptions should be removed (unsubsribed) before the object is destroyed.
    procedure _Unsubscribe(const xLink:TRtcObjectLink; const xChannel:RtcWideString); virtual; abstract;

  public
    constructor Create(xServer:boolean); virtual;
    destructor Destroy; override;

    procedure Kill; override;

    { Free all Objects maintained by this Object Manager and send
      "Destroy" messages to the remote Object Manager to do the same. }
    procedure FreeObjects; virtual;

    { Linked Objects created directly need to use the "_RemoteCreate" method immediately
      after creating a TRtcObjectLink to request the creation of a remote Linked Object.
        xParam.xName = ClassName registered on the remote side, used to create a connected object remotely.
        xParam = Parameters for the remote Constructor.
      WARNING: This method clears "Param" data, but it does NOT destroy the "Param" instance. }
    procedure _RemoteCreate(const xLink:TRtcObjectLink; xParam:TRtcObjectCall); virtual; abstract;

    { Send "xLink[xParam.xName]:=xParam;" to the remote Linked Object.
      When called with "xNow=True", each property change will be sent immediately.
      With default behavior ("xNow=False"), setting the same Property again will overwrite the last assignment.
      WARNING: This method clears all "xParam" data, but it does NOT destroy the "xParam" instance. }
    procedure _SetProp(const xLink:TRtcObjectLink; xParam:TRtcObjectCall; xNow:boolean=False); virtual; abstract;

    { An event handler for "xParam.xName" on the "xLink" object was just set? Call _SetEvent() with "Param.asBoolean=True".
      An event handler for "xParam.xName" on the "xLink" object was just removed? Call _SetEvent() with "Param.asBoolean=False".
      WARNING: This method clears all "xParam" data, but it does NOT destroy the "xParam" instance. }
    procedure _SetEvent(const xLink:TRtcObjectLink; xParam:TRtcObjectCall); virtual; abstract;

    { Send Event Call "xLink.<xParam.xName>(xParam);" to the remote Linked Object.
      NOTE: Only Active events [ OnEventSet(xParam.asBoolean=True) ] have assigned event handlers.
      WARNING: This method clears all "xParam" data, but it does NOT destroy the "xParam" instance. }
    procedure _CallEvent(const xLink:TRtcObjectLink; xParam:TRtcObjectCall); virtual; abstract;

    { Send Method Call "xLink.<xParam.xName>(xParam);" to the remote Linked Object.
      WARNING: This method clears all "xParam" data, but it does NOT destroy the "xParam" instance. }
    procedure _CallMethod(const xLink:TRtcObjectLink; xParam:TRtcObjectCall); virtual; abstract;

    { Broadcast "<xParam.xName>(xParam);" to all *local* Linked Objects subscribed to the channel "xChannel".
      WARNING: This method clears all "xParam" data, but it does NOT destroy the "xParam" instance. }
    procedure _Broadcast(const xChannel:RtcWideString; xParam:TRtcObjectCall); virtual; abstract;

    { BeginUpdate and EndUpdate can be used to postpone sending changes until all updates have been prepared }
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    // Find TRtcObjectLink for TRtcObjectID, returns NIL if not found
    function FindLink(const xOID:TRtcObjectID):TRtcObjectLink; overload;

    // Find TRtcObjectLink for TObject (TRtcObjectLink's Parent instance), returns NIL if not found
    function FindLink(const xObject:TObject):TRtcObjectLink; overload;

    { Find OID for Object "xObject". Returns 0 if called with "xObject=nil",
      raises an Exception if "xObject<>nil" but Object was NOT found. }
    function FindOID(const xObject:TObject):TRtcObjectID;

    { Find Object with OID "xOID". Returns NIL if called with "xOID=0",
      raises an Exception if "xOID<>0" but Object was NOT found. }
    function FindObject(const xOID:TRtcObjectID):TObject;

    { Call this method if this object received a "Destroy" request remotely.
      This is used to avoid preparing and/or sending data to the remote side. }
    procedure RemoteDestroyed; virtual;

    // Has this Object Manager been destroyed on the remote side?
    property isRemoteDestroyed:boolean read FRemoteDestroyed;

    { "isServer" will be TRUE if the Object Manager was created on the Server
      where all Objects should be NON-Visual, FALSE if it was created on the Client. }
    property isServer:boolean read FIsServer;

    { isUpdating returns TRUE if we are inside a BeginUpdate/EndUpdate block. }
    property isUpdating:boolean read GetIsUpdating;

    { Custom data storage, NOT used directly by the RTC Object Manager.
      The "Info" property is accessible "globally", so it can be used for sending
      custom information to RTC Objects (for example, user access rights). }
    property Info:TRtcInfo read FInfo;

    { Object Manager has to SET this property to "xOID" before calling a RTC Linked Object constructor.
      It will be cleared back to "0" by TRtcObjectLink if a Linked Object was created using the ID. }
    property CreatingObjectID:TRtcObjectID read FCreatingObjectID write FCreatingObjectID;
    end;

  {@abstract(RTC Object Link definition / abstract class)
    All methods with a "_" prefix are used for sending data to the *remote* Linked Object, while
    methods with a "Do" prefix are used for executing data received from a *remote* Linked Object. }
  TRtcObjectLink = class
  private
    FOwner:TObject;
    FManager:TRtcObjectManager;
    FSubs:TStringObjList;
    FOID:TRtcObjectID;
    FRemoteDestroyed:boolean;
    FCreator:boolean;

  public
    { xOwner = The Owner of this Link (will be destroyed if remote Object is destroyed)
      xManager = Object Manager where the object is being created.
        [ Example: MyCom:=TRtcObjectLink.Create(<the owner of this link>,GetRtcObjectManager); ] @html(<br><br>)
      NOTE: The Object will be linked to the current Thread's active Object Manager
        and the Object Managers "Get Next Object ID" method will be used to get the Object ID. @html(<br>)
      IMPORTANT: Linked Objects created in code also need to use the "Manager._RemoteCreate" method
        after creating a TRtcObjectLink instance to request the creation of a remote Linked Object. }
    constructor Create(xOwner:TObject; xManager:TRtcObjectManager); virtual;

    { TRtcObjectLink object HAS TO BE destroyed by the Object which has created it.
      By destroying the TRtcObjectLink object, the remote side will also be notified to destroy its copy. }
    destructor Destroy; override;

    { Call this method to signal that this object received a "Destroy" request remotely.
      This is used to avoid sending another "Destroy" request to the remote side.
      Returns TRUE if this is the 1st time RemoteDestroyed was called for this Object. }
    function RemoteDestroyed:boolean; virtual;

    { Has this Linked Object been remotely destroyed? }
    function isRemoteDestroyed:boolean;

    { Called by the Object Manager when the Owner has to be destroyed.
      Default implementation will call "Owner.Free", but you can override
      this method if more than the "Owner" object should be destroyed. }
    procedure DestroyOwner; virtual;

    { Subscribe to receive *local* Broadcasts on channel "xChannel" }
    procedure Subscribe(const xChannel:RtcWideString); virtual;

    { Unsubscribe from receiving *local* Broadcasts on channel "xChannel".
      All subscriptions will be automatically removed (unsubsribed) when the object is destroyed. }
    procedure Unsubscribe(const xChannel:RtcWideString); virtual;

    { Unsubscribe from receiving *local* Broadcasts on ALL Channels (removes ALL subscriptions). }
    procedure UnsubscribeAll;

  { Methods to be used by TRtcObjectManager when data is received for this RTC Object }

    { We have received a request from our *remote* Linked Object counterpart
      to Set our property "Param.xName" to value stored in "Param".
      [ *remote* Linked Object used the "_SetProp" method ] }
    procedure DoPropSet(Sender:TObject; xParam:TRtcObjectCall); virtual; abstract;

    { We have received information from our *remote* Linked Object counterpart
      that the *remote* Event "Param.xName" state is now "Param.asBoolean" (True=Active, False=Inactive).
      [ *remote* Linked Object used the "_SetEvent" method ] }
    procedure DoEventSet(Sender:TObject; xParam:TRtcObjectCall); virtual; abstract;

    { We have received a request from our *remote* Linked Object counterpart
      to Call our Event "Param.xName" using parameters stored in "Param".
      [ *remote* Linked Object used the "_CallEvent" method ] }
    procedure DoEventCall(Sender:TObject; xParam:TRtcObjectCall); virtual; abstract;

    { We have received a request from our *remote* Linked Object counterpart
      to Call our Method "Param.xName" using parameters stored in "Param".
      [ *remote* Linked Object used the "_CallMethod" method ] }
    procedure DoMethodCall(Sender:TObject; xParam:TRtcObjectCall); virtual; abstract;

    { We have received a Broadcast from a local Object Manager to call our
      Broadcast method "Param.xName" using parameters stored in "Param".
      [ Linked Object used the "_Broadcast" method ] }
    procedure DoBroadcast(Sender:TObject; xParam:TRtcObjectCall); virtual; abstract;

    { OID = Object (Owner) ID }
    property OID:TRtcObjectID read FOID;
    { Manager = TRtcObjectManager responsible for this TRtcObjectLink }
    property Manager:TRtcObjectManager read FManager;
    { Owner = Object for which this TRtcObjectLink was created }
    property Owner:TObject read FOwner;
    { Returns TRUE if the Object was created directly and NOT because of a remote call }
    property isCreator:boolean read FCreator;
    end;

  { RTC Object Link support options }
  TRtcObjectLinkSupport = ( // Object Manager never created (disabled)
                            ol_None,
                            // Object Manager has to be activated manually
                            ol_Manual,
                            // Object Manager auto-activated for creation of objects on the Server
                            ol_AutoServer,
                            // Object Manager auto-activated for creation of objects on the Client
                            ol_AutoClient,
                            // Object Manager auto-activated for creation of objects on the Client or Server
                            ol_AutoBoth);

  { Received request from "Sender" to create object for class "Param.xName" for
    the Object Manager "Param.Manager" using parameters stored in "Param" }
  TRtcObjectConstructor = procedure(Sender:TObject; Param:TRtcObjectCall);

// Does file with name "fname" exist?
function File_Exists(const fname:RtcWideString):boolean;
// Size of the file with name "fname"
function File_Size(const fname:RtcWideString):int64;

// Read "Size" bytes of file "fname", starting at "Loc" (0=begining), using "AccessMode" (default = rtc_ShareDenyNone)
function Read_File(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
// Read complete file "fname", using "AccessMode" (default = rtc_ShareDenyNone)
function Read_File(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;

// Read "Size" bytes of file "fname", starting at "Loc" (0=begining), using "AccessMode" (default = rtc_ShareDenyNone)
function Read_FileEx(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcByteArray; overload;
// Read complete file "fname", using "AccessMode" (default = rtc_ShareDenyNone)
function Read_FileEx(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcByteArray; overload;

{ Scan up to "Size" bytes of file "fname" for RtcByteArray "search_string",
  starting at "Loc" (0=beginning), using "AccessMode" (default = rtc_ShareDenyNone)
  and up to "BufferSize" memory for scanning the file.
  Larger buffer size will increase scanning speed, but use more memory.
  Recommended are "BufferSize" values of 16000 or more bytes. @html(<br><br>)
  If "search_string" is found, its location in file is returned (0=beginning).
  If "search_string" is not found, this function returns -1. }
function Scan_File(const fname:RtcWideString; const search_string:RtcByteArray; BufferSize:integer; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):int64;

// Delete file "fname"
function Delete_File(const fname:RtcWideString):boolean;
// Rename file "old_name" to "new_name"
function Rename_File(const old_name,new_name:RtcWideString):boolean;

// Write "Data" to file "fname" starting at "Loc" (0=begining, -1 = end), using "AccessMode" (default = rtc_ShareExclusive)
function Write_File(const fname:RtcWideString; const Data:RtcString; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
// Write "Data" to file "fname", overwriting old file, using "AccessMode" (default = rtc_ShareExclusive)
function Write_File(const fname:RtcWideString; const Data:RtcString; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;

// Write "Data" to file "fname" starting at "Loc" (0=begining, -1 = end), using "AccessMode" (default = rtc_ShareExclusive)
function Write_FileEx(const fname:RtcWideString; const Data:RtcByteArray; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
// Write "Data" to file "fname", overwriting old file, using "AccessMode" (default = rtc_ShareExclusive)
function Write_FileEx(const fname:RtcWideString; const Data:RtcByteArray; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;

// File date and time
function File_Age(const fname:RtcWideString):TDateTime;
// Get Temporary Directory path
function GetTempDirectory:RtcWideString;
// Get Temporary File Name inside a Temporary Directory
function GetTempFile:RtcWideString;

// Append "Plus" byte array to the "Dest" byte array
procedure AddBytes(var Dest:RtcByteArray; const Plus:RtcByteArray); overload;
// Append "len" items from the "Plus" byte array to the "Dest" byte array, starting from "loc" index (0-based)
procedure AddBytes(var Dest:RtcByteArray; const Plus:RtcByteArray; loc:Integer; len:Integer=-1); overload;

// Delete "len" items from the beginning of the "Dest" byte array
procedure DelBytes(var Dest:RTcByteArray; Len:Integer);

// Returns TRUE if "Source" can safely be used with RtcStringTo* functions (no data loss?)
function isRtcStringOK(const Source:RtcString):boolean;

// If "Source" contains Unicode characters and "RTC_STRING_CHECK" = TRUE, raise an Exception
procedure RtcStringCheck(const Source:RtcString);

// Return a "byte" String containing a zero-terminated "Source" byte array
function RtcBytesZeroToString(const Source:RtcByteArray):RtcString; overload;
// Return a zero-terminated Byte array containing the "Source" string
function RtcStringToBytesZero(const Source:RtcString):RtcByteArray; overload;

// Return a "byte" String containing "len" bytes from a zero-terminated "Source" byte array, starting from "loc" index (0-based)
function RtcBytesZeroToString(const Source:RtcByteArray; loc:Integer; len:Integer=-1):RtcString; overload;
// Return a zero-terminated Byte array containing "len" characters from the "Source" string, starting from "loc" index (1-based)
function RtcStringToBytesZero(const Source:RtcString; loc:Integer; len:Integer=-1):RtcByteArray; overload;

// Return a String containing the "Source" byte array
function RtcBytesToString(const Source:RtcByteArray):RtcString; overload;
// Return a byte array containing the "Source" string
function RtcStringToBytes(const Source:RtcString):RtcByteArray; overload;

// Return a String containing "len" bytes from the "Source" byte array, starting from "loc" index (0-based)
function RtcBytesToString(const Source:RtcByteArray; loc:Integer; len:Integer=-1):RtcString; overload;
// Return a byte array containing "len" characters from the "Source" string, starting from "loc" index (1-based)
function RtcStringToBytes(const Source:RtcString; loc:Integer; len:Integer=-1):RtcByteArray; overload;

// Return a "byte" String from a zero-terminated "Source"
function RtcPBytesZeroToString(var Source):RtcString;

// Return "Len" characters from "Source" byte array
function RtcPBytesToString(var Source; Len:Cardinal):RtcString; overload;
// Copy "Len" characters from "Source" to "Dest" byte array
procedure RtcStringToPBytes(Source:RtcString; var Dest; Len:Cardinal); overload;
// Copy "Len" characters starting from "Loc" index in "Source", to "Dest" byte array
procedure RtcStringToPBytes(Source:RtcString; var Dest; Loc, Len:Cardinal); overload;
// Copy "Len" characters starting from "Loc" index in "Source", to "Dest" byte array and add #0 at the end
procedure RtcStringToPBytesZero(Source:RtcString; var Dest; Loc, Len:Cardinal); overload;

// Return a WideString containing "len" characters from the "Source" byte array starting from "loc" index
function RtcBytesToWideString(const Source:RtcByteArray; loc:Integer=0; len:Integer=-1):RtcWideString;
// Return a byte array containing "len" bytes from the "Source" WideString starting from "loc" index
function RtcWideStringToBytes(const Source:RtcWideString; loc:Integer=1; len:Integer=-1):RtcByteArray;

// Encode String to be used as Http URL.
{ If Safe=TRUE, the following characters will NOT be encoded:
  Ampersand ("&")
  Semi-colon (";")
  Equals ("=")
  Question mark ("?") }
function URL_Encode(const AStr:RtcString; Safe:boolean=False):RtcString;
// Decode Http URL String to be used in Delphi code
{ If Strict=TRUE, any errors in AStr (malformed string) will raise an exception.
  If Strict=FALSE, errors in AStr will be ignored, so that string part
  which can be properly decoded will be decoded, the rest will remain untouched. }
function URL_Decode(const AStr:RtcString; Strict:boolean=False):RtcString;

// Encode a binary String to Mime (Base-64).
// When called with "toJSON=True", line breaks are NOT inserted
function Mime_Encode(const s: RtcString; toJSON:boolean=False): RtcString;
// decode a Mime (Base-64) String to a binary String
function Mime_Decode(const s: RtcString): RtcString;

// Encode a binary String to Mime (Base-64).
// When called with "toJSON=True", line breaks are NOT inserted
function Mime_EncodeEx(const s: RtcByteArray; toJSON:boolean=False): RtcByteArray;
// decode a Mime (Base-64) String to a binary String
function Mime_DecodeEx(const s: RtcByteArray): RtcByteArray;

// Convert RtcWideString to a normal UTF-8 encoded String
function Utf8Encode(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): RtcString;
// Convert a normal UTF-8 encoded String to a RtcWideString
function Utf8Decode(const Source: RtcString; SourceOffset:Integer=1; SourceLength:Integer=-1):RtcWideString;

// Convert RtcWideString to an UTF-8 encoded byte array
function Utf8EncodeEx(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): RtcByteArray;
// Convert an UTF-8 encoded byte array to a RtcWideString
function Utf8DecodeEx(const Source: RtcByteArray; SourceOffset:Integer=0; SourceLength:Integer=-1):RtcWideString;

// Convert a Currency value to a "RTC Currency String" (used internaly by RTC)
function Curr2Str(v:Currency):RtcString;
// Convert a "RTC Currency String" to a Currency value (used internaly by RTC)
function Str2Curr(const s:RtcString):Currency;

// Convert a Floating-point value to a "RTC Floating-point String" (used internaly by RTC)
function Str2Float(const s:RtcString):rtcFloat;
// Convert a "RTC Floating-point String" to a Floating-point Value (used internaly by RTC)
function Float2Str(v:rtcFloat):RtcString;

// Convert a DateTime value to a compact Date-Time format (used internaly by RTC)
function DateTime2Str3(v:TDateTime):RtcString;
// Convert a DateTime value to a "RTC DateTime String" (used internaly by RTC)
function DateTime2Str2(v:TDateTime):RtcString;
// Convert a DateTime value to a "RTC DateTime String" (used internaly by RTC)
function DateTime2Str(v:TDateTime):RtcString;
// Convert a "RTC DateTime String" to a DateTime value (used internaly by RTC)
function Str2DateTime(s:RtcString):TDateTime;

// Convert a DateTime value to a ISO Date Time String (used internaly by RTC)
function DateTime2ISOStr(v:TDateTime;withMS:boolean=False):RtcString;
// Convert a ISO Date Time String to a DateTime value (used internaly by RTC)
function ISOStr2DateTime(s:RtcString):TDateTime;

{ Check if Type is a simple value type (not a FunctionInfo, Record, DataSet or Array) }
function isSimpleValueType(typ:TRtcValueTypes):boolean;

{ Check if object contains a simple value type (not a FunctionInfo, Record, DataSet or Array) }
function isSimpleValue(obj:TRtcValueObject):boolean;

{ Check if 's' appears to be an XML String. }
function isXMLString(const s:RtcString):boolean; overload;
{ Check if 's' appears to be an XML String. }
function isXMLString(const s:RtcByteArray):boolean; overload;

{ Combine two RTC types and return the best type to hold values from type1 and type2.
  Combining anything  with a NIL type will return the not-NIL  type.
  Combining anything with a String type will always return a String type. }
function rtcCombineTypes(const type1,type2:TRtcValueTypes):TRtcValueTypes;

// Return Type Name for rtc type
function rtcTypeName(const type1:TRtcValueTypes):RtcString;

// 1-based "at" and Result
function PosEx(const c,s:RtcString):integer; overload;
function PosEx(const c,s:RtcString; at:integer):integer; overload;

function PosEx(const c:RtcString; const s:RtcByteArray):integer; overload;
function PosEx(const c:RtcString; const s:RtcByteArray; at:integer):integer; overload;

{$IFNDEF FPC_WIDESTRING}
function PosEx(const c:RtcChar; const s:RtcString):integer; overload;
function PosEx(const c:RtcChar; const s:RtcString; at:integer):integer; overload;
{$ENDIF}

// 0-based "at" and Result
function PosEx(const c,s:RtcByteArray):integer; overload;
function PosEx(const c,s:RtcByteArray; at:integer):integer; overload;

function LWord2Str(i:longword):RtcString;
function Int2Str(i:integer):RtcString; overload;
function Int2Str(i:int64):RtcString; overload;

function Str2Int(const s:RtcString):integer; overload;
function Str2IntDef(const s:RtcString; def:integer):integer; overload;
function Str2LWord(const s:RtcString):longword; overload;
function Str2LWordDef(const s:RtcString; def:longword):longword; overload;
function Str2Int64(const s:RtcString):Int64; overload;
function Str2Int64Def(const s:RtcString; def:int64):int64; overload;

{ Cross-platform version of "GetTickCount" (in miliseconds).
  WARNING: Because "Cardinal" is a 32-bit integer type and this function returns
  the number of milliseconds (1/1000th of a second) since the last System start, it
  should NOT be used for Time measuremenet on Systems which run longer than 49 days
  without a System restart, because GetTickTime will overflow once every 49 days. }
function GetTickTime:Cardinal;

{ Return ID of the current Thread (GetCurrentThreadID) }
function GetMyThreadID:RtcThrID;

{ Set Current Thread's RTC Object Manager (call with "NIL" to clear).
  If there was a RTC Object Manager already set for the current thread,
  the old RTC Object Manager will be returned. If not, returns NIL. }
function SetRtcObjectManager(const xManager:TRtcObjectManager):TRtcObjectManager;

{ Get Current Thread's RTC Object Manager.
  Raises an exception if no RTC Object Manager was set for this thread. }
function GetRtcObjectManager:TRtcObjectManager;

{ Checks the Current Thread's RTC Object Manager.
  Returns NIL if no RTC Object Manager was set for this thread. }
function CheckRtcObjectManager:TRtcObjectManager;

{ Client has to register constructors for all RTC Object types which can be created remotely.
  This HAS TO BE done from the "initialization" section of the unit implementing the RTC Object class. }
procedure RegisterRtcObjectConstructor(const xClassName:RtcWideString; xProc:TRtcObjectConstructor);

{ All registered RTC Object Constructors also have to be unregitered.
  This HAS TO BE done from the "finalization" section of the unit implementing the RTC Object class. }
procedure UnregisterRtcObjectConstructor(const xClassName:RtcWideString);

{ This function is used to find the appropriate RTC Object Constructor. }
function FindRtcObjectConstructor(const xClassName:RtcWideString):TRtcObjectConstructor;

implementation

{ RTC Object Links }

{$IFDEF IDE_XE3up}
uses SyncObjs;
{$ENDIF}

var
  fObjManagers:TObjList;
  fObjManCS:TRtcCritSec;
  fObjConstructors:TStringPtrList;

function SetRtcObjectManager(const xManager:TRtcObjectManager):TRtcObjectManager;
  var
    tid:RtcThrID;
    obj:TObject;
  begin
  tid:=GetMyThreadId;
  fObjManCS.Enter;
  try
    obj:=fObjManagers.search(tid);
    if assigned(obj) then
      Result:=TRtcObjectManager(obj)
    else
      Result:=nil;
    if obj<>xManager then
      begin
      if assigned(obj) then
        fObjManagers.remove(tid);
      if assigned(xManager) then
        fObjManagers.insert(tid,xManager);
      end;
  finally
    fObjManCS.Leave;
    end;
  end;

function GetRtcObjectManager:TRtcObjectManager;
  var
    tid:RtcThrID;
    obj:TObject;
  begin
  tid:=GetMyThreadId;
  fObjManCS.Enter;
  try
    obj:=fObjManagers.search(tid);
    if assigned(obj) then
      Result:=TRtcObjectManager(obj)
    else
      begin
      Result:=nil;
      raise ERtcObjectLinks.Create('No active RTC Object Manager.');
      end;
  finally
    fObjManCS.Leave;
    end;
  end;

function CheckRtcObjectManager:TRtcObjectManager;
  var
    tid:RtcThrID;
    obj:TObject;
  begin
  tid:=GetMyThreadId;
  fObjManCS.Enter;
  try
    obj:=fObjManagers.search(tid);
    if assigned(obj) then
      Result:=TRtcObjectManager(obj)
    else
      Result:=nil;
  finally
    fObjManCS.Leave;
    end;
  end;

procedure RegisterRtcObjectConstructor(const xClassName:RtcWideString; xProc:TRtcObjectConstructor);
  begin
  fObjConstructors.insert(xClassName,@xProc);
  end;

procedure UnregisterRtcObjectConstructor(const xClassName:RtcWideString);
  begin
  fObjConstructors.remove(xClassName);
  end;

function FindRtcObjectConstructor(const xClassName:RtcWideString):TRtcObjectConstructor;
  var
    p:pointer;
  begin
  p:=fObjConstructors.search(xClassName);
  if assigned(p) then
    Result:=TRtcObjectConstructor(p)
  else
    Result:=nil;
  end;

{ TRtcObjectCall }

constructor TRtcObjectCall.Create(xManager:TRtcObjectManager);
  begin
  inherited Create;
  if xManager=nil then
    ERtcObjectLinks.Create('TRtcObjectCall.Create called with xManager=nil');
  FManager:=xManager;
  FName:='';
  end;

destructor TRtcObjectCall.Destroy;
  begin
  FName:='';
  FManager:=nil;
  inherited;
  end;

{ TRtcObjectLink }

constructor TRtcObjectLink.Create(xOwner:TObject;xManager:TRtcObjectManager);
  begin
  inherited Create;
  if xOwner=nil then
    raise ERtcObjectLinks.Create('TRtcObjectLink.Create called with xOwner=nil');
  if xManager=nil then
    raise ERtcObjectLinks.Create('TRtcObjectLink.Create called with xManager=nil');
  FSubs:=nil;
  FOwner:=xOwner;
  FManager:=xManager;
  if FManager.CreatingObjectID<>0 then // remote constructor
    begin
    FOID:=FManager.CreatingObjectID;
    FManager.CreatingObjectID:=0;
    FCreator:=False;
    end
  else
    begin
    FOID:=FManager.GetNextObjectID;
    FCreator:=True;
    end;
  FRemoteDestroyed:=False;
  FManager.AddObject(FOID,self,FOwner);
  end;

destructor TRtcObjectLink.Destroy;
  begin
  UnSubscribeAll;
  if assigned(FManager) then
    begin
    FManager.RemoveObject(FOID,FOwner);
    if RemoteDestroyed then
      FManager._RemoteDestroy(self);
    end;
  FOwner:=nil;
  FManager:=nil;
  inherited;
  end;

function TRtcObjectLink.RemoteDestroyed:boolean;
  var
    c,c2:TComponent;
    clink:TRtcObjectLink;
    i:integer;
  begin
  Result:=False;
  if not FRemoteDestroyed then
    begin
    FRemoteDestroyed:=True;
    if not FManager.isRemoteDestroyed then
      begin
      Result:=True;
      if (FOwner is TComponent) then
        begin
        { All our children have already been destroyed remotely,
          so there is no need to send "Destroy" requests for children. }
        c:=TComponent(FOwner);
        if c.ComponentCount>0 then
          begin
          for i:=0 to c.ComponentCount-1 do
            begin
            c2:=c.Components[i];
            if assigned(c2) then
              begin
              clink:=FManager.FindLink(c2);
              if assigned(clink) then
                clink.RemoteDestroyed;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

function TRtcObjectLink.isRemoteDestroyed: boolean;
  begin
  if FRemoteDestroyed then
    Result:=True
  else if FManager.isRemoteDestroyed then
    Result:=True
  else
    Result:=False;
  end;

procedure TRtcObjectLink.DestroyOwner;
  begin
  FOwner.Free;
  // Our instance will be destroyed by the Owner
  end;

procedure TRtcObjectLink.Subscribe(const xChannel:RtcWideString);
  begin
  if not assigned(FManager) then
    raise ERtcObjectLinks.Create('Manager undefined, can NOT subscribe to channel "'+xChannel+'".');
  if not assigned(FSubs) then
    FSubs:=tStringObjList.Create(8);
  if FSubs.search(xChannel)=nil then
    begin
    FManager._Subscribe(self,xChannel);
    FSubs.insert(xChannel,self);
    end;
  end;

procedure TRtcObjectLink.Unsubscribe(const xChannel:RtcWideString);
  begin
  if assigned(FSubs) then
    if FSubs.search(xChannel)<>nil then
      begin
      FSubs.remove(xChannel);
      if assigned(FManager) then
        FManager._Unsubscribe(self,xChannel);
      end;
  end;

procedure TRtcObjectLink.UnSubscribeAll;
  var
    xChannel:RtcWideString;
    xObj:TObject;
  begin
  if assigned(FSubs) then
    begin
    while not FSubs.Empty do
      begin
      xChannel:=FSubs.search_min(xObj);
      Unsubscribe(xChannel);
      end;
    RtcFreeAndNil(FSubs);
    end;
  end;

{ TRtcObjectManager }

constructor TRtcObjectManager.Create(xServer:boolean);
  begin
  inherited Create;
  FIsServer:=xServer;
  FNextObjectID:=0;
  FUpdating:=0;
  FLinks:=tObjList.Create(128);
  FObjs:=tObjList.Create(128);
  FInfo:=tRtcInfo.Create;
  end;

destructor TRtcObjectManager.Destroy;
  begin
  FreeObjects;
  RtcFreeAndNil(FLinks);
  RtcFreeAndNil(FObjs);
  RtcFreeAndNil(FInfo);
  inherited;
  end;

procedure TRtcObjectManager.FreeObjects;
  var
    o:TObject;
    i:RtcIntPtr;
  begin
  // Destroy all child objects
  if assigned(FLinks) then
    while not FLinks.Empty do
      begin
      i:=FLinks.search_max(o);
      if i>0 then
        begin
        if assigned(o) and (o is TRtcObjectLink) then
          TRtcObjectLink(o).DestroyOwner
        else
          raise ERtcObjectLinks.Create('TRtcObjectManager.FLinks corrupt');
        end
      else
        raise ERtcObjectLinks.Create('TRtcObjectManager.FLinks corrupt');
      end;
  end;

procedure TRtcObjectManager.Kill;
  begin
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

procedure TRtcObjectManager.AddObject(const xOID: TRtcObjectID; const xLink: TRtcObjectLink; const xObject: TObject);
  begin
  if FLinks.search(xOID)<>nil then
    raise ERtcObjectLinks.Create('Object with ID '+IntToStr(xOID)+' already exists.');
  FObjs.insert(RtcIntPtr(xObject), xLink);
  FLinks.insert(xOID, xLink);
  end;

procedure TRtcObjectManager.RemoveObject(const xOID: TRtcObjectID; const xObject: TObject);
  begin
  if (FLinks.search(xOID)<>nil) and
     (FObjs.search(RtcIntPtr(xObject))<>nil) then
    begin
    FLinks.remove(xOID);
    FObjs.remove(RtcIntPtr(xObject));
    if FObjs.Count=0 then // All objects destroyed
      FNextObjectID:=0; // Initialize the Object ID generator to save space
    end;
  end;

function TRtcObjectManager.FindLink(const xOID: TRtcObjectID): TRtcObjectLink;
  var
    o:TObject;
  begin
  o:=FLinks.search(xOID);
  if assigned(o) then
    Result:=TRtcObjectLink(o)
  else
    Result:=nil;
  end;

function TRtcObjectManager.FindLink(const xObject: TObject): TRtcObjectLink;
  var
    o:TObject;
  begin
  o:=FObjs.search(RtcIntPtr(xObject));
  if assigned(o) then
    Result:=TRtcObjectLink(o)
  else
    Result:=nil;
  end;

function TRtcObjectManager.FindObject(const xOID: TRtcObjectID): TObject;
  var
    o:TObject;
  begin
  if xOID=RTC_NIL_OBJECT_ID then
    Result:=RTC_NIL_OBJECT
  else
    begin
    o:=FLinks.search(xOID);
    if not assigned(o) then
      raise ERtcObjectLinks.Create('TRtcObjectManager.FindObject: Object with xOID='+IntToStr(xOID)+' does NOT exist')
    else if not assigned(TRtcObjectLink(o).Owner) then
      raise ERtcObjectLinks.Create('TRtcObjectManager.FindObject: Object with xOID='+IntToStr(xOID)+' is corrupt (no Owner set)')
    else
      Result:=TRtcObjectLink(o).Owner;
    end;
  end;

function TRtcObjectManager.FindOID(const xObject: TObject): TRtcObjectID;
  var
    o:TObject;
  begin
  if xObject=RTC_NIL_OBJECT then
    Result:=RTC_NIL_OBJECT_ID
  else
    begin
    o:=FObjs.search(RtcIntPtr(xObject));
    if not assigned(o) then
      raise ERtcObjectLinks.Create('TRtcObjectManager.FindOID: Object not found')
    else if TRtcObjectLink(o).OID=RTC_NIL_OBJECT_ID then
      raise ERtcObjectLinks.Create('TRtcObjectManager.FindOID: Found Object has invalid ID')
    else
      Result:=TRtcObjectLink(o).OID;
    end;
  end;

function TRtcObjectManager.GetIsUpdating: boolean;
  begin
  Result:=FUpdating>0;
  end;

function TRtcObjectManager.GetNextObjectID: TRtcObjectID;
  begin
  if FIsServer then // Server-side Object Manager uses positive IDs
    Inc(FNextObjectID)
  else // Client-side Object Manager uses negative IDs to avoid collision
    Dec(FNextObjectID);
  Result:=FNextObjectID;
  end;

procedure TRtcObjectManager.BeginUpdate;
  begin
  Inc(FUpdating);
  end;

procedure TRtcObjectManager.EndUpdate;
  begin
  if FUpdating=0 then
    raise ERtcObjectLinks.Create('EndUpdate without BeginUpdate');
  Dec(FUpdating);
  end;

procedure TRtcObjectManager.RemoteDestroyed;
  begin
  FRemoteDestroyed:=True;
  end;

{ RTC Utility functions }

const
  MARK_NAME:RtcChar=':'; // Char after NAME
  MARK_TYPE:RtcChar='='; // Char after TYPE
  MARK_LEN_START:RtcChar='"'; // Char to mark Long String start
  MARK_LEN_END:RtcChar='"'; // Char to mark Long String end
  MARK_MID:RtcChar=','; // Char after mid String
  MARK_END:RtcChar=';'; // Char to mark END-OF-LINE
  MARK_LENGTH=1;

const
  // set of simple value types
  TRtcSimpleValueTypes:set of TRtcValueTypes = [
                               rtc_Null,
                               rtc_Variable,
                               rtc_Exception,
                               rtc_Text,rtc_String,rtc_WideString,
                               rtc_Boolean,
                               rtc_Integer,rtc_Cardinal,rtc_LargeInt,
                               rtc_Float,rtc_Currency,rtc_DateTime,
                               rtc_ByteStream,rtc_OID,
                               rtc_ByteArray ];

function rtcTypeName(const type1:TRtcValueTypes):RtcString;
  begin
  Result:=RTC_TYPE2FULLNAME_CONV[type1];
  end;

{ Combine two RTC types and return the best type to hold values from type1 and type2 }
function rtcCombineTypes(const type1,type2:TRtcValueTypes):TRtcValueTypes;
  begin
  if type1=type2 then
    Result:=type1
  else if type1=rtc_Null then
    Result:=type2
  else if type2=rtc_Null then
    Result:=type1
  else if (type1=rtc_WideString) or (type2=rtc_WideString) then
    Result:=rtc_WideString
  else if (type1=rtc_Text) or (type2=rtc_Text) then
    Result:=rtc_Text
  else if (type1=rtc_String) or (type2=rtc_String) then
    Result:=rtc_String
  else if (type1 in [rtc_Integer..rtc_DateTime,rtc_Variant,rtc_Boolean]) and
          (type2 in [rtc_Integer..rtc_DateTime,rtc_Variant,rtc_Boolean]) then
    begin
    if type1>type2 then
      Result:=type1
    else
      Result:=type2;
    end
  else if (type1=rtc_DataSet) or (type2=rtc_DataSet) then
    Result:=rtc_DataSet
  else if (type1=rtc_Record) or (type2=rtc_Record) then
    Result:=rtc_Record
  else if (type1=rtc_Array) or (type2=rtc_Array) then
    Result:=rtc_Array
  else if (type1=rtc_Variant) or (type2=rtc_Variant) then
    Result:=rtc_Variant
  else
    Result:=type1;
  end;

function TrimCopy(const S: RtcString; I, L:integer): RtcString;
  begin
  L:=L+I-1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
    begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
    end;
  end;

{ Check if 's' appears to be an XML String. }
function isXMLString(const s:RtcString):boolean;
  var
    a:integer;
  begin
  Result:=False;
  if s<>'' then
    begin
    for a:=1 to length(s) do
      if s[a]='<' then
        begin
        if a=length(s) then
          Break
        {$IFDEF RTC_BYTESTRING}
        else if s[a+1] in ['!','?','a'..'z','A'..'Z','-','.','/'] then
        {$ELSE}
        else if Pos(s[a+1],'!?abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-./')>0 then
        {$ENDIF}
          Result:=True;
        end
      {$IFDEF RTC_BYTESTRING}
      else  if not (s[a] in [#9,#10,#13,#32]) then
      {$ELSE}
      else  if Pos(s[a],#9#10#13#32)>0 then
      {$ENDIF}
        Break;
    end;
  end;

{ Check if 's' appears to be an XML String. }
function isXMLString(const s:RtcByteArray):boolean;
  var
    a:integer;
  begin
  Result:=False;
  if length(s)>0 then
    begin
    for a:=0 to length(s)-1 do
      if s[a]=Byte('<') then
        begin
        if a=length(s) then
          Break
        {$IFDEF RTC_BYTESTRING}
        else if RtcChar(s[a+1]) in ['!','?','a'..'z','A'..'Z','-','.','/'] then
        {$ELSE}
        else if Pos(RtcChar(s[a+1]),'!?abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-./')>0 then
        {$ENDIF}
          Result:=True;
        end
      {$IFDEF RTC_BYTESTRING}
      else  if not (RtcChar(s[a]) in [#9,#10,#13,#32]) then
      {$ELSE}
      else  if Pos(RtcChar(s[a]),#9#10#13#32)>0 then
      {$ENDIF}
        Break;
    end;
  end;

function isSimpleValueType(typ:TRtcValueTypes):boolean;
  begin
  Result:=typ in TRtcSimpleValueTypes;
  end;

function isSimpleValue(obj:TRtcValueObject):boolean;
  begin
  if not assigned(obj) then
    Result:=True
  else
    Result:=obj.GetType in TRtcSimpleValueTypes;
  end;

function GetMyThreadID:RtcThrID;
{$IFDEF Windows}
  begin
  Result:=RtcThrID(GetCurrentThreadID);
  end;
{$ELSE}{$IFDEF FPC}
  begin
  Result:=RtcThrID(GetCurrentThreadID);
  end;
{$ELSE}{$IFDEF POSIX}
  begin
  Result:=RtcThrID(pthread_self);
  end;
{$ELSE}{$IFDEF CLR}
  begin
  Result:=System.Threading.Thread.CurrentThreadId;
  end;
{$ELSE}
  begin
  Result:=0;
  {$MESSAGE WARN 'GetMyThreadID implementation missing.'}
  end;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

function GetTickTime:Cardinal;
{$IFDEF WINDOWS}
  begin
  Result:=GetTickCount;
  end;
{$ELSE}{$IFDEF POSIX}
  {$IFDEF MACOSX}
    begin
    Result:=AbsoluteToNanoseconds(UpTime) div 1000000;
    end;
  {$ELSE}{$IFDEF MACIOS}
    var
      tv: timeval;
    begin
    gettimeofday(tv, nil);
    Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
    end;
  {$ELSE}
    var
      tv: timeval;
    begin
    gettimeofday(tv, nil);
    Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
    end;
  {$ENDIF}{$ENDIF}
{$ELSE}{$IFDEF FPC}
  var
    b: tms;
  begin
  Result := fpTimes(b)*10;
  end;
{$ELSE}
  begin
  Result:=0;
  {$MESSAGE WARN 'GetTickTime implementation missing.'}
  end;
{$ENDIF}{$ENDIF}{$ENDIF}

function GetTempDirectory:RtcWideString;
{$IFDEF WINDOWS}
  var
    tempFolder: array[0..MAX_PATH] of Char;
  begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
  end;
{$ELSE}{$IFDEF POSIX}
  begin
  Result := GetEnvironmentVariable('TMPDIR');
  end;
{$ELSE}{$IFDEF FPC}
  begin
  Result := SysUtils.GetTempDir(true);
  end;
{$ELSE}
  begin
  Result:='';
  {$MESSAGE WARN 'GetTempDirectory implementation missing.'}
  end;
{$ENDIF}{$ENDIF}{$ENDIF}

function GetTempFile:RtcWideString;
{$IFDEF WINDOWS}
  var
    tempFile: array[0..MAX_PATH] of Char;
    tempFolder: array[0..MAX_PATH] of Char;
  begin
  GetTempPath(MAX_PATH, @tempFolder);
  if GetTempFileName(@tempFolder, 'RTC', 0, @tempFile)<>0 then
    result := StrPas(tempFile)
  else
    result := '';
  end;
{$ELSE}{$IFDEF POSIX}
  var
    tempFolder:String;
    a: integer;
    fn:RtcWideString;
  begin
  tempFolder:=GetTempDirectory;
  if tempFolder[length(tempFolder)]<>'/' then
    tempFolder:=tempFolder+'/';
  tempFolder:=tempFolder+'tmp_';
  repeat
    fn:='';
    for a := 1 to 16 do
      fn:=fn+RtcWideChar(Ord('a')+random(26));
    until not File_Exists(tempFolder+fn+'.rtc');
  end;
{$ELSE}{$IFDEF FPC}
  var
    tempFolder: String;
  begin
  tempFolder:=GetTempDirectory;
  Result:=GetTempFileName(tempFolder, 'RTC');
  end;
{$ELSE}
  begin
  {$MESSAGE WARN 'GetTempFile implementation missing.'}
  end;
{$ENDIF}{$ENDIF}{$ENDIF}

function URL_Encode(const AStr: RtcString; Safe:boolean=False): RtcString;
  const
    ToHex:array[$0..$F] of RtcChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  var
    sb: byte;
    a,b:integer;
  begin
  Result:='';
  SetLength(Result, Length(AStr) * 3);
  b:=0;
  for a:=1 to length(AStr) do
    case AStr[a] of
      '0'..'9',
      'A'..'Z',
      'a'..'z',
      ',', '*', '@', '.', '_', '-', '/', '(', ')', '$', ':':
        begin
        Inc(b);
        Result[b]:=AStr[a];
        end;
      '&', ';', '=', '?':
        begin
        Inc(b);
        if Safe then // do not encode
          Result[b]:=AStr[a]
        else
          begin
          sb:=Ord(AStr[a]);
          Result[b]:='%';
          Result[b+1]:=ToHex[sb shr 4];
          Result[b+2]:=ToHex[sb and $F];
          Inc(b,2);
          end;
        end;
      ' ':
        begin
        Inc(b);
        Result[b]:='+';
        end;
      else
        begin
        sb:=Ord(AStr[a]);
        Inc(b);
        Result[b]:='%';
        Result[b+1]:=ToHex[sb shr 4];
        Result[b+2]:=ToHex[sb and $F];
        Inc(b,2);
        end;
      end;
  SetLength(Result, b);
  end;

function URL_Decode(const AStr: RtcString; Strict:boolean=False): RtcString;
  const
    fromHex:array[Ord('0')..Ord('9')] of byte = (0,1,2,3,4,5,6,7,8,9);
    fromHexA:array[Ord('A')..Ord('F')] of byte = ($A,$B,$C,$D,$E,$F);
    fromHexB:array[Ord('a')..Ord('f')] of byte = ($A,$B,$C,$D,$E,$F);
  var
    sb: byte;
    a,b:integer;
    ok: boolean;
  begin
  Result:='';
  SetLength(Result, Length(AStr));
  b:=0; a:=0;
  while a<length(AStr) do
    begin
    Inc(a);
    case AStr[a] of
      '%':
        begin
        ok:=True;

        Inc(a);
        case AStr[a] of
          '0'..'9': sb:=fromHex[Ord(AStr[a])] shl 4;
          'A'..'F': sb:=fromHexA[Ord(AStr[a])] shl 4;
          'a'..'f': sb:=fromHexB[Ord(AStr[a])] shl 4;
          else
            begin
            sb:=0;
            ok:=False;
            if not Strict then
              begin
              Inc(b);
              Result[b]:='%';
              Inc(b);
              Result[b]:=AStr[a];
              end
            else
              raise EConvertError.Create('Error decoding URL: '+String(AStr));
            end;
          end;

        if ok then
          begin
          Inc(a);
          case AStr[a] of
            '0'..'9': Inc(sb,fromHex[Ord(AStr[a])]);
            'A'..'F': Inc(sb,fromHexA[Ord(AStr[a])]);
            'a'..'f': Inc(sb,fromHexB[Ord(AStr[a])]);
            else
              begin
              ok:=False;
              if not Strict then
                begin
                Inc(b);
                Result[b]:='%';
                Inc(b);
                Result[b]:=AStr[a-1];
                Inc(b);
                Result[b]:=AStr[a];
                end
              else
                raise EConvertError.Create('Error decoding URL: '+String(AStr));
              end;
            end;

          if ok then
            begin
            Inc(b);
            Result[b]:=RtcChar(sb);
            end;
          end;
        end;
      '+':
        begin
        Inc(b);
        Result[b]:=' ';
        end;
      else
        begin
        Inc(b);
        Result[b]:=AStr[a];
        end;
      end;
    end;
  SetLength(Result, b);
  end;

{ TRtcByteArrayStream }

constructor TRtcByteArrayStream.Create(const AData: RtcByteArray);
  begin
  inherited Create;
  if assigned(AData) then
    FData:=Copy(AData,0,length(AData))
  else
    SetLength(FData,0);
  FPosition:=0;
  end;

function TRtcByteArrayStream.Read(var Buffer; Count: Integer): Longint;
  begin
  Result := Length(FData) - FPosition;
  if Result > Count then Result := Count;
  Move(FData[FPosition], Buffer, Result);
  Inc(FPosition, Result);
  end;

function TRtcByteArrayStream.ReadBytes(Count: Integer): RtcByteArray;
  var
    Len: Integer;
  begin
  Len := Length(FData) - FPosition;
  if Len > Count then Len := Count;
  Result:=Copy(FData,FPosition,Len);
  Inc(FPosition, Len);
  end;

function TRtcByteArrayStream.Seek(Offset: Integer; Origin: Word): Longint;
  begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FData) - Offset;
  end;
  if FPosition > Length(FData) then
    FPosition := Length(FData)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
  end;

procedure TRtcByteArrayStream.SetSize(NewSize: Integer);
  begin
  SetLength(FData, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
  end;

function TRtcByteArrayStream.Write(const Buffer; Count: Integer): Longint;
  begin
  Result := Count;
  SetLength(FData, (FPosition + Result));
  Move(Buffer, FData[FPosition], Result);
  Inc(FPosition, Result);
  end;

procedure TRtcByteArrayStream.WriteBytes(const AData: RtcByteArray);
  begin
  if assigned(AData) then
    Write(AData[0], Length(AData));
  end;

{ TRtcFileStream }

destructor TRtcFileStream.Destroy;
  begin
  try
    Close;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcFileStream.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcFileStream.Kill;
  begin
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

procedure TRtcFileStream.Open(const fname:RtcWideString);
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  l:=0;
  if f=RTC_INVALID_FILE_HDL then
    raise ERtcInfo.Create('Unable to open file for read access.');
  end;

procedure TRtcFileStream.Close;
  begin
  if f<>RTC_INVALID_FILE_HDL then
    begin
    FileClose(f);
    f:=RTC_INVALID_FILE_HDL;
    end;
  end;

function TRtcFileStream.ReadEx(Size: int64): RtcByteArray;
  var
    sRead:int64;
  begin
  if f=RTC_INVALID_FILE_HDL then
    raise ERtcInfo.Create('File not open.')
  else
    begin
    if Size=0 then
      begin
      SetLength(Result,0);
      Exit;
      end
    else if Size<0 then
      begin
      Size:=FileSeek(f,int64(0),2)-L;
      if (Size<=0) or (FileSeek(f,L,0)<>L) then
        begin
        SetLength(Result,0);
        Exit;
        end;
      end;
    SetLength(Result,Size);
    sRead:=FileRead(f,Result[0],Size);
    Inc(L,sRead);
    if sRead<Size then
      SetLength(Result,sRead);
    end;
  end;

{$IFDEF RTC_BYTESTRING}
function TRtcFileStream.Read(Size: int64): RtcString;
  var
    sRead:int64;
  begin
  if f=RTC_INVALID_FILE_HDL then
    raise ERtcInfo.Create('File not open.')
  else
    begin
    if Size=0 then
      begin
      SetLength(Result,0);
      Exit;
      end
    else if Size<0 then
      begin
      Size:=FileSeek(f,int64(0),2)-L;
      if (Size<=0) or (FileSeek(f,L,0)<>L) then
        begin
        SetLength(Result,0);
        Exit;
        end;
      end;
    SetLength(Result,Size);
    sRead:=FileRead(f,Result[1],Size);
    Inc(L,sRead);
    if sRead<Size then
      SetLength(Result,sRead);
    end;
  end;
{$ELSE}
function TRtcFileStream.Read(Size: int64): RtcString;
  begin
  Result:=RtcBytesToString(ReadEx(Size));
  end;
{$ENDIF}

procedure TRtcFileStream.Seek(Loc: int64);
  begin
  if f=RTC_INVALID_FILE_HDL then
    raise ERtcInfo.Create('File not open.')
  else
    begin
    if Loc<0 then Loc:=0;
    l:=FileSeek(f,Loc,0);
    if l<>LOC then raise ERtcInfo.Create('Error seeking through file.');
    end;
  end;

function File_Exists(const fname:RtcWideString):boolean;
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    Result:=False
  else
    begin
    FileClose(f);
    Result:=True;
    end;
  end;

function File_Size(const fname:RtcWideString):int64;
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    Result:=-1
  else
    begin
    Result:=FileSeek(f,int64(0),2);
    FileClose(f);
    end;
  end;

function File_Age(const fname:RtcWideString):TDateTime;
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    Result:=-1
  else
    begin
    Result:=FileDateToDateTime(FileGetDate(f));
    FileClose(f);
    end;
  end;

function Read_FileEx(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcByteArray; overload;
  var
    f:TRtcFileHdl;
    sRead:int64;
  begin
  SetLength(Result,0);
  case AccessMode of
    rtc_ShareDenyNone:  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                f:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  if f=RTC_INVALID_FILE_HDL then
    Exit
  else
    begin
    try
      if Loc<0 then
        Loc:=0;
      if Size<0 then
        Size:=FileSeek(f,int64(0),2)-Loc;
      if FileSeek(f,Loc,0)<>Loc then
        Exit;
      SetLength(Result,Size);
      sRead:=FileRead(f,Result[0],Size);
      if sRead<Size then
        SetLength(Result,sRead);
    finally
      FileClose(f);
      end;
    end;
  end;

function Read_FileEx(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcByteArray; overload;
  begin
  Result:=Read_FileEx(fname,0,-1,AccessMode);
  end;

{$IFDEF RTC_BYTESTRING}
function Read_File(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
  var
    f:TRtcFileHdl;
    sRead:int64;
  begin
  SetLength(Result,0);
  case AccessMode of
    rtc_ShareDenyNone:  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                f:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  if f=RTC_INVALID_FILE_HDL then
    Exit
  else
    begin
    try
      if Loc<0 then
        Loc:=0;
      if Size<0 then
        Size:=FileSeek(f,int64(0),2)-Loc;
      if FileSeek(f,Loc,0)<>Loc then
        Exit;
      SetLength(Result,Size);
      sRead:=FileRead(f,Result[1],Size);
      if sRead<Size then
        SetLength(Result,sRead);
    finally
      FileClose(f);
      end;
    end;
  end;

function Read_File(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
  begin
  Result:=Read_File(fname,0,-1,AccessMode);
  end;
{$ELSE}
function Read_File(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
  begin
  Result:=RtcBytesToString(Read_FileEx(fname,Loc,Size,AccessMode));
  end;

function Read_File(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
  begin
  Result:=RtcBytesToString(Read_FileEx(fname,0,-1,AccessMode));
  end;
{$ENDIF}

function PosEx(const c,s:RtcByteArray):integer;
  var
    a,b,lc:integer;
  begin
  lc:=length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=0 to length(s)-1 do
        if s[a]=c[0] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=0 to length(s)-2 do
        if (s[a]=c[0]) and (s[a+1]=c[1]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=0 to length(s)-lc do
        if (s[a]=c[0]) and (s[a+1]=c[1]) and (s[a+2]=c[2]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>=0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c,s:RtcByteArray; at:integer):integer;
  var
    a,b,lc:integer;
  begin
  lc:=length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=at to length(s)-1 do
        if s[a]=c[0] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=at to length(s)-2 do
        if (s[a]=c[0]) and (s[a+1]=c[1]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=at to length(s)-lc do
        if (s[a]=c[0]) and (s[a+1]=c[1]) and (s[a+2]=c[2]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>=0 then
            Break;
          end;
      end;
    end;
  end;

function Scan_File(const fname:RtcWideString; const search_string:RtcByteArray; BufferSize:integer; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):int64;
  var
    f:TRtcFileHdl;
    test:RtcByteArray;
    have_size,
    want_size,
    old_size, mypos, len:int64;
  begin
  Result:=-1;
  case AccessMode of
    rtc_ShareDenyNone:  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                f:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  if (f<>RTC_INVALID_FILE_HDL) and (Size>0) and (length(search_string)>0) then
    begin
    try
      if Loc<0 then
        Loc:=0;
      if Size<0 then
        Size:=FileSeek(f,int64(0),2)-Loc;
      if FileSeek(f,Loc,0)<>Loc then
        Exit;
      old_size:=0;
      len:=length(search_string);
      BufferSize:=BufferSize+len;
      test:=nil;
      if BufferSize<Size then
        SetLength(test,BufferSize)
      else
        SetLength(test,Size);
      repeat
        // Do not read more than requested
        if Size>BufferSize-old_size then
          want_size:=BufferSize-old_size // Do not oveflow our memory buffer
        else
          want_size:=Size;

        // Read next block behind last one
        have_size := FileRead(f, test[old_size], want_size);
        if have_size<=0 then // no more data to read!
          Break
        else if length(test)>old_size+have_size then // less data read than memory reserved
          SetLength(test, old_size+have_size);

        mypos:=PosEx(search_string, test);
        if mypos>=0 then
          begin
          // Loc = last reading location
          // mypos = substring location
          // rd_size = bytes left-over from last read
          Result:=Loc+mypos-old_size;
          Break;
          end
        else if (have_size=want_size) and (Size>0) then // expecting more data
          begin
          // Copy last "len" bytes to the beginning of our test RtcByteArray
          Move(test[old_size+have_size-len],test[0],len);
          old_size:=len;

          Dec(Size,have_size);
          Inc(Loc,have_size);
          end
        else // this was last block read
          Break;
        until False;
    finally
      FileClose(f);
      end;
    end
  else if (f<>RTC_INVALID_FILE_HDL) then
    FileClose(f);
  end;

function Delete_File(const fname:RtcWideString):boolean;
  begin
  if File_Exists(fname) then
    Result:=DeleteFile(fname)
  else
    Result:=False;
  end;

function Rename_File(const old_name,new_name:RtcWideString):boolean;
  begin
  Result:=SysUtils.RenameFile(old_name,new_name);
  end;

function Write_FileEx(const fname:RtcWideString; const Data:RtcByteArray; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
  var
    f:TRtcFileHdl;
  begin
  Result:=False;
  case AccessMode of
    rtc_ShareDenyNone: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyWrite);
    else f:=FileOpen(fname,fmOpenReadWrite+fmShareExclusive);
    end;
  if f=RTC_INVALID_FILE_HDL then
    f:=FileCreate(fname);
  if f<>RTC_INVALID_FILE_HDL then
    begin
    // if Loc<0 then Loc:=0;
    try
      if length(Data)>0 then
        begin
        if Loc<0 then
          begin
          FileSeek(f,0,2);
          if FileWrite(f,data[0],length(data))=length(data) then
            Result:=True;
          end
        else
          begin
          if FileSeek(f,Loc,0)=Loc then
            if FileWrite(f,data[0],length(data))=length(data) then
              Result:=True;
          end;
        end;
    finally
      FileClose(f);
      end;
    end;
  end;

// Write "Data" to file "fname", overwriting old file.
function Write_FileEx(const fname:RtcWideString; const Data:RtcByteArray; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
  begin
  DeleteFile(fname);
  Result:=Write_FileEx(fname, data, 0, AccessMode);
  end;

{$IFDEF RTC_BYTESTRING}
function Write_File(const fname:RtcWideString; const Data:RtcString; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
  var
    f:TRtcFileHdl;
  begin
  Result:=False;
  case AccessMode of
    rtc_ShareDenyNone: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyWrite);
    else f:=FileOpen(fname,fmOpenReadWrite+fmShareExclusive);
    end;
  if f=RTC_INVALID_FILE_HDL then
    f:=FileCreate(fname);
  if f<>RTC_INVALID_FILE_HDL then
    begin
    // if Loc<0 then Loc:=0;
    try
      if length(Data)>0 then
        begin
        if Loc<0 then
          begin
          FileSeek(f,0,2);
          if FileWrite(f,data[1],length(data))=length(data) then
            Result:=True;
          end
        else
          begin
          if FileSeek(f,Loc,0)=Loc then
            if FileWrite(f,data[1],length(data))=length(data) then
              Result:=True;
          end;
        end;
    finally
      FileClose(f);
      end;
    end;
  end;

// Write "Data" to file "fname", overwriting old file.
function Write_File(const fname:RtcWideString; const Data:RtcString; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
  begin
  DeleteFile(fname);
  Result:=Write_File(fname, data, 0, AccessMode);
  end;
{$ELSE}
function Write_File(const fname:RtcWideString; const Data:RtcString; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
  begin
  Result:=Write_FileEx(fname, RtcStringToBytes(Data), Loc, AccessMode);
  end;

function Write_File(const fname:RtcWideString; const Data:RtcString; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
  begin
  Result:=Write_FileEx(fname, RtcStringToBytes(Data), AccessMode);
  end;
{$ENDIF}

function Utf8ByteCount(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): Integer;
  var
    c, i: Cardinal;
  begin
  Result := 0;
  if Source = '' then Exit;
  if SourceLength<0 then SourceLength:=length(Source)-SourceOffset+1;
  for i:=SourceOffset to SourceOffset+SourceLength-1 do
    begin
    c := Cardinal(Source[i]);
    if c > $7F then
      begin
      if c > $7FF then
        Inc(Result);
      Inc(Result);
      end;
    Inc(Result);
    end;
  end;

procedure UnicodeToUtf8Bytes(var Dest: RtcString; DestOffset: Cardinal; const Source: RtcWideString; SourceOffset: Integer = 1; SourceLength: Integer = -1);
  var
    i, count: Cardinal;
    c: Cardinal;
  begin
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset+1;
  if SourceLength<=0 then Exit;
  count := DestOffset;

  for i:=SourceOffset to SourceOffset+SourceLength-1 do
    begin
    c := Cardinal(Source[i]);
    if c <= $7F then
      begin
      Dest[count] := RtcChar(c);
      Inc(count);
      end
    else if c > $7FF then
      begin
      Dest[count] := RtcChar($E0 or (c shr 12));
      Dest[count+1] := RtcChar($80 or ((c shr 6) and $3F));
      Dest[count+2] := RtcChar($80 or (c and $3F));
      Inc(count,3);
      end
    else //  $7F < Source[i] <= $7FF
      begin
      Dest[count] := RtcChar($C0 or (c shr 6));
      Dest[count+1] := RtcChar($80 or (c and $3F));
      Inc(count,2);
      end;
    end;
  end;

function UnicodeCharCount(const Source: RtcString; SourceOffset:Integer=1; SourceLength:Integer=-1): Integer;
  var
    SourceBytes, i: Cardinal;
    c: Byte;
  begin
  Result := 0;
  if SourceLength<0 then SourceLength:=length(Source)-SourceOffset+1;
  if SourceLength<=0 then Exit;
  SourceBytes:=SourceLength+SourceOffset-1;
  i := SourceOffset;

{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    while (i <= SourceBytes) do
      begin
      c := RtcUnicodeToAnsiChar( Word(Source[i]) );
      Inc(i);
      if (c and $80) <> 0 then
        begin
        c := c and $3F;
        if (c and $20) <> 0 then
          begin
          if i > SourceBytes then Exit;          // incomplete multibyte Char
          c := RtcUnicodeToAnsiChar( Word(Source[i]) );
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
          end;
        if i > SourceBytes then Exit;        // incomplete multibyte Char
        c := RtcUnicodeToAnsiChar( Word(Source[i]) );
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
        end;
      Inc(Result);
      end;
    end
  else
{$ENDIF}
    begin
    while (i <= SourceBytes) do
      begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
        begin
        c := c and $3F;
        if (c and $20) <> 0 then
          begin
          if i > SourceBytes then Exit;          // incomplete multibyte Char
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
          end;
        if i > SourceBytes then Exit;        // incomplete multibyte Char
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
        end;
      Inc(Result);
      end;
    end;
  end;

function Utf8Decode(const Source: RtcString; SourceOffset:Integer=1; SourceLength:Integer=-1):RtcWideString;
  var
    SourceBytes, MaxDestChars, i, count: Cardinal;
    c: Byte;
    wc: Cardinal;
  begin
  if SourceLength<0 then SourceLength:=length(Source)-SourceOffset+1;
  if SourceLength<=0 then
    begin
    Result:='';
    Exit;
    end;
  MaxDestChars:=UnicodeCharCount(Source,SourceOffset,SourceLength);
  SetLength(Result,MaxDestChars);
  SourceBytes:=SourceLength+SourceOffset-1;
  count := 0;
  i := SourceOffset;

{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    while i <= SourceBytes do
      begin
      wc :=  RtcUnicodeToAnsiChar( Word(Source[i]) );
      Inc(i);
      if (wc and $80) <> 0 then
        begin
        wc := wc and $3F;
        if (wc and $20) <> 0 then
          begin
          if i > SourceBytes then Exit;          // incomplete multibyte Char
          c := RtcUnicodeToAnsiChar( Word(Source[i]) );
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
          wc := (wc shl 6) or (c and $3F);
          end;
        if i > SourceBytes then Exit;        // incomplete multibyte Char
        c :=  RtcUnicodeToAnsiChar( Word(Source[i]) );
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Inc(count);
        Result[count] := RtcWideChar((wc shl 6) or (c and $3F));
        end
      else
        begin
        Inc(count);
        Result[count] := RtcWideChar(wc);
        end;
      end;
    end
  else
{$ENDIF}
    begin
    while i <= SourceBytes do
      begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
        begin
        wc := wc and $3F;
        if (wc and $20) <> 0 then
          begin
          if i > SourceBytes then Exit;          // incomplete multibyte Char
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
          wc := (wc shl 6) or (c and $3F);
          end;
        if i > SourceBytes then Exit;        // incomplete multibyte Char
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Inc(count);
        Result[count] := RtcWideChar((wc shl 6) or (c and $3F));
        end
      else
        begin
        Inc(count);
        Result[count] := RtcWideChar(wc);
        end;
      end;
    end;
  end;

function Utf8Encode(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): RtcString;
  var
    alen:Cardinal;
  begin
  alen:=Utf8ByteCount(Source,SourceOffset,SourceLength);
  SetLength(Result,alen);
  UnicodeToUtf8Bytes(Result,1,Source,SourceOffset,SourceLength);
  end;

procedure UnicodeToUtf8BytesEx(var Dest: RtcByteArray; DestOffset: Cardinal; const Source: RtcWideString; SourceOffset: Integer = 1; SourceLength: Integer = -1);
  var
    i, count: Cardinal;
    c: Cardinal;
  begin
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset+1;
  if SourceLength<=0 then Exit;
  count := DestOffset;
  for i:=SourceOffset to SourceOffset+SourceLength-1 do
    begin
    c := Cardinal(Source[i]);
    if c <= $7F then
      begin
      Dest[count] := c;
      Inc(count);
      end
    else if c > $7FF then
      begin
      Dest[count] := $E0 or (c shr 12);
      Dest[count+1] := $80 or ((c shr 6) and $3F);
      Dest[count+2] := $80 or (c and $3F);
      Inc(count,3);
      end
    else //  $7F < Source[i] <= $7FF
      begin
      Dest[count] := $C0 or (c shr 6);
      Dest[count+1] := $80 or (c and $3F);
      Inc(count,2);
      end;
    end;
  end;

function UnicodeCharCountEx(const Source: RtcByteArray; SourceOffset:Integer=0; SourceLength:Integer=-1): Integer;
  var
    SourceBytes, i: Cardinal;
    c: Byte;
  begin
  Result := 0;
  if SourceLength<0 then SourceLength:=length(Source)-SourceOffset;
  if SourceLength<=0 then Exit;
  SourceBytes:=SourceLength+SourceOffset;
  i := SourceOffset;
  while (i < SourceBytes) do
    begin
    c := Source[i];
    Inc(i);
    if (c and $80) <> 0 then
      begin
      c := c and $3F;
      if (c and $20) <> 0 then
        begin
        if i >= SourceBytes then Exit;          // incomplete multibyte Char
        c := Source[i];
        Inc(i);
        if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
        end;
      if i >= SourceBytes then Exit;        // incomplete multibyte Char
      c := Source[i];
      Inc(i);
      if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
    Inc(Result);
    end;
  end;

function Utf8DecodeEx(const Source: RtcByteArray; SourceOffset:Integer=0; SourceLength:Integer=-1):RtcWideString;
  var
    SourceBytes, MaxDestChars, i, count: Cardinal;
    c: Byte;
    wc: Cardinal;
  begin
  if SourceLength<0 then SourceLength:=length(Source)-SourceOffset;
  if SourceLength<=0 then
    begin
    Result:='';
    Exit;
    end;
  MaxDestChars:=UnicodeCharCountEx(Source,SourceOffset,SourceLength);
  SetLength(Result,MaxDestChars);
  SourceBytes:=SourceLength+SourceOffset;
  count := 0;
  i := SourceOffset;
  while i < SourceBytes do
    begin
    wc := Cardinal(Source[i]);
    Inc(i);
    if (wc and $80) <> 0 then
      begin
      wc := wc and $3F;
      if (wc and $20) <> 0 then
        begin
        if i >= SourceBytes then Exit;          // incomplete multibyte Char
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
        wc := (wc shl 6) or (c and $3F);
        end;
      if i >= SourceBytes then Exit;        // incomplete multibyte Char
      c := Byte(Source[i]);
      Inc(i);
      if (c and $C0) <> $80 then Exit;       // malformed trail byte

      Inc(count);
      Result[count] := RtcWideChar((wc shl 6) or (c and $3F));
      end
    else
      begin
      Inc(count);
      Result[count] := RtcWideChar(wc);
      end;
    end;
  end;

function Utf8EncodeEx(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): RtcByteArray;
  var
    alen:Cardinal;
  begin
  alen:=Utf8ByteCount(Source,SourceOffset,SourceLength);
  SetLength(Result,alen);
  UnicodeToUtf8BytesEx(Result,0,Source,SourceOffset,SourceLength);
  end;

function AnsiToBytes(const Source: RtcWideString):RtcString;
  var
    len, i: Cardinal;
  begin
  len:=length(Source);
  SetLength(Result,len);
  if len = 0 then Exit;

  for i:=1 to len do
    Result[i] := RtcChar(Source[i]);
  end;

function BytesToAnsi(const Source: RtcString):RtcWideString;
  var
    i, len: Cardinal;
  begin
  len:=length(Source);
  SetLength(Result, len);
  if len = 0 then Exit;

  for i:=1 to len do
    Result[i] := RtcWideChar(Source[i]);
  end;

procedure RtcStringCheck(const Source:RtcString);
{$IFDEF RTC_BYTESTRING}
  begin
  // No need to check, there can be no characters above 255 in Source
  end;
{$ELSE}
  var
    len,i: Cardinal;
  begin
  if RTC_STRING_CHECK then
    begin
    len:=length(Source);
    if len = 0 then Exit;
    for i:=1 to len do
      if Ord(Source[i])>255 then
        if RTC_STRING_FIXMODE=rtcStr_NoFIX then
          raise ERtcInfo.Create('RtcStringCheck: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i])
        else if RtcUnicodeToAnsiChar(Ord(Source[i]))=RTC_INVALID_CHAR then // invalid char
          raise ERtcInfo.Create('RtcStringCheck: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
    end;
  end;
{$ENDIF}

function isRtcStringOK(const Source:RtcString):boolean;
{$IFDEF RTC_BYTESTRING}
  begin
  Result:=True;
  // No need to check, there can be no characters above 255 in Source
  end;
{$ELSE}
  var
    len,i: Cardinal;
  begin
  Result:=True;
  len:=length(Source);
  if len = 0 then Exit;
  for i:=1 to len do
    if Ord(Source[i])>255 then
      if RTC_STRING_FIXMODE=rtcStr_NoFIX then
        begin
        Result:=False;
        Break;
        end
      else if RtcUnicodeToAnsiChar(Ord(Source[i]))=RTC_INVALID_CHAR then
        begin
        Result:=False;
        Break;
        end;
  end;
{$ENDIF}

function RtcBytesToString(const Source:RtcByteArray):RtcString;
  var
    len: Cardinal;
{$IFNDEF RTC_BYTESTRING}
    i, k: Cardinal;
{$ENDIF}
  begin
  len:=length(Source);
  SetLength(Result, len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[0],Result[1],len);
{$ELSE}
  k := 0;
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar( RtcAnsiToUnicodeChar(Source[k]) );
      Inc(k);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcBytesToString(const Source:RtcByteArray; loc:Integer; len:Integer=-1):RtcString;
{$IFNDEF RTC_BYTESTRING}
  var
    i, k: Cardinal;
{$ENDIF}
  begin
  if len<0 then len:=length(Source)-loc;
  SetLength(Result, len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[loc],Result[1],len);
{$ELSE}
  k := loc;
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar( RtcAnsiToUnicodeChar(Source[k]) );
      Inc(k);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcPBytesZeroToString(var Source):RtcString;
  var
    len, i: Cardinal;
    Src: PRtcByte;
  begin
  len:=0;
  Src:=PRtcByte(Addr(Source));
  while Src^<>0 do
    begin
    Inc(len);
    Inc(Src);
    end;
  SetLength(Result, len);
  if len = 0 then Exit;
  
  Src:=PRtcByte(Addr(Source));
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar( RtcAnsiToUnicodeChar(Src^) );
      Inc(Src);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar(Src^);
      Inc(Src);
      end;
    end;
  end;

function RtcPBytesToString(var Source; Len:Cardinal):RtcString;
  var
    i: Cardinal;
    Src: PRtcByte;
  begin
  SetLength(Result, Len);
  if len = 0 then Exit;
  Src:=PRtcByte(Addr(Source));
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar( RtcAnsiToUnicodeChar(Src^) );
      Inc(Src);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar(Src^);
      Inc(Src);
      end;
    end;
  end;

procedure RtcStringToPBytes(Source:RtcString; var Dest; Len:Cardinal);
  var
    i: Cardinal;
    Dst: PRtcByte;
  begin
  if len = 0 then Exit;
  Dst:=PRtcByte(Addr(Dest));
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        begin
        Dst^:=RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Dst^=RTC_INVALID_CHAR) then
          raise ERtcInfo.Create('RtcStringToPBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
        end
      else
        Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        raise ERtcInfo.Create('RtcStringToPBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end;
  end;

procedure RtcStringToPBytes(Source:RtcString; var Dest; Loc, Len:Cardinal);
  var
    i: Cardinal;
    Dst: PRtcByte;
  begin
  if len = 0 then Exit;
  Dst:=PRtcByte(Addr(Dest));
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=loc to loc+len-1 do
      begin
      if Ord(Source[i])>255 then
        begin
        Dst^:=RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Dst^=RTC_INVALID_CHAR) then
          raise ERtcInfo.Create('RtcStringToPBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
        end
      else
        Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=loc to loc+len-1 do
      begin
      if Ord(Source[i])>255 then
        raise ERtcInfo.Create('RtcStringToPBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=loc to loc+len-1 do
      begin
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  end;

procedure RtcStringToPBytesZero(Source:RtcString; var Dest; Loc, Len:Cardinal);
  var
    i: Cardinal;
    Dst: PRtcByte;
  begin
  Dst:=PRtcByte(Addr(Dest));
  if len = 0 then
    begin
    Dst^:=0;
    Exit;
    end;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=loc to loc+len-1 do
      begin
      if Ord(Source[i])>255 then
        begin
        Dst^:=RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Dst^=RTC_INVALID_CHAR) then
          raise ERtcInfo.Create('RtcStringToPBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
        end
      else
        Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=loc to loc+len-1 do
      begin
      if Ord(Source[i])>255 then
        begin
        Dst^:=0; // close dest string
        raise ERtcInfo.Create('RtcStringToPBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
        end;
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=loc to loc+len-1 do
      begin
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end;
  Dst^:=0;
  end;

function RtcBytesZeroToString(const Source:RtcByteArray):RtcString;
  var
    len: Integer;
    i, k: Cardinal;
  begin
  len:=length(Source);
  SetLength(Result, len);
  if len = 0 then Exit;
  k := 0;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      if Source[k]=0 then
        begin
        SetLength(Result,i-1);
        Break;
        end
      else
        Result[i] := RtcChar( RtcAnsiToUnicodeChar(Source[k]) );
      Inc(k);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      if Source[k]=0 then
        begin
        SetLength(Result,i-1);
        Break;
        end
      else
        Result[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
  end;

function RtcBytesZeroToString(const Source:RtcByteArray; loc:Integer; len:Integer=-1):RtcString;
  var
    i, k: Cardinal;
  begin
  if len<0 then len:=length(Source)-loc;
  SetLength(Result, len);
  if len = 0 then Exit;
  k := loc;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      if Source[k]=0 then
        begin
        SetLength(Result,i-1);
        Break;
        end
      else
        Result[i] := RtcChar( RtcAnsiToUnicodeChar(Source[k]) );
      Inc(k);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      if Source[k]=0 then
        begin
        SetLength(Result,i-1);
        Break;
        end
      else
        Result[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
  end;

function RtcStringToBytes(const Source:RtcString):RtcByteArray;
  var
    len: Cardinal;
{$IFNDEF RTC_BYTESTRING}
    i, k: Cardinal;
{$ENDIF}
  begin
  len:=length(Source);
  SetLength(Result, len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[1],Result[0],len);
{$ELSE}
  k := 0;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        begin
        Result[k] := RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Result[k]=RTC_INVALID_CHAR) then
          raise ERtcInfo.Create('RtcStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
        end
      else
        Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        raise ERtcInfo.Create('RtcStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
      Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcStringToBytes(const Source:RtcString; loc:Integer; len:Integer=-1):RtcByteArray;
{$IFNDEF RTC_BYTESTRING}
  var
    i, k: Cardinal;
{$ENDIF}
  begin
  if len<0 then len:=length(Source)-loc+1;
  SetLength(Result, len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[loc],Result[0],len);
{$ELSE}
  k := loc;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        begin
        Result[i]:=RtcUnicodeToAnsiChar(Ord(Source[k]));
        if RTC_STRING_CHECK and (Result[i]=RTC_INVALID_CHAR) then
          raise ERtcInfo.Create('RtcStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Source[k]);
        end
      else
        Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        raise ERtcInfo.Create('RtcStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Source[k]);
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=0 to len-1 do
      begin
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcStringToBytesZero(const Source:RtcString):RtcByteArray;
  var
    len: Cardinal;
{$IFNDEF RTC_BYTESTRING}
    i, k: Cardinal;
{$ENDIF}
  begin
  len:=length(Source);
  SetLength(Result, len+1);
  Result[len]:=0;
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[1],Result[0],len);
{$ELSE}
  k := 0;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        begin
        Result[k]:=RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Result[k]=RTC_INVALID_CHAR) then
          raise ERtcInfo.Create('RtcStringToBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
        end
      else
        Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        raise ERtcInfo.Create('RtcStringToBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Source[i]);
      Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcStringToBytesZero(const Source:RtcString; loc:Integer; len:Integer=-1):RtcByteArray;
{$IFNDEF RTC_BYTESTRING}
  var
    i, k: Cardinal;
{$ENDIF}
  begin
  if len<0 then len:=length(Source)-loc+1;
  SetLength(Result, len+1);
  Result[len]:=0;
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[loc],Result[0],len);
{$ELSE}
  k := loc;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        begin
        Result[i]:=RtcUnicodeToAnsiChar(Ord(Source[k]));
        if RTC_STRING_CHECK and (Result[i]=RTC_INVALID_CHAR) then
          raise ERtcInfo.Create('RtcStringToBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Source[k]);
        end
      else
        Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        raise ERtcInfo.Create('RtcStringToBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Source[k]);
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=0 to len-1 do
      begin
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcBytesToWideString(const Source:RtcByteArray; loc:Integer=0; len:Integer=-1):RtcWideString;
  var
    i, k: Cardinal;
  begin
  if len<0 then len:=length(Source)-loc;
  SetLength(Result, len);
  if len = 0 then Exit;
  k := loc;
  for i:=1 to len do
    begin
    Result[i] := RtcWideChar(Source[k]);
    Inc(k);
    end;
  end;

function RtcWideStringToBytes(const Source:RtcWideString; loc:Integer=1; len:Integer=-1):RtcByteArray;
  var
    i, k: Cardinal;
  begin
  if len<0 then len:=length(Source)-loc+1;
  SetLength(Result, len);
  if len = 0 then Exit;
  k := loc;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        begin
        Result[i]:=RtcUnicodeToAnsiChar(Ord(Source[k]));
        if RTC_STRING_CHECK and (Result[i]=RTC_INVALID_CHAR) then
          raise ERtcInfo.Create('RtcWideStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Source[k]);
        end
      else
        Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        raise ERtcInfo.Create('RtcWideStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Source[k]);
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=0 to len-1 do
      begin
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end;
  end;

procedure AddBytes(var Dest:RtcByteArray; const Plus:RtcByteArray; loc:Integer; len:Integer=-1);
  var
    olen:Integer;
  begin
  olen:=length(Dest);
  if len<0 then len:=length(Plus)-loc;
  if len<=0 then Exit;
  SetLength(Dest,olen+len);
  Move(Plus[loc],Dest[olen],len);
  end;

procedure AddBytes(var Dest:RtcByteArray; const Plus:RtcByteArray);
  var
    len, olen:Integer;
  begin
  len:=length(Plus);
  if len<=0 then Exit;
  olen:=length(Dest);
  SetLength(Dest,olen+len);
  Move(Plus[0],Dest[olen],len);
  end;

procedure DelBytes(var Dest:RTcByteArray; Len:Integer);
  begin
  if Len>0 then
    if length(Dest)>Len then
      Dest:=Copy(Dest,Len,length(Dest)-Len)
    else
      SetLength(Dest,0);
  end;


//=====================================================
//  Mime functions
//=====================================================

const
  MIME_ENCODED_LINE_BREAK = 76;
  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;

const
  MIME_ENCODE_TABLE: array[0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072,
    073, 074, 075, 076, 077, 078, 079, 080,
    081, 082, 083, 084, 085, 086, 087, 088,
    089, 090, 097, 098, 099, 100, 101, 102,
    103, 104, 105, 106, 107, 108, 109, 110,
    111, 112, 113, 114, 115, 116, 117, 118,
    119, 120, 121, 122, 048, 049, 050, 051,
    052, 053, 054, 055, 056, 057, 043, 047);

  MIME_PAD_CHAR = Byte('=');

  MIME_DECODE_TABLE: array[Byte] of Cardinal = (
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 062, 255, 255, 255, 063,
    052, 053, 054, 055, 056, 057, 058, 059,
    060, 061, 255, 255, 255, 255, 255, 255,
    255, 000, 001, 002, 003, 004, 005, 006,
    007, 008, 009, 010, 011, 012, 013, 014,
    015, 016, 017, 018, 019, 020, 021, 022,
    023, 024, 025, 255, 255, 255, 255, 255,
    255, 026, 027, 028, 029, 030, 031, 032,
    033, 034, 035, 036, 037, 038, 039, 040,
    041, 042, 043, 044, 045, 046, 047, 048,
    049, 050, 051, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);

type
  PByte4 = ^TByte4;
  TByte4 = packed record
    b1, b2, b3, b4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    b1, b2, b3: Byte;
  end;

procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
  var
    b: Cardinal;
    InnerLimit, OuterLimit: RtcIntPtr;
    InPtr: PByte3;
    OutPtr: PByte4;
  begin
  if InputByteCount < MIME_DECODED_LINE_BREAK then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := RtcIntPtr(InPtr);
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := RtcIntPtr(InPtr) + InputByteCount;

  repeat

    repeat
      b := InPtr^.b1;
      b := b shl 8;
      b := b or InPtr^.b2;
      b := b shl 8;
      b := b or InPtr^.b3;
      Inc(InPtr);

      OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b1 := MIME_ENCODE_TABLE[b];
      Inc(OutPtr);
      until RtcIntPtr(InPtr) >= InnerLimit;

    if InnerLimit<OuterLimit then
      begin
      OutPtr^.b1 := 13;
      OutPtr^.b2 := 10;
      OutPtr := PByte4(RtcIntPtr(OutPtr)+2);
      Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
      end
    else
      Break;

    until InnerLimit > OuterLimit;
  end;

procedure MimeEncodeNoCRLF(const InputBuffer;
                           const InputByteCount: Cardinal;
                           out OutputBuffer);
  var
    b: Cardinal;
    InnerLimit, OuterLimit: RtcIntPtr;
    InPtr: PByte3;
    OutPtr: PByte4;
  begin
  if InputByteCount = 0 then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := RtcIntPtr(InPtr) + OuterLimit;

  while RtcIntPtr(InPtr) < InnerLimit do
    begin
    b := InPtr^.b1;
    b := b shl 8;
    b := b or InPtr^.b2;
    b := b shl 8;
    b := b or InPtr^.b3;
    Inc(InPtr);

    OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b1 := MIME_ENCODE_TABLE[b];
    Inc(OutPtr);
    end;

  case InputByteCount - OuterLimit of
    1:begin
      b := InPtr^.b1;
      b := b shl 4;
      OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b1 := MIME_ENCODE_TABLE[b];
      OutPtr.b3 := MIME_PAD_CHAR;
      OutPtr.b4 := MIME_PAD_CHAR;
      end;
    2:begin
      b := InPtr^.b1;
      b := b shl 8;
      b := b or InPtr^.b2;
      b := b shl 2;
      OutPtr.b3 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b1 := MIME_ENCODE_TABLE[b];
      OutPtr.b4 := MIME_PAD_CHAR;
      end;
    end;
  end;

function MimeDecodePartial(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
  var
    lByteBuffer, lByteBufferSpace, c: Cardinal;
    InPtr, OuterLimit: ^Byte;
    OutPtr: PByte3;
  begin
  if InputBytesCount > 0 then
    begin
    InPtr := @InputBuffer;
    OuterLimit := Pointer( RtcIntPtr(InPtr) + InputBytesCount );
    OutPtr := @OutputBuffer;
    lByteBuffer := ByteBuffer;
    lByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
      begin
      c := MIME_DECODE_TABLE[InPtr^];
      Inc(InPtr);
      if c = $FF then Continue;
      lByteBuffer := lByteBuffer shl 6;
      lByteBuffer := lByteBuffer or c;
      Dec(lByteBufferSpace);

      if lByteBufferSpace <> 0 then Continue;

      OutPtr^.b3 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr^.b2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr^.b1 := Byte(lByteBuffer);
      lByteBuffer := 0;
      Inc(OutPtr);
      lByteBufferSpace := 4;
      end;
    ByteBuffer := lByteBuffer;
    ByteBufferSpace := lByteBufferSpace;
    Result := RtcIntPtr(OutPtr) - RtcIntPtr(@OutputBuffer);
    end
  else
    Result := 0;
  end;

function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
  var
    lByteBuffer: Cardinal;
  begin
  case ByteBufferSpace of
    1:begin
      lByteBuffer := ByteBuffer shr 2;
      PByte3(@OutputBuffer)^.b2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
      Result := 2;
      end;
    2:begin
      lByteBuffer := ByteBuffer shr 4;
      PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
      Result := 1;
      end;
    else
      Result := 0;
    end;
  end;

function Mime_EncodeEx(const s: RtcByteArray; toJSON:boolean=False): RtcByteArray;
  var
    l: Cardinal;
    aSize, iDelta, ODelta: Cardinal;
  begin
  if length(s)>0 then
    begin
    l := length(s);
    if toJSON then
      begin
      iDelta:=0;
      ODelta:=0;
      if l > 0 then
        aSize := (l + 2) div 3 * 4
      else
        aSize:=l;
      SetLength(Result, aSize);
      end
    else
      begin
      if l > 0 then
        aSize := (l + 2) div 3 * 4 + (l - 1) div MIME_DECODED_LINE_BREAK * 2
      else
        aSize:=l;
      SetLength(Result, aSize);
      MimeEncodeFullLines(s[0], l, Result[0]);
      iDelta := l div MIME_DECODED_LINE_BREAK;
      ODelta := iDelta * (MIME_ENCODED_LINE_BREAK + 2);
      iDelta := iDelta * MIME_DECODED_LINE_BREAK;
      end;
    MimeEncodeNoCRLF(s[iDelta],
                     l - iDelta,
                     Result[ODelta]);
    end
  else
    SetLength(Result,0);
  end;

function Mime_DecodeEx(const s: RtcByteArray): RtcByteArray;
  var
    ByteBuffer, ByteBufferSpace: Cardinal;
    aSize,l: Cardinal;
  begin
  if Pointer(s) <> nil then
    begin
    l := length(s);
    aSize:= (l + 3) div 4 * 3;
    SetLength(Result, aSize);
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    l := MimeDecodePartial(s[0], l, Result[0], ByteBuffer, ByteBufferSpace);
    l := l +  MimeDecodePartialEnd(Result[l], ByteBuffer, ByteBufferSpace);
    SetLength(Result, l);
    end
  else
    SetLength(Result,0);
  end;

{$IFDEF RTC_BYTESTRING}
function Mime_Encode(const s: RtcString; toJSON:boolean=False): RtcString;
  var
    l: Cardinal;
    aSize, iDelta, ODelta: Cardinal;
  begin
  if length(s)>0 then
    begin
    l := length(s);
    if toJSON then
      begin
      iDelta:=0;
      ODelta:=0;
      if l > 0 then
        aSize := (l + 2) div 3 * 4
      else
        aSize:=l;
      SetLength(Result, aSize);
      end
    else
      begin
      if l > 0 then
        aSize := (l + 2) div 3 * 4 + (l - 1) div MIME_DECODED_LINE_BREAK * 2
      else
        aSize:=l;
      SetLength(Result, aSize);
      MimeEncodeFullLines(s[1], l, Result[1]);
      iDelta := l div MIME_DECODED_LINE_BREAK;
      if toJSON then
        ODelta := iDelta * (MIME_ENCODED_LINE_BREAK + 4)
      else
        ODelta := iDelta * (MIME_ENCODED_LINE_BREAK + 2);
      iDelta := iDelta * MIME_DECODED_LINE_BREAK;
      end;
    MimeEncodeNoCRLF(s[iDelta+1],
                     l - iDelta,
                     Result[ODelta+1]);
    end
  else
    SetLength(Result,0);
  end;

function Mime_Decode(const s: RtcString): RtcString;
  var
    ByteBuffer, ByteBufferSpace: Cardinal;
    aSize,l: Cardinal;
  begin
  if Pointer(s) <> nil then
    begin
    l := length(s);
    aSize:= (l + 3) div 4 * 3;
    SetLength(Result, aSize);
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    l := MimeDecodePartial(s[1], l, Result[1], ByteBuffer, ByteBufferSpace);
    l := l +  MimeDecodePartialEnd(Result[l+1], ByteBuffer, ByteBufferSpace);
    SetLength(Result, l);
    end
  else
    SetLength(Result,0);
  end;
{$ELSE}
function Mime_Encode(const s: RtcString; toJSON:boolean=False): RtcString;
  begin
  Result:=RtcBytesToString(Mime_EncodeEx(RtcStringToBytes(s),toJSON));
  end;

function Mime_Decode(const s: RtcString): RtcString;
  begin
  Result:=RtcBytesToString(Mime_DecodeEx(RtcStringToBytes(s)));
  end;
{$ENDIF}

function StrToTypeEx(const typ:RtcString; Loc,Len:integer):TRtcValueTypes;
  procedure StrTypeError;
    begin
    Result:=rtc_Null;
    raise ERtcInfo.Create('Unknown object type specifier at '+IntToStr(Loc)+': '+Copy(String(typ),Loc,Len));
    end;
  begin
  case Len of
    1:begin
      case typ[Loc] of
        'X': Result:=rtc_Null;
        'E': Result:=rtc_Exception;
        'V': Result:=rtc_Variable;
        'S': Result:=rtc_String;
        'W': Result:=rtc_WideString;
        'T': Result:=rtc_Text;
        'B': Result:=rtc_Boolean;
        'I': Result:=rtc_Integer;
        'K': Result:=rtc_Cardinal;
        'L': Result:=rtc_LargeInt;
        'F': Result:=rtc_Float;
        'C': Result:=rtc_Currency;
        'D': Result:=rtc_DateTime;
        'O': Result:=rtc_OID;
        else StrTypeError;
        end;
      end;
    2:begin
      case typ[Loc] of
        'F': if typ[Loc+1]='C' then Result:=rtc_Function
             else StrTypeError;
        'D': if typ[Loc+1]='S' then Result:=rtc_DataSet
             else StrTypeError;
        'A': if typ[Loc+1]='R' then Result:=rtc_Array
             else StrTypeError;
        'R': if typ[Loc+1]='E' then Result:=rtc_Record
             else StrTypeError;
        'B': if typ[Loc+1]='S' then Result:=rtc_ByteStream
             else if typ[Loc+1]='A' then Result:=rtc_ByteArray
             else StrTypeError;
        else
          StrTypeError;
        end;
      end;
    else
      StrTypeError;
    end;
  end;

function StrToFieldType(const typ:RtcString):TRtcFieldTypes;
  procedure FieldTypeError;
    begin
    Result:=ft_Unknown;
    raise ERtcInfo.Create('Unknown Field Type specifier "'+String(typ)+'".');
    end;
  begin
  case length(typ) of
    1:begin
      case typ[1] of
        'U': Result:=ft_Unknown;
        'S': Result:=ft_String;
        'I': Result:=ft_Integer;
        'B': Result:=ft_Boolean;
        'F': Result:=ft_Float;
        'C': Result:=ft_Currency;
        'T': Result:=ft_Time;
        'D': Result:=ft_DateTime;
        'O': Result:=ft_Blob;
        'M': Result:=ft_Memo;
        'G': Result:=ft_Graphic;
        'W': Result:=ft_WideString;
        'L': Result:=ft_Largeint;
        'V': Result:=ft_Variant;
        else FieldTypeError;
        end;
      end;
    2:begin
      case typ[1] of
        'A':case typ[2] of
              'I': Result:=ft_AutoInc;
              'D': Result:=ft_ADT;
              'R': Result:=ft_Array
              else FieldTypeError;
              end;
        'B':case typ[2] of
              'C': Result:=ft_BCD;
              'Y': Result:=ft_Bytes;
              'B': Result:=ft_Byte;
              else FieldTypeError;
              end;
        'C':case typ[2] of
              'U': Result:=ft_Cursor;
              'O': Result:=ft_Connection;
              else FieldTypeError;
              end;
        'D':case typ[2] of
              'D': Result:=ft_Date;
              'O': Result:=ft_DBaseOle;
              'S': Result:=ft_DataSet;
              'T': Result:=ft_TimeStamp;
              else FieldTypeError;
              end;
        'F':case typ[2] of
              'M': Result:=ft_FmtMemo;
              'C': Result:=ft_FixedChar;
              'B': Result:=ft_FMTBcd;
              'E': Result:=ft_Extended;
              else FieldTypeError;
              end;
        'G':case typ[2] of
              'U': Result:=ft_Guid;
              else FieldTypeError;
              end;
        'S':case typ[2] of
              'I': Result:=ft_Smallint;
              'H': Result:=ft_Shortint;
              'T': Result:=ft_Stream;
              'N': Result:=ft_Single;
              else FieldTypeError;
              end;
        'W':case typ[2] of
              'I': Result:=ft_Word;
              'C': Result:=ft_FixedWideChar;
              'M': Result:=ft_WideMemo;
              'L': Result:=ft_LongWord;
              else FieldTypeError;
              end;
        'V':case typ[2] of
              'B': Result:=ft_VarBytes;
              else FieldTypeError;
              end;
        'P':case typ[2] of
              'O': Result:=ft_ParadoxOle;
              'A': Result:=ft_Params;
              else FieldTypeError;
              end;
        'T':case typ[2] of
              'B': Result:=ft_TypedBinary;
              'O': Result:=ft_TimeStampOffset;
              else FieldTypeError;
              end;
        'R':case typ[2] of
              'F': Result:=ft_Reference;
              else FieldTypeError;
              end;
        'O':case typ[2] of
              'B': Result:=ft_OraBlob;
              'C': Result:=ft_OraClob;
              'T': Result:=ft_OraTimeStamp;
              'I': Result:=ft_OraInterval;
              'J': Result:=ft_Object;
              else FieldTypeError;
              end;
        'I':case typ[2] of
              'T': Result:=ft_Interface;
              'D': Result:=ft_IDispatch;
              else FieldTypeError;
              end;
        else
          FieldTypeError;
        end;
      end;
    else
      FieldTypeError;
    end;
  end;

function nullValueCode:RtcString;
  begin
  Result:=TRtcValueObject.code_toShortString(RTC_TYPE2STR_CONV[rtc_Null],'');
  end;

function nullValueXMLrpc:RtcString;
  begin
  Result:='<value><nil/></value>';
  end;

function nullValueJSON:RtcString;
  begin
  Result:='null';
  end;

{$IFNDEF FPC_WIDESTRING}

function PosEx(const c:RtcChar; const s:RtcString; at:integer):integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=at to length(s) do
    if s[a]=c then
      begin
      Result:=a;
      Break;
      end;
  end;

function PosEx(const c:RtcChar; const s:RtcString):integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=1 to length(s) do
    if s[a]=c then
      begin
      Result:=a;
      Break;
      end;
  end;

{$ENDIF}

function PosEx(const c,s:RtcString):integer;
  var
    a,b,lc:integer;
  begin
  lc:=length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=1 to length(s) do
        if s[a]=c[1] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=1 to length(s)-1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=1 to length(s)-lc+1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) and (s[a+2]=c[3]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c,s:RtcString; at:integer):integer;
  var
    a,b,lc:integer;
  begin
  lc:=length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=at to length(s) do
        if s[a]=c[1] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=at to length(s)-1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=at to length(s)-lc+1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) and (s[a+2]=c[3]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c:RtcString; const s:RtcByteArray):integer;
  var
    a,b,lc:integer;
  begin
  lc:=length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=1 to length(s) do
        if s[a-1]=Byte(c[1]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=1 to length(s)-1 do
        if (s[a-1]=Byte(c[1])) and (s[a]=Byte(c[2])) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=1 to length(s)-lc+1 do
        if (s[a-1]=Byte(c[1])) and (s[a]=Byte(c[2])) and (s[a+1]=Byte(c[3])) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if s[a+b-1]<>Byte(c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c:RtcString; const s:RtcByteArray; at:integer):integer;
  var
    a,b,lc:integer;
  begin
  lc:=length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=at to length(s) do
        if s[a-1]=Byte(c[1]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=at to length(s)-1 do
        if (s[a-1]=Byte(c[1])) and (s[a]=Byte(c[2])) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=at to length(s)-lc+1 do
        if (s[a-1]=Byte(c[1])) and (s[a]=Byte(c[2])) and (s[a+1]=Byte(c[3])) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if s[a+b-1]<>Byte(c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

{$IFDEF IDE_XE2up}
  {$DEFINE RTC_FORMATSET}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE RTC_FORMATSET}
{$ENDIF}

{$IFDEF RTC_FORMATSET}
var
  RtcFormatSettings:TFormatSettings;
{$ENDIF}

function MakeDecimal(const s:RtcString):String;
  var
    p:longint;
  begin
  Result:=String(s);
  p:=Pos('.',Result);
  if p>0 then
  {$IFDEF RTC_FORMATSET}
    Result[p]:=RtcFormatSettings.DecimalSeparator;
  {$ELSE}
    Result[p]:=DecimalSeparator;
  {$ENDIF}
  end;

function Str2Curr(const s:RtcString):Currency;
  begin
{$IFDEF RTC_FORMATSET}
  Result:=StrToCurr(MakeDecimal(s),RtcFormatSettings);
{$ELSE}
  Result:=StrToCurr(MakeDecimal(s));
{$ENDIF}
  end;

function FillZero(const s:RtcString;len:integer):RtcString;
  begin
  Result:=s;
  while length(Result)<len do
    Result:='0'+Result;
  end;

function LWord2Str(i:longword):RtcString;
  begin
  Result:=RtcString(IntToStr(i));
  end;

function Int2Str(i:integer):RtcString;
  begin
  Result:=RtcString(IntToStr(i));
  end;

function Int2Str(i:int64):RtcString;
  begin
  Result:=RtcString(IntToStr(i));
  end;

function Curr2Str(v:Currency):RtcString;
  var
    p:longint;
    temp:String;
  begin
{$IFDEF RTC_FORMATSET}
  temp:=CurrToStr(v,RtcFormatSettings);
  Result:=RtcString(temp);
  if RtcFormatSettings.DecimalSeparator<>'.' then
    begin
    p:=Pos(RtcFormatSettings.DecimalSeparator,Temp);
    if p>0 then Result[p]:='.';
    end;
{$ELSE}
  temp:=CurrToStr(v);
  Result:=RtcString(temp);
  if DecimalSeparator<>'.' then
    begin
    p:=Pos(DecimalSeparator,Temp);
    if p>0 then Result[p]:='.';
    end;
{$ENDIF}
  end;

function Str2Int(const s:RtcString):integer;
  begin
  Result:=StrToInt(String(s));
  end;

function Str2IntDef(const s:RtcString; def:integer):integer;
  begin
  Result:=StrToIntDef(String(s),def);
  end;

function Str2LWord(const s:RtcString):longword;
  begin
  Result:=StrToInt64(String(s));
  end;

function Str2LWordDef(const s:RtcString; def:longword):longword;
  begin
  Result:=StrToInt64Def(String(s),def);
  end;

function Str2Int64(const s:RtcString):Int64;
  begin
  Result:=StrToInt64(String(s));
  end;

function Str2Int64Def(const s:RtcString; def:int64):int64;
  begin
  Result:=StrToInt64Def(String(s),def);
  end;

function Str2Float(const s:RtcString):rtcFloat;
  begin
{$IFDEF RTC_FORMATSET}
  Result:=StrToFloat(MakeDecimal(s),RtcFormatSettings);
{$ELSE}
  Result:=StrToFloat(MakeDecimal(s));
{$ENDIF}
  end;

function Float2Str(v:rtcFloat):RtcString;
  var
    p:longint;
    temp:String;
  begin
{$IFDEF RTC_FORMATSET}
  temp:=FloatToStr(v,RtcFormatSettings);
  Result:=RtcString(temp);
  if RtcFormatSettings.DecimalSeparator<>'.' then
    begin
    p:=Pos(RtcFormatSettings.DecimalSeparator,Temp);
    if p>0 then Result[p]:='.';
    end;
{$ELSE}
  temp:=FloatToStr(v);
  Result:=RtcString(temp);
  if DecimalSeparator<>'.' then
    begin
    p:=Pos(DecimalSeparator,Temp);
    if p>0 then Result[p]:='.';
    end;
{$ENDIF}
  end;

function Str2DateTime(s:RtcString):TDateTime;
  var
    y,m,d,
    hh,mm,ss,ms:word;
    a:integer;
  function GetNum(const sep:RtcString; max:byte):word;
    begin
    if s='' then
      Result:=0
    else if sep='' then
      begin
      if length(s)<=max then
        Result:=Str2Int(s)
      else
        Result:=Str2Int(Copy(s,1,max));
      s:='';
      end
    else
      begin
      a:=Pos(sep,s);
      if a<=0 then a:=length(s)+1;
      if a>max+1 then
        begin
        a:=max;
        try
          Result:=Str2Int(Copy(s,1,a));
        except
          raise EConvertError.Create('Invalid DateTime format.');
          end;
        Delete(s,1,a);
        end
      else
        begin
        try
          Result:=Str2Int(Copy(s,1,a-1));
        except
          raise EConvertError.Create('Invalid DateTime format.');
          end;
        Delete(s,1,a);
        end;
      s:=Trim(s);
      end;
    end;
  begin
  try
    Result:=0;
    if PosEx('-',s)>0 then
      begin
      y:=GetNum('-',4);
      m:=GetNum('-',2);
      if PosEx('T',s)>0 then
        d:=GetNum('T',2)
      else
        d:=GetNum(' ',2);
      if (y>0) or (m>0) or (d>0) then
        begin
        if m=0 then m:=1;
        if d=0 then d:=1;
        Result:=EncodeDate(y,m,d);
        end;
      end
    else if PosEx('/',s)>0 then
      begin
      y:=GetNum('/',4);
      m:=GetNum('/',2);
      if PosEx('T',s)>0 then
        d:=GetNum('T',2)
      else
        d:=GetNum(' ',2);
      if (y>0) or (m>0) or (d>0) then
        begin
        if m=0 then m:=1;
        if d=0 then d:=1;
        Result:=EncodeDate(y,m,d);
        end;
      end
    else if (PosEx('T',s)>0) or (PosEx(':',s)<=0) then
      begin
      y:=GetNum('-',4);
      m:=GetNum('-',2);
      if PosEx('T',s)>0 then
        d:=GetNum('T',2)
      else
        d:=GetNum(' ',2);
      if (y>0) or (m>0) or (d>0) then
        begin
        if m=0 then m:=1;
        if d=0 then d:=1;
        Result:=EncodeDate(y,m,d);
        end;
      end;

    if PosEx(':',s)>0 then
      begin
      hh:=GetNum(':',2);
      mm:=GetNum(':',2);
      ss:=GetNum('.',2);
      if PosEx('+',s)>0 then
        ms:=GetNum('+',3)
      else if PosEx('-',s)>0 then
        ms:=GetNum('-',3)
      else
        ms:=GetNum('',3);
      if Result>=0 then
        Result:=Result+EncodeTime(hh,mm,ss,ms)
      else
        Result:=Result-EncodeTime(hh,mm,ss,ms);
      end;
  except
    on E:Exception do
      raise EConvertError.Create(E.Message+#13#10'Invalid date format.');
    end;
  end;

const
  MSecsPerDay   = 24*60*60*1000;
  UnixDateDelta = 25569;

function JSONStr2DateTime(s:RtcString):TDateTime;
  var
    i,j,sig:int64;
    a:integer;
  function GetNum(const sep:RtcString; max:byte):int64;
    begin
    if s='' then
      Result:=0
    else if sep='' then
      begin
      if length(s)<=max then
        Result:=Str2Int64(s)
      else
        Result:=Str2Int64(Copy(s,1,max));
      s:='';
      end
    else
      begin
      a:=Pos(sep,s);
      if a<=0 then a:=length(s)+1;
      if a>max+1 then
        begin
        a:=max;
        try
          Result:=Str2Int64(Copy(s,1,a));
        except
          raise EConvertError.Create('Invalid DateTime format.');
          end;
        Delete(s,1,a);
        end
      else
        begin
        try
          Result:=Str2Int64(Copy(s,1,a-1));
        except
          raise EConvertError.Create('Invalid DateTime format.');
          end;
        Delete(s,1,a);
        end;
      s:=Trim(s);
      end;
    end;
  begin
  try
    if s[1]='-' then
      begin
      sig:=-1;
      Delete(s,1,1);
      end
    else
      sig:=1;

    if PosEx('-',s)>0 then
      begin
      i:= GetNum('-',20) * sig;
      j:= GetNum(')',4);
      end
    else if PosEx('+',s)>0 then
      begin
      i:= GetNum('+',20) * sig;
      j:= -GetNum(')',4);
      end
    else if PosEx(')',s)>0 then
      begin
      i:= GetNum(')',20) * sig;
      j:= 0;
      end
    else
      begin
      i:= GetNum(' ',20) * sig;
      j:= 0;
      end;
    Result := (i+(j*60*60*10)) / MSecsPerDay + UnixDateDelta;
  except
    on E:Exception do
      raise EConvertError.Create(E.Message+#13#10'Invalid date format.');
    end;
  end;

function DateTime2JsonDate(dt:TDateTime):RtcString;
  begin
  Result := Int2Str( round( (dt - UnixDateDelta) * MSecsPerDay ) );
  end;

function DateTime2Str(v:TDateTime):RtcString;
  var
    y,m,d:word;
    hh,mm,ss,ms:word;
  begin
  if v=0 then
    Result:=''
  else if trunc(v)=0 then // Time only
    begin
    DecodeTime(v, hh,mm,ss,ms);
    Result:=Int2Str(hh)+':'+Int2Str(mm)+':'+Int2Str(ss)+'.'+Int2Str(ms);
    end
  else if frac(v)=0 then // Date only
    begin
    DecodeDate(v, y,m,d);
    if (y>0) or (m>0) or (d>0) then
      Result:=Int2Str(y)+'-'+Int2Str(m)+'-'+Int2Str(d)
    else
      Result:='';
    end
  else // Date and Time
    begin
    DecodeDate(v, y,m,d);
    DecodeTime(v, hh,mm,ss,ms);
    if (y>0) or (m>0) or (d>0) then
      Result:=Int2Str(y)+'-'+Int2Str(m)+'-'+Int2Str(d)+' '+
              Int2Str(hh)+':'+Int2Str(mm)+':'+Int2Str(ss)+'.'+Int2Str(ms)
    else
      Result:=Int2Str(hh)+':'+Int2Str(mm)+':'+Int2Str(ss)+'.'+Int2Str(ms);
    end;
  end;

function DateTime2Str2(v:TDateTime):RtcString;
  var
    y,m,d:word;
    hh,mm,ss,ms:word;
  begin
  if v=0 then
    Result:=''
  else if trunc(v)=0 then // Time only
    begin
    DecodeTime(v, hh,mm,ss,ms);
    Result:=FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2)+'.'+FillZero(Int2Str(ms),3);
    end
  else if frac(v)=0 then // Date only
    begin
    DecodeDate(v, y,m,d);
    if (y>0) or (m>0) or (d>0) then
      Result:=FillZero(Int2Str(y),4)+'-'+FillZero(Int2Str(m),2)+'-'+FillZero(Int2Str(d),2)
    else
      Result:='';
    end
  else // Date and Time
    begin
    DecodeDate(v, y,m,d);
    DecodeTime(v, hh,mm,ss,ms);
    if (y>0) or (m>0) or (d>0) then
      Result:=FillZero(Int2Str(y),4)+'-'+FillZero(Int2Str(m),2)+'-'+FillZero(Int2Str(d),2)+' '+
              FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2)+'.'+FillZero(Int2Str(ms),3)
    else
      Result:=FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2)+'.'+FillZero(Int2Str(ms),3);
    end;
  end;

function DateTime2Str3(v:TDateTime):RtcString;
  var
    y,m,d:word;
    hh,mm,ss,ms:word;
  begin
  if v=0 then
    Result:=''
  else if trunc(v)=0 then // Time only
    begin
    DecodeTime(v, hh,mm,ss,ms);
    Result:=FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2);
    if (ms>0) then
      Result:=Result+'.'+FillZero(Int2Str(ms),3);
    end
  else if frac(v)=0 then // Date only
    begin
    DecodeDate(v, y,m,d);
    if (y>0) or (m>0) or (d>0) then
      Result:=FillZero(Int2Str(y),4)+'-'+FillZero(Int2Str(m),2)+'-'+FillZero(Int2Str(d),2)
    else
      Result:='';
    end
  else // Date and Time
    begin
    DecodeDate(v, y,m,d);
    DecodeTime(v, hh,mm,ss,ms);
    if (y>0) or (m>0) or (d>0) then
      Result:=FillZero(Int2Str(y),4)+'-'+FillZero(Int2Str(m),2)+'-'+FillZero(Int2Str(d),2)+'T'+
              FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2)
    else
      Result:=FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2);
    if (ms>0) then
      Result:=Result+'.'+FillZero(Int2Str(ms),3);
    end;
  end;

function DateTime2ISOStr(v:TDateTime; withMS:boolean=False):RtcString;
  var
    y,m,d:word;
    hh,mm,ss,ms:word;
  begin
  if v=0 then
    Result:='00000000T00:00:00'
  else if trunc(v)=0 then // Time only
    begin
    DecodeTime(v, hh,mm,ss,ms);
    Result:='00000000T'+
            FillZero(Int2Str(hh),2)+':'+
            FillZero(Int2Str(mm),2)+':'+
            FillZero(Int2Str(ss),2);
    if withMS and (ms>0) then
      Result:=Result+'.'+FillZero(Int2Str(ms),3);
    end
  else if frac(v)=0 then // Date only
    begin
    DecodeDate(v, y,m,d);
    Result:=FillZero(Int2Str(y),4)+
            FillZero(Int2Str(m),2)+
            FillZero(Int2Str(d),2)+
            'T00:00:00';
    end
  else // Date and Time
    begin
    DecodeDate(v, y,m,d);
    DecodeTime(v, hh,mm,ss,ms);
    Result:=FillZero(Int2Str(y),4)+
            FillZero(Int2Str(m),2)+
            FillZero(Int2Str(d),2)+'T'+
            FillZero(Int2Str(hh),2)+':'+
            FillZero(Int2Str(mm),2)+':'+
            FillZero(Int2Str(ss),2);
    if withMS and (ms>0) then
      Result:=Result+'.'+FillZero(Int2Str(ms),3);
    end;
  end;

function ISOStr2DateTime(s:RtcString):TDateTime;
  var
    y,m,d,
    hh,mm,ss,ms:word;
    s2:RtcString;
    a:integer;
  function GetNum(const sep:RtcString):word;
    begin
    if s='' then
      Result:=0
    else if sep='' then
      begin
      Result:=Str2Int(s);
      s:='';
      end
    else
      begin
      a:=Pos(sep,s);
      if a<=0 then a:=length(s)+1;
      try
        Result:=Str2Int(Copy(s,1,a-1));
      except
        raise EConvertError.Create('Invalid DateTime format.');
        end;
      Delete(s,1,a);
      s:=Trim(s);
      end;
    end;
  begin
  try
    Result:=0;
    a:=PosEx('T',s);
    if (a>0) or (PosEx(':',s)<=0) then // date included or time not included
      begin
      if PosEx('-',s)>0 then
        begin
        y:=GetNum('-');
        m:=GetNum('-');
        d:=GetNum('T');
        end
      else
        begin
        if a>0 then
          begin
          s2:=Copy(s,1,a-1);
          Delete(s,1,a);
          end
        else
          begin
          s2:=s;
          s:='';
          end;
        if length(s2)>=4 then
          begin
          y:=Str2Int(Copy(s2,1,4));
          Delete(s2,1,4);
          end
        else
          y:=0;
        if length(s2)>=2 then
          begin
          m:=Str2Int(Copy(s2,1,2));
          Delete(s2,1,2);
          end
        else
          m:=0;
        if length(s2)>=2 then
          begin
          d:=Str2Int(Copy(s2,1,2));
          Delete(s2,1,2);
          end
        else
          d:=0;
        end;

      if (y>0) or (m>0) or (d>0) then
        Result:=EncodeDate(y,m,d);

      if length(s2)>0 then
        raise EConvertError.Create('Date Part too long.');
      end;

    if length(s)>0 then // time included
      begin
      hh:=GetNum(':');
      mm:=GetNum(':');
      ss:=GetNum('.');
      ms:=GetNum('+');
      if (hh>0) or (mm>0) or (ss>0) or (ms>0) then
        if Result>=0 then
          Result:=Result+EncodeTime(hh,mm,ss,ms)
        else
          Result:=Result-EncodeTime(hh,mm,ss,ms);
      end;
  except
    on E:Exception do
      raise EConvertError.Create(E.Message+#13#10'Invalid DateTime format.');
    end;
  end;

{ TRtcValueObject }

procedure TRtcValueObject.Kill;
  begin
  {$IFNDEF NEXTGEN}Free;{$ENDIF}
  end;

class function TRtcValueObject.code_fromLongString(const typ,s:RtcString; var at:integer):RtcString;
  var
    loc1,loc,len,ls,lt:integer;
    val:RtcString;
  begin
  ls:=length(s);
  lt:=length(typ);

  if at<0 then
    raise ERtcInfo.Create('Starting position < 0 !?')
  else if at>=ls then
    raise ERtcInfo.Create('No more data. Starting position beyond end-of-String.')
  else if at+lt+1>=ls then
    raise ERtcInfo.Create('Not enough data to include type info.')
  else if s[at+lt+1]<>MARK_TYPE then
    raise ERtcInfo.Create('End Mark not found.');

  for loc:=1 to lt do
    if s[at+loc]<>typ[loc] then
      raise ERtcInfo.Create('Expected object type specifier not found.');

  loc1:=at+lt+1; // position of MARK_TYPE

  if s[loc1+1]=MARK_END then
    begin
    Result:='';
    at:=loc1+1;
    end
  else
    begin
    loc:=PosEx(MARK_LEN_START, s, loc1+1); // position of MARK_LEN
    if loc<=0 then
      raise ERtcInfo.Create('String START-MARK missing.');

    try
      val:=Copy(s, loc1+1, loc-loc1-1);
      if val='' then
        len:=0
      else
        len:=Str2Int(val);
    except
      on E:Exception do
        raise ERtcInfo.Create('Length value missing.');
      end;

    if ls<loc+len+2 then
      raise ERtcInfo.Create('Not enough data.')
    else if s[loc+len+1]<>MARK_LEN_END then
      raise ERtcInfo.Create('String END-MARK missing.')
    else if s[loc+len+2]<>MARK_END then
      raise ERtcInfo.Create('COMMAND END-MARK missing.');

    Result:=Copy(s,loc+1,len); // get String after LENGTH-MARK
    at:=loc+len+2; // update pointer
    end;
  end;

class procedure TRtcValueObject.code_fromByteStream(const typ, s: RtcString; var at: integer; const bs: TStream);
  var
    loc1,loc,len,ls,lt:integer;
    val:RtcString;
  {$IFNDEF RTC_BYTESTRING}
    data:RtcByteArray;
  {$ENDIF}
  begin
  ls:=length(s);
  lt:=length(typ);

  if at<0 then
    raise ERtcInfo.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise ERtcInfo.Create('No more data. Starting position beyond end-of-String.')
  else if at+lt+1>=ls then
    raise ERtcInfo.Create('Not enough data to include type info.')
  else if s[at+lt+1]<>MARK_TYPE then
    raise ERtcInfo.Create('End Mark not found.');

  for loc:=1 to lt do
    if s[at+loc]<>typ[loc] then
      raise ERtcInfo.Create('Expected object type specifier not found.');

  loc1:=at+lt+1; // position of MARK_TYPE

  if s[loc1+1]=MARK_END then
    begin
    bs.Size:=0;
    at:=loc1+1;
    end
  else
    begin
    loc:=PosEx(MARK_LEN_START, s, loc1+1); // position of MARK_LEN
    if loc<=0 then
      raise ERtcInfo.Create('String START-MARK missing.');

    try
      val:=Copy(s, loc1+1, loc-loc1-1);
      if val='' then
        len:=0
      else
        len:=Str2Int(val);
    except
      on E:Exception do
        raise ERtcInfo.Create('Length value missing.');
      end;

    if ls<loc+len+2 then
      raise ERtcInfo.Create('Not enough data.')
    else if s[loc+len+1]<>MARK_LEN_END then
      raise ERtcInfo.Create('String END-MARK missing.')
    else if s[loc+len+2]<>MARK_END then
      raise ERtcInfo.Create('COMMAND END-MARK missing.');

  {$IFDEF RTC_BYTESTRING}
    bs.Size:=0;
    bs.Write(s[loc+1],len);
  {$ELSE}
    data:=RtcStringToBytes(s,loc+1,len);
    bs.Size:=0;
    bs.Write(data[0],len);
    SetLength(data,0);
  {$ENDIF}
    bs.Position:=0; // set to starting position

    at:=loc+len+2; // update pointer
    end;
  end;

class function TRtcValueObject.code_fromByteArray(const typ, s: RtcString; var at: integer):RtcByteArray;
  var
    loc1,loc,len,ls,lt:integer;
    val:RtcString;
  begin
  ls:=length(s);
  lt:=length(typ);

  if at<0 then
    raise ERtcInfo.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise ERtcInfo.Create('No more data. Starting position beyond end-of-String.')
  else if at+lt+1>=ls then
    raise ERtcInfo.Create('Not enough data to include type info.')
  else if s[at+lt+1]<>MARK_TYPE then
    raise ERtcInfo.Create('End Mark not found.');

  for loc:=1 to lt do
    if s[at+loc]<>typ[loc] then
      raise ERtcInfo.Create('Expected object type specifier not found.');

  loc1:=at+lt+1; // position of MARK_TYPE

  if s[loc1+1]=MARK_END then
    begin
    SetLength(Result,0);
    at:=loc1+1;
    end
  else
    begin
    loc:=PosEx(MARK_LEN_START, s, loc1+1); // position of MARK_LEN
    if loc<=0 then
      raise ERtcInfo.Create('String START-MARK missing.');

    try
      val:=Copy(s, loc1+1, loc-loc1-1);
      if val='' then
        len:=0
      else
        len:=Str2Int(val);
    except
      on E:Exception do
        raise ERtcInfo.Create('Length value missing.');
      end;

    if ls<loc+len+2 then
      raise ERtcInfo.Create('Not enough data.')
    else if s[loc+len+1]<>MARK_LEN_END then
      raise ERtcInfo.Create('String END-MARK missing.')
    else if s[loc+len+2]<>MARK_END then
      raise ERtcInfo.Create('COMMAND END-MARK missing.');

    Result:=RtcStringToBytes(s,loc+1,len);

    at:=loc+len+2; // update pointer
    end;
  end;

class function TRtcValueObject.code_fromShortNameString(const typ, s: RtcString; var at: integer):RtcWideString;
  begin
  Result:=Utf8Decode(code_fromShortString(typ,s,at));
  end;

class function TRtcValueObject.code_fromShortString(const typ,s:RtcString; var at:integer):RtcString;
  var
    loc,len,ls,lt:integer;
  begin
  ls:=length(s);
  lt:=length(typ);

  if at<0 then
    raise ERtcInfo.Create('Starting position < 0 !?')
  else if at>=ls then
    raise ERtcInfo.Create('No more data. Starting position beyond end-of-String.')
  else if at+lt+1>ls then
    raise ERtcInfo.Create('Not enough data to include type info.')
  else if s[at+lt+1]<>MARK_TYPE then
    raise ERtcInfo.Create('End Mark not found.');

  for loc:=1 to lt do
    if s[at+loc]<>typ[loc] then
      raise ERtcInfo.Create('Expected object type specifier not found.');

  loc:=at+lt+1; // position of MARK_TYP

  len:=PosEx(MARK_END, s, loc+1); // position of MARK_END
  if len<=0 then
    raise ERtcInfo.Create('END-MARK missing.');

  Result:=Copy(s,loc+1,len-loc-1); // get String between MARK_TYP and MARK_END
  at:=len; // update pointner
  end;

class function TRtcValueObject.code_fromNameString(const s: RtcString; var at:integer):RtcWideString;
  var
    len:integer;
  begin
  len:=PosEx(MARK_NAME, s, at+1); // position of MARK_END
  if len<=0 then
    raise ERtcInfo.Create('END-MARK missing.');

  Result:=Utf8Decode(s, at+1, len-at-1); // get String up to MARK_NAME
  at:=len; // update pointer
  end;

class function TRtcValueObject.code_fromMidString(const s: RtcString; var at:integer):RtcString;
  var
    len:integer;
  begin
  len:=PosEx(MARK_MID, s, at+1); // position of MARK_MID
  if len<=0 then
    raise ERtcInfo.Create('MID-MARK missing.');

  Result:=Copy(s, at+1, len-at-1); // get String up to MARK_MID
  at:=len; // update pointer
  end;

class function TRtcValueObject.code_fromEndString(const s: RtcString; var at:integer):RtcString;
  var
    len:integer;
  begin
  len:=PosEx(MARK_END, s, at+1); // position of MARK_END
  if len<=0 then
    raise ERtcInfo.Create('END-MARK missing.');

  Result:=Copy(s, at+1, len-at-1); // get String up to MARK_END
  at:=len; // update pointer
  end;

class function TRtcValueObject.code_checkStrType(const s: RtcString; const at:integer): TRtcValueTypes;
  var
    loc:integer;
  begin
  loc:=PosEx(MARK_TYPE, s, at+1);
  if loc<=0 then
    raise ERtcInfo.Create('Object type specifier not found.');

  Result:=StrToTypeEx(s,at+1,loc-at-1);
  end;

class function TRtcValueObject.code_toByteStream(const typ: RtcString; bs: TStream): RtcString;
  var
    loc:integer;
  {$IFDEF RTC_BYTESTRING}
    data:RtcString;
  {$ELSE}
    data:RtcByteArray;
  {$ENDIF}
  begin
  if assigned(bs) and (bs.Size>0) then
    begin
    // Data
    SetLength(data,bs.Size);
    loc:=bs.Position;
    try
      bs.Position:=0;
    {$IFDEF RTC_BYTESTRING}
      bs.Read(data[1],bs.Size);
    {$ELSE}
      bs.Read(data[0],bs.Size);
    {$ENDIF}
    finally
      bs.Position:=loc;
      end;

    // Value Header
  {$IFDEF RTC_BYTESTRING}
    Result:=typ +MARK_TYPE+ Int2Str(bs.Size) +MARK_LEN_START+data+MARK_LEN_END+MARK_END;
  {$ELSE}
    Result:=typ +MARK_TYPE+ Int2Str(bs.Size) +MARK_LEN_START+ RtcBytesToString(data) +MARK_LEN_END+MARK_END;
  {$ENDIF}
    end
  else
    Result:=typ +MARK_TYPE+MARK_END;
  end;

class function TRtcValueObject.code_toByteArray(const typ: RtcString; const ba: RtcByteArray): RtcString;
  begin
  if assigned(ba) and (length(ba)>0) then
    Result:=typ +MARK_TYPE+ Int2Str(length(ba)) +MARK_LEN_START+ RtcBytesToString(ba) +MARK_LEN_END+MARK_END
  else
    Result:=typ +MARK_TYPE+MARK_END;
  end;

class function TRtcValueObject.code_toLongString(const typ,s: RtcString): RtcString;
  begin
  if length(s)>0 then
    Result:=typ +MARK_TYPE+ Int2Str(length(s)) +MARK_LEN_START+ s +MARK_LEN_END+MARK_END
  else
    Result:=typ +MARK_TYPE+MARK_END;
  end;

class function TRtcValueObject.code_toShortNameString(const typ: RtcString; const s:RtcWideString): RtcString;
  begin
  Result:=code_toShortString(typ,Utf8Encode(s));
  end;

class function TRtcValueObject.code_toShortString(const typ,s: RtcString): RtcString;
  begin
  if length(s)>0 then
    Result:=typ +MARK_TYPE+ s +MARK_END
  else
    Result:=typ +MARK_TYPE+MARK_END;
  end;

class function TRtcValueObject.code_toNameString(const s:RtcWideString): RtcString;
  begin
  Result:=Utf8Encode(s)+MARK_NAME;
  end;

class function TRtcValueObject.code_toMidString(const s: RtcString): RtcString;
  begin
  Result:=s + MARK_MID;
  end;

class function TRtcValueObject.code_toEndString(const s: RtcString): RtcString;
  begin
  Result:=s + MARK_END;
  end;

function EncodeXMLrpc(const s:RtcString):RtcString;
  var
    a,b:integer;
  begin
  Result:='';
  b:=length(s);
  for a:=1 to length(s) do
    case s[a] of
      '<':Inc(b,3);
      '&':Inc(b,4);
      end;
  SetLength(Result,b);
  b:=1;
  for a:=1 to length(s) do
    case s[a] of
      '<':begin
          Result[b]:='&';
          Result[b+1]:='l';
          Result[b+2]:='t';
          Result[b+3]:=';';
          Inc(b,4);
          end;
      '&':begin
          Result[b]:='&';
          Result[b+1]:='a';
          Result[b+2]:='m';
          Result[b+3]:='p';
          Result[b+4]:=';';
          Inc(b,5);
          end;
      else
          begin
          Result[b]:=s[a];
          Inc(b);
          end;
      end;
  end;

function DecodeXMLrpc(const s:RtcString):RtcString;
  var
    a,b,c,i,len:integer;
    st:RtcString;
  begin
  Result:='';
  len:=length(s);
  SetLength(Result,length(s));
  a:=1; b:=0;
  while a<=len do
    begin
    if s[a]='&' then
      begin
      c:=a;
      i:=len;
      if i>c+7 then i:=c+7;

      while (c<i) and (s[c]<>';') do Inc(c);
      if s[c]<>';' then
        begin
        Inc(b);
        Result[b]:=s[a];
        Inc(a);
        end
      else
        begin
        if (c>a) and (s[a+1]='#') then
          st:=Copy(s,a+2,c-a-2)
        else
          st:=Upper_Case(Copy(s,a+1,c-a-1));
        Inc(b);
        if s[a+1]='#' then
          begin
          i:=Str2IntDef(st,-1);
          if (i>=0) and (i<=255) then
            Result[b]:=RtcChar(i)
          else
            begin
            Result[b]:=s[a];
            c:=a;
            end;
          end
        else if st='LT' then
          Result[b]:='<'
        else if st='GT' then
          Result[b]:='>'
        else if st='AMP' then
          Result[b]:='&'
        else if st='QUOT' then
          Result[b]:='"'
        else if st='APOS' then
          Result[b]:=#39
        else if st='NBSP' then
          Result[b]:=' '
        else
          begin
          Result[b]:=s[a];
          c:=a;
          end;
        a:=c+1;
        end;
      end
    else
      begin
      Inc(b);
      Result[b]:=s[a];
      Inc(a);
      end;
    end;
  SetLength(Result,b);
  end;

class function TRtcValueObject.xmlrpc_readString(const s: RtcString; var at: integer): RtcString;
  begin
  Result:=DecodeXMLrpc(xmlrpc_readValue(s,at));
  end;

class function TRtcValueObject.xmlrpc_writeString(const s: RtcString): RtcString;
  begin
  Result:=EncodeXMLrpc(s);
  end;

class procedure TRtcValueObject.xmlrpc_readByteStream(const s: RtcString; var at: integer; const bs: TStream);
  var
    val,val2:RtcByteArray;
  begin
  if at<0 then
    raise ERtcInfo.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise ERtcInfo.Create('No more data. Starting position beyond end-of-String.');

  bs.Size:=0;
  val:= RtcStringToBytes( xmlrpc_readValue(s,at) );
  if length(val)>0 then
    begin
    val2:=Mime_DecodeEx(val);
    SetLength(val,0);
    if length(val2)>0 then
      begin
      bs.Write(val2[0],length(val2));
      bs.Position:=0; // move pointer back to the beginning of the stream
      SetLength(val2,0);
      end;
    end;
  end;

class function TRtcValueObject.xmlrpc_readByteArray(const s: RtcString; var at: integer):RtcByteArray;
  var
    val:RtcByteArray;
  begin
  if at<0 then
    raise ERtcInfo.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise ERtcInfo.Create('No more data. Starting position beyond end-of-String.');

  SetLength(Result,0);
  val:= RtcStringToBytes( xmlrpc_readValue(s,at) );
  if length(val)>0 then
    Result:=Mime_DecodeEx(val);
  end;

class function TRtcValueObject.xmlrpc_readNameString(const s: RtcString; var at: integer):RtcWideString;
  begin
  Result:=Utf8Decode(xmlrpc_readString(s,at));
  end;

class function TRtcValueObject.xmlrpc_writeByteStream(bs: TStream):RtcString;
  var
    val:RtcByteArray;
    loc:integer;
  begin
  if assigned(bs) and (bs.Size>0) then
    begin
    SetLength(val, bs.size);
    // Copy Data from ByteStream to temp RtcByteArray
    loc:=bs.Position;
    try
      bs.Position:=0;
      bs.Read(val[0],bs.Size);
    finally
      bs.Position:=loc;
      end;

    Result:= RtcBytesToString( Mime_EncodeEx(val) );
    SetLength(val,0);
    end
  else
    Result:='';
  end;

class function TRtcValueObject.xmlrpc_writeByteArray(const ba:RtcByteArray):RtcString;
  begin
  if assigned(ba) and (length(ba)>0) then
    Result:= RtcBytesToString( Mime_EncodeEx(ba) )
  else
    Result:='';
  end;

class function TRtcValueObject.xmlrpc_writeNameString(const s:RtcWideString): RtcString;
  begin
  Result:=xmlrpc_writeString(Utf8Encode(s));
  end;

class procedure TRtcValueObject.xmlrpc_OpenTag(const tag:RtcString; var closing_tags:rtcClosingTagsType);
  begin
  SetLength(closing_tags, length(closing_tags)+1);
  closing_tags[length(closing_tags)-1]:='/'+tag;
  end;

class function TRtcValueObject.xmlrpc_CloseTag(const tag:RtcString; var closing_tags:rtcClosingTagsType):boolean;
  begin
  if tag='' then // not a TAG
    Result:=False
  else if (length(closing_tags)>0) and (closing_tags[length(closing_tags)-1]=tag) then
    begin
    SetLength(closing_tags,length(closing_tags)-1);
    Result:=True;
    end
  else
    Result:=False;
  end;

class function TRtcValueObject.xmlrpc_FirstCloseTag(const closing_tags:rtcClosingTagsType):RtcString;
  begin
  if length(closing_tags)>0 then
    Result:=closing_tags[length(closing_tags)-1]
  else
    Result:='';
  end;

class function TRtcValueObject.xmlrpc_TagsToXML(const closing:rtcClosingTagsType):RtcString;
  var
    a:integer;
  begin
  Result:='';
  for a:=length(closing)-1 downto 0 do
    Result:=Result+'<'+closing[a]+'>';
  end;

class procedure TRtcValueObject.xmlrpc_skipWhitespace(const s: RtcString; var at: integer);
  var
    len:integer;
  begin
  len:=length(s);
  {$IFDEF RTC_BYTESTRING}
  while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
  {$ELSE}
  while (at<len) and (Pos(s[at+1],#32#9#13#10)>0) do Inc(at);
  {$ENDIF}
  end;

class function TRtcValueObject.xmlrpc_readTag(const s: RtcString; var at: integer; const tag_want:RtcString=''; skipWhitespace:boolean=True): RtcString;
  var
    len,at2:integer;
  begin
  len:=length(s);
  if at>=len then
    Result:=''
  else if s[at+1]<>'<' then
    raise ERtcInfo.Create('XML-RPC Error: Tag opening "<" expected, but "'+s[at]+'" found.')
  else
    begin
    {$IFDEF RTC_BYTESTRING}
    while (at+3<len) and (s[at+1]='<') and (s[at+2] in ['!','?']) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
    {$ELSE}
    while (at+3<len) and (s[at+1]='<') and ( (s[at+2]='!') or (s[at+2]='?') ) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
    {$ENDIF}
      begin
      case s[at+2] of
        '!':begin
            if s[at+3]='-' then
              begin
              at2:=PosEx('-->',s,at+1);
              if at2<0 then
                raise ERtcInfo.Create('XML-RPC Error: Tag closing "-->" expected, but missing.');
              at:=at2+2;
              end
            else
              begin
              at2:=PosEx('>',s,at+1);
              if at2<0 then
                raise ERtcInfo.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
              at:=at2;
              end;
            end;
        '?':begin
            at2:=PosEx('?>',s,at+1);
            if at2<0 then
              raise ERtcInfo.Create('XML-RPC Error: Tag closing "?>" expected, but missing.');
            at:=at2+1;
            end;
        end;
    {$IFDEF RTC_BYTESTRING}
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
    {$ELSE}
      while (at<len) and (Pos(s[at+1],#32#9#13#10)>0) do Inc(at);
    {$ENDIF}
      end;

    at2:=PosEx('>',s,at+1);
    if at2<0 then
      raise ERtcInfo.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
    // return text inside '< >'
    Result:=TrimCopy(s,at+2,at2-at-2);
    // Position behind '>'
    at:=at2;

    if skipWhiteSpace then
    {$IFDEF RTC_BYTESTRING}
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
    {$ELSE}
      while (at<len) and (Pos(s[at+1],#32#9#13#10)>0) do Inc(at);
    {$ENDIF}

    if Result<>'' then
      if Result[length(Result)]='/' then
        Result:=TrimCopy(Result,1,length(Result)-1)+'/'
      else if Result[1]='/' then
        Result:='/'+TrimCopy(Result,2,length(Result)-1);
    end;
  if (tag_want<>'') then
    begin
    if Result='' then
      raise ERtcInfo.Create('XML-RPC Error: Tag <'+String(tag_want)+'> expected, but missing.')
    else if Upper_Case(Result)<>tag_want then
      raise ERtcInfo.Create('XML-RPC Error: Tag <'+String(tag_want)+'> expected, but <'+String(Result)+'> found.');
    end;
  end;

class function TRtcValueObject.xmlrpc_checkTag(const s: RtcString; at: integer): RtcString;
  var
    len,at2:integer;
  begin
  len:=length(s);
  if at>=len then
    Result:=''
  else if s[at+1]<>'<' then
    Result:=''
  else
    begin
    {$IFDEF RTC_BYTESTRING}
    while (at+3<len) and (s[at+1]='<') and (s[at+2] in ['!','?']) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
    {$ELSE}
    while (at+3<len) and (s[at+1]='<') and ( (s[at+2]='!') or (s[at+2]='?') ) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
    {$ENDIF}
      begin
      case s[at+2] of
        '!':begin
            if s[at+3]='-' then
              begin
              at2:=PosEx('-->',s,at+1);
              if at2<0 then
                raise ERtcInfo.Create('XML-RPC Error: Tag closing "-->" expected, but missing.');
              at:=at2+2;
              end
            else
              begin
              at2:=PosEx('>',s,at+1);
              if at2<0 then
                raise ERtcInfo.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
              at:=at2;
              end;
            end;
        '?':begin
            at2:=PosEx('?>',s,at+1);
            if at2<0 then
              raise ERtcInfo.Create('XML-RPC Error: Tag closing "?>" expected, but missing.');
            at:=at2+1;
            end;
        end;
      {$IFDEF RTC_BYTESTRING}
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
      {$ELSE}
      while (at<len) and (Pos(s[at+1],#32#9#13#10)>0) do Inc(at);
      {$ENDIF}
      end;

    at2:=PosEx('>',s,at+1);
    if at2<0 then
      raise ERtcInfo.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
    // return text inside '< >'
    Result:=TrimCopy(s,at+2,at2-at-2);

    if Result<>'' then
      if Result[length(Result)]='/' then
        Result:=TrimCopy(Result,1,length(Result)-1)+'/'
      else if Result[1]='/' then
        Result:='/'+TrimCopy(Result,2,length(Result)-1);
    end;
  end;

class procedure TRtcValueObject.xmlrpc_skipTag(const s: RtcString; var at: integer; skipWhiteSpace:boolean=True);
  var
    len,at2:integer;
  begin
  len:=length(s);
  if at>=len then
    begin
    end
  else if s[at+1]<>'<' then
    raise ERtcInfo.Create('XML-RPC Error: Tag opening "<" expected, but "'+s[at]+'" found.')
  else
    begin
    {$IFDEF RTC_BYTESTRING}
    while (at+3<len) and (s[at+1]='<') and (s[at+2] in ['!','?']) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
    {$ELSE}
    while (at+3<len) and (s[at+1]='<') and ( (s[at+2]='!') or (s[at+2]='?') ) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
    {$ENDIF}
      begin
      case s[at+2] of
        '!':begin
            if s[at+3]='-' then
              begin
              at2:=PosEx('-->',s,at+1);
              if at2<0 then
                raise ERtcInfo.Create('XML-RPC Error: Tag closing "-->" expected, but missing.');
              at:=at2+2;
              end
            else
              begin
              at2:=PosEx('>',s,at+1);
              if at2<0 then
                raise ERtcInfo.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
              at:=at2;
              end;
            end;
        '?':begin
            at2:=PosEx('?>',s,at+1);
            if at2<0 then
              raise ERtcInfo.Create('XML-RPC Error: Tag closing "?>" expected, but missing.');
            at:=at2+1;
            end;
        end;
      {$IFDEF RTC_BYTESTRING}
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
      {$ELSE}
      while (at<len) and (Pos(s[at+1],#32#9#13#10)>0) do Inc(at);
      {$ENDIF}
      end;

    at2:=PosEx('>',s,at+1);
    if at2<0 then
      raise ERtcInfo.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
    // Position behind '>'
    at:=at2;
    if skipWhiteSpace then
    {$IFDEF RTC_BYTESTRING}
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
    {$ELSE}
      while (at<len) and (Pos(s[at+1],#32#9#13#10)>0) do Inc(at);
    {$ENDIF}
    end;
  end;

class function TRtcValueObject.xmlrpc_readValue(const s: RtcString; var at: integer): RtcString;
  var
    at2:integer;
  begin
  if at>=length(s) then
    Result:=''
  else if s[at+1]='<' then
    Result:=''
  else
    begin
    at2:=PosEx('<',s,at+1);
    if at2<0 then
      raise ERtcInfo.Create('XML-RPC Error: Tag opening "<" expected, but missing.');
    // return text before '<'
    Result:=Copy(s,at+1,at2-at-1);
    // Position at '<'
    at:=at2-1;
    end;
  end;

class function TRtcValueObject.xmlrpc_readTrimNameValue(const s: RtcString; var at: integer):RtcWideString;
  begin
  Result:=Utf8Decode(xmlrpc_readTrimValue(s,at));
  end;

class function TRtcValueObject.xmlrpc_readTrimValue(const s: RtcString; var at: integer): RtcString;
  var
    at2:integer;
  begin
  if at>=length(s) then
    Result:=''
  else if s[at+1]='<' then
    Result:=''
  else
    begin
    at2:=PosEx('<',s,at+1);
    if at2<0 then
      raise ERtcInfo.Create('XML-RPC Error: Tag opening "<" expected, but missing.');
    // return text before '<'
    Result:=TrimCopy(s,at+1,at2-at-1);
    // Position at '<'
    at:=at2-1;
    end;
  end;

class function TRtcValueObject.xmlrpc_checkStrType(const s: RtcString; const at: integer): TRtcValueTypes;
  var
    xtag:RtcString;
    at2:integer;
  begin
  at2:=at;
  xmlrpc_skipWhitespace(s, at2);
  xtag:=Upper_Case(xmlrpc_checkTag(s,at2));

  // Skip ?XML header, if present
  if xtag='' then
    raise ERtcInfo.Create('XML-RPC Error: Type identifier expected.');

  // Check if this is a complex structure
  if (xtag='METHODRESPONSE') or (xtag='PARAMS') or (xtag='PARAM') then
    begin
    xmlrpc_skipTag(s,at2); // <methodResponse>
    xtag:=Upper_Case(xmlrpc_checkTag(s,at2));
    Result:=rtc_Variant;
    Exit;
    end;

  // Anything can be inside a 'VALUE' tag
  if xtag='VALUE' then
    begin
    repeat
      xmlrpc_skipTag(s,at2); // <value>
      xtag:=Upper_Case(xmlrpc_checkTag(s,at2));
      while xtag='DATA' do
        begin
        xmlrpc_skipTag(s,at2); // <data>
        xtag:=Upper_Case(xmlrpc_checkTag(s,at2));
        end;
      until xtag<>'VALUE';

    if xtag='' then // not followed by a tag, containing a String
      xtag:='STRING'
    else if xtag='/VALUE' then // empty <value></value> set means an empty String
      xtag:='STRING';
    end;

  // Now check the type
  if xtag='FAULT' then
    Result:=rtc_Exception
  else if (xtag='METHODCALL') or (xtag='CALL') then
    Result:=rtc_Function
  else if (xtag='I4') or (xtag='INT') then
    Result:=rtc_LargeInt
  else if xtag='BOOLEAN' then
    Result:=rtc_Boolean
  else if xtag='STRING' then
    Result:=rtc_Text
  else if xtag='DOUBLE' then
    Result:=rtc_Float
  else if (xtag='DATETIME.ISO8601') or (xtag='DATETIME') or (xtag='TIMESTAMP') then
    Result:=rtc_DateTime
  else if (xtag='BASE64') or (xtag='BASE64BINARY') or (xtag='BINARY') then
    Result:=rtc_ByteStream
  else if (xtag='NAME') then
    begin
    xmlrpc_skipTag(s,at2); // <name>

    xtag:=Upper_Case(xmlrpc_readTrimValue(s,at2));
    if (xtag=RTC_XMLRPC_DataSetFieldsName) or
       (xtag=RTC_XMLRPC_DataSetRowsName) then
      Result:=rtc_DataSet
    else if (xtag='FAULTCODE') or (xtag='FAULTSTRING') then
      Result:=rtc_Exception
    else
      Result:=rtc_Record;
    end
  else if (xtag='STRUCT') or (xtag='METHODFAULT') or (xtag='MEMBER') then
    begin
    xmlrpc_skipTag(s,at2); // <struct>

    xtag:=Upper_Case(xmlrpc_checkTag(s,at2));
    if xtag='MEMBER' then
      begin
      xmlrpc_skipTag(s,at2); // <member>
      xtag:=Upper_Case(xmlrpc_checkTag(s,at2));
      end;

    if xtag='NAME' then
      begin
      xmlrpc_skipTag(s,at2); // <name>

      xtag:=Upper_Case(xmlrpc_readTrimValue(s,at2));
      if (xtag=RTC_XMLRPC_DataSetFieldsName) or
         (xtag=RTC_XMLRPC_DataSetRowsName) then
        Result:=rtc_DataSet
      else if (xtag='FAULTCODE') or (xtag='FAULTSTRING') then
        Result:=rtc_Exception
      else
        Result:=rtc_Record;
      end
    else if xtag<>'' then
      Result:=rtc_Record
    else
      raise ERtcInfo.Create('XML-RPC Error: <Struct><Member> Tags followed by plain data (<Name> expected).');
    end
  else if xtag='ARRAY' then
    Result:=rtc_Array
  else if (xtag<>'') and (xtag[length(xtag)]='/') then
    Result:=rtc_Null
  else
    raise ERtcInfo.Create('XML-RPC Error: Type identifier expected, <'+String(xtag)+'> found.');
  end;

class procedure TRtcValueObject.xmlrpc_skipValueOpen(const tag:RtcString; const s: RtcString; var at: integer; var closing_tags:rtcClosingTagsType);
  var
    at3:integer;
    xtag:RtcString;
  begin
  xmlrpc_skipWhitespace(s, at);
  xtag:=Upper_Case(xmlrpc_checkTag(s,at));

  // Skip ?XML header, if present
  if xtag='' then
    raise ERtcInfo.Create('XML-RPC Error: Type identifier expected.');

  // Skip 'METHODRESPONSE', if present.
  if (xtag='METHODRESPONSE') or (xtag='PARAMS') or (xtag='PARAM') then
    begin
    if tag<>'METHODRESPONSE' then
      raise ERtcInfo.Create('XML-RPC Error: Tag <'+String(xtag)+'> found, while <'+String(tag)+'> was expected.');

    if xtag='METHODRESPONSE' then
      begin
      xmlrpc_OpenTag(xtag,closing_tags);
      xmlrpc_skipTag(s,at); // <methodResponse>
      end;
    Exit;
    end;

  // skip 'FAULT', if present
  if xtag='FAULT' then
    begin
    xmlrpc_OpenTag(xtag, closing_tags);

    xmlrpc_skipTag(s,at); // <fault>

    xtag:=Upper_Case(xmlrpc_checkTag(s,at));
    end;

  // Anything can be inside a 'VALUE' tag
  if xtag='VALUE' then
    begin
    repeat
      xmlrpc_OpenTag(xtag, closing_tags);
      xmlrpc_skipTag(s,at,False); // <value> - don't skip whitespace

      at3:=at;
      xmlrpc_skipWhitespace(s,at);
      xtag:=Upper_Case(xmlrpc_checkTag(s,at));

      while xtag='DATA' do // skip all <DATA> tags
        begin
        xmlrpc_OpenTag(xtag, closing_tags);
        xmlrpc_skipTag(s,at,False); // <data>

        at3:=at;
        xmlrpc_skipWhitespace(s,at);
        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        end;
      until xtag<>'VALUE';

    if (xtag='') or (xtag='/VALUE') then // empty String or <value></value>
      begin
      if tag<>'STRING' then
        raise ERtcInfo.Create('XML-RPC Error: Tag <'+String(xtag)+'> found, but <'+String(tag)+'> expected.');
      at:=at3;
      Exit;
      end
    else if xtag[1]='/' then // closing tag, but not </value>
      raise ERtcInfo.Create('XML-RPC Error: Type identifier or </value> expected (<'+String(xtag)+'> found).');
    end;

  if xtag='' then
    raise ERtcInfo.Create('XML-RPC Error: Type identifier <'+String(tag)+'> expected, but missing.')
  else if (xtag='NAME') or (xtag='MEMBER') then
    begin
    if tag<>'STRUCT' then
      raise ERtcInfo.Create('XML-RPC Error: Tag <'+String(xtag)+'> found, while <'+String(tag)+'> was expected.');
    end
  else
    begin
    xmlrpc_OpenTag(xtag, closing_tags);
    xmlrpc_skipTag(s,at,False); // <int>/<i4>/<string>/<base64>/... - don't skip whitespace
    if xtag='I4' then xtag:='INT';
    if (xtag='DATETIME') or (xtag='TIMESTAMP') then xtag:='DATETIME.ISO8601';
    if (xtag='BASE64BINARY') or (xtag='BINARY') then xtag:='BASE64';
    if xtag='METHODFAULT' then xtag:='STRUCT';
    if xtag='CALL' then xtag:='METHODCALL';
    if tag<>xtag then
      raise ERtcInfo.Create('XML-RPC Error: Tag <'+String(xtag)+'> found, while <'+String(tag)+'> was expected.');
    end;
  end;

class procedure TRtcValueObject.xmlrpc_skipValueClose(const s: RtcString; var at: integer; var closing_tags:rtcClosingTagsType);
  var
    xtag:RtcString;
  begin
  xmlrpc_skipWhitespace(s, at);
  while length(closing_tags)>0 do
    begin
    xtag:=Upper_Case(xmlrpc_checkTag(s,at));

    if xmlrpc_CloseTag(xtag,closing_tags) then
      xmlrpc_skipTag(s,at) // <"xtag">
    else
      raise ERtcInfo.Create('XML-RPC Error: Closing Tag(s) missing: '+String(xmlrpc_TagsToXML(closing_tags)));
    end;
  end;

class procedure TRtcValueObject.xmlrpc_skipNull(const s: RtcString; var at: integer);
  var
    xtag:RtcString;
    closing_tags:rtcClosingTagsType;
  begin
  SetLength(closing_tags,0);
  try
    xmlrpc_skipWhitespace(s, at);
    xtag:=Upper_Case(xmlrpc_checkTag(s,at));
    // Skip ?XML header, if present
    if xtag='' then
      raise ERtcInfo.Create('XML-RPC Error: Type identifier expected.');

    // Skip 'FAULT', if present
    if xtag='FAULT' then
      begin
      xmlrpc_OpenTag(xtag, closing_tags);

      xmlrpc_skipTag(s,at); // <fault>

      xtag:=Upper_Case(xmlrpc_checkTag(s,at));
      end;
    // Anything can be inside a 'VALUE' tag
    if xtag='VALUE' then
      begin
      repeat
        xmlrpc_OpenTag(xtag, closing_tags);
        xmlrpc_skipTag(s,at); // <value> including whitespace

        xtag:=Upper_Case(xmlrpc_checkTag(s,at));

        while xtag='DATA' do
          begin
          xmlrpc_OpenTag(xtag, closing_tags);
          xmlrpc_skipTag(s,at); // <value> including whitespace

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          end;
        until xtag<>'VALUE';
      end;

    if (xtag<>'') and (xtag[length(xtag)]='/') then
      begin
      xmlrpc_skipTag(s,at); // <nil/>

      while length(closing_tags)>0 do
        begin
        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        if xmlrpc_CloseTag(xtag,closing_tags) then
          xmlrpc_skipTag(s,at)
        else
          raise ERtcInfo.Create('XML-RPC Error: Closing Tag(s) missing: '+String(xmlrpc_TagsToXML(closing_tags)));
        end;
      end
    else
      raise ERtcInfo.Create('XML-RPC Error: <NIL/> expected, <'+String(xtag)+'> found.');
  finally
    SetLength(closing_tags,0);
    end;
  end;

{ Encode a Unicode String to a JSON Ansi String }
function JSON_EncodeString(const s:RtcWideString):RtcString;
  const
    ToHex:array[$0..$F] of RtcChar = ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
  var
    a,b:integer;
    sb:longint;
  begin
  Result:='';
  b:=length(s);
  for a:=1 to length(s) do
    case s[a] of
      '"', // "
      '\', // \
      #8,  // b
      #12, // f
      #10, // n
      #13, // r
      #9:  // t
        Inc(b);
      '/': if RTC_JSON_GenEscapeSlash then Inc(b);
      else
        begin
        sb:=Ord(s[a]);
        if (sb<$20) or (sb>$7E) then
          Inc(b,5);
        end;
      end;
  SetLength(Result,b);
  b:=1;
  for a:=1 to length(s) do
    begin
    case s[a] of
      '"':begin
          Result[b]:='\';
          Result[b+1]:='"';
          Inc(b,2);
          end;
      '\':begin
          Result[b]:='\';
          Result[b+1]:='\';
          Inc(b,2);
          end;
      '/':begin
          if RTC_JSON_GenEscapeSlash then
            begin
            Result[b]:='\';
            Result[b+1]:='/';
            Inc(b,2);
            end
          else
            begin
            Result[b]:=RtcChar(s[a]);
            Inc(b);
            end;
          end;
      #8: begin
          Result[b]:='\';
          Result[b+1]:='b';
          Inc(b,2);
          end;
      #9: begin
          Result[b]:='\';
          Result[b+1]:='t';
          Inc(b,2);
          end;
      #10:begin
          Result[b]:='\';
          Result[b+1]:='n';
          Inc(b,2);
          end;
      #12:begin
          Result[b]:='\';
          Result[b+1]:='f';
          Inc(b,2);
          end;
      #13:begin
          Result[b]:='\';
          Result[b+1]:='r';
          Inc(b,2);
          end;
      else
          begin
          sb:=Ord(s[a]);
          if (sb<$20) or (sb>$7E) then
            begin
            Result[b]:='\';
            Result[b+1]:='u';
            Result[b+2]:=ToHex[(sb shr 12) and $F];
            Result[b+3]:=ToHex[(sb shr 8) and $F];
            Result[b+4]:=ToHex[(sb shr 4) and $F];
            Result[b+5]:=ToHex[sb and $F];
            Inc(b,6);
            end
          else
            begin
            Result[b]:=RtcChar(s[a]);
            Inc(b);
            end;
          end;
      end;
    end;
  end;

{ Decode a valid JSON String (without "") to a plain Unicode String }
function JSON_DecodeString(const s:RtcWideString):RtcWideString;

  function from_Hex(ch:RtcWideChar):longint;
    begin
    case ch of
      '0'..'9': Result:=Ord(ch)-Ord('0');
      'A'..'F': Result:=Ord(ch)-Ord('A')+10;
      'a'..'f': Result:=Ord(ch)-Ord('a')+10;
      else raise ERtcInfo.Create('JSON Error: Bad Hex Char in escape sequence \u '+ch);
      end;
    end;

  var
    a,b,len:integer;
    sb:longint;
  begin
  Result:='';
  a:=1;b:=0;
  len:=length(s);
  while a<=len do
    begin
    if s[a]='\' then
      begin
      Inc(a);
      if a>len then
        raise ERtcInfo.Create('JSON Error: Escape sequence broken');
      case s[a] of
        '"': Inc(b); // "
        '\': Inc(b); // \
        '/': Inc(b); // /
        'b': Inc(b); // #8
        'f': Inc(b); // #12
        'n': Inc(b); // #10
        'r': Inc(b); // #13
        't': Inc(b); // #9
        'u': if a+4<=len then
                begin
                Inc(a,4);
                Inc(b);
                end
             else raise ERtcInfo.Create('JSON Error: Incomplete escape sequence \u');
        else raise ERtcInfo.Create('JSON Error: Unknown escape sequence \'+s[a]);
        end;
      Inc(a);
      end
    else
      begin
      Inc(a);
      Inc(b);
      end;
    end;
  SetLength(Result,b);
  b:=1;
  a:=1;
  len:=length(s);
  while a<=len do
    begin
    if s[a]='\' then
      begin
      Inc(a); // skip \
      if a>len then
        raise ERtcInfo.Create('JSON Error: Escape sequence broken');
      case s[a] of
        '"': Result[b]:='"';
        '\': Result[b]:='\';
        '/': Result[b]:='/';
        'b': Result[b]:=#8;
        'f': Result[b]:=#12;
        'n': Result[b]:=#10;
        'r': Result[b]:=#13;
        't': Result[b]:=#9;
        'u': if a+4<=len then
                begin
                sb:= (from_Hex(s[a+1]) shl 12) +
                     (from_Hex(s[a+2]) shl 8) +
                     (from_Hex(s[a+3]) shl 4) +
                      from_Hex(s[a+4]);
                Inc(a,4); // skip hex value
                Result[b]:=RtcWideChar(sb);
                end
             else raise ERtcInfo.Create('JSON Error: Incomplete escape sequence \u');
        else raise ERtcInfo.Create('JSON Error: Unknown escape sequence \'+s[a]);
        end;
      Inc(a); // skip escape Char
      Inc(b);
      end
    else
      begin
      Result[b]:=s[a];
      Inc(a);
      Inc(b);
      end;
    end;
  end;

class procedure TRtcValueObject.json_skipWhitespace(const s:RtcWideString; var at: integer);
  var
    len:integer;
  begin
  len:=length(s);
  while (at<len) and (Pos(s[at+1],#32#9#12#13#10)>0) do Inc(at);
  end;

class procedure TRtcValueObject.json_skipTag(const tag, s:RtcWideString; var at: integer);
  var
    a:integer;
    ok:boolean;
  begin
  json_skipWhitespace(s,at);
  if (at+length(tag)>length(s)) then
    raise ERtcInfo.Create('JSON Error: Missing '+tag+' tag.')
  else
    begin
    ok:=True;
    for a:=1 to length(tag) do
      if s[at+a]<>tag[a] then
        begin
        ok:=False;
        Break;
        end;
    if ok then
      Inc(at, length(tag))
    else
      raise ERtcInfo.Create('JSON Error: Missing '+tag+' tag. Found '+Copy(s,at+1,length(tag))+' instead.');
    end;
  end;

class function TRtcValueObject.json_checkTag(const tag, s:RtcWideString; var at: integer; autoSkip:boolean=False):boolean;
  var
    a:integer;
  begin
  json_skipWhitespace(s,at);
  if (at+length(tag)>length(s)) then
    Result:=False
  else
    begin
    Result:=True;
    for a:=1 to length(tag) do
      if s[at+a]<>tag[a] then
        begin
        Result:=False;
        Break;
        end;
    if autoSkip and Result then
      Inc(at, length(tag));
    end;
  end;

class procedure TRtcValueObject.json_skipNull(const s:RtcWideString; var at: integer);
  begin
  json_skipTag('null',s,at);
  end;

class function TRtcValueObject.json_checkStrType(const s:RtcWideString; const at: integer): TRtcValueTypes;
  var
    at2:integer;
    s2:RtcWideString;
  begin
  if length(s)<=at then
    raise ERtcInfo.Create('JSON Error: End of Data (JSON broken).')
  else
    begin
    case s[at+1] of
      '{':  begin
            Result:=Rtc_Record;
            if RTC_JSON_ParseTypedFunctions or RTC_JSON_ParseTypedDataSet then
              begin
              at2:=at+1;
              json_skipWhitespace(s,at2);
              if length(s)>at2+4 then
                if (s[at2+1]='"') and (s[at2+2]='\') and (s[at2+3]='/') then
                  begin
                  case s[at2+4] of
                    'd':if RTC_JSON_ParseTypedDataSet then
                          if  json_checkTag(RTC_JSON_DataSetFieldsName,s,at2) or
                              json_checkTag(RTC_JSON_DataSetRowsName,s,at2)   then
                            Result:=rtc_DataSet;
                    'm':if RTC_JSON_ParseTypedFunctions then
                          if json_checkTag(RTC_JSON_FunctionName,s,at2)      then
                            Result:=rtc_Function;
                    end;
                  end;
              end;
            end;
      '[':  Result:=rtc_Array;
      't',
      'f':  Result:=rtc_Boolean;
      'n':  Result:=rtc_Null;
      '"':  begin
            Result:=rtc_Text;
            if RTC_JSON_ParseTypedDateTime or
               RTC_JSON_ParseTypedByteStream or
               RTC_JSON_ParseTypedException then
              begin
              if length(s)>at+4 then
                if (s[at+2]='\') and (s[at+3]='/') then
                  begin
                  at2:=at;
                  case s[at+4] of
                    'D':if RTC_JSON_ParseTypedDateTime then
                          if json_checkTag(RTC_JSON_DateTimeStr,s,at2) then
                            Result:=rtc_DateTime;
                    'd':if RTC_JSON_ParseTypedDateTime then
                          if json_checkTag(RTC_JSON_DateTimeISOStr,s,at2) then
                            Result:=rtc_DateTime;
                    'b':if RTC_JSON_ParseTypedByteStream then
                          if json_checkTag(RTC_JSON_ByteStreamStr,s,at2) then
                            Result:=rtc_ByteStream;
                    'e':if RTC_JSON_ParseTypedException then
                          if json_checkTag(RTC_JSON_ExceptionStr,s,at2) then
                            Result:=rtc_Exception;
                    end;
                  end;
              end;
            end;
      '-','0'..'9':
        begin
        at2:=at;
        s2:=json_readNumber(s,at2);
        if length(s2)>21 then
          Result:=rtc_String
        else if (Pos('.',s2)<=0) and (Pos('E',s2)<=0) and (Pos('e',s2)<=0) then
          begin
          if StrToInt64Def(s2,$FFFFFFFF)=$FFFFFFFF then
            Result:=rtc_String
          else
            Result:=rtc_LargeInt;
          end
        else
          begin
          if StrToFloatDef(MakeDecimal(RtcString(s2)),$FFFFFFFF)=$FFFFFFFF then
            Result:=rtc_String
          else
            Result:=rtc_Float;
          end;
        end;
      else
        raise ERtcInfo.Create('JSON Error: Type identifier expected, '+String(s[at+1])+' found.');
      end;
    end;
  end;

class function TRtcValueObject.json_readString(const s:RtcWideString; var at: integer; skippedQuote:boolean=False):RtcWideString;
  var
    at2,len:integer;
  begin
  if at>=length(s) then
    raise ERtcInfo.Create('JSON Error: String expected, Eof found.')
  else if (skippedQuote=False) and (s[at+1]<>'"') then
    begin
    if (s[at+1]='-') or (Pos(s[at+1],'0123456789')>0) then
      Result:=json_readNumber(s,at)
    else
      raise ERtcInfo.Create('JSON Error: String opening " expected.');
    end
  else
    begin
    len:=length(s);
    if skippedQuote then
      at2:=at+1
    else
      at2:=at+2; // skip opening '"'
    if at2>len then
      raise ERtcInfo.Create('JSON Error: String closing " missing.');

    len:=length(s);
    while at2<=len do
      begin
      if s[at2]='\' then
        begin
        Inc(at2);
        if at2>len then
          raise ERtcInfo.Create('JSON Error: Escape sequence broken');
        case s[at2] of
          '"': Inc(at2); // "
          '\': Inc(at2); // \
          '/': Inc(at2); // /
          'b': Inc(at2); // #8
          'f': Inc(at2); // #12
          'n': Inc(at2); // #10
          'r': Inc(at2); // #13
          't': Inc(at2); // #9
          'u': begin
               if at2+5<=len then Inc(at2,5)
               else raise ERtcInfo.Create('JSON Error: Incomplete escape sequence \u');
               end;
          else
            raise ERtcInfo.Create('JSON Error: Unknown escape sequence \'+s[at2]);
          end;
        end
      else if s[at2]='"' then
        Break
      else
        Inc(at2);
      end;
    if skippedQuote then
      Result:=JSON_DecodeString(Copy(s,at+1,at2-at-1))
    else
      Result:=JSON_DecodeString(Copy(s,at+2,at2-at-2));
    at:=at2;
    end;
  end;

class function TRtcValueObject.json_readNumber(const s:RtcWideString; var at: integer):RtcWideString;
  var
    at2,len:integer;
  begin
  if at>=length(s) then
    raise ERtcInfo.Create('JSON Error: Number expected, Eof found.')
  else
    begin
    at2:=at;
    len:=length(s);
    while at2<len do
      begin
      case s[at2+1] of
        '-','0'..'9','e','E','+','.': Inc(at2);
        else Break;
        end;
      end;
    Result:=Copy(s,at+1,at2-at);
    at:=at2;
    end;
  end;

class procedure TRtcValueObject.json_readByteStream(const s:RtcWideString; var at: integer; const bs: TStream);
  var
    val,val2:RtcByteArray;
  begin
  if at<0 then
    raise ERtcInfo.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise ERtcInfo.Create('No more data. Starting position beyond end-of-String.');

  bs.Size:=0;
  val := RtcWideStringToBytes( json_readString(s,at, json_checkTag(RTC_JSON_ByteStreamStr,s,at,true) ) );
  if length(val)>0 then
    begin
    val2:=Mime_DecodeEx(val);
    SetLength(val,0);
    if length(val2)>0 then
      begin
      bs.Write(val2[0],length(val2));
      bs.Position:=0; // move pointer back to the beginning of the stream
      SetLength(val2,0);
      end;
    end;
  end;

class function TRtcValueObject.json_readByteArray(const s:RtcWideString; var at: integer):RtcByteArray;
  var
    val:RtcByteArray;
  begin
  if at<0 then
    raise ERtcInfo.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise ERtcInfo.Create('No more data. Starting position beyond end-of-String.');

  SetLength(Result,0);
  val := RtcWideStringToBytes( json_readString(s,at, json_checkTag(RTC_JSON_ByteStreamStr,s,at,true) ) );
  if length(val)>0 then
    Result:=Mime_DecodeEx(val);
  end;

class function TRtcValueObject.json_writeByteStream(bs: TStream):RtcString;
  var
  {$IFDEF RTC_BYTESTRING}
    val:RtcString;
  {$ELSE}
    val:RtcByteArray;
  {$ENDIF}
    loc:integer;
  begin
  Result:='';
  if assigned(bs) and (bs.Size>0) then
    begin
    SetLength(val, bs.size);
    // Copy Data from ByteStream to temp RtcByteArray
    loc:=bs.Position;
    try
      bs.Position:=0;
    {$IFDEF RTC_BYTESTRING}
      bs.Read(val[1],bs.Size);
    {$ELSE}
      bs.Read(val[0],bs.Size);
    {$ENDIF}
    finally
      bs.Position:=loc;
      end;
  {$IFDEF RTC_BYTESTRING}
    Result:= Mime_Encode(val,True);
  {$ELSE}
    Result:= RtcBytesToString( Mime_EncodeEx(val,True) );
  {$ENDIF}
    SetLength(val,0);
    end;
  end;

class function TRtcValueObject.json_writeByteArray(const ba:RtcByteArray):RtcString;
  begin
  if assigned(ba) and (length(ba)>0) then
    Result:= RtcBytesToString( Mime_EncodeEx(ba,True) )
  else
    Result:='';
  end;

class function TRtcValueObject.ObjectFromType(const typ: TRtcValueTypes): TRtcValueObject;
  begin
  case typ of
    rtc_Null:         Result:=nil;
    rtc_Function:     Result:=TRtcFunctionInfo.Create;
    rtc_Exception:    Result:=TRtcExceptionValue.Create;
    rtc_Variable:     Result:=TRtcVariableName.Create;
    rtc_Array:        Result:=TRtcArray.Create;
    rtc_Record:       Result:=TRtcRecord.Create;
    rtc_DataSet:      Result:=TRtcDataSet.Create;
    rtc_String:       Result:=TRtcStringValue.Create;
    rtc_WideString:   Result:=TRtcWideStringValue.Create;
    rtc_Text:         Result:=TRtcTextValue.Create;
    rtc_Boolean:      Result:=TRtcBooleanValue.Create;
    rtc_Integer:      Result:=TRtcIntegerValue.Create;
    rtc_Cardinal:     Result:=TRtcCardinalValue.Create;
    rtc_LargeInt:     Result:=TRtcLargeIntValue.Create;
    rtc_Float:        Result:=TRtcFloatValue.Create;
    rtc_Currency:     Result:=TRtcCurrencyValue.Create;
    rtc_DateTime:     Result:=TRtcDateTimeValue.Create;
    rtc_ByteStream:   Result:=TRtcByteStream.Create;
    rtc_Variant:      Result:=TRtcValue.Create;
    rtc_OID:          Result:=TRtcOIDValue.Create;
    rtc_ByteArray:    Result:=TRtcByteArray.Create;
    else
      raise ERtcInfo.Create('Unsupported object type in function ObjectFromType.');
    end;
  end;

class function TRtcValueObject.ObjectFromXMLRPC(const s: RtcString; var at: integer): TRtcValueObject;
  var
    typ:TRtcValueTypes;
  begin
  typ:=xmlrpc_checkStrType(s,at);
  if typ=rtc_Null then
    begin
    xmlrpc_skipNull(s,at);
    Result:=nil;
    end
  else
    begin
    Result:=ObjectFromType(typ);
    Result.from_XMLRPC(s,at);
    end;
  xmlrpc_skipWhitespace(s,at);
  end;

class function TRtcValueObject.ObjectFromCode(const s: RtcString; var at:integer): TRtcValueObject;
  var
    typ:TRtcValueTypes;
  begin
  typ:=code_checkStrType(s,at);
  if typ=rtc_Null then
    begin
    code_fromShortString(RTC_TYPE2STR_CONV[rtc_Null],s,at);
    Result:=nil;
    end
  else
    begin
    Result:=ObjectFromType(typ);
    Result.from_Code(s,at);
    end;
  end;

class function TRtcValueObject.ObjectFromVariant(const v: Variant): TRtcValueObject;
  begin
  case TVarData(V).VType of
    varEmpty,varNull:
      begin
      Result:=nil;
      end;
    varBoolean:
      begin
      Result:=TRtcBooleanValue.Create;
      TRtcBooleanValue(Result).SetBoolean(v);
      end;
    {$IFNDEF IDE_1}
    varLongWord,
    varInt64:
      begin
      Result:=TRtcLargeIntValue.Create;
      TRtcLargeIntValue(Result).SetLargeInt(v);
      end;
    {$ENDIF}

    {$IFNDEF IDE_1}
    varShortInt,
    varWord,
    {$ENDIF}
    varByte,
    varSmallint,
    varInteger:
      begin
      Result:=TRtcIntegerValue.Create;
      {$IFDEF IDE_0}
        TRtcIntegerValue(Result).SetInteger(LongInt(V));
      {$ELSE}
        TRtcIntegerValue(Result).SetInteger(v);
      {$ENDIF}
      end;
    varSingle,
    varDouble:
      begin
      Result:=TRtcFloatValue.Create;
      TRtcFloatValue(Result).SetFloat(v);
      end;
    varCurrency:
      begin
      Result:=TRtcCurrencyValue.Create;
      TRtcCurrencyValue(Result).SetCurrency(v);
      end;
    varDate:
      begin
      Result:=TRtcDateTimeValue.Create;
      TRtcDateTimeValue(Result).SetDateTime(v);
      end;
    {$IFNDEF IDE_0}
    varStrArg,
    {$ENDIF}
    varString:
      begin
      Result:=TRtcStringValue.Create;
      TRtcStringValue(Result).SetString(RtcString(v));
      end;
  {$IFDEF UNICODE}
    varUString:
      begin
      Result:=TRtcTextValue.Create;
      TRtcTextValue(Result).SetText(v);
      end;
  {$ENDIF}
    varOleStr:
      begin
      Result:=TRtcWideStringValue.Create;
      TRtcWideStringValue(Result).SetWideString(v);
      end;
    else
      raise EConvertError.Create('Unsupported Variant type.');
    end;
  end;

class function TRtcValueObject.ObjectFromCode(const s: RtcString): TRtcValueObject;
  var
    at:integer;
  begin
  at:=0;
  Result:=ObjectFromCode(s,at);
  if at<>length(s) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcValueObject.ObjectFromJSON(const s:RtcWideString; var at: integer): TRtcValueObject;
  var
    typ:TRtcValueTypes;
  begin
  json_skipWhitespace(s,at);
  typ:=json_checkStrType(s,at);
  if typ=rtc_Null then
    begin
    json_skipNull(s,at);
    Result:=nil;
    end
  else
    begin
    Result:=ObjectFromType(typ);
    Result.from_JSON(s,at);
    end;
  json_skipWhitespace(s,at);
  end;

class function TRtcValueObject.ObjectFromJSON(const s:RtcWideString): TRtcValueObject;
  var
    at:integer;
  begin
  at:=0;
  Result:=ObjectFromJSON(s,at);
  if at<>length(s) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcValueObject.ObjectFromXMLRPC(const s: RtcString): TRtcValueObject;
  var
    at:integer;
  begin
  at:=0;
  Result:=ObjectFromXMLRPC(s,at);
  if at<>length(s) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcValueObject.from_Code(const s: RtcString);
  var
    at:integer;
  begin
  at:=0;
  from_Code(s,at);
  if at<>length(s) then
    raise ERtcInfo.Create('String contains more data than expected.');
  end;

procedure TRtcValueObject.from_JSON(const s:RtcWideString);
  var
    at:integer;
  begin
  at:=0;
  from_JSON(s,at);
  if at<>length(s) then
    raise ERtcInfo.Create('String contains more data than expected.');
  end;

procedure TRtcValueObject.from_XMLRPC(const s: RtcString);
  var
    at:integer;
  begin
  at:=0;
  from_XMLRPC(s,at);
  if at<>length(s) then
    raise ERtcInfo.Create('String contains more data than expected.');
  end;

function TRtcValueObject.toXMLrpcRequest: RtcString;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    s.Add('<?xml version="1.0" encoding="UTF-8"?>'#13#10);
    to_XMLRPC(s);

    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toXMLrpcRequestEx: RtcByteArray;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    s.Add('<?xml version="1.0" encoding="UTF-8"?>'#13#10);
    to_XMLRPC(s);

    Result:=s.GetEx;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toXMLrpcResponse: RtcString;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    if GetType=rtc_Exception then
      begin
      s.Add('<?xml version="1.0" encoding="UTF-8"?>'#13#10'<methodResponse><fault>'#13#10);
      to_XMLRPC(s);
      s.Add(#13#10'</fault></methodResponse>');
      end
    else
      begin
      s.Add('<?xml version="1.0" encoding="UTF-8"?>'#13#10'<methodResponse><params><param>'#13#10);
      to_XMLRPC(s);
      s.Add(#13#10'</param></params></methodResponse>');
      end;

    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toXMLrpcResponseEx: RtcByteArray;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    if GetType=rtc_Exception then
      begin
      s.Add('<?xml version="1.0" encoding="UTF-8"?>'#13#10'<methodResponse><fault>'#13#10);
      to_XMLRPC(s);
      s.Add(#13#10'</fault></methodResponse>');
      end
    else
      begin
      s.Add('<?xml version="1.0" encoding="UTF-8"?>'#13#10'<methodResponse><params><param>'#13#10);
      to_XMLRPC(s);
      s.Add(#13#10'</param></params></methodResponse>');
      end;

    Result:=s.GetEx;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toCode: RtcString;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    to_Code(s);
    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toCodeEx: RtcByteArray;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    to_Code(s);
    Result:=s.GetEx;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toJSON: RtcString;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    to_JSON(s);
    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toJSONEx: RtcByteArray;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    to_JSON(s);
    Result:=s.GetEx;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toXMLrpc: RtcString;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    to_XMLRPC(s);
    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toXMLrpcEx: RtcByteArray;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    to_XMLRPC(s);
    Result:=s.GetEx;
  finally
    s.Free;
    end;
  end;

procedure TRtcValueObject.Extracted;
  begin
  // standard implementation - do nothing
  end;

{ TRtcSimpleValue }

{$WARNINGS OFF}

function TRtcSimpleValue.GetBoolean: boolean;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Boolean.');
  end;

function TRtcSimpleValue.GetCurrency: Currency;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Currency.');
  end;

function TRtcSimpleValue.GetDateTime: TDateTime;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to TDateTime.');
  end;

function TRtcSimpleValue.GetException:RtcWideString;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to TRtcExceptionValue.');
  end;

function TRtcSimpleValue.GetVarName:RtcWideString;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to TRtcVariableName.');
  end;

function TRtcSimpleValue.GetInteger: rtcInteger;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Integer.');
  end;

function TRtcSimpleValue.GetCardinal: rtcCardinal;
  begin
  Result:=GetLargeInt;
  end;

function TRtcSimpleValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=GetInteger;
  end;

function TRtcSimpleValue.GetFloat: rtcFloat;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Float.');
  end;

function TRtcSimpleValue.GetString: RtcString;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to String.');
  end;

function TRtcSimpleValue.GetWideString: RtcWideString;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to WideString.');
  end;

function TRtcSimpleValue.GetText:RtcWideString;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Text.');
  end;

function TRtcSimpleValue.GetByteArray: RtcByteArray;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to RtcByteArray.');
  end;

procedure TRtcSimpleValue.SetBoolean(const Value: boolean);
  begin
  raise EConvertError.Create('Can not convert Boolean to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetCurrency(const Value: Currency);
  begin
  raise EConvertError.Create('Can not convert Currency to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetDateTime(const Value: TDateTime);
  begin
  raise EConvertError.Create('Can not convert TDateTime to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetException(const Value:RtcWideString);
  begin
  raise EConvertError.Create('Can not convert TRtcExceptionValue to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetVarName(const Value:RtcWideString);
  begin
  raise EConvertError.Create('Can not convert TRtcVariableName to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetInteger(const Value: rtcInteger);
  begin
  raise EConvertError.Create('Can not convert Integer to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetCardinal(const Value: rtcCardinal);
  begin
  SetLargeInt(Value);
  end;

procedure TRtcSimpleValue.SetLargeInt(const Value: rtcLargeInt);
  begin
  SetInteger(Value);
  end;

procedure TRtcSimpleValue.SetNull(const Value: boolean);
  begin
  raise ERtcInfo.Create('Can not re-set '+ClassName+' to NULL.');
  end;

procedure TRtcSimpleValue.SetFloat(const Value: rtcFloat);
  begin
  raise EConvertError.Create('Can not convert Float to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetString(const Value: RtcString);
  begin
  raise EConvertError.Create('Can not convert String to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetWideString(const Value: RtcWideString);
  begin
  SetString(Utf8Encode(Value));
  end;

procedure TRtcSimpleValue.SetText(const Value:RtcWideString);
  begin
  SetString(Utf8Encode(Value));
  end;

procedure TRtcSimpleValue.SetByteArray(const Value: RtcByteArray);
  begin
  raise EConvertError.Create('Can not convert RtcByteArray to '+ClassName+'.');
  end;

function TRtcSimpleValue.GetVariant: Variant;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Variant.');
  end;

function TRtcSimpleValue.SetVariant(const Value: Variant): boolean;
  begin
  raise EConvertError.Create('Can not convert Variant to '+ClassName+'.');
  end;

function TRtcSimpleValue.GetByteStream: TStream;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to TStream.');
  end;

procedure TRtcSimpleValue.SetByteStream(const Value: TStream);
  begin
  raise EConvertError.Create('Can not convert TStream to '+ClassName+'.');
  end;

function TRtcSimpleValue.GetOID: TRtcObjectID;
  begin
  Result:=GetLargeInt;
  end;

procedure TRtcSimpleValue.SetOID(const Value: TRtcObjectID);
  begin
  SetLargeInt(Value);
  end;

function TRtcSimpleValue.GetLinkedObject: TObject;
  begin
  Result:=GetRtcObjectManager.FindObject(GetLargeInt);
  end;

procedure TRtcSimpleValue.SetLinkedObject(const Value: TObject);
  begin
  SetLargeInt(GetRtcObjectManager.FindOID(Value));
  end;

{$WARNINGS ON}

procedure TRtcSimpleValue.Extracted;
  begin
  // simple value objects - destroy the container
  {$IFNDEF NEXTGEN}Free;{$ENDIF}
  end;

{ TRtcExceptionValue }

constructor TRtcExceptionValue.Create;
  begin
  inherited;
  FValue:='';
  end;

constructor TRtcExceptionValue.Create(const Value:RtcWideString);
  begin
  inherited Create;
  FValue:=Value;
  end;

destructor TRtcExceptionValue.Destroy;
  begin
  try
    FValue:='';
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcExceptionValue.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcExceptionValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Exception;
  end;

function TRtcExceptionValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_EXCEPTION_TYPES;
  end;

procedure TRtcExceptionValue.SetException(const Value:RtcWideString);
  begin
  FValue:=Value;
  end;

function TRtcExceptionValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcExceptionValue.SetVariant(const Value: Variant): boolean;
  begin
  Result:=False;
  end;

function TRtcExceptionValue.GetException:RtcWideString;
  begin
  Result:=FValue;
  end;

function TRtcExceptionValue.GetString: RtcString;
  begin
  Result:=RtcString(FValue);
  end;

function TRtcExceptionValue.GetByteArray: RtcByteArray;
  begin
  Result:=Utf8EncodeEx(FValue);
  end;

function TRtcExceptionValue.GetText:RtcWideString;
  begin
  Result:=FValue;
  end;

function TRtcExceptionValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(FValue);
  end;

class function TRtcExceptionValue.NullValue:RtcWideString;
  begin
  Result:='';
  end;

procedure TRtcExceptionValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcExceptionValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcExceptionValue(Value).FValue;
  end;

function TRtcExceptionValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcExceptionValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcExceptionValue.to_Code(const Result:TRtcHugeString);
  begin
  try
  Result.Add( code_toLongString(RTC_TYPE2STR_CONV[GetType], Utf8Encode(FValue) ) );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcExceptionValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcExceptionValue.to_JSON(const Result: TRtcHugeString);
  begin
  if RTC_JSON_GenTypedException then
    Result.Add(RTC_JSON_ExceptionStr)
  else
    Result.Add('"');
  Result.Add(JSON_EncodeString(FValue));
  Result.Add('"');
  end;

procedure TRtcExceptionValue.to_XMLRPC(const Result:TRtcHugeString);
  var
    fcode,fstring:RtcWideString;
    a:integer;
  begin
  if length(FValue)=0 then
    begin
    fcode:='0';
    fstring:=FValue;
    end
  else if FValue[1]='#' then
    begin
    a:=Pos(':',FValue);
    if a<=0 then // only the code, no String.
      begin
      fcode:=Copy(FValue,2,length(FValue)-1);
      if fcode='' then
        fstring:=FValue
      else
        fstring:='';
      end
    else if a>2 then // code and String
      begin
      fcode:=Copy(FValue,2,a-2);
      fstring:=Copy(FValue,a+1,length(FValue)-a);
      end
    else // String starts with an empty '#:' - no code
      begin
      fcode:='';
      fstring:=FValue;
      end;

    for a:=1 to length(fcode) do
      case fcode[a] of
        '-','+','0'..'9':
          begin
          end;
        else
          begin
          fcode:='';
          fstring:=FValue;
          Break;
          end;
        end;

    if fcode='' then fcode:='0';
    end
  else
    begin
    fcode:='0';
    fstring:=FValue;
    end;

  Result.Add('<value><struct>'#13#10'<member><name>faultCode</name><value><int>');
  Result.Add( RtcString(fcode) );
  Result.Add('</int></value></member>'#13#10'<member><name>faultString</name><value><string>');
  Result.Add( xmlrpc_writeString(Utf8Encode(fstring)) );
  Result.Add('</string></value></member>'#13#10'</struct></value>');
  end;

procedure TRtcExceptionValue.from_Code(const s: RtcString; var at:integer);
  begin
  FValue:=Utf8Decode(code_fromLongString(RTC_TYPE2STR_CONV[GetType], s, at));
  end;

procedure TRtcExceptionValue.from_JSON(const s:RtcWideString; var at: integer);
  begin
  FValue:=json_readString(s, at, json_checkTag(RTC_JSON_ExceptionStr,s,at,true));
  end;

procedure TRtcExceptionValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    s1:RtcString;
    xtag, fcode,fstring:RtcString;
    xval:TRtcValueObject;
    c_tag:RtcString;
  begin
  fcode:='';
  fstring:='';
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRUCT',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    c_tag:=xmlrpc_FirstCloseTag(tags);

    xtag:=Upper_Case(xmlrpc_checkTag(s,at));
    if xtag='MEMBER' then
      begin
      repeat
        xmlrpc_skipTag(s,at); // <member>

        xmlrpc_readTag(s,at,'NAME');
        s1:=Upper_Case(xmlrpc_readTrimValue(s,at));
        xmlrpc_readTag(s,at,'/NAME');

        if s1='FAULTCODE' then
          begin
          s1:=Upper_Case(xmlrpc_checkTag(s,at));
          if s1<>'/MEMBER' then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            if assigned(xval) then
              begin
              try
                if isSimpleValue(xval) then
                  fcode:=TRtcSimpleValue(xval).GetString
                else
                  raise ERtcInfo.Create('XML-RPC Error parsing "fault": faultCode value expected, complex object found.');
              finally
                RtcFreeAndNil(xval);
                end;
              end;
            end;
          end
        else if s1='FAULTSTRING' then
          begin
          s1:=Upper_Case(xmlrpc_checkTag(s,at));
          if s1<>'/MEMBER' then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            if assigned(xval) then
              begin
              try
                if isSimpleValue(xval) then
                  fstring:=TRtcSimpleValue(xval).GetString
                else
                  raise ERtcInfo.Create('XML-RPC Error parsing "fault": faultString value expected, complex object found.');
              finally
                RtcFreeAndNil(xval);
                end;
              end;
            end;
          end
        else
          raise ERtcInfo.Create('XML-RPC Error: "faultCode" or "faultString" expected, but "'+String(s1)+'" found.');
        xmlrpc_readTag(s,at,'/MEMBER');

        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        until xtag<>'MEMBER';
      end
    else if xtag='NAME' then
      begin
      repeat
        xmlrpc_readTag(s,at,'NAME');
        s1:=Upper_Case(xmlrpc_readTrimValue(s,at));
        xmlrpc_readTag(s,at,'/NAME');

        if s1='FAULTCODE' then
          begin
          s1:=Upper_Case(xmlrpc_checkTag(s,at));
          if (s1<>c_tag) and (s1<>'NAME') then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            if assigned(xval) then
              begin
              try
                if isSimpleValue(xval) then
                  fcode:=TRtcSimpleValue(xval).GetString
                else
                  raise ERtcInfo.Create('XML-RPC Error parsing "fault": faultCode value expected, complex object found.');
              finally
                RtcFreeAndNil(xval);
                end;
              end;
            end;
          end
        else if s1='FAULTSTRING' then
          begin
          s1:=Upper_Case(xmlrpc_checkTag(s,at));
          if (s1<>c_tag) and (s1<>'NAME') then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            if assigned(xval) then
              begin
              try
                if isSimpleValue(xval) then
                  fstring:=TRtcSimpleValue(xval).GetString
                else
                  raise ERtcInfo.Create('XML-RPC Error parsing "fault": faultString value expected, complex object found.');
              finally
                RtcFreeAndNil(xval);
                end;
              end;
            end;
          end
        else
          raise ERtcInfo.Create('XML-RPC Error: "faultCode" or "faultString" expected, but "'+String(s1)+'" found.');

        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        until xtag<>'NAME';
      end;

    if (fcode<>'') and (fcode<>'0') then
      FValue:='#'+String(fcode)+':'+Utf8Decode(fstring)
    else
      FValue:=Utf8Decode(fstring);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

{ TRtcByteStream }

constructor TRtcByteStream.Create;
  begin
  inherited;
  FValue:=nil;
  end;

destructor TRtcByteStream.Destroy;
  begin
  try
    RtcFreeAndNil(FValue);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcByteStream.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

class function TRtcByteStream.NullValue: TStream;
  begin
  Result:=nil;
  end;

function TRtcByteStream.GetType: TRtcValueTypes;
  begin
  Result:=rtc_ByteStream;
  end;

procedure TRtcByteStream.CopyFrom(Value: TRtcValueObject);
  var
    bs:TStream;
    loc:int64;
  begin
  if assigned(FValue) then
    FValue.Size:=0
  else
    FValue:=rtcByteStream.Create;

  bs:=TRtcByteStream(Value).GetByteStream;
  loc:=bs.Position;
  try
    bs.Position:=0;
    FValue.CopyFrom(bs,bs.Size);
    FValue.Position:=loc;
  finally
    bs.Position:=loc;
    end;
  end;

function TRtcByteStream.copyOf: TRtcValueObject;
  begin
  Result:=TRtcByteStream.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcByteStream.from_Code(const s: RtcString; var at: integer);
  begin
  if assigned(FValue) then
    FValue.Size:=0
  else
    FValue:=rtcByteStream.Create;
  code_fromByteStream(RTC_TYPE2STR_CONV[GetType],s,at,FValue);
  end;

procedure TRtcByteStream.from_JSON(const s:RtcWideString; var at: integer);
  begin
  if assigned(FValue) then
    FValue.Size:=0
  else
    FValue:=rtcByteStream.Create;

  json_readByteStream(s,at,FValue);
  end;

procedure TRtcByteStream.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  if assigned(FValue) then
    FValue.Size:=0
  else
    FValue:=rtcByteStream.Create;

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('BASE64',s,at,tags);

    xmlrpc_readByteStream(s,at,FValue);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcByteStream.to_Code(const Result:TRtcHugeString);
  begin
  try
    Result.Add( code_toByteStream(RTC_TYPE2STR_CONV[GetType], FValue) );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcByteStream.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcByteStream.to_JSON(const Result: TRtcHugeString);
  begin
  if RTC_JSON_GenTypedByteStream then
    Result.Add(RTC_JSON_ByteStreamStr)
  else
    Result.Add('"');
  Result.Add(json_writeByteStream(FValue));
  Result.Add('"');
  end;

procedure TRtcByteStream.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><base64>');
  Result.Add(xmlrpc_writeByteStream(FValue));
  Result.Add('</base64></value>');
  end;

function TRtcByteStream.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_BYTESTREAM_TYPES;
  end;

function TRtcByteStream.GetByteStream: TStream;
  begin
  if not assigned(FValue) then
    FValue:=rtcByteStream.Create;
  Result:=FValue;
  end;

function TRtcByteStream.GetString: RtcString;
  var
    loc:int64;
  {$IFNDEF RTC_BYTESTRING}
    data:RtcByteArray;
  {$ENDIF}
  begin
  if not assigned(FValue) then
    SetLength(Result,0)
  else if FValue.Size=0 then
    SetLength(Result,0)
  else
    begin
  {$IFDEF RTC_BYTESTRING}
    SetLength(Result, FValue.Size);
  {$ELSE}
    SetLength(data, FValue.Size);
  {$ENDIF}
    loc:=FValue.Position;
    try
      FValue.Position:=0;
    {$IFDEF RTC_BYTESTRING}
      FValue.Read(Result[1],length(Result));
    {$ELSE}
      FValue.Read(data[0],length(data));
    {$ENDIF}
    finally
      FValue.Position:=loc;
      end;
  {$IFNDEF RTC_BYTESTRING}
    Result:= RtcBytesToString(data);
    SetLength(data,0);
  {$ENDIF}
    end;
  end;

function TRtcByteStream.GetText:RtcWideString;
  begin
  Result:=RtcWideString(GetString);
  end;

function TRtcByteStream.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(GetString);
  end;

function TRtcByteStream.GetVariant: Variant;
  begin
  Result:=GetString;
  end;

function TRtcByteStream.GetByteArray: RtcByteArray;
  var
    loc:int64;
  begin
  if not assigned(FValue) then
    SetLength(Result,0)
  else if FValue.Size=0 then
    SetLength(Result,0)
  else
    begin
    SetLength(Result, FValue.Size);
    loc:=FValue.Position;
    try
      FValue.Position:=0;
      FValue.Read(Result[0],length(Result));
    finally
      FValue.Position:=loc;
      end;
    end;
  end;

procedure TRtcByteStream.SetByteStream(const Value: TStream);
  var
    loc:int64;
  begin
  if Value=NullValue then
    RtcFreeAndNil(FValue)
  else if (Value<>FValue) then
    begin
    if assigned(FValue) then
      FValue.Size:=0
    else
      FValue:=rtcByteStream.Create;

    loc:=Value.Position;
    Value.Position:=0;
    try
      FValue.CopyFrom(Value,Value.Size);
      FValue.Position:=loc;
    finally
      Value.Position:=loc;
      end;
    end;
  end;

procedure TRtcByteStream.SetByteArray(const Value: RtcByteArray);
  begin
  if length(Value)=0 then
    RtcFreeAndNil(FValue)
  else
    begin
    if assigned(FValue) then
      FValue.Size:=0
    else
      FValue:=rtcByteStream.Create;
    FValue.Write(Value[0],length(Value));
    FValue.Position:=0;
    end;
  end;

procedure TRtcByteStream.SetNull(const Value: boolean);
  begin
  if Value then
    RtcFreeAndNil(FValue);
  end;

constructor TRtcByteStream.Create(const Value: TStream);
  begin
  inherited Create;
  FValue:=Value;
  end;

procedure TRtcByteStream.Extracted;
  begin
  // remove pointer to the Byte Stream, so it isn't destroyed with the container
  FValue:=nil;
  // before it is destroyed
  inherited;
  end;

{ TRtcByteArray }

constructor TRtcByteArray.Create;
  begin
  inherited;
  FValue:=nil;
  end;

destructor TRtcByteArray.Destroy;
  begin
  try
    FValue:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcByteArray.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

class function TRtcByteArray.NullValue: RtcByteArray;
  begin
  Result:=nil;
  end;

function TRtcByteArray.GetType: TRtcValueTypes;
  begin
  Result:=rtc_ByteArray;
  end;

procedure TRtcByteArray.CopyFrom(Value: TRtcValueObject);
  var
    ba:RtcByteArray;
  begin
  ba:=TRtcByteArray(Value).GetByteArray;
  if assigned(ba) then
    FValue:=Copy(ba,0,length(ba))
  else
    FValue:=nil;
  end;

function TRtcByteArray.copyOf: TRtcValueObject;
  begin
  Result:=TRtcByteArray.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcByteArray.from_Code(const s: RtcString; var at: integer);
  begin
  FValue:=code_fromByteArray(RTC_TYPE2STR_CONV[GetType],s,at);
  end;

procedure TRtcByteArray.from_JSON(const s:RtcWideString; var at: integer);
  begin
  FValue:=json_readByteArray(s,at);
  end;

procedure TRtcByteArray.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('BASE64',s,at,tags);

    FValue:=xmlrpc_readByteArray(s,at);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcByteArray.to_Code(const Result:TRtcHugeString);
  begin
  try
    Result.Add( code_toByteArray(RTC_TYPE2STR_CONV[GetType], FValue) );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcByteArray.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcByteArray.to_JSON(const Result: TRtcHugeString);
  begin
  if RTC_JSON_GenTypedByteStream then
    Result.Add(RTC_JSON_ByteStreamStr)
  else
    Result.Add('"');
  Result.Add(json_writeByteArray(FValue));
  Result.Add('"');
  end;

procedure TRtcByteArray.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><base64>');
  Result.Add(xmlrpc_writeByteArray(FValue));
  Result.Add('</base64></value>');
  end;

function TRtcByteArray.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_BYTESTREAM_TYPES;
  end;

function TRtcByteArray.GetByteArray: RtcByteArray;
  begin
  if not assigned(FValue) then
    SetLength(FValue,0);
  Result:=FValue;
  end;

function TRtcByteArray.NewByteArray(NewSize:Integer): RtcByteArray;
  begin
  SetLength(FValue,NewSize);
  if NewSize>0 then
    FillChar(FValue[0],NewSize,0);
  Result:=FValue;
  end;

function TRtcByteArray.GetString: RtcString;
  begin
  Result:=RtcBytesToString(FValue);
  end;

function TRtcByteArray.GetText:RtcWideString;
  begin
  Result:=RtcWideString(GetString);
  end;

function TRtcByteArray.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(GetString);
  end;

function TRtcByteArray.GetVariant: Variant;
  begin
  Result:=GetString;
  end;

procedure TRtcByteArray.SetByteArray(const Value: RtcByteArray);
  begin
  if not assigned(Value) then
    FValue:=nil
  else if length(Value)=0 then
    FValue:=nil
  else
    FValue:=Copy(Value,0,length(Value));
  end;

procedure TRtcByteArray.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=nil;
  end;

constructor TRtcByteArray.Create(const Value: RtcByteArray);
  begin
  inherited Create;
  FValue:=Value;
  end;

procedure TRtcByteArray.Extracted;
  begin
  // remove pointer to the Byte Array, so it isn't destroyed with the container
  FValue:=nil;
  // before it is destroyed
  inherited;
  end;

{ TRtcVariableName }

constructor TRtcVariableName.Create;
  begin
  inherited;
  FValue:='';
  end;

destructor TRtcVariableName.Destroy;
  begin
  try
    FValue:='';
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcVariableName.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcVariableName.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Variable;
  end;

function TRtcVariableName.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_VARIABLE_TYPES;
  end;

function TRtcVariableName.GetVarName:RtcWideString;
  begin
  Result:=FValue;
  end;

function TRtcVariableName.GetString: RtcString;
  begin
  Result:=RtcString(FValue);
  end;

function TRtcVariableName.GetText:RtcWideString;
  begin
  Result:=FValue;
  end;

function TRtcVariableName.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(FValue);
  end;

function TRtcVariableName.GetByteArray: RtcByteArray;
  begin
  Result:=Utf8EncodeEx(FValue);
  end;

class function TRtcVariableName.NullValue:RtcWideString;
  begin
  Result:='';
  end;

procedure TRtcVariableName.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcVariableName.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcVariableName(Value).FValue;
  end;

function TRtcVariableName.copyOf: TRtcValueObject;
  begin
  Result:=TRtcVariableName.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcVariableName.to_Code(const Result:TRtcHugeString);
  begin
  try
    Result.Add( code_toLongString(RTC_TYPE2STR_CONV[GetType], Utf8Encode(FValue)) );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcVariableName.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcVariableName.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add('"');
  Result.Add(JSON_EncodeString(FValue) );
  Result.Add('"');
  end;

procedure TRtcVariableName.to_XMLRPC(const Result:TRtcHugeString);
  begin
{$IFNDEF RTC_XMLRPC_VALUESTRING}
  Result.Add('<value><string>');
{$ELSE}
  Result.Add('<value>');
{$ENDIF}

  Result.Add(xmlrpc_writeNameString(FValue));

{$IFNDEF RTC_XMLRPC_VALUESTRING}
  Result.Add('</string></value>');
{$ELSE}
  Result.Add('</value>');
{$ENDIF}
  end;

procedure TRtcVariableName.from_Code(const s: RtcString; var at:integer);
  begin
  FValue:=Utf8Decode(code_fromLongString(RTC_TYPE2STR_CONV[GetType], s, at));
  end;

procedure TRtcVariableName.from_JSON(const s:RtcWideString; var at: integer);
  begin
  FValue:=json_readString(s,at);
  end;

procedure TRtcVariableName.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRING',s,at,tags);

    FValue:=xmlrpc_readNameString(s,at);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcVariableName.SetVarName(const Value:RtcWideString);
  begin
  FValue:=Value;
  end;

function TRtcVariableName.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcVariableName.SetVariant(const Value: Variant): boolean;
  begin
  Result:=False;
  end;

constructor TRtcVariableName.Create(const Value:RtcWideString);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcBooleanValue }

constructor TRtcBooleanValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcBooleanValue.Create(Value: boolean);
  begin
  inherited Create;
  FValue:=Value;
  end;

function TRtcBooleanValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Boolean;
  end;

function TRtcBooleanValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_BOOLEAN_TYPES;
  end;

function TRtcBooleanValue.GetBoolean: boolean;
  begin
  Result:=FValue;
  end;

function TRtcBooleanValue.GetCurrency: Currency;
  begin
  if FValue then
    Result:=1
  else
    Result:=0;
  end;

function TRtcBooleanValue.GetInteger: rtcInteger;
  begin
  if FValue then
    Result:=1
  else
    Result:=0;
  end;

function TRtcBooleanValue.GetCardinal: rtcCardinal;
  begin
  if FValue then
    Result:=1
  else
    Result:=0;
  end;

function TRtcBooleanValue.GetFloat: rtcFloat;
  begin
  if FValue then
    Result:=1
  else
    Result:=0;
  end;

function TRtcBooleanValue.GetString: RtcString;
  begin
  if FValue then
    Result:='T'
  else
    Result:='F';
  end;

function TRtcBooleanValue.GetText:RtcWideString;
  begin
  if FValue then
    Result:='T'
  else
    Result:='F';
  end;

function TRtcBooleanValue.GetWideString: RtcWideString;
  begin
  if FValue then
    Result:='T'
  else
    Result:='F';
  end;

function TRtcBooleanValue.GetByteArray: RtcByteArray;
  begin
  if FValue then
    begin
    SetLength(Result,1);
    Result[0]:=1;
    end
  else
    begin
    SetLength(Result,1);
    Result[0]:=0;
    end;
  end;

class function TRtcBooleanValue.NullValue: boolean;
  begin
  Result:=False;
  end;

procedure TRtcBooleanValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcBooleanValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcBooleanValue(Value).FValue;
  end;

function TRtcBooleanValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcBooleanValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcBooleanValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if FValue then
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], 'T') )
    else
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], '') );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcBooleanValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcBooleanValue.to_JSON(const Result: TRtcHugeString);
  begin
  if FValue then
    Result.Add('true')
  else
    Result.Add('false');
  end;

procedure TRtcBooleanValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  if FValue then
    Result.Add('<value><boolean>1</boolean></value>')
  else
    Result.Add('<value><boolean>0</boolean></value>');
  end;

procedure TRtcBooleanValue.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
  begin
  data:=Upper_Case(code_fromShortString(RTC_TYPE2STR_CONV[GetType], s, at));
  if (data='') or (data='0') or
     (data='F') or (data='N') or
     (data='FALSE') or (data='NO') then
    FValue:=False
  else
    FValue:=True;
  end;

procedure TRtcBooleanValue.from_JSON(const s:RtcWideString; var at: integer);
  begin
  if s[at+1]='t' then
    begin
    json_skipTag('true',s,at);
    FValue:=True;
    end
  else if s[at+1]='f' then
    begin
    json_skipTag('false',s,at);
    FValue:=False;
    end
  else
    raise ERtcInfo.Create('JSON Error: Invalid Value (boolean).');
  end;

procedure TRtcBooleanValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:RtcString;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('BOOLEAN',s,at,tags);

    data:=Upper_Case(xmlrpc_readTrimValue(s,at));
    if (data='') or (data='0') or
       (data='F') or (data='N') or
       (data='FALSE') or (data='NO') then
      FValue:=False
    else
      FValue:=True;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcBooleanValue.SetBoolean(const Value: boolean);
  begin
  FValue:=Value;
  end;

function TRtcBooleanValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcBooleanValue.SetVariant(const Value: Variant): boolean;
  begin
  if TVarData(Value).VType=varBoolean then
    begin
    FValue:=Value;
    Result:=True;
    end
  else
    Result:=False;
  end;

{ TRtcStringValue }

constructor TRtcStringValue.Create;
  begin
  inherited;
  FValue:='';
  end;

constructor TRtcStringValue.Create(const Value: RtcString);
  begin
  inherited Create;
  FValue:=Value;
  end;

destructor TRtcStringValue.Destroy;
  begin
  try
    FValue:='';
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcStringValue.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcStringValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_String;
  end;

function TRtcStringValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_STRING_TYPES;
  end;

function TRtcStringValue.GetBoolean: boolean;
  var
    v:RtcString;
  begin
  v:=Upper_Case(FValue);
  if (v='') or (v='0') or
     (v='F') or (v='N') or
     (v='FALSE') or (v='NO') then
    Result:=False
  else
    Result:=True;
  end;

function TRtcStringValue.GetCurrency: Currency;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Curr(FValue);
  end;

function TRtcStringValue.GetDateTime: TDateTime;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2DateTime(FValue);
  end;

function TRtcStringValue.GetInteger: rtcInteger;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Int(FValue);
  end;

function TRtcStringValue.GetCardinal: rtcCardinal;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2LWord(FValue);
  end;

function TRtcStringValue.GetLargeInt: rtcLargeInt;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Int64(FValue);
  end;

function TRtcStringValue.GetFloat: rtcFloat;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Float(FValue);
  end;

function TRtcStringValue.GetString: RtcString;
  begin
  Result:=FValue;
  end;

function TRtcStringValue.GetText:RtcWideString;
  begin
  Result:=RtcWideString(FValue);
  end;

function TRtcStringValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(FValue);
  end;

function TRtcStringValue.GetByteArray: RtcByteArray;
  begin
  Result:=RtcStringToBytes(FValue);
  end;

class function TRtcStringValue.NullValue: RtcString;
  begin
  Result:='';
  end;

procedure TRtcStringValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcStringValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcStringValue(Value).FValue;
  end;

function TRtcStringValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcStringValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcStringValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    Result.Add( code_toLongString(RTC_TYPE2STR_CONV[GetType], FValue) );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcStringValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcStringValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add('"');
  Result.Add(JSON_EncodeString(RtcWideString(FValue)));
  Result.Add('"');
  end;

procedure TRtcStringValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
{$IFNDEF RTC_XMLRPC_VALUESTRING}
  Result.Add('<value><string>');
{$ELSE}
  Result.Add('<value>');
{$ENDIF}

  Result.Add(xmlrpc_writeString(FValue));

{$IFNDEF RTC_XMLRPC_VALUESTRING}
  Result.Add('</string></value>');
{$ELSE}
  Result.Add('</value>');
{$ENDIF}
  end;

procedure TRtcStringValue.from_Code(const s: RtcString; var at:integer);
  begin
  FValue:=code_fromLongString(RTC_TYPE2STR_CONV[GetType], s, at);
  end;

procedure TRtcStringValue.from_JSON(const s:RtcWideString; var at: integer);
  begin
  FValue:=RtcString(json_readString(s,at));
  end;

procedure TRtcStringValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRING',s,at,tags);

    FValue:=xmlrpc_readString(s,at);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcStringValue.SetString(const Value: RtcString);
  begin
  RtcStringCheck(Value);
  FValue:=Value;
  end;

function TRtcStringValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcStringValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_0}
    varStrArg,
    {$ENDIF}
    varOleStr,varString:
      begin
      FValue:=RtcString(Value);
      Result:=True;
      end
    else
      Result:=False;
    end;
  end;

{ TRtcWideStringValue }

constructor TRtcWideStringValue.Create;
  begin
  inherited;
  FValue:='';
  end;

constructor TRtcWideStringValue.Create(const Value: RtcWideString);
  begin
  inherited Create;
  FValue:=Value;
  end;

destructor TRtcWideStringValue.Destroy;
  begin
  try
    FValue:='';
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcWideStringValue.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcWideStringValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_WideString;
  end;

function TRtcWideStringValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_STRING_TYPES;
  end;

function TRtcWideStringValue.GetBoolean: boolean;
  var
    v:RtcWideString;
  begin
{$IFDEF UNICODE}
  v:=UpperCase(FValue);
{$ELSE}
  v:=UpperCaseStr(FValue);
{$ENDIF}
  if (v='') or (v='0') or
     (v='F') or (v='N') or
     (v='FALSE') or (v='NO') then
    Result:=False
  else
    Result:=True;
  end;

function TRtcWideStringValue.GetCurrency: Currency;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Curr(RtcString(FValue));
  end;

function TRtcWideStringValue.GetDateTime: TDateTime;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2DateTime(RtcString(FValue));
  end;

function TRtcWideStringValue.GetInteger: rtcInteger;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt(FValue);
  end;

function TRtcWideStringValue.GetCardinal: rtcCardinal;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt64(FValue);
  end;

function TRtcWideStringValue.GetLargeInt: rtcLargeInt;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt64(FValue);
  end;

function TRtcWideStringValue.GetFloat: rtcFloat;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Float(RtcString(FValue));
  end;

function TRtcWideStringValue.GetString: RtcString;
  begin
  Result:=RtcString(FValue);
  end;

function TRtcWideStringValue.GetWideString: RtcWideString;
  begin
  Result:=FValue;
  end;

function TRtcWideStringValue.GetText:RtcWideString;
  begin
  Result:=RtcWideString(FValue);
  end;

function TRtcWideStringValue.GetByteArray: RtcByteArray;
  begin
  Result:=Utf8EncodeEx(FValue);
  end;

class function TRtcWideStringValue.NullValue: RtcWideString;
  begin
  Result:='';
  end;

procedure TRtcWideStringValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcWideStringValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcWideStringValue(Value).FValue;
  end;

function TRtcWideStringValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcWideStringValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcWideStringValue.to_Code(const Result:TRtcHugeString);
  var
    str:RtcByteArray;
  begin
  try
    SetLength(str, length(FValue)*SizeOf(RtcWideChar));
    if length(str)>0 then
      Move(FValue[1],str[0],length(str));
    Result.Add( code_toLongString(RTC_TYPE2STR_CONV[GetType], RtcBytesToString(str) ) );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcWideStringValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcWideStringValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add('"');
  Result.Add(JSON_EncodeString(FValue));
  Result.Add('"');
  end;

procedure TRtcWideStringValue.from_Code(const s: RtcString; var at:integer);
  var
  {$IFDEF RTC_BYTESTRING}
    str:RtcString;
  {$ELSE}
    str:RtcByteArray;
  {$ENDIF}
  begin
  {$IFDEF RTC_BYTESTRING}
    str:= code_fromLongString(RTC_TYPE2STR_CONV[GetType], s, at);
  {$ELSE}
    str:= RtcStringToBytes(code_fromLongString(RTC_TYPE2STR_CONV[GetType], s, at));
  {$ENDIF}
  FValue:='';
  SetLength(FValue,length(str) div SizeOf(RtcWideChar));
  if length(str)>0 then
  {$IFDEF RTC_BYTESTRING}
    Move(str[1],FValue[1],length(str));
  {$ELSE}
    Move(str[0],FValue[1],length(str));
  {$ENDIF}
  end;

procedure TRtcWideStringValue.from_JSON(const s:RtcWideString; var at: integer);
  begin
  FValue:=json_readString(s,at);
  end;

procedure TRtcWideStringValue.to_XMLRPC(const Result:TRtcHugeString);
  var
    str:RtcString;
  begin
  str:=Utf8Encode(FValue);
{$IFNDEF RTC_XMLRPC_VALUESTRING}
  Result.Add('<value><string>');
{$ELSE}
  Result.Add('<value>');
{$ENDIF}

  Result.Add(xmlrpc_writeString(str));

{$IFNDEF RTC_XMLRPC_VALUESTRING}
  Result.Add('</string></value>');
{$ELSE}
  Result.Add('</value>');
{$ENDIF}
  str:='';
  end;

procedure TRtcWideStringValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    str:RtcString;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRING',s,at,tags);

    str:=xmlrpc_readString(s,at);
    FValue:=Utf8Decode(str);
    str:='';

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcWideStringValue.SetWideString(const Value: RtcWideString);
  begin
  FValue:=Value;
  end;

procedure TRtcWideStringValue.SetString(const Value: RtcString);
  begin
  RtcStringCheck(Value);
  FValue:=Utf8Decode(Value);
  end;

function TRtcWideStringValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcWideStringValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_0}
    varStrArg,
    {$ENDIF}
    varOleStr,varString{$IFDEF UNICODE},varUString{$ENDIF}:
      begin
      FValue:=Value;
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

{ TRtcTextValue }

constructor TRtcTextValue.Create;
  begin
  inherited;
  FValue:='';
  end;

destructor TRtcTextValue.Destroy;
  begin
  try
    FValue:='';
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcTextValue.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcTextValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Text;
  end;

function TRtcTextValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_STRING_TYPES;
  end;

function TRtcTextValue.GetBoolean: boolean;
  var
    v:RtcWideString;
  begin
{$IFDEF UNICODE}
  v:=UpperCase(FValue);
{$ELSE}
  v:=UpperCaseStr(FValue);
{$ENDIF}
  if (v='') or (v='0') or
     (v='F') or (v='N') or
     (v='FALSE') or (v='NO') then
    Result:=False
  else
    Result:=True;
  end;

function TRtcTextValue.GetCurrency: Currency;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Curr(RtcString(GetText));
  end;

function TRtcTextValue.GetDateTime: TDateTime;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2DateTime(RtcString(GetText));
  end;

function TRtcTextValue.GetInteger: rtcInteger;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt(GetText);
  end;

function TRtcTextValue.GetCardinal: rtcCardinal;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt64(GetText);
  end;

function TRtcTextValue.GetLargeInt: rtcLargeInt;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt64(GetText);
  end;

function TRtcTextValue.GetFloat: rtcFloat;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Float(RtcString(GetText));
  end;

function TRtcTextValue.GetString: RtcString;
  begin
  Result:=RtcString(FValue);
  end;

function TRtcTextValue.GetText:RtcWideString;
  begin
  Result:=FValue;
  end;

function TRtcTextValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(FValue);
  end;

function TRtcTextValue.GetByteArray: RtcByteArray;
  begin
  Result:=Utf8EncodeEx(FValue);
  end;

procedure TRtcTextValue.SetString(const Value: RtcString);
  begin
  RtcStringCheck(Value);
  FValue:=Utf8Decode(Value);
  end;

procedure TRtcTextValue.SetText(const Value:RtcWideString);
  begin
  FValue:=Value;
  end;

procedure TRtcTextValue.SetWideString(const Value: RtcWideString);
  begin
  FValue:=RtcWideString(Value);
  end;

procedure TRtcTextValue.SetByteArray(const Value: RtcByteArray);
  begin
  FValue:=Utf8DecodeEx(Value);
  end;

class function TRtcTextValue.NullValue:RtcWideString;
  begin
  Result:='';
  end;

procedure TRtcTextValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcTextValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcTextValue(Value).FValue;
  end;

function TRtcTextValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcTextValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcTextValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    Result.Add( code_toLongString(RTC_TYPE2STR_CONV[GetType], Utf8Encode(FValue)) );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcTextValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcTextValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add('"');
  Result.Add(JSON_EncodeString(FValue));
  Result.Add('"');
  end;

procedure TRtcTextValue.from_Code(const s: RtcString; var at:integer);
  begin
  FValue:=Utf8Decode( code_fromLongString(RTC_TYPE2STR_CONV[GetType], s, at) );
  end;

procedure TRtcTextValue.from_JSON(const s:RtcWideString; var at: integer);
  begin
  FValue:=json_readString(s,at);
  end;

procedure TRtcTextValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRING',s,at,tags);

    FValue:=Utf8Decode(xmlrpc_readString(s,at));

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcTextValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
{$IFNDEF RTC_XMLRPC_VALUESTRING}
  Result.Add('<value><string>');
{$ELSE}
  Result.Add('<value>');
{$ENDIF}

  Result.Add(xmlrpc_writeString( Utf8Encode(FValue) ));

{$IFNDEF RTC_XMLRPC_VALUESTRING}
  Result.Add('</string></value>');
{$ELSE}
  Result.Add('</value>');
{$ENDIF}
  end;

function TRtcTextValue.GetVariant: Variant;
  begin
  Result:=GetText;
  end;

function TRtcTextValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_0}
    varStrArg,
    {$ENDIF}
    varOleStr,varString{$IFDEF UNICODE},varUString{$ENDIF}:
      begin
      SetText(Value);
      Result:=True;
      end
    else
      Result:=False;
    end;
  end;

constructor TRtcTextValue.Create(const Value:RtcWideString);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcIntegerValue }

function TRtcIntegerValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Integer;
  end;

function TRtcIntegerValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_INTEGER_TYPES;
  end;

function TRtcIntegerValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcIntegerValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetInteger: rtcInteger;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetCardinal: rtcCardinal;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetString: RtcString;
  begin
  Result:=Int2Str(FValue);
  end;

function TRtcIntegerValue.GetText:RtcWideString;
  begin
  Result:=IntToStr(FValue);
  end;

function TRtcIntegerValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(IntToStr(FValue));
  end;

class function TRtcIntegerValue.NullValue: rtcInteger;
  begin
  Result:=0;
  end;

procedure TRtcIntegerValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcIntegerValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcIntegerValue(Value).FValue;
  end;

function TRtcIntegerValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcIntegerValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcIntegerValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if FValue<>0 then
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], Int2Str(FValue)) )
    else
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], '') );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcIntegerValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcIntegerValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add(Int2Str(FValue));
  end;

procedure TRtcIntegerValue.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
  begin
  data:=code_fromShortString(RTC_TYPE2STR_CONV[GetType], s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Int(data);
  end;

procedure TRtcIntegerValue.from_JSON(const s:RtcWideString; var at: integer);
  var
    data:RtcWideString;
  begin
  data:=json_readNumber(s,at);
  if data='' then
    FValue:=0
  else
    FValue:=StrToInt(data);
  end;

procedure TRtcIntegerValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><i4>');
  Result.Add(Int2Str(FValue));
  Result.Add('</i4></value>');
  end;

procedure TRtcIntegerValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:RtcString;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('INT',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=Str2Int(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcIntegerValue.SetInteger(const Value: rtcInteger);
  begin
  FValue:=Value;
  end;

function TRtcIntegerValue.GetVariant: Variant;
  begin
  {$IFDEF IDE_0}
  Result:=LongInt(FValue);
  {$ELSE}
  Result:=FValue;
  {$ENDIF}
  end;

function TRtcIntegerValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_1}
    varLongWord,
    varInt64,
    varShortInt,
    varWord,
    {$ENDIF}
    varByte,
    varSmallint,
    varInteger:
      begin
      {$IFDEF IDE_0}
      FValue:=LongInt(Value);
      {$ELSE}
      FValue:=Value;
      {$ENDIF}
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcIntegerValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcIntegerValue.Create(Value: rtcInteger);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcCardinalValue }

constructor TRtcCardinalValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcCardinalValue.Create(Value: rtcCardinal);
  begin
  inherited Create;
  FValue:=Value;
  end;

function TRtcCardinalValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Cardinal;
  end;

function TRtcCardinalValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_INTEGER_TYPES;
  end;

function TRtcCardinalValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcCardinalValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcCardinalValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcCardinalValue.GetInteger: rtcInteger;
  begin
  Result:=FValue;
  end;

function TRtcCardinalValue.GetCardinal: rtcCardinal;
  begin
  Result:=FValue;
  end;

function TRtcCardinalValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=FValue;
  end;

function TRtcCardinalValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcCardinalValue.GetString: RtcString;
  begin
  Result:=LWord2Str(FValue);
  end;

function TRtcCardinalValue.GetText:RtcWideString;
  begin
  Result:=IntToStr(FValue);
  end;

function TRtcCardinalValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(IntToStr(FValue));
  end;

class function TRtcCardinalValue.NullValue: rtcCardinal;
  begin
  Result:=0;
  end;

procedure TRtcCardinalValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcCardinalValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcCardinalValue(Value).FValue;
  end;

function TRtcCardinalValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcCardinalValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcCardinalValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if FValue<>0 then
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], LWord2Str(FValue)) )
    else
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], '') );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcCardinalValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcCardinalValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add(LWord2Str(FValue));
  end;

procedure TRtcCardinalValue.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
  begin
  data:=code_fromShortString(RTC_TYPE2STR_CONV[GetType], s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2LWord(data);
  end;

procedure TRtcCardinalValue.from_JSON(const s:RtcWideString; var at: integer);
  var
    data:RtcWideString;
  begin
  data:=json_readNumber(s,at);
  if data='' then
    FValue:=0
  else
    FValue:=StrToInt64(data);
  end;

procedure TRtcCardinalValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><i4>');
  Result.Add(LWord2Str(FValue));
  Result.Add('</i4></value>');
  end;

procedure TRtcCardinalValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:RtcString;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('INT',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=Str2LWord(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcCardinalValue.SetInteger(const Value: rtcInteger);
  begin
  FValue:=Value;
  end;

procedure TRtcCardinalValue.SetCardinal(const Value: rtcCardinal);
  begin
  FValue:=Value;
  end;

function TRtcCardinalValue.GetVariant: Variant;
  begin
  {$IFDEF IDE_0}
  Result:=rtcCardinal(FValue);
  {$ELSE}
  Result:=FValue;
  {$ENDIF}
  end;

function TRtcCardinalValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_1}
    varLongWord,
    varInt64,
    varShortInt,
    varWord,
    {$ENDIF}
    varByte,
    varSmallint,
    varInteger:
      begin
      {$IFDEF IDE_0}
      FValue:=rtcCardinal(Value);
      {$ELSE}
      FValue:=Value;
      {$ENDIF}
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

{ TRtcLargeIntValue }

function TRtcLargeIntValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_LargeInt;
  end;

function TRtcLargeIntValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcLargeIntValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetInteger: rtcInteger;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetCardinal: rtcCardinal;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetString: RtcString;
  begin
  Result:=Int2Str(FValue);
  end;

function TRtcLargeIntValue.GetText:RtcWideString;
  begin
  Result:=IntToStr(FValue);
  end;

function TRtcLargeIntValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(IntToStr(FValue));
  end;

class function TRtcLargeIntValue.NullValue: rtcLargeInt;
  begin
  Result:=0;
  end;

procedure TRtcLargeIntValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcLargeIntValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcLargeIntValue(Value).FValue;
  end;

function TRtcLargeIntValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcLargeIntValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcLargeIntValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if FValue<>0 then
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], Int2Str(FValue)) )
    else
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], '') );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcLargeIntValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcLargeIntValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add(Int2Str(FValue));
  end;

procedure TRtcLargeIntValue.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
  begin
  data:=code_fromShortString(RTC_TYPE2STR_CONV[GetType], s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Int64(data);
  end;

procedure TRtcLargeIntValue.from_JSON(const s:RtcWideString; var at: integer);
  var
    data:RtcWideString;
  begin
  data:=json_readNumber(s,at);
  if data='' then
    FValue:=0
  else
    FValue:=StrToInt64(data);
  end;

procedure TRtcLargeIntValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:RtcString;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('INT',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=Str2Int64(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcLargeIntValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><i4>');
  Result.Add(Int2Str(FValue));
  Result.Add('</i4></value>');
  end;

procedure TRtcLargeIntValue.SetInteger(const Value: rtcInteger);
  begin
  FValue:=Value;
  end;

procedure TRtcLargeIntValue.SetCardinal(const Value: rtcCardinal);
  begin
  FValue:=Value;
  end;

procedure TRtcLargeIntValue.SetLargeInt(const Value: rtcLargeInt);
  begin
  FValue:=Value;
  end;

function TRtcLargeIntValue.GetVariant: Variant;
  begin
  {$IFDEF IDE_1}
  Result:=LongInt(FValue);
  {$ELSE}
  Result:=FValue;
  {$ENDIF}
  end;

function TRtcLargeIntValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_1}
    varLongWord,
    varInt64:
      begin
      FValue:=Value;
      Result:=True;
      end;
    {$ENDIF}

    {$IFNDEF IDE_1}
    varShortInt,
    varWord,
    {$ENDIF}
    varByte,
    varSmallint,
    varInteger:
      begin
      {$IFDEF IDE_1}
      FValue:=LongInt(Value);
      {$ELSE}
      FValue:=Value;
      {$ENDIF}
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcLargeIntValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcLargeIntValue.Create(Value: rtcLargeInt);
  begin
  inherited Create;
  FValue:=Value;
  end;

function TRtcLargeIntValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_INTEGER_TYPES;
  end;

{ TRtcOIDValue }

function TRtcOIDValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_OID;
  end;

function TRtcOIDValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_INTEGER_TYPES;
  end;

function TRtcOIDValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcOIDValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcOIDValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcOIDValue.GetInteger: rtcInteger;
  begin
  Result:=FValue;
  end;

function TRtcOIDValue.GetCardinal: rtcCardinal;
  begin
  Result:=FValue;
  end;

function TRtcOIDValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=FValue;
  end;

function TRtcOIDValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcOIDValue.GetString: RtcString;
  begin
  Result:=Int2Str(FValue);
  end;

function TRtcOIDValue.GetText:RtcWideString;
  begin
  Result:=IntToStr(FValue);
  end;

function TRtcOIDValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(IntToStr(FValue));
  end;

class function TRtcOIDValue.NullValue: TRtcObjectID;
  begin
  Result:=RTC_NIL_OBJECT_ID;
  end;

procedure TRtcOIDValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcOIDValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcOIDValue(Value).FValue;
  end;

function TRtcOIDValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcOIDValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcOIDValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if FValue<>0 then
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], Int2Str(FValue)) )
    else
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], '') );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcOIDValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcOIDValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add(Int2Str(FValue));
  end;

procedure TRtcOIDValue.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
  begin
  data:=code_fromShortString(RTC_TYPE2STR_CONV[GetType], s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Int64(data);
  end;

procedure TRtcOIDValue.from_JSON(const s:RtcWideString; var at: integer);
  var
    data:RtcWideString;
  begin
  data:=json_readNumber(s,at);
  if data='' then
    FValue:=0
  else
    FValue:=StrToInt64(data);
  end;

procedure TRtcOIDValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:RtcString;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('INT',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=Str2Int64(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcOIDValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><i4>');
  Result.Add(Int2Str(FValue));
  Result.Add('</i4></value>');
  end;

procedure TRtcOIDValue.SetInteger(const Value: rtcInteger);
  begin
  FValue:=Value;
  end;

procedure TRtcOIDValue.SetCardinal(const Value: rtcCardinal);
  begin
  FValue:=Value;
  end;

procedure TRtcOIDValue.SetLargeInt(const Value: rtcLargeInt);
  begin
  FValue:=Value;
  end;

function TRtcOIDValue.GetVariant: Variant;
  begin
  {$IFDEF IDE_1}
  Result:=LongInt(FValue);
  {$ELSE}
  Result:=FValue;
  {$ENDIF}
  end;

function TRtcOIDValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_1}
    varLongWord,
    varInt64:
      begin
      FValue:=Value;
      try
        SetLargeInt(FValue);
      except
        FValue:=RTC_NIL_OBJECT_ID;
        raise;
        end;
      Result:=True;
      end;
    {$ENDIF}

    {$IFNDEF IDE_1}
    varShortInt,
    varWord,
    {$ENDIF}
    varByte,
    varSmallint,
    varInteger:
      begin
      FValue:=Value;
      try
        SetLargeInt(FValue);
      except
        FValue:=RTC_NIL_OBJECT_ID;
        raise;
        end;
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcOIDValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcOIDValue.Create(Value: TRtcObjectID);
  begin
  inherited Create;
  FValue := Value;
  end;

function TRtcOIDValue.GetOID: TRtcObjectID;
  begin
  Result:=FValue;
  end;

procedure TRtcOIDValue.SetOID(const Value: TRtcObjectID);
  begin
  FValue:=Value;
  end;

function TRtcOIDValue.GetLinkedObject: TObject;
  begin
  Result:= GetRtcObjectManager.FindObject(FValue);
  end;

procedure TRtcOIDValue.SetLinkedObject(const Value: TObject);
  begin
  FValue:= GetRtcObjectManager.FindOID(Value);
  end;

{ TRtcFloatValue }

function TRtcFloatValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Float;
  end;

function TRtcFloatValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcFloatValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcFloatValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcFloatValue.GetInteger: rtcInteger;
  begin
  Result:=round(FValue);
  end;

function TRtcFloatValue.GetCardinal: rtcCardinal;
  begin
  Result:=round(FValue);
  end;

function TRtcFloatValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=round(FValue);
  end;

function TRtcFloatValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcFloatValue.GetString: RtcString;
  begin
  Result:=Float2Str(FValue);
  end;

function TRtcFloatValue.GetText:RtcWideString;
  begin
  Result:=RtcWideString(Float2Str(FValue));
  end;

function TRtcFloatValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(Float2Str(FValue));
  end;

class function TRtcFloatValue.NullValue: rtcFloat;
  begin
  Result:=0;
  end;

procedure TRtcFloatValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcFloatValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcFloatValue(Value).FValue;
  end;

function TRtcFloatValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcFloatValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcFloatValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if FValue<>0 then
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], Float2Str(FValue)) )
    else
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], '') );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcFloatValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcFloatValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add(Float2Str(FValue));
  end;

procedure TRtcFloatValue.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
  begin
  data:=code_fromShortString(RTC_TYPE2STR_CONV[GetType], s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Float(data);
  end;

procedure TRtcFloatValue.from_JSON(const s:RtcWideString; var at: integer);
  var
    data:RtcWideString;
  begin
  data:=json_readNumber(s,at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Float(RtcString(data));
  end;

procedure TRtcFloatValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    begin
    Result.Add('<value><double>');
    Result.Add(Float2Str(FValue));
    Result.Add('</double></value>');
    end
  else
    Result.Add('<value><double>0</double></value>');
  end;

procedure TRtcFloatValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:RtcString;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('DOUBLE',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=Str2Float(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcFloatValue.SetFloat(const Value: rtcFloat);
  begin
  FValue:=Value;
  end;

function TRtcFloatValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcFloatValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    varSingle,
    varDouble:
      begin
      FValue:=Value;
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcFloatValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcFloatValue.Create(Value: rtcFloat);
  begin
  inherited Create;
  FValue:=Value;
  end;

function TRtcFloatValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_FLOAT_TYPES;
  end;

{ TRtcCurrencyValue }

function TRtcCurrencyValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Currency;
  end;

function TRtcCurrencyValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcCurrencyValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcCurrencyValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcCurrencyValue.GetInteger: rtcInteger;
  begin
  Result:=round(FValue);
  end;

function TRtcCurrencyValue.GetCardinal: rtcCardinal;
  begin
  Result:=round(FValue);
  end;

function TRtcCurrencyValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=round(FValue);
  end;

function TRtcCurrencyValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcCurrencyValue.GetString: RtcString;
  begin
  Result:=Curr2Str(FValue);
  end;

function TRtcCurrencyValue.GetText:RtcWideString;
  begin
  Result:=RtcWideString(Curr2Str(FValue));
  end;

function TRtcCurrencyValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(Curr2Str(FValue));
  end;

class function TRtcCurrencyValue.NullValue: Currency;
  begin
  Result:=0;
  end;

procedure TRtcCurrencyValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcCurrencyValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcCurrencyValue(Value).FValue;
  end;

function TRtcCurrencyValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcCurrencyValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcCurrencyValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if FValue<>0 then
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], Curr2Str(FValue)) )
    else
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], '') );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcCurrencyValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcCurrencyValue.to_JSON(const Result: TRtcHugeString);
  begin
  Result.Add(Curr2Str(FValue));
  end;

procedure TRtcCurrencyValue.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
  begin
  data:=code_fromShortString(RTC_TYPE2STR_CONV[GetType], s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Curr(data);
  end;

procedure TRtcCurrencyValue.from_JSON(const s:RtcWideString; var at: integer);
  var
    data:RtcWideString;
  begin
  data:=json_readNumber(s,at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Curr(RtcString(data));
  end;

procedure TRtcCurrencyValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    begin
    Result.Add('<value><double>');
    Result.Add(Curr2Str(FValue));
    Result.Add('</double></value>');
    end
  else
    Result.Add('<value><double>0</double></value>');
  end;

procedure TRtcCurrencyValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:RtcString;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('DOUBLE',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=Str2Curr(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcCurrencyValue.SetCurrency(const Value: Currency);
  begin
  FValue:=Value;
  end;

function TRtcCurrencyValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcCurrencyValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    varCurrency:
      begin
      FValue:=Value;
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcCurrencyValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcCurrencyValue.Create(Value: Currency);
  begin
  inherited Create;
  FValue:=Value;
  end;

function TRtcCurrencyValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_FLOAT_TYPES;
  end;

{ TRtcDateTimeValue }

function TRtcDateTimeValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_DateTime;
  end;

function TRtcDateTimeValue.GetBoolean: boolean;
  begin
  Result:=FValue<>0;
  end;

function TRtcDateTimeValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcDateTimeValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcDateTimeValue.GetInteger: rtcInteger;
  begin
  Result:=trunc(FValue);
  end;

function TRtcDateTimeValue.GetCardinal: rtcCardinal;
  begin
  Result:=trunc(FValue);
  end;

function TRtcDateTimeValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=trunc(FValue);
  end;

function TRtcDateTimeValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcDateTimeValue.GetString: RtcString;
  begin
  Result:=DateTime2Str(FValue);
  end;

function TRtcDateTimeValue.GetText:RtcWideString;
  begin
  Result:=RtcWideString(DateTime2Str(FValue));
  end;

function TRtcDateTimeValue.GetWideString: RtcWideString;
  begin
  Result:=RtcWideString(DateTime2Str(FValue));
  end;

class function TRtcDateTimeValue.NullValue: TDateTime;
  begin
  Result:=0;
  end;

procedure TRtcDateTimeValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcDateTimeValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcDateTimeValue(Value).FValue;
  end;

function TRtcDateTimeValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcDateTimeValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcDateTimeValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if FValue<>0 then
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], DateTime2Str(FValue)) )
    else
      Result.Add( code_toShortString(RTC_TYPE2STR_CONV[GetType], '') );
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcDateTimeValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcDateTimeValue.to_JSON(const Result: TRtcHugeString);
  begin
  if RTC_JSON_GenTypedDateTime then
    begin
    Result.Add(RTC_JSON_DateTimeStr);
    Result.Add(DateTime2JsonDate(FValue));
    Result.Add(')\/"');
    end
  else
    Result.Add('"'+DateTime2Str3(FValue)+'"');
  end;

procedure TRtcDateTimeValue.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
  begin
  data:=code_fromShortString(RTC_TYPE2STR_CONV[GetType], s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2DateTime(data);
  end;

function UnixMSecToDateTime(const AValue: Int64): TDateTime;
  begin
  Result := AValue / MSecsPerDay + UnixDateDelta;
  end;

function DateTimeToUnixMSec(const AValue: TDateTime): Int64;
  begin
  Result := Round((AValue - UnixDateDelta) * MSecsPerDay);
  end;

procedure TRtcDateTimeValue.from_JSON(const s:RtcWideString; var at: integer);
  var
    data:RtcWideString;
  begin
  if json_checkTag(RTC_JSON_DateTimeStr,s,at,true) then
    begin // "JSON" Date
    data:=json_readString(s,at,true);
    if data='' then
      FValue:=0
    else
      FValue:=JSONStr2DateTime(RtcString(data));
    end
  else if json_checkTag(RTC_JSON_DateTimeISOStr,s,at,true) then
    begin // ISO Date
    data:=json_readString(s,at,true);
    if data='' then
      FValue:=0
    else
      FValue:=Str2DateTime(RtcString(data));
    end
  else // ISO Date
    begin
    data:=json_readString(s,at,false);
    if data='' then
      FValue:=0
    else
      FValue:=Str2DateTime(RtcString(data));
    end;
  end;

procedure TRtcDateTimeValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><dateTime.iso8601>');
  Result.Add(DateTime2ISOStr(FValue));
  Result.Add('</dateTime.iso8601></value>');
  end;

procedure TRtcDateTimeValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    data:RtcString;
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('DATETIME.ISO8601',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    FValue:=ISOStr2DateTime(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcDateTimeValue.SetDateTime(const Value: TDateTime);
  begin
  FValue:=Value;
  end;

function TRtcDateTimeValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcDateTimeValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    varDate,
    varSingle,
    varDouble:
      begin
      FValue:=Value;
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcDateTimeValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcDateTimeValue.Create(Value: TDateTime);
  begin
  inherited Create;
  FValue:=Value;
  end;

function TRtcDateTimeValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_FLOAT_TYPES;
  end;

{ TRtcAbsValue }

function TRtcAbsValue.GetNull: boolean;
  begin
  Result:= GetObject=nil;
  end;

function TRtcAbsValue.GetBoolean: boolean;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcBooleanValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetBoolean
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetBoolean
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to Boolean.');
  end;

function TRtcAbsValue.GetCurrency: Currency;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcCurrencyValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetCurrency
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetCurrency
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to Currency.');
  end;

function TRtcAbsValue.GetDateTime: TDateTime;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcDateTimeValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetDateTime
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetDateTime
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to TDateTime.');
  end;

function TRtcAbsValue.GetException:RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcExceptionValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetException
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetException
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to TRtcExceptionValue.');
  end;

function TRtcAbsValue.GetVarName:RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcVariableName.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetVarName
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetVarName
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to TRtcVariableName.');
  end;

function TRtcAbsValue.GetInteger: rtcInteger;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcIntegerValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetInteger
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetInteger
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to Integer.');
  end;

function TRtcAbsValue.GetCardinal: rtcCardinal;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcCardinalValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetCardinal
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetCardinal
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to Cardinal.');
  end;

function TRtcAbsValue.GetLargeInt: rtcLargeInt;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcLargeIntValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetLargeInt
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetLargeInt
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to LargeInt.');
  end;

function TRtcAbsValue.GetOID: TRtcObjectID;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcOIDValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetOID
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetOID
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to a TRtcObjectID.');
  end;

function TRtcAbsValue.GetLinkedObject: TObject;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=RTC_NIL_OBJECT
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetLinkedObject
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetLinkedObject
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to a Linked Object.');
  end;

function TRtcAbsValue.GetFloat: rtcFloat;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcFloatValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetFloat
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetFloat
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to Float.');
  end;

function TRtcAbsValue.GetString: RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcStringValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetString
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetString
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsString
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsString
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsString
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to String.');
  end;

function TRtcAbsValue.GetWideString: RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcWideStringValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetWideString
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetWideString
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsWideString
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsWideString
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsWideString
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to WideString.');
  end;

function TRtcAbsValue.GetText:RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcTextValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetText
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetText
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsText
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsText
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsText
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to Text.');
  end;

function TRtcAbsValue.GetByteArray: RtcByteArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    SetLength(Result,0)
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetByteArray
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetByteArray
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to RtcByteArray.');
  end;

function TRtcAbsValue.GetByteStream: TStream;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=TRtcByteStream.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetByteStream
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetByteStream
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to ByteStream.');
  end;

function TRtcAbsValue.GetArray: TRtcArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    begin
    if AutoCreate then
      Result:=NewArray
    else
      Result:=TRtcArray.NullValue;
    end
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetArray
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to TRtcArray.');
  end;

function TRtcAbsValue.GetRecord: TRtcRecord;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    begin
    if AutoCreate then
      Result:=NewRecord
    else
      Result:=TRtcRecord.NullValue
    end
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetRecord
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to TRtcRecord.');
  end;

function TRtcAbsValue.GetDataSet: TRtcDataSet;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    begin
    if AutoCreate then
      Result:=NewDataSet
    else
      Result:=TRtcDataSet.NullValue;
    end
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetDataSet
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to TRtcDataSet.');
  end;

function TRtcAbsValue.GetFunctionInfo: TRtcFunctionInfo;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    begin
    if AutoCreate then
      Result:=NewFunction
    else
      Result:=TRtcFunctionInfo.NullValue;
    end
  else if gobj is TRtcFunctionInfo then
    Result:=TRtcFunctionInfo(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetFunctionInfo
  else
    raise EConvertError.Create('Unable to convert '+gobj.ClassName+' to TRtcFunctionInfo.');
  end;

procedure TRtcAbsValue.SetNull(const Value: boolean);
  begin
  if Value then
    SetObject(nil,True);
  end;

procedure TRtcAbsValue.SetArray(const Value: TRtcArray);
  begin
  SetObject(Value, True);
  end;

procedure TRtcAbsValue.SetFunctionInfo(const Value: TRtcFunctionInfo);
  begin
  SetObject(Value, True);
  end;

procedure TRtcAbsValue.SetDataSet(const Value: TRtcDataSet);
  begin
  SetObject(Value, True);
  end;

procedure TRtcAbsValue.SetRecord(const Value: TRtcRecord);
  begin
  SetObject(Value, True);
  end;

procedure TRtcAbsValue.SetException(const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetException(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcExceptionValue) then
      begin
      gobj:=TRtcExceptionValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetException',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcExceptionValue(gobj).SetException(Value);
    end;
  end;

procedure TRtcAbsValue.SetVarName(const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetVarName(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcVariableName) then
      begin
      gobj:=TRtcVariableName.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetVarName',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcVariableName(gobj).SetVarName(Value);
    end;
  end;

procedure TRtcAbsValue.SetBoolean(const Value: boolean);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetBoolean(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcBooleanValue) then
      begin
      gobj:=TRtcBooleanValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetBoolean',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcBooleanValue(gobj).SetBoolean(Value);
    end;
  end;

procedure TRtcAbsValue.SetCurrency(const Value: Currency);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetCurrency(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcCurrencyValue) then
      begin
      gobj:=TRtcCurrencyValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetCurrency',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcCurrencyValue(gobj).SetCurrency(Value);
    end;
  end;

procedure TRtcAbsValue.SetDateTime(const Value: TDateTime);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetDateTime(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcDateTimeValue) then
      begin
      gobj:=TRtcDateTimeValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetDateTime',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcDateTimeValue(gobj).SetDateTime(Value);
    end;
  end;

procedure TRtcAbsValue.SetInteger(const Value: rtcInteger);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetInteger(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcIntegerValue) then
      begin
      gobj:=TRtcIntegerValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetInteger',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcIntegerValue(gobj).SetInteger(Value);
    end;
  end;

procedure TRtcAbsValue.SetCardinal(const Value: rtcCardinal);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetCardinal(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcCardinalValue) then
      begin
      gobj:=TRtcCardinalValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetCardinal',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcCardinalValue(gobj).SetCardinal(Value);
    end;
  end;

procedure TRtcAbsValue.SetOID(const Value: TRtcObjectID);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetOID(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcOIDValue) then
      begin
      gobj:=TRtcOIDValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetOID',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcOIDValue(gobj).SetOID(Value);
    end;
  end;

procedure TRtcAbsValue.SetLinkedObject(const Value: TObject);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetLinkedObject(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcOIDValue) then
      begin
      gobj:=TRtcOIDValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetOID',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcOIDValue(gobj).SetLinkedObject(Value);
    end;
  end;

procedure TRtcAbsValue.SetLargeInt(const Value: rtcLargeInt);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetLargeInt(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcLargeIntValue) then
      begin
      gobj:=TRtcLargeIntValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetLargeInt',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcLargeIntValue(gobj).SetLargeInt(Value);
    end;
  end;

procedure TRtcAbsValue.SetFloat(const Value: rtcFloat);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetFloat(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcFloatValue) then
      begin
      gobj:=TRtcFloatValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetFloat',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcFloatValue(gobj).SetFloat(Value);
    end;
  end;

procedure TRtcAbsValue.SetString(const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetString(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcStringValue) then
      begin
      gobj:=TRtcStringValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetString',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcStringValue(gobj).SetString(Value);
    end;
  end;

procedure TRtcAbsValue.SetWideString(const Value: RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetWideString(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcWideStringValue) then
      begin
      gobj:=TRtcWideStringValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetWideString',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcWideStringValue(gobj).SetWideString(Value);
    end;
  end;

procedure TRtcAbsValue.SetText(const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetText(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcTextValue) then
      begin
      gobj:=TRtcTextValue.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetText',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcTextValue(gobj).SetText(Value);
    end;
  end;

procedure TRtcAbsValue.SetVariant(const Value: Variant);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetVariant(Value)
  else
    begin
    if assigned(gobj) then
      if not (gobj is TRtcSimpleValue) then
        raise ERtcInfo.Create('Value already assigned. Set to NULL before assigning another value.')
      else if TRtcSimpleValue(gobj).SetVariant(Value) then
        Exit; // value changed

    gobj:=TRtcValueObject.ObjectFromVariant(Value);
    try
      if gobj<>nil then
        if gobj is TRtcAbsArray then
          TRtcAbsArray(gobj).AutoCreate:=AutoCreate
        else if gobj is TRtcAbsRecord then
          TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
        else if gobj is TRtcAbsValue then
          TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.SetVariant',E,'INFO');
        gobj.Free;
        raise;
        end;
      end;
    end;
  end;

procedure TRtcAbsValue.SetByteArray(const Value: RtcByteArray);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetByteArray(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcByteArray) then
      begin
      gobj:=TRtcByteArray.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetByteArray',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcByteArray(gobj).SetByteArray(Value);
    end;
  end;

procedure TRtcAbsValue.SetByteStream(const Value: TStream);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and (gobj is TRtcValue) then
    TRtcValue(gobj).SetByteStream(Value)
  else
    begin
    if not assigned(gobj) or not (gobj is TRtcByteStream) then
      begin
      gobj:=TRtcByteStream.Create;
      try
        SetObject(gobj);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcAbsValue.SetByteStream',E,'INFO');
          RtcFreeAndNil(gobj);
          raise;
          end;
        end;
      end;
    TRtcByteStream(gobj).SetByteStream(Value);
    end;
  end;

procedure TRtcAbsValue.SetAsObject(const Value: TRtcValueObject);
  begin
  SetObject(Value);
  end;

function TRtcAbsValue.GetVariant: Variant;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if gobj=nil then
    Result:=Null
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetVariant
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetVariant
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Variant.');
  end;

function TRtcAbsValue.NewArray: TRtcArray;
  begin
  Result:=TRtcArray.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.NewArray',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsValue.NewRecord: TRtcRecord;
  begin
  Result:=TRtcRecord.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.NewRecord',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsValue.NewDataSet: TRtcDataSet;
  begin
  Result:=TRtcDataSet.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.NewDataSet',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsValue.NewFunction(const func_name:RtcWideString=''): TRtcFunctionInfo;
  begin
  Result:=TRtcFunctionInfo.Create;
  Result.FunctionName:=func_name;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.NewFunction('+func_name+')',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsValue.NewByteArray(InitialSize:Integer=0): RtcByteArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if (gobj=nil) or not (gobj is TRtcByteArray) then
    begin
    gobj:=TRtcByteArray.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewByteArray',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcByteArray(gobj).SetNull(True);
  Result:=TRtcByteArray(gobj).NewByteArray(InitialSize);
  end;

function TRtcAbsValue.NewBoolean: boolean;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if (gobj=nil) or not (gobj is TRtcBooleanValue) then
    begin
    gobj:=TRtcBooleanValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewBoolean',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcBooleanValue(gobj).SetNull(True);
  Result:=TRtcBooleanValue(gobj).GetBoolean;
  end;

function TRtcAbsValue.NewCurrency: Currency;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcCurrencyValue) then
    begin
    gobj:=TRtcCurrencyValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewCurrency',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcCurrencyValue(gobj).SetNull(True);
  Result:=TRtcCurrencyValue(gobj).GetCurrency;
  end;

function TRtcAbsValue.NewDateTime: TDateTime;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcDateTimeValue) then
    begin
    gobj:=TRtcDateTimeValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewDateTime',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcDateTimeValue(gobj).SetNull(True);
  Result:=TRtcDateTimeValue(gobj).GetDateTime;
  end;

function TRtcAbsValue.NewException:RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcExceptionValue) then
    begin
    gobj:=TRtcExceptionValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewException',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcExceptionValue(gobj).SetNull(True);
  Result:=TRtcExceptionValue(gobj).GetException;
  end;

function TRtcAbsValue.NewVariable:RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcVariableName) then
    begin
    gobj:=TRtcVariableName.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewVariable',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcVariableName(gobj).SetNull(True);
  Result:=TRtcVariableName(gobj).GetVarName;
  end;

function TRtcAbsValue.NewInteger: rtcInteger;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcIntegerValue) then
    begin
    gobj:=TRtcIntegerValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewInteger',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcIntegerValue(gobj).SetNull(True);
  Result:=TRtcIntegerValue(gobj).GetInteger;
  end;

function TRtcAbsValue.NewCardinal: rtcCardinal;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcCardinalValue) then
    begin
    gobj:=TRtcCardinalValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewCardinal',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcCardinalValue(gobj).SetNull(True);
  Result:=TRtcCardinalValue(gobj).GetCardinal;
  end;

function TRtcAbsValue.NewLargeInt: rtcLargeInt;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcLargeIntValue) then
    begin
    gobj:=TRtcLargeIntValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewLargeInt',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcLargeIntValue(gobj).SetNull(True);
  Result:=TRtcLargeIntValue(gobj).GetLargeInt;
  end;

function TRtcAbsValue.NewFloat: rtcFloat;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcFloatValue) then
    begin
    gobj:=TRtcFloatValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewFloat',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcFloatValue(gobj).SetNull(True);
  Result:=TRtcFloatValue(gobj).GetFloat;
  end;

function TRtcAbsValue.NewString: RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcStringValue) then
    begin
    gobj:=TRtcStringValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewString',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcStringValue(gobj).SetNull(True);
  Result:=TRtcStringValue(gobj).GetString;
  end;

function TRtcAbsValue.NewWideString: RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcWideStringValue) then
    begin
    gobj:=TRtcWideStringValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewWideString',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcWideStringValue(gobj).SetNull(True);
  Result:=TRtcWideStringValue(gobj).GetWideString;
  end;

function TRtcAbsValue.NewText:RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcTextValue) then
    begin
    gobj:=TRtcTextValue.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewText',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcTextValue(gobj).SetNull(True);
  Result:=TRtcTextValue(gobj).GetText;
  end;

function TRtcAbsValue.NewByteStream: TStream;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) or not (gobj is TRtcByteStream) then
    begin
    gobj:=TRtcByteStream.Create;
    try
      SetObject(gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsValue.NewByteStream',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcByteStream(gobj).SetNull(True);
  Result:=TRtcByteStream(gobj).GetByteStream;
  end;

procedure TRtcAbsValue.SetCode(const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned. Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromCode(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.SetCode',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

function TRtcAbsValue.GetCode: RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) then
    Result:=nullValueCode
  else
    Result:=gobj.toCode;
  end;

procedure TRtcAbsValue.SetXMLrpc(const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned. Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromXMLrpc(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.SetXMLrpc',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

function TRtcAbsValue.GetXMLrpc: RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) then
    Result:=nullValueXMLrpc
  else
    Result:=gobj.toXMLrpc;
  end;

procedure TRtcAbsValue.SetJSON(const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned. Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromJSON(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.SetJSON',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

function TRtcAbsValue.GetJSON:RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject;
  if not assigned(gobj) then
    Result:=RtcWideString(nullValueJSON)
  else
    Result:=RtcWideString(gobj.toJSON);
  end;


function TRtcAbsValue.CheckType(typ: TRtcValueTypes): boolean;
  begin
  Result:=TypeCheck(typ);
  end;

{ TRtcValue }

constructor TRtcValue.Create;
  begin
  inherited;
  FValue:=nil;
  end;

destructor TRtcValue.Destroy;
  begin
  try
    Clear;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcValue.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcValue.Clear;
  begin
  SetNull(True);
  end;

function TRtcValue.GetType: TRtcValueTypes;
  begin
  if not assigned(FValue) then
    Result:=rtc_Null
  else
    Result:=FValue.GetType;
  end;

function TRtcValue.GetObject:TRtcValueObject;
  begin
  Result:=FValue;
  end;

procedure TRtcValue.SetObject(const pValue: TRtcValueObject; asCopy:boolean=False);
  var
    old:TRtcValueObject;
  begin
  if FValue<>pValue then
    begin
    if assigned(FValue) then
      begin
      if pValue<>nil then
        begin
        if FValue is TRtcSimpleValue then
          begin
          old:=FValue;
          if asCopy then
            FValue:=pValue.CopyOf
          else
            FValue:=pValue;
          old.Free;
          end
        else
          raise ERtcInfo.Create('Value of type '+FValue.ClassName+' allready assigned.'#13#10+
                                 'Set to NULL before assigning a different object.');
        end
      else
        begin
        if asCopy then FValue.Free;
        FValue:=nil;
        end;
      end
    else
      begin
      if asCopy then
        FValue:=pValue.copyOf
      else
        FValue:=pValue;
      end;
    end;
  end;

procedure TRtcValue.CopyFrom(pValue: TRtcValueObject);
  begin
  if not GetNull then
    raise ERtcInfo.Create('Can not merge objects. This Value Object already has data.');
  SetObject(TRtcValue(pValue).GetObject, True);
  end;

function TRtcValue.copyOf: TRtcValueObject;
  begin
  if assigned(FValue) then
    Result:=FValue.copyOf
  else
    Result:=nil;
  end;

procedure TRtcValue.to_Code(const Result:TRtcHugeString);
  begin
  try
    if not assigned(FValue) then
      Result.Add(nullValueCode)
    else
      FValue.to_Code(Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcValue.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcValue.to_JSON(const Result: TRtcHugeString);
  begin
  if not assigned(FValue) then
    Result.Add(RtcString(nullValueJSON))
  else
    FValue.to_JSON(Result);
  end;

procedure TRtcValue.from_Code(const s: RtcString; var at:integer);
  begin
  if assigned(FValue) then
    raise ERtcInfo.Create('Can not merge data. TRtcValue object is already in use.');

  FValue:=ObjectFromCode(s,at);
  end;

procedure TRtcValue.from_JSON(const s:RtcWideString; var at: integer);
  begin
  if assigned(FValue) then
    raise ERtcInfo.Create('Can not merge data. TRtcValue object is already in use.');

  FValue:=ObjectFromJSON(s,at);
  end;

procedure TRtcValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  if not assigned(FValue) then
    Result.Add(nullValueXMLrpc)
  else
    FValue.to_XMLRPC(Result);
  end;

procedure TRtcValue.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    xname, xtag:RtcString;
    xval:TRtcValueObject;
    xrec:TRtcRecord;
    xarr:TRtcArray;
    idx:integer;
    have_params,
    have_name,
    inside_param,
    have_param:boolean;
  begin
  if assigned(FValue) then
    raise ERtcInfo.Create('Can not merge data. TRtcValue object is already in use.');

  if xmlrpc_checkStrType(s,at)=rtc_Variant then
    begin
    xval:=nil;

    SetLength(tags,0);
    try
      xmlrpc_skipValueOpen('METHODRESPONSE',s,at,tags);
      xmlrpc_skipWhitespace(s,at);

      xtag:=Upper_Case(xmlrpc_checkTag(s,at));
      if (xtag='PARAMS/') then
        begin
        xmlrpc_skipTag(s,at); // <PARAMS>
        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        end
      else if (xtag='PARAMS') or (xtag='PARAM') then // we could have parameters
        begin
        if xtag='PARAMS' then
          begin
          have_params:=True;
          xmlrpc_skipTag(s,at); // <PARAMS>
          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          end
        else
          have_params:=False;

        if xtag='/PARAMS' then // no parameters (empty array)
          begin
          // newArray(RTC_XMLRPC_ParamsAsArrayName)
          end
        else if (xtag='PARAM') or (xtag='NAME') then // we have a parameter!
          begin
          if xtag='PARAM' then
            begin
            xmlrpc_skipTag(s,at); // <PARAM>
            inside_param:=True;
            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            end
          else
            inside_param:=False;

          if xtag='/PARAM' then
            begin
            xval:=nil;
            xmlrpc_skipTag(s,at); // </PARAM>
            end
          else if xtag='NAME' then // receiving named parameter list
            begin
            have_param:=True;
            have_name:=False;

            xrec:=TRtcRecord.Create;
            repeat
              xmlrpc_readTag(s,at,'NAME');
              xname:=xmlrpc_readTrimValue(s,at);
              xmlrpc_readTag(s,at,'/NAME');

              xtag:=Upper_Case(xmlrpc_checkTag(s,at));
              if xtag<>'/PARAM' then
                begin
                xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
                xrec.as_Object[xname]:=xval;
                xval:=nil;
                end;

              if inside_param then
                begin
                xtag:=Upper_Case(xmlrpc_checkTag(s,at));
                if (xtag='/PARAM') then
                  begin
                  inside_param:=False;
                  have_param:=False;
                  xmlrpc_skipTag(s,at);
                  end;
                end;

              xtag:=Upper_Case(xmlrpc_checkTag(s,at));
              if xtag='NAME' then
                begin
                if have_param then
                  have_name:=True
                else
                  Break;
                end
              else if xtag='PARAM' then
                begin
                if not have_param and not have_name then
                  begin
                  inside_param:=True;
                  have_param:=True;
                  xmlrpc_skipTag(s,at);
                  end
                else
                  Break;
                end
              else
                Break;
              until false;
            xval:=xrec;
            end
          else
            begin
            // read all data stored in this parameter
            xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
            if inside_param then xmlrpc_readTag(s,at,'/PARAM');
            end;

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          if (xtag='/PARAMS') or (xtag='/METHODRESPONSE') then // A single <PARAM>
            begin
            // a single parameter
            FValue:=xval;
            xval:=nil;
            end
          else if xtag='PARAM' then // More than one <PARAM> - it's an array!
            begin
            xarr:=TRtcArray.Create;
            if assigned(xval) then
              begin
              xarr.SetObject(0,xval);
              xval:=nil;
              end;

            xval:=xarr;

            idx:=1;
            repeat
              xmlrpc_skipTag(s,at); // <PARAM>

              xtag:=Upper_Case(xmlrpc_checkTag(s,at));
              if xtag<>'/PARAM' then
                begin
                // read all data stored in this parameter
                xarr.SetObject(idx,TRtcValueObject.ObjectFromXMLrpc(s,at));
                xmlrpc_readTag(s,at,'/PARAM');
                end
              else
                begin
                xval:=nil;
                xmlrpc_skipTag(s,at); // </PARAM>
                end;

              Inc(idx);
              xtag:=Upper_Case(xmlrpc_checkTag(s,at));
              until xtag<>'PARAM';

            FValue:=xval;
            xval:=nil;
            end;
          end
        else if have_params then
          begin
          // read all data stored in this parameter
          xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
          xtag:=Upper_Case(xmlrpc_checkTag(s,at));

          if xtag='/PARAMS' then // A single <PARAM>
            begin
            // Our data is in xval
            FValue:=xval;
            xval:=nil;
            end
          else // More than one parameter - it's an array!
            begin
            xarr:=TRtcArray.Create;
            if assigned(xval) then
              begin
              xarr.SetObject(0,xval);
              xval:=nil;
              end;
            xval:=xarr;

            idx:=1;
            repeat
              // read all data stored in this parameter
              xarr.SetObject(idx,TRtcValueObject.ObjectFromXMLrpc(s,at));
              Inc(idx);
              xtag:=Upper_Case(xmlrpc_checkTag(s,at));
              until xtag='/PARAMS';

            FValue:=xval;
            xval:=nil;
            end;
          end;

        if have_params then
          xmlrpc_readTag(s,at,'/PARAMS');
        end
      else if xtag<>'/METHODRESPONSE' then // receiving data without <PARAMS><PARAM>
        begin
        if xtag='NAME' then // receiving named parameter list
          begin
          xrec:=TRtcRecord.Create;
          xval:=xrec;
          repeat
            xmlrpc_readTag(s,at,'NAME');
            xname:=xmlrpc_readTrimValue(s,at);
            xmlrpc_readTag(s,at,'/NAME');

            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            if xtag<>'/METHODRESPONSE' then
              begin
              try
                xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
                xrec.as_Object[xname]:=xval;
              finally
                xval:=xrec;
                end;
              xtag:=Upper_Case(xmlrpc_checkTag(s,at));
              if xtag='/METHODRESPONSE' then
                Break;
              end
            else
              Break;
            until false;
          end
        else
          begin
          // read all data stored in this parameter
          xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          end;

        if xtag='/METHODRESPONSE' then // A single <PARAM>
          begin
          // Our data is in xval
          FValue:=xval;
          xval:=nil;
          end
        else // More than one parameter - it's an array!
          begin
          xarr:=TRtcArray.Create;
          if assigned(xval) then
            begin
            xarr.SetObject(0,xval);
            xval:=nil;
            end;
          xval:=xarr;

          idx:=1;
          repeat
            // read all data stored in this parameter
            xarr.SetObject(idx,TRtcValueObject.ObjectFromXMLrpc(s,at));
            Inc(idx);
            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            until xtag='/METHODRESPONSE';

          FValue:=xval;
          xval:=nil;
          end;
        end;

      xmlrpc_skipValueClose(s,at,tags);
    finally
      SetLength(tags,0);
      xval.Free;
      end;
    end
  else
    FValue:=ObjectFromXMLrpc(s,at);
  end;

class function TRtcValue.FromCode(const data: RtcString; var at:integer): TRtcValue;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcValue.Create;
  try
    Result.from_Code(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.FromCode('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcValue.FromCode(const data: RtcString): TRtcValue;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcValue.FromJSON(const data:RtcWideString; var at: integer): TRtcValue;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcValue.Create;
  try
    Result.from_JSON(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.FromJSON('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcValue.FromJSON(const data:RtcWideString): TRtcValue;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromJSON(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcValue.FromXMLrpc(const data: RtcString; var at: integer): TRtcValue;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcValue.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsValue.FromXMLrpc('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcValue.FromXMLrpc(const data: RtcString): TRtcValue;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLrpc(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcValue.Extracted;
  begin
  if assigned(FValue) then
    begin
    FValue.Extracted;
    FValue:=nil;
    end;
  {$IFNDEF NEXTGEN}Free;{$ENDIF}
  end;

procedure TRtcValue.Extract;
  begin
  if assigned(FValue) then
    begin
    FValue.Extracted;
    FValue:=nil;
    end;
  end;

function TRtcValue.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  if assigned(FValue) then
    Result:=FValue.TypeCheck(typ)
  else
    Result:= (typ=rtc_Null);
  end;

{ TRtcAbsRecord }

function TRtcAbsRecord.GetNull(const index:RtcWideString): boolean;
  begin
  Result:= GetObject(index)=nil;
  end;

function TRtcAbsRecord.GetArray(const index:RtcWideString): TRtcArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    begin
    if AutoCreate then
      Result:=NewArray(index)
    else
      Result:=TRtcArray.NullValue;
    end
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetArray
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcArray.');
  end;

function TRtcAbsRecord.GetRecord(const index:RtcWideString): TRtcRecord;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    begin
    if AutoCreate then
      Result:=NewRecord(index)
    else
      Result:=TRtcRecord.NullValue;
    end
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetRecord
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcRecord.');
  end;

function TRtcAbsRecord.GetDataSet(const index:RtcWideString): TRtcDataSet;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    begin
    if AutoCreate then
      Result:=NewDataSet(index)
    else
      Result:=TRtcDataSet.NullValue;
    end
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetDataSet
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcDataSet.');
  end;

function TRtcAbsRecord.GetFunctionInfo(const index:RtcWideString): TRtcFunctionInfo;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    begin
    if AutoCreate then
      Result:=NewFunction(index)
    else
      Result:=TRtcFunctionInfo.NullValue;
    end
  else if gobj is TRtcFunctionInfo then
    Result:=TRtcFunctionInfo(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetFunctionInfo
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcFunctionInfo.');
  end;

function TRtcAbsRecord.GetBoolean(const index:RtcWideString): boolean;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcBooleanValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetBoolean
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetBoolean
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Boolean.');
  end;

function TRtcAbsRecord.GetCurrency(const index:RtcWideString): Currency;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcCurrencyValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetCurrency
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetCurrency
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Currency.');
  end;

function TRtcAbsRecord.GetDateTime(const index:RtcWideString): TDateTime;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcDateTimeValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetDateTime
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetDateTime
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TDateTime.');
  end;

function TRtcAbsRecord.GetException(const index:RtcWideString):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcExceptionValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetException
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetException
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcExceptionValue.');
  end;

function TRtcAbsRecord.GetVarName(const index:RtcWideString):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcVariableName.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetVarName
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetVarName
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcVariableName.');
  end;

function TRtcAbsRecord.GetInteger(const index:RtcWideString): rtcInteger;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcIntegerValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetInteger
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetInteger
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Integer.');
  end;

function TRtcAbsRecord.GetCardinal(const index:RtcWideString): rtcCardinal;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcCardinalValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetCardinal
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetCardinal
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Cardinal.');
  end;

function TRtcAbsRecord.GetLargeInt(const index:RtcWideString): rtcLargeInt;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcLargeIntValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetLargeInt
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetLargeInt
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to LargeInt.');
  end;

function TRtcAbsRecord.GetOID(const index:RtcWideString): TRtcObjectID;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcOIDValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetOID
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetOID
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to a Linked Object.');
  end;

function TRtcAbsRecord.GetLinkedObject(const index:RtcWideString): TObject;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=RTC_NIL_OBJECT
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetLinkedObject
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetLinkedObject
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to a Linked Object.');
  end;

function TRtcAbsRecord.GetFloat(const index:RtcWideString): rtcFloat;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcFloatValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetFloat
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetFloat
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Float.');
  end;

function TRtcAbsRecord.GetString(const index:RtcWideString): RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcStringValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetString
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetString
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsString
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsString
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsString
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to String.');
  end;

function TRtcAbsRecord.GetWideString(const index:RtcWideString): RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcWideStringValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetWideString
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetWideString
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsWideString
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsWideString
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsWideString
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to WideString.');
  end;

function TRtcAbsRecord.GetText(const index:RtcWideString):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcTextValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetText
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetText
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsText
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsText
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsText
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Text.');
  end;

function TRtcAbsRecord.GetVariant(const index:RtcWideString): Variant;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=Null
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetVariant
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetVariant
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Variant.');
  end;

function TRtcAbsRecord.GetByteArray(const index:RtcWideString): RtcByteArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcByteArray.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetByteArray
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetByteArray
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to ByteArray.');
  end;

function TRtcAbsRecord.GetByteStream(const index:RtcWideString): TStream;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcByteStream.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetByteStream
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetByteStream
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to ByteStream.');
  end;

function TRtcAbsRecord.GetValueType(const index:RtcWideString): TRtcValueTypes;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=rtc_Null
  else
    Result:=gobj.GetType;
  end;

function TRtcAbsRecord.CheckType(const index:RtcWideString; typ: TRtcValueTypes): boolean;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=(typ=rtc_Null)
  else
    Result:=gobj.TypeCheck(typ);
  end;

function TRtcAbsRecord.Check_Type(const index: RtcString; typ: TRtcValueTypes): boolean;
  begin
  Result:=CheckType(RtcWideString(index),typ);
  end;

procedure TRtcAbsRecord.SetNull(const index:RtcWideString; const Value: boolean);
  begin
  if Value then
    SetObject(index, nil, True);
  end;

procedure TRtcAbsRecord.SetArray(const index:RtcWideString; const Value: TRtcArray);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsRecord.SetRecord(const index:RtcWideString; const Value: TRtcRecord);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsRecord.SetDataSet(const index:RtcWideString; const Value: TRtcDataSet);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsRecord.SetFunctionInfo(const index:RtcWideString; const Value: TRtcFunctionInfo);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsRecord.SetBoolean(const index:RtcWideString; const Value: boolean);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcBooleanValue) then
    begin
    gobj:=TRtcBooleanValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetBoolean('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcBooleanValue(gobj).SetBoolean(Value);
  end;

procedure TRtcAbsRecord.SetCurrency(const index:RtcWideString; const Value: Currency);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcCurrencyValue) then
    begin
    gobj:=TRtcCurrencyValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetCurrency('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcCurrencyValue(gobj).SetCurrency(Value);
  end;

procedure TRtcAbsRecord.SetDateTime(const index:RtcWideString; const Value: TDateTime);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcDateTimeValue) then
    begin
    gobj:=TRtcDateTimeValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetDateTime('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcDateTimeValue(gobj).SetDateTime(Value);
  end;

procedure TRtcAbsRecord.SetException(const index:RtcWideString; const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcExceptionValue) then
    begin
    gobj:=TRtcExceptionValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetException('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcExceptionValue(gobj).SetException(Value);
  end;

procedure TRtcAbsRecord.SetVarName(const index:RtcWideString; const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcVariableName) then
    begin
    gobj:=TRtcVariableName.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetVarName('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcVariableName(gobj).SetVarName(Value);
  end;

procedure TRtcAbsRecord.SetInteger(const index:RtcWideString; const Value: rtcInteger);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcIntegerValue) then
    begin
    gobj:=TRtcIntegerValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetInteger('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcIntegerValue(gobj).SetInteger(Value);
  end;

procedure TRtcAbsRecord.SetCardinal(const index:RtcWideString; const Value: rtcCardinal);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcCardinalValue) then
    begin
    gobj:=TRtcCardinalValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetCardinal('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcCardinalValue(gobj).SetCardinal(Value);
  end;

procedure TRtcAbsRecord.SetOID(const index:RtcWideString; const Value: TRtcObjectID);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcOIDValue) then
    begin
    gobj:=TRtcOIDValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetOID('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcOIDValue(gobj).SetOID(Value);
  end;

procedure TRtcAbsRecord.SetLinkedObject(const index:RtcWideString; const Value: TObject);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcOIDValue) then
    begin
    gobj:=TRtcOIDValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetOID('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcOIDValue(gobj).SetLinkedObject(Value);
  end;

procedure TRtcAbsRecord.SetLargeInt(const index:RtcWideString; const Value: rtcLargeInt);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcLargeIntValue) then
    begin
    gobj:=TRtcLargeIntValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetLargeInt('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcLargeIntValue(gobj).SetLargeInt(Value);
  end;

procedure TRtcAbsRecord.SetFloat(const index:RtcWideString; const Value: rtcFloat);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcFloatValue) then
    begin
    gobj:=TRtcFloatValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetFloat('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcFloatValue(gobj).SetFloat(Value);
  end;

procedure TRtcAbsRecord.SetString(const index:RtcWideString; const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcStringValue) then
    begin
    gobj:=TRtcStringValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetString('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcStringValue(gobj).SetString(Value);
  end;

procedure TRtcAbsRecord.SetWideString(const index:RtcWideString; const Value: RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcWideStringValue) then
    begin
    gobj:=TRtcWideStringValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetWideString('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcWideStringValue(gobj).SetWideString(Value);
  end;

procedure TRtcAbsRecord.SetText(const index:RtcWideString; const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcTextValue) then
    begin
    gobj:=TRtcTextValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetText('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcTextValue(gobj).SetText(Value);
  end;

procedure TRtcAbsRecord.SetByteArray(const index:RtcWideString; const Value: RtcByteArray);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcByteArray) then
    begin
    gobj:=TRtcByteArray.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetByteArray('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcByteArray(gobj).SetByteArray(Value);
  end;

procedure TRtcAbsRecord.SetByteStream(const index:RtcWideString; const Value: TStream);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcByteStream) then
    begin
    gobj:=TRtcByteStream.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.SetByteStream('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcByteStream(gobj).SetByteStream(Value);
  end;

procedure TRtcAbsRecord.SetVariant(const index:RtcWideString; const Value: Variant);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if assigned(gobj) then
    if not (gobj is TRtcSimpleValue) then
      raise ERtcInfo.Create('Value already assigned. Set to NULL before assigning another value.')
    else if TRtcSimpleValue(gobj).SetVariant(Value) then
      Exit; // value changed

  gobj:=TRtcValueObject.ObjectFromVariant(Value);
  try
    if gobj<>nil then
      if gobj is TRtcAbsArray then
        TRtcAbsArray(gobj).AutoCreate:=AutoCreate
      else if gobj is TRtcAbsRecord then
        TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
      else if gobj is TRtcAbsValue then
        TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(index,gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsRecord.SetVariant('+index+')',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

procedure TRtcAbsRecord.SetAsObject(const index:RtcWideString; Value: TRtcValueObject);
  begin
  SetObject(index, Value);
  end;

function TRtcAbsRecord.NewArray(const index:RtcWideString): TRtcArray;
  begin
  Result:=TRtcArray.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsRecord.NewArray('+index+')',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsRecord.NewRecord(const index:RtcWideString): TRtcRecord;
  begin
  Result:=TRtcRecord.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsRecord.NewRecord('+index+')',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsRecord.NewDataSet(const index:RtcWideString): TRtcDataSet;
  begin
  Result:=TRtcDataSet.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsRecord.NewDataSet('+index+')',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsRecord.NewFunction(const index:RtcWideString; const func_name:RtcWideString=''): TRtcFunctionInfo;
  begin
  Result:=TRtcFunctionInfo.Create;
  Result.FunctionName:=func_name;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsRecord.NewFunction('+index+','+func_name+')',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsRecord.NewByteArray(const index:RtcWideString; InitialSize:Integer=0): RtcByteArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcByteArray) then
    begin
    gobj:=TRtcByteArray.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewByteArray('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcByteArray(gobj).SetNull(True);
  Result:=TRtcByteArray(gobj).NewByteArray(InitialSize);
  end;

function TRtcAbsRecord.NewBoolean(const index:RtcWideString): boolean;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcBooleanValue) then
    begin
    gobj:=TRtcBooleanValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewBoolean('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcBooleanValue(gobj).SetNull(True);
  Result:=TRtcBooleanValue(gobj).GetBoolean;
  end;

function TRtcAbsRecord.NewCurrency(const index:RtcWideString): Currency;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcCurrencyValue) then
    begin
    gobj:=TRtcCurrencyValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewCurrency('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcCurrencyValue(gobj).SetNull(True);
  Result:=TRtcCurrencyValue(gobj).GetCurrency;
  end;

function TRtcAbsRecord.NewDateTime(const index:RtcWideString): TDateTime;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcDateTimeValue) then
    begin
    gobj:=TRtcDateTimeValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewDateTime('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcDateTimeValue(gobj).SetNull(True);
  Result:=TRtcDateTimeValue(gobj).GetDateTime;
  end;

function TRtcAbsRecord.NewException(const index:RtcWideString):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcExceptionValue) then
    begin
    gobj:=TRtcExceptionValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewException('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcExceptionValue(gobj).SetNull(True);
  Result:=TRtcExceptionValue(gobj).GetException;
  end;

function TRtcAbsRecord.NewVariable(const index:RtcWideString):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcVariableName) then
    begin
    gobj:=TRtcVariableName.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewVariable('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcVariableName(gobj).SetNull(True);
  Result:=TRtcVariableName(gobj).GetVarName;
  end;

function TRtcAbsRecord.NewInteger(const index:RtcWideString): rtcInteger;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcIntegerValue) then
    begin
    gobj:=TRtcIntegerValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewInteger('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcIntegerValue(gobj).SetNull(True);
  Result:=TRtcIntegerValue(gobj).GetInteger;
  end;

function TRtcAbsRecord.NewCardinal(const index:RtcWideString): rtcCardinal;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcCardinalValue) then
    begin
    gobj:=TRtcCardinalValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewCardinal('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcCardinalValue(gobj).SetNull(True);
  Result:=TRtcCardinalValue(gobj).GetCardinal;
  end;

function TRtcAbsRecord.NewLargeInt(const index:RtcWideString): rtcLargeInt;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcLargeIntValue) then
    begin
    gobj:=TRtcLargeIntValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewLargeInt('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcLargeIntValue(gobj).SetNull(True);
  Result:=TRtcLargeIntValue(gobj).GetLargeInt;
  end;

function TRtcAbsRecord.NewFloat(const index:RtcWideString): rtcFloat;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcFloatValue) then
    begin
    gobj:=TRtcFloatValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewFloat('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcFloatValue(gobj).SetNull(True);
  Result:=TRtcFloatValue(gobj).GetFloat;
  end;

function TRtcAbsRecord.NewString(const index:RtcWideString): RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcStringValue) then
    begin
    gobj:=TRtcStringValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewString('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcStringValue(gobj).SetNull(True);
  Result:=TRtcStringValue(gobj).GetString;
  end;

function TRtcAbsRecord.NewWideString(const index:RtcWideString): RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcWideStringValue) then
    begin
    gobj:=TRtcWideStringValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewWideString('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcWideStringValue(gobj).SetNull(True);
  Result:=TRtcWideStringValue(gobj).GetWideString;
  end;

function TRtcAbsRecord.New_Array(const index: RtcString): TRtcArray;
  begin
  Result:=NewArray(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Boolean(const index: RtcString): boolean;
  begin
  Result:=NewBoolean(RtcWideString(index));
  end;

function TRtcAbsRecord.New_ByteArray(const index: RtcString; InitialSize:Integer=0): RtcByteArray;
  begin
  Result:=NewByteArray(RtcWideString(index),InitialSize);
  end;

function TRtcAbsRecord.New_ByteStream(const index: RtcString): TStream;
  begin
  Result:=NewByteStream(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Currency(const index: RtcString): Currency;
  begin
  Result:=NewCurrency(RtcWideString(index));
  end;

function TRtcAbsRecord.New_DataSet(const index: RtcString): TRtcDataSet;
  begin
  Result:=NewDataSet(RtcWideString(index));
  end;

function TRtcAbsRecord.New_DateTime(const index: RtcString): TDateTime;
  begin
  Result:=NewDateTime(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Exception(const index: RtcString):RtcWideString;
  begin
  Result:=NewException(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Float(const index: RtcString): rtcFloat;
  begin
  Result:=NewFloat(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Function(const index: RtcString; const func_name:RtcWideString): TRtcFunctionInfo;
  begin
  Result:=NewFunction(RtcWideString(index),func_name);
  end;

function TRtcAbsRecord.New_Integer(const index: RtcString): rtcInteger;
  begin
  Result:=NewInteger(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Cardinal(const index: RtcString): rtcCardinal;
  begin
  Result:=NewCardinal(RtcWideString(index));
  end;

function TRtcAbsRecord.New_LargeInt(const index: RtcString): rtcLargeInt;
  begin
  Result:=NewLargeInt(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Record(const index: RtcString): TRtcRecord;
  begin
  Result:=NewRecord(RtcWideString(index));
  end;

function TRtcAbsRecord.New_String(const index: RtcString): RtcString;
  begin
  Result:=NewString(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Text(const index: RtcString):RtcWideString;
  begin
  Result:=NewText(RtcWideString(index));
  end;

function TRtcAbsRecord.New_Variable(const index: RtcString):RtcWideString;
  begin
  Result:=NewVariable(RtcWideString(index));
  end;

function TRtcAbsRecord.New_WideString(const index: RtcString): RtcWideString;
  begin
  Result:=NewWideString(RtcWideString(index));
  end;

function TRtcAbsRecord.NewText(const index:RtcWideString):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcTextValue) then
    begin
    gobj:=TRtcTextValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewText('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcTextValue(gobj).SetNull(True);
  Result:=TRtcTextValue(gobj).GetText;
  end;

function TRtcAbsRecord.NewByteStream(const index:RtcWideString): TStream;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcByteStream) then
    begin
    gobj:=TRtcByteStream.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsRecord.NewByteStream('+index+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcByteStream(gobj).SetNull(True);
  Result:=TRtcByteStream(gobj).GetByteStream;
  end;

function TRtcAbsRecord.GetCode(const index:RtcWideString): RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=nullValueCode
  else
    Result:=gobj.toCode;
  end;

procedure TRtcAbsRecord.SetCode(const index:RtcWideString; const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned to field "'+String(index)+'". Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromCode(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(index,gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsRecord.SetCode('+index+')',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

procedure TRtcAbsRecord.SetJSON(const index, Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned to field "'+String(index)+'". Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromJSON(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(index,gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsRecord.SetJSON('+index+')',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

function TRtcAbsRecord.GetJSON(const index:RtcWideString):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=RtcWideString(nullValueJSON)
  else
    Result:=RtcWideString(gobj.toJSON);
  end;

function TRtcAbsRecord.GetXMLrpc(const index:RtcWideString): RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=nullValueXMLrpc
  else
    Result:=gobj.toXMLrpc;
  end;

function TRtcAbsRecord.Get_Array(const index: RtcString): TRtcArray;
  begin
  Result:=GetArray(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Boolean(const index: RtcString): boolean;
  begin
  Result:=GetBoolean(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_ByteArray(const index: RtcString): RtcByteArray;
  begin
  Result:=GetByteArray(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_ByteStream(const index: RtcString): TStream;
  begin
  Result:=GetByteStream(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Code(const index: RtcString): RtcString;
  begin
  Result:=GetCode(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Currency(const index: RtcString): Currency;
  begin
  Result:=GetCurrency(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_DataSet(const index: RtcString): TRtcDataSet;
  begin
  Result:=GetDataSet(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_DateTime(const index: RtcString): TDateTime;
  begin
  Result:=GetDateTime(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Exception(const index: RtcString):RtcWideString;
  begin
  Result:=GetException(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Float(const index: RtcString): rtcFloat;
  begin
  Result:=GetFloat(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_FunctionInfo(const index: RtcString): TRtcFunctionInfo;
  begin
  Result:=GetFunctionInfo(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Integer(const index: RtcString): rtcInteger;
  begin
  Result:=GetInteger(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Cardinal(const index: RtcString): rtcCardinal;
  begin
  Result:=GetCardinal(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_LargeInt(const index: RtcString): rtcLargeInt;
  begin
  Result:=GetLargeInt(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Null(const index: RtcString): boolean;
  begin
  Result:=GetNull(RtcWideString(Index));
  end;

function TRtcAbsRecord.Get_Object(const index: RtcString): TRtcValueObject;
  begin
  Result:=GetObject(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Record(const index: RtcString): TRtcRecord;
  begin
  Result:=GetRecord(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_String(const index: RtcString): RtcString;
  begin
  Result:=GetString(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Text(const index: RtcString):RtcWideString;
  begin
  Result:=GetText(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_ValueType(const index: RtcString): TRtcValueTypes;
  begin
  Result:=GetValueType(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_Variant(const index: RtcString): Variant;
  begin
  Result:=GetVariant(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_OID(const index: RtcString): TRtcObjectID;
  begin
  Result:=GetOID(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_LinkedObject(const index: RtcString): TObject;
  begin
  Result:=GetLinkedObject(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_VarName(const index: RtcString):RtcWideString;
  begin
  Result:=GetVarName(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_WideString(const index: RtcString): RtcWideString;
  begin
  Result:=GetWideString(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_JSON(const index: RtcString):RtcWideString;
  begin
  Result:=GetJSON(RtcWideString(index));
  end;

function TRtcAbsRecord.Get_XMLrpc(const index: RtcString): RtcString;
  begin
  Result:=GetXMLrpc(RtcWideString(index));
  end;

procedure TRtcAbsRecord.SetXMLrpc(const index:RtcWideString; const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned to field "'+String(index)+'". Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromXMLrpc(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(index,gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsRecord.SetXMLrpc('+index+')',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

procedure TRtcAbsRecord.Set_Array(const index: RtcString; const Value: TRtcArray);
  begin
  SetArray(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_AsObject(const index: RtcString; Value: TRtcValueObject);
  begin
  SetAsObject(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Boolean(const index: RtcString; const Value: boolean);
  begin
  SetBoolean(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_ByteArray(const index: RtcString; const Value: RtcByteArray);
  begin
  SetByteArray(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_ByteStream(const index: RtcString; const Value: TStream);
  begin
  SetByteStream(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Code(const index, Value: RtcString);
  begin
  SetCode(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Currency(const index: RtcString; const Value: Currency);
  begin
  SetCurrency(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_DataSet(const index: RtcString; const Value: TRtcDataSet);
  begin
  SetDataSet(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_DateTime(const index: RtcString; const Value: TDateTime);
  begin
  SetDateTime(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Exception(const index: RtcString; const Value:RtcWideString);
  begin
  SetException(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Float(const index: RtcString; const Value: rtcFloat);
  begin
  SetFloat(RtcWideString(index),value);
  end;

procedure TRtcAbsRecord.Set_FunctionInfo(const index: RtcString; const Value: TRtcFunctionInfo);
  begin
  SetFunctionInfo(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Integer(const index: RtcString; const Value: rtcInteger);
  begin
  SetInteger(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Cardinal(const index: RtcString; const Value: rtcCardinal);
  begin
  SetCardinal(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_LargeInt(const index: RtcString; const Value: rtcLargeInt);
  begin
  SetLargeInt(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Null(const index: RtcString; const Value: boolean);
  begin
  SetNull(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Object(const index: RtcString; Value: TRtcValueObject; asCopy: boolean);
  begin
  SetObject(RtcWideString(index),Value,asCopy);
  end;

procedure TRtcAbsRecord.Set_Record(const index: RtcString; const Value: TRtcRecord);
  begin
  SetRecord(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_String(const index, Value: RtcString);
  begin
  SetString(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Text(const index: RtcString; const Value:RtcWideString);
  begin
  SetText(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_Variant(const index: RtcString; const Value: Variant);
  begin
  SetVariant(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_VarName(const index: RtcString; const Value:RtcWideString);
  begin
  SetVarName(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_OID(const index: RtcString; const Value: TRtcObjectID);
  begin
  SetOID(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_LinkedObject(const index: RtcString; const Value: TObject);
  begin
  SetLinkedObject(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_WideString(const index: RtcString; const Value: RtcWideString);
  begin
  SetWideString(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_XMLrpc(const index, Value: RtcString);
  begin
  SetXMLrpc(RtcWideString(index),Value);
  end;

procedure TRtcAbsRecord.Set_JSON(const index: RtcString; const Value:RtcWideString);
  begin
  SetJSON(RtcWideString(index),Value);
  end;

{ TRtcRecord }

constructor TRtcRecord.Create;
  begin
  inherited;
  FValues:=nil;
  end;

destructor TRtcRecord.Destroy;
  begin
  try
    Clear;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcRecord.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRecord.Clear;
  begin
  if assigned(FValues) then
    begin
    FValues.DestroyObjects;
    RtcFreeAndNil(FValues);
    end;
  end;

function TRtcRecord.GetFieldCount: integer;
  begin
  if assigned(FValues) then
    Result:=FValues.Count
  else
    Result:=0;
  end;

function TRtcRecord.Count: integer;
  begin
  Result:=GetFieldCount;
  end;

function TRtcRecord.GetFieldName(index: integer):RtcWideString;
  begin
  if not assigned(FValues) then
    Result:=''
  else if (index>=0) and (index<FValues.Count) then
    Result:=FValues.Strings[index]
  else
    Result:='';
  end;

procedure TRtcRecord.SetFieldName(index: integer; const pValue:RtcWideString);
  var
    idx:integer;
  begin
  if not assigned(FValues) then
    FValues:=tRtcFastStringObjList.Create;

  idx:=FValues.Find(pValue);
  if (idx>=0) then // already found.
    begin
    if (idx<>index) then
      raise ERtcInfo.Create('Field with name "'+String(pValue)+'" already exists.');
    end
  else if (index>=0) and (index<FValues.Count) then
    FValues.Strings[index]:=pValue
  else
    FValues.Add(pValue);
  end;

function TRtcRecord.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Record;
  end;

function TRtcRecord.Get_FieldName(index: integer): RtcString;
  begin
  Result:=RtcString(GetFieldName(index));
  end;

function TRtcRecord.GetObject(const index:RtcWideString): TRtcValueObject;
  var
    idx:integer;
  begin
  if assigned(FValues) then
    begin
    idx:=FValues.Find(index);
    if idx>=0 then
      Result:=TRtcValueObject(FValues.Objects[idx])
    else
      Result:=nil;
    end
  else
    Result:=nil;
  end;

procedure TRtcRecord.SetObject(const index:RtcWideString; pValue: TRtcValueObject; asCopy:boolean=False);
  var
    idx:integer;
    gobj:TRtcValueObject;
  begin
  if index='' then
    raise ERtcInfo.Create('TRtcRecord.SetObject: Fields without a name not allowed.');

  if assigned(FValues) then
    begin
    idx:=FValues.Find(index);
    if idx>=0 then
      begin
      gobj:=TRtcValueObject(FValues.Objects[idx]);
      if gobj<>pValue then
        begin
        if pValue<>nil then
          begin
          if not assigned(gobj) or (gobj is TRtcSimpleValue) then
            begin
            if asCopy then
              FValues.Objects[idx]:=pValue.copyOf
            else
              FValues.Objects[idx]:=pValue;
            gobj.Free;
            end
          else if gobj is TRtcValue then
            TRtcValue(gobj).SetObject(pValue, asCopy)
          else
            raise ERtcInfo.Create('Value of type '+gobj.ClassName+' allready assigned to "'+String(index)+'".'#13#10+
                                   'Set ['+String(index)+'] to NULL before assigning a different object.')
          end
        else
          begin
          if asCopy then gobj.Free;
          FValues.Objects[idx]:=nil; // do not call Delete(idx), or other fields Index positions would change
          end;
        end;
      end
    else if pValue<>nil then
      begin
      if asCopy then
        FValues.Add(index, pValue.copyOf)
      else
        FValues.Add(index, pValue);
      end
    else
      FValues.Add(index, pValue);
    end
  else if pValue<>nil then
    begin
    FValues:=tRtcFastStringObjList.Create;
    if asCopy then
      FValues.Add(index, pValue.copyOf)
    else
      FValues.Add(index, pValue);
    end
  else
    begin
    FValues:=tRtcFastStringObjList.Create;
    FValues.Add(index, pValue);
    end;
  end;

procedure TRtcRecord.Set_FieldName(index: integer; const pValue: RtcString);
  begin
  SetFieldName(index,RtcWideString(pValue));
  end;

class function TRtcRecord.NullValue: TRtcRecord;
  begin
  Result:=nil;
  end;

procedure TRtcRecord.CopyFrom(pValue: TRtcValueObject);
  var
    idx:integer;
    mylist:tRtcFastStringObjList;
    gobj:TRtcValueObject;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge objects. This Record already has data.');

  if assigned(TRtcRecord(pValue).FValues) then
    begin
    mylist:=tRtcFastStringObjList.Create;
    try
      with TRtcRecord(pValue).FValues do
        for idx:=0 to Count-1 do
          begin
          gobj:=TRtcValueObject(Objects[idx]);
          if assigned(gobj) then
            begin
            try
              gobj:=gobj.copyOf;
            except
              on E:Exception do
                begin
                if LOG_INFO_ERRORS then
                  Log('TRtcRecord.CopyFrom ('+IntToStr(idx)+'/'+IntToStr(Count)+')',E,'INFO');
                raise;
                end;
              end;
            end;
          mylist.Add(Strings[idx], gobj);
          end;
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcRecord.CopyFrom',E,'INFO');
        RtcFreeAndNil(mylist);
        raise;
        end;
      end;
    end
  else
    mylist:=nil;

  FValues:=mylist;
  end;

function TRtcRecord.copyOf: TRtcValueObject;
  begin
  Result:=TRtcRecord.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcRecord.from_Code(const s: RtcString; var at:integer);
  var
    fname:RtcWideString;
    data:RtcString;
    idx,cnt:integer;
    gobj:TObject;
    val:tRtcFastStringObjList;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Record data. TRtcRecord object is already in use.');

  data:=code_fromShortString(RTC_TYPE2STR_CONV[rtc_Record],s,at);
  try
    if data='' then
      cnt:=0
    else
      cnt:=Str2Int(data);
  except
    raise ERtcInfo.Create('TRtcRecord.from_Code: Field Count missing.');
    end;

  val:=tRtcFastStringObjList.Create;
  try
    for idx:=0 to cnt-1 do
      begin
      fname:=code_fromNameString(s,at);
      gobj:=ObjectFromCode(s,at);
      val.Add(fname, gobj);
      end;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcRecord.from_Code('+IntToStr(at)+')',E,'INFO');
      val.DestroyObjects;
      RtcFreeAndNil(val);
      raise;
      end;
    end;

  if assigned(FValues) then
    begin
    FValues.DestroyObjects;
    RtcFreeAndNil(FValues);
    end;

  FValues:=val;
  end;

procedure TRtcRecord.to_Code(const Result:TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
  begin
  try
    Result.Add( code_toShortString(RTC_TYPE2STR_CONV[rtc_Record],Int2Str(GetFieldCount)) );
    for idx:=0 to GetFieldCount-1 do
      begin
      try
        {$IFDEF RtcUpperCaseFieldNames}
          Result.Add(code_toNameString(UpperCaseStr(GetFieldName(idx))));
        {$ELSE}
          Result.Add(code_toNameString(GetFieldName(idx)));
        {$ENDIF}
        gobj:=TRtcValueObject(FValues.Objects[idx]);
        if assigned(gobj) then
          gobj.to_Code(Result)
        else
          Result.Add(nullValueCode);
      except
        on E:Exception do
          raise ERtcInfo.Create('fld#'+IntToStr(idx)+' '+E.ClassName+': '+E.Message);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcRecord.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

class function TRtcRecord.FromCode(const data: RtcString; var at:integer): TRtcRecord;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcRecord.Create;
  try
    Result.from_Code(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcRecord.FromCode('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcRecord.FromCode(const data: RtcString): TRtcRecord;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcRecord.FromJSON(const data:RtcWideString; var at: integer): TRtcRecord;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcRecord.Create;
  try
    Result.from_JSON(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcRecord.FromJSON('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcRecord.FromJSON(const data:RtcWideString): TRtcRecord;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromJSON(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcRecord.FromXMLrpc(const data: RtcString; var at: integer): TRtcRecord;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcRecord.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcRecord.FromXMLrpc('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcRecord.FromXMLrpc(const data: RtcString): TRtcRecord;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLRPC(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcRecord.from_JSON(const s:RtcWideString; var at: integer);
  var
    xname:RtcWideString;
    xval:TRtcValueObject;
    val:tRtcFastStringObjList;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Record data. TRtcRecord object is already in use.');

  json_skipTag('{',s,at);

  val:=tRtcFastStringObjList.Create;
  try
    repeat
      if json_checkTag('"',s,at) then
        begin
        xname:=json_readString(s,at);

        json_skipTag(':',s,at);

        xval:=TRtcValueObject.ObjectFromJSON(s,at);

        val.Add(xname, xval);

        json_checkTag(',',s,at,true); // have more? skip ","
        end
      else if json_checkTag('}',s,at,true) then
        Break
      else
        raise ErtcInfo.Create('JSON Error: Missing "}", object structure incomplete.');
      until False;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcRecord.from_JSON('+IntToStr(at)+')',E,'INFO');
      val.DestroyObjects;
      RtcFreeAndNil(val);
      raise;
      end;
    end;

  FValues:=val;
  end;

procedure TRtcRecord.to_JSON(const Result: TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
  begin
  Result.Add('{');
  for idx:=0 to GetFieldCount-1 do
    begin
    if idx>0 then Result.Add(',');
    Result.Add('"'+JSON_EncodeString(GetFieldName(idx))+'":');
    gobj:=TRtcValueObject(FValues.Objects[idx]);
    if assigned(gobj) then
      gobj.to_JSON(Result)
    else
      Result.Add(nullValueJSON);
    end;
  Result.Add('}');
  end;

procedure TRtcRecord.to_XMLRPC(const Result:TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
  begin
  Result.Add('<value><struct>'#13#10);
  for idx:=0 to GetFieldCount-1 do
    begin
    Result.Add('<member><name>');
    Result.Add(Utf8Encode(GetFieldName(idx)));
    Result.Add('</name>');
    gobj:=TRtcValueObject(FValues.Objects[idx]);
    if assigned(gobj) then
      gobj.to_XMLRPC(Result)
    else
      Result.Add(nullValueXMLrpc);
    Result.Add('</member>'#13#10);
    end;
  Result.Add('</struct></value>');
  end;

function TRtcRecord.GetAsString: RtcString;
  var
    res:TRtcHugeString;
    idx:integer;
    b:boolean;
    fname:RtcWideString;
  begin
  res:=TRtcHugeString.Create;
  try
    b:=False;
    Res.Add('(');
    for idx:=0 to Count-1 do
      begin
      fname:=GetFieldName(idx);
      if not GetNull(fname) then
        begin
        if b then Res.Add('; ') else b:=True;
        Res.Add(RtcString(fname));
        Res.Add('=');
        Res.Add(GetString(fname));
        end;
      end;
    Res.Add(')');
    Result:=res.Get;
  finally
    res.Free;
    end;
  end;

function TRtcRecord.GetAsText:RtcWideString;
  var
    res:TRtcHugeString;
    idx:integer;
    b:boolean;
    fname:RtcWideString;
  begin
  res:=TRtcHugeString.Create;
  try
    b:=False;
    Res.Add('(');
    for idx:=0 to Count-1 do
      begin
      fname:=GetFieldName(idx);
      if not GetNull(fname) then
        begin
        if b then Res.Add('; ') else b:=True;
        Res.Add(Utf8Encode(fname));
        Res.Add('=');
        Res.Add(Utf8Encode(GetText(fname)));
        end;
      end;
    Res.Add(')');
    Result:=Utf8Decode(res.Get);
  finally
    res.Free;
    end;
  end;

function TRtcRecord.GetAsWideString: RtcWideString;
  var
    res:TRtcHugeString;
    idx:integer;
    b:boolean;
    fname:RtcWideString;
  begin
  res:=TRtcHugeString.Create;
  try
    b:=False;
    Res.Add('(');
    for idx:=0 to Count-1 do
      begin
      fname:=GetFieldName(idx);
      if not GetNull(fname) then
        begin
        if b then Res.Add('; ') else b:=True;
        Res.Add(Utf8Encode(fname));
        Res.Add('=');
        Res.Add(Utf8Encode(GetWideString(fname)));
        end;
      end;
    Res.Add(')');
    Result:=Utf8Decode(res.Get);
  finally
    res.Free;
    end;
  end;

procedure TRtcRecord.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    xname:RtcWideString;
    xtag:RtcString;
    xval:TRtcValueObject;
    val:tRtcFastStringObjList;
    c_tag:RtcString;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Record data. TRtcRecord object is already in use.');

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRUCT',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    c_tag:=xmlrpc_FirstCloseTag(tags);

    val:=tRtcFastStringObjList.Create;
    try
      xtag:=Upper_Case(xmlrpc_checkTag(s,at));
      if xtag='MEMBER' then // <member><name>..</name><value>...</value></member>
        begin
        repeat
          xmlrpc_skipTag(s,at); // <member>

          xmlrpc_readTag(s,at,'NAME');
          xname:=Utf8Decode(xmlrpc_readTrimValue(s,at));
          xmlrpc_readTag(s,at,'/NAME');

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          if xtag<>'/MEMBER' then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            val.Add(xname, xval);
            end;
          xmlrpc_readTag(s,at,'/MEMBER');

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          until xtag<>'MEMBER';
        end
      else if xtag='NAME' then // <name>..</name><value>...</value>
        begin
        repeat
          xmlrpc_readTag(s,at,'NAME');
          xname:=Utf8Decode(xmlrpc_readTrimValue(s,at));
          xmlrpc_readTag(s,at,'/NAME');

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          if (xtag<>'NAME') and (xtag<>c_tag) then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            val.Add(xname,xval);
            end;
          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          until xtag<>'NAME';
        end;
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcRecord.from_XMLrpc('+IntToStr(at)+')',E,'INFO');
        val.DestroyObjects;
        RtcFreeAndNil(val);
        raise;
        end;
      end;

    FValues:=val;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcRecord.Extract(const index:RtcWideString);
  var
    idx:integer;
    gobj:TRtcValueObject;
  begin
  if index='' then
    raise ERtcInfo.Create('TRtcRecord.Extract: Fields without a name not allowed.');

  if assigned(FValues) then
    begin
    idx:=FValues.Find(index);
    if idx>=0 then
      begin
      gobj:=TRtcValueObject(FValues.Objects[idx]);
      if assigned(gobj) then
        begin
        gobj.Extracted;
        FValues.Objects[idx]:=nil; // do not call Delete(idx), or other fields Index positions would change
        end;
      end;
    end;
  end;

procedure TRtcRecord._Extract(const index: RtcString);
  begin
  Extract(RtcWideString(index));
  end;

function TRtcRecord.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_RECORD_TYPES;
  end;

{ TRtcFunctionInfo }

constructor TRtcFunctionInfo.Create;
  begin
  inherited;
  FunctionName:='';
  end;

destructor TRtcFunctionInfo.Destroy;
  begin
  try
    Clear;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcFunctionInfo.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcFunctionInfo.Clear;
  begin
  inherited;
  FunctionName:='';
  end;

function TRtcFunctionInfo.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Function;
  end;

class function TRtcFunctionInfo.NullValue: TRtcFunctionInfo;
  begin
  Result:=nil;
  end;

procedure TRtcFunctionInfo.CopyFrom(pValue: TRtcValueObject);
  begin
  inherited CopyFrom(pValue);
  FunctionName:=TRtcFunctionInfo(pValue).FunctionName;
  end;

function TRtcFunctionInfo.copyOf: TRtcValueObject;
  begin
  Result:=TRtcFunctionInfo.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcFunctionInfo.to_Code(const Result:TRtcHugeString);
  begin
  try
    Result.Add( code_toShortNameString(RTC_TYPE2STR_CONV[rtc_Function], FunctionName));
    inherited to_Code(Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcFunctionInfo.to_Code',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcFunctionInfo.to_JSON(const Result: TRtcHugeString);
  var
    arr:TRtcArray;
    i,idx:integer;
    gobj:TRtcValueObject;
    nm:RtcWideString;
    id:RtcString;
  begin
  if not RTC_JSON_GenTypedFunctions then
    inherited to_JSON(Result)
  else
    begin
    i:=Pos(RtcWideString(':#'),FunctionName);
    if i<=0 then
      begin // JSON + REST
      Result.Add('{');
      Result.Add(RTC_JSON_FunctionName);
      Result.Add(':"');
      Result.Add(JSON_EncodeString(FunctionName));
      Result.Add('"');
      for idx:=0 to GetFieldCount-1 do
        begin
        Result.Add(',');
        Result.Add('"'+JSON_EncodeString(GetFieldName(idx))+'":');
        gobj:=TRtcValueObject(FValues.Objects[idx]);
        if assigned(gobj) then
          gobj.to_JSON(Result)
        else
          Result.Add(nullValueJSON);
        end;
      Result.Add('}');
      end
    else
      begin // JSON RPC
      nm:=Copy(FunctionName,1,i-1);
      id:=RtcString(Copy(FunctionName,i+2,length(FunctionName)-i-1));
      if GetFieldCount=0 then // no parameters
        begin
        Result.Add('{"method":"');
        Result.Add(JSON_EncodeString(nm));
        Result.Add('","params":[],"id":'+id+'}');
        end
      else if (GetFieldCount=1) and (GetValueType(RTC_JSON_ParamsAsArrayName)=rtc_Array) then // params as array
        begin
        arr:=GetArray(RTC_JSON_ParamsAsArrayName);
        if arr.FieldCount=0 then
          begin
          Result.Add('{"method":"');
          Result.Add(JSON_EncodeString(nm));
          Result.Add('","params":[],"id":'+id+'}');
          end
        else
          begin
          Result.Add('{"method":"');
          Result.Add(JSON_EncodeString(nm));
          Result.Add('","params":');
          arr.to_JSON(Result);
          Result.Add(',"id":'+id+'}');
          end;
        end
      else
        begin
        Result.Add('{"method":"');
        Result.Add(JSON_EncodeString(nm));
        Result.Add('","params":[');
        inherited to_JSON(Result);
        Result.Add('],"id":'+id+'}');
        end;
      end;
    end;
  end;

procedure TRtcFunctionInfo.from_Code(const s: RtcString; var at:integer);
  var
    fname:RtcWideString;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Function Calls. TRtcFunctionInfo is already in use.');

  fname:=code_fromShortNameString(RTC_TYPE2STR_CONV[rtc_Function], s, at);
  inherited from_Code(s,at);
  FunctionName:=fname;
  end;

procedure TRtcFunctionInfo.from_JSON(const s:RtcWideString; var at: integer);
  var
    xname:RtcWideString;
    xval:TRtcValueObject;
    val:tRtcFastStringObjList;
    noname:boolean;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Record data. TRtcRecord object is already in use.');

  json_skipTag('{',s,at);

  noname:=True;
  val:=tRtcFastStringObjList.Create;
  try
    repeat
      if json_checkTag('"',s,at) then
        begin
        if noname then
          if json_checkTag(RTC_JSON_FunctionName,s,at,true) then
            begin
            json_skipTag(':',s,at);

            json_skipWhitespace(s,at);
            FunctionName:=json_readString(s,at);

            json_checkTag(',',s,at,true); // have more? skip ","
            noname:=False;
            Continue;
            end;

        xname:=json_readString(s,at);

        json_skipTag(':',s,at);

        xval:=TRtcValueObject.ObjectFromJSON(s,at);

        val.Add(xname, xval);

        json_checkTag(',',s,at,true); // have more? skip ","
        end
      else if json_checkTag('}',s,at,true) then
        Break
      else
        raise ErtcInfo.Create('JSON Error: Missing "}", object structure incomplete.');
      until False;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcRecord.from_JSON('+IntToStr(at)+')',E,'INFO');
      val.DestroyObjects;
      RtcFreeAndNil(val);
      raise;
      end;
    end;

  FValues:=val;
  end;

class function TRtcFunctionInfo.FromCode(const data: RtcString; var at:integer): TRtcFunctionInfo;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcFunctionInfo.Create;
  try
    Result.from_Code(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcFunctionInfo.FromCode('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcFunctionInfo.FromCode(const data: RtcString): TRtcFunctionInfo;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcFunctionInfo.FromJSON(const data:RtcWideString; var at: integer): TRtcFunctionInfo;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcFunctionInfo.Create;
  try
    Result.from_JSON(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcFunctionInfo.FromJSON('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcFunctionInfo.FromJSON(const data:RtcWideString): TRtcFunctionInfo;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromJSON(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcFunctionInfo.FromXMLrpc(const data: RtcString; var at: integer): TRtcFunctionInfo;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcFunctionInfo.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcFunctionInfo.FromXMLrpc('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcFunctionInfo.FromXMLrpc(const data: RtcString): TRtcFunctionInfo;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLrpc(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcFunctionInfo.to_XMLRPC(const Result:TRtcHugeString);
  var
    idx:integer;
    arr:TRtcArray;
    gobj:TRtcValueObject;
  begin
  if GetFieldCount=0 then // no parameters
    begin
    Result.Add('<methodCall><methodName>');
    Result.Add(xmlrpc_writeNameString(FunctionName));
    Result.Add('</methodName></methodCall>');
    end
  else if (GetFieldCount=1) and (GetValueType(RTC_XMLRPC_ParamsAsArrayName)=rtc_Array) then // params as array
    begin
    arr:=GetArray(RTC_XMLRPC_ParamsAsArrayName);
    if arr.FieldCount=0 then
      begin
      Result.Add('<methodCall><methodName>');
      Result.Add(xmlrpc_writeNameString(FunctionName));
      Result.Add('</methodName><params></params></methodCall>');
      end
    else
      begin
      Result.Add('<methodCall><methodName>');
      Result.Add(xmlrpc_writeNameString(FunctionName));
      Result.Add('</methodName><params>'#13#10);
      for idx:=0 to arr.GetFieldCount-1 do
        begin
        Result.Add('<param>');
        gobj:=TRtcValueObject(arr.GetObject(idx));
        if assigned(gobj) then
          gobj.to_XMLRPC(Result)
        else
          Result.Add(nullValueXMLrpc);
        Result.Add('</param>'#13#10);
        end;
      Result.Add('</params></methodCall>');
      end;
    end
  else
    begin
    Result.Add('<methodCall><methodName>');
    Result.Add(xmlrpc_writeNameString(FunctionName));
    Result.Add('</methodName><params>'#13#10'<param>');
    inherited to_XMLRPC(Result);
    Result.Add('</param>'#13#10'</params></methodCall>');
    end;
  end;

procedure TRtcFunctionInfo.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    tmp:RtcWideString;
    c_tag, xtag:RtcString;
    xname:RtcWideString;
    xval:TRtcValueObject;
    xrec:TRtcRecord;
    xarr:TRtcArray;
    idx:integer;
    have_params,
    have_name,
    inside_param,
    have_param:boolean;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Record data. TRtcRecord object is already in use.');

  xval:=nil;

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('METHODCALL',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    c_tag:=xmlrpc_FirstCloseTag(tags);

    xmlrpc_readTag(s,at,'METHODNAME');
    FunctionName:=xmlrpc_readTrimNameValue(s,at);
    xmlrpc_readTag(s,at,'/METHODNAME');

    xtag:=Upper_Case(xmlrpc_checkTag(s,at));
    if (xtag='PARAMS/') then
      begin
      xmlrpc_skipTag(s,at); // <PARAMS>
      xtag:=Upper_Case(xmlrpc_checkTag(s,at));
      FValues:=tRtcFastStringObjList.Create; // no parameters
      end
    else if (xtag='PARAMS') or (xtag='PARAM') then // we could have parameters
      begin
      if xtag='PARAMS' then
        begin
        have_params:=True;
        xmlrpc_skipTag(s,at); // <PARAMS>
        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        end
      else
        have_params:=False;

      if xtag='/PARAMS' then // no parameters (empty array)
        newArray(RTC_XMLRPC_ParamsAsArrayName)
      else if (xtag='PARAM') or (xtag='NAME') then // we have a parameter!
        begin
        if xtag='PARAM' then
          begin
          xmlrpc_skipTag(s,at); // <PARAM>
          inside_param:=True;
          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          end
        else
          inside_param:=False;

        if xtag='/PARAM' then
          begin
          xval:=nil;
          xmlrpc_skipTag(s,at); // </PARAM>
          end
        else if xtag='NAME' then // receiving named parameter list
          begin
          have_param:=True;
          have_name:=False;

          xrec:=TRtcRecord.Create;
          repeat
            xmlrpc_readTag(s,at,'NAME');
            xname:=xmlrpc_readTrimNameValue(s,at);
            xmlrpc_readTag(s,at,'/NAME');

            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            if xtag<>'/PARAM' then
              begin
              xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
              xrec.SetObject(xname,xval);
              xval:=nil;
              end;

            if inside_param then
              begin
              xtag:=Upper_Case(xmlrpc_checkTag(s,at));
              if (xtag='/PARAM') then
                begin
                inside_param:=False;
                have_param:=False;
                xmlrpc_skipTag(s,at);
                end;
              end;

            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            if xtag='NAME' then
              begin
              if have_param then
                have_name:=True
              else
                Break;
              end
            else if xtag='PARAM' then
              begin
              if not have_param and not have_name then
                begin
                inside_param:=True;
                have_param:=True;
                xmlrpc_skipTag(s,at);
                end
              else
                Break;
              end
            else
              Break;
            until false;
          xval:=xrec;
          end
        else
          begin
          // read all data stored in this parameter
          xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
          if inside_param then xmlrpc_readTag(s,at,'/PARAM');
          end;

        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        if (xtag='/PARAMS') or (xtag=c_tag) then // A single <PARAM>
          begin
          if assigned(xval) and (xval.GetType=rtc_Record) then // standard structure with named parameters
            begin
            FValues:=tRtcFastStringObjList.Create;
            try
              // Move all data to our parameter list and destroy the original
              xrec:=TRtcRecord(xval);
              xval:=nil;
              try
                for idx:=0 to xrec.FValues.Count-1 do
                  begin
                  tmp:=xrec.FValues.Strings[idx];
                  FValues.Add(tmp, xrec.FValues.Objects[idx]);
                  xrec.FValues.Objects[idx]:=nil;
                  end;
              finally
                RtcFreeAndNil(xrec);
                end;
            except
              on E:Exception do
                begin
                if LOG_INFO_ERRORS then
                  Log('TRtcFunctionInfo.from_XMLrpc /PARAMS ('+IntToStr(at)+')',E,'INFO');
                FValues.DestroyObjects;
                RtcFreeAndNil(FValues);
                raise;
                end;
              end;
            end
          else // 1-item array
            begin
            xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
            if assigned(xval) then
              begin
              xarr.SetObject(0,xval);
              xval:=nil;
              end;
            end;
          end
        else if xtag='PARAM' then // More than one <PARAM> - it's an array!
          begin
          xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
          if assigned(xval) then
            begin
            xarr.SetObject(0,xval);
            xval:=nil;
            end;

          idx:=1;
          repeat
            xmlrpc_skipTag(s,at); // <PARAM>

            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            if xtag<>'/PARAM' then
              begin
              // read all data stored in this parameter
              xarr.SetObject(idx,TRtcValueObject.ObjectFromXMLrpc(s,at));
              xmlrpc_readTag(s,at,'/PARAM');
              end
            else
              begin
              xval:=nil;
              xmlrpc_skipTag(s,at); // </PARAM>
              end;

            Inc(idx);
            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            until xtag<>'PARAM';
          end;
        end
      else if have_params then
        begin
        // read all data stored in this parameter
        xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);

        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        if xtag='/PARAMS' then // A single value inside <PARAMS>
          begin
          if assigned(xval) and (xval.GetType=rtc_Record) then // standard structure with named parameters
            begin
            FValues:=tRtcFastStringObjList.Create;
            try
              // Move all data to our parameter list and destroy the original
              xrec:=TRtcRecord(xval);
              xval:=nil;
              try
                for idx:=0 to xrec.FValues.Count-1 do
                  begin
                  tmp:=xrec.FValues.Strings[idx];
                  FValues.Add(tmp, xrec.FValues.Objects[idx]);
                  xrec.FValues.Objects[idx]:=nil;
                  end;
              finally
                RtcFreeAndNil(xrec);
                end;
            except
              on E:Exception do
                begin
                if LOG_INFO_ERRORS then
                  Log('TRtcFunctionInfo.from_XMLrpc /PARAMS 2 ('+IntToStr(at)+')',E,'INFO');
                FValues.DestroyObjects;
                RtcFreeAndNil(FValues);
                raise;
                end;
              end;
            end
          else // 1-item array
            begin
            xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
            if assigned(xval) then
              begin
              xarr.SetObject(0,xval);
              xval:=nil;
              end;
            end;
          end
        else // More than one value - it's an array!
          begin
          xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
          if assigned(xval) then
            begin
            xarr.setObject(0,xval);
            xval:=nil;
            end;

          idx:=1;
          repeat
            // read all data stored in this parameter
            xarr.SetObject(idx,TRtcValueObject.ObjectFromXMLrpc(s,at));
            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            Inc(idx);
            until xtag='/PARAMS';
          end;
        end;

      if have_params then
        xmlrpc_readTag(s,at,'/PARAMS');
      end
    else if xtag<>c_tag then // receiving data without <PARAMS><PARAM>
      begin
      if xtag='NAME' then // receiving named parameter list
        begin
        xrec:=TRtcRecord.Create;
        repeat
          xmlrpc_readTag(s,at,'NAME');
          xname:=xmlrpc_readTrimNameValue(s,at);
          xmlrpc_readTag(s,at,'/NAME');

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          if xtag<>c_tag then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            xrec.SetObject(xname,xval);
            xval:=nil;
            xtag:=Upper_Case(xmlrpc_checkTag(s,at));
            if xtag=c_tag then
              Break;
            end
          else
            Break;
          until false;
        xval:=xrec;
        end
      else
        begin
        // read all data stored in this parameter
        xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        end;

      if xtag=c_tag then // A single <PARAM>
        begin
        if assigned(xval) and (xval.GetType=rtc_Record) then // standard structure with named parameters
          begin
          FValues:=tRtcFastStringObjList.Create;
          try
            // Move all data to our parameter list and destroy the original
            xrec:=TRtcRecord(xval);
            xval:=nil;
            try
              for idx:=0 to xrec.FValues.Count-1 do
                begin
                tmp:=xrec.FValues.Strings[idx];
                FValues.Add(tmp, xrec.FValues.Objects[idx]);
                xrec.FValues.Objects[idx]:=nil;
                end;
            finally
              RtcFreeAndNil(xrec);
              end;
          except
            on E:Exception do
              begin
              if LOG_INFO_ERRORS then
                Log('TRtcFunctionInfo.from_XMLrpc '+xtag+' ('+Int2Str(at)+')',E,'INFO');
              FValues.DestroyObjects;
              RtcFreeAndNil(FValues);
              raise;
              end;
            end;
          end
        else // 1-item array
          begin
          xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
          if assigned(xval) then
            begin
            xarr.SetObject(0,xval);
            xval:=nil;
            end;
          end;
        end
      else // More than one parameter - it's an array!
        begin
        xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
        if assigned(xval) then
          begin
          xarr.SetObject(0,xval);
          xval:=nil;
          end;

        idx:=1;
        repeat
          // read all data stored in this parameter
          xarr.SetObject(idx,TRtcValueObject.ObjectFromXMLrpc(s,at));
          Inc(idx);
          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          until xtag=c_tag;
        end;
      end
    else
      FValues:=tRtcFastStringObjList.Create; // no parameters

    xmlrpc_skipValueClose(s,at,tags);
  finally
    RtcFreeAndNil(xval);
    SetLength(tags,0);
    end;
  end;

function TRtcFunctionInfo.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_FUNCTION_TYPES;
  end;

{ TRtcAbsArray }

function TRtcAbsArray.GetNull(index: integer): boolean;
  begin
  Result:= GetObject(index)=nil;
  end;

function TRtcAbsArray.GetArray(index: integer): TRtcArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    begin
    if AutoCreate then
      Result:=NewArray(index)
    else
      Result:=TRtcArray.NullValue;
    end
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetArray
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcArray.');
  end;

function TRtcAbsArray.GetDataSet(index: integer): TRtcDataSet;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    begin
    if AutoCreate then
      Result:=NewDataSet(index)
    else
      Result:=TRtcDataSet.NullValue;
    end
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetDataSet
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcDataSet.');
  end;

function TRtcAbsArray.GetRecord(index: integer): TRtcRecord;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    begin
    if AutoCreate then
      Result:=NewRecord(index)
    else
      Result:=TRtcRecord.NullValue;
    end
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetRecord
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcRecord.');
  end;

function TRtcAbsArray.GetFunctionInfo(index: integer): TRtcFunctionInfo;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    begin
    if AutoCreate then
      Result:=NewFunction(index)
    else
      Result:=TRtcFunctionInfo.NullValue;
    end
  else if gobj is TRtcFunctionInfo then
    Result:=TRtcFunctionInfo(gobj)
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetFunctionInfo
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcFunctionInfo.');
  end;

function TRtcAbsArray.GetBoolean(index: integer): boolean;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcBooleanValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetBoolean
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetBoolean
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Boolean.');
  end;

function TRtcAbsArray.GetCurrency(index: integer): Currency;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcCurrencyValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetCurrency
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetCurrency
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Currency.');
  end;

function TRtcAbsArray.GetDateTime(index: integer): TDateTime;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcDateTimeValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetDateTime
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetDateTime
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TDateTime.');
  end;

function TRtcAbsArray.GetException(index: integer):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcExceptionValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetException
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetException
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcExceptionValue.');
  end;

function TRtcAbsArray.GetVarName(index: integer):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcVariableName.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetVarName
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetVarName
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcVariableName.');
  end;

function TRtcAbsArray.GetInteger(index: integer): rtcInteger;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcIntegerValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetInteger
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetInteger
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Integer.');
  end;

function TRtcAbsArray.GetCardinal(index: integer): rtcCardinal;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcCardinalValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetCardinal
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetCardinal
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Cardinal.');
  end;

function TRtcAbsArray.GetLargeInt(index: integer): rtcLargeInt;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcLargeIntValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetLargeInt
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetLargeInt
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to LargeInt.');
  end;

function TRtcAbsArray.GetOID(index: integer): TRtcObjectID;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcOIDValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetOID
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetOID
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to TRtcObjectID.');
  end;

function TRtcAbsArray.GetLinkedObject(index: integer): TObject;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=RTC_NIL_OBJECT
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetLinkedObject
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetLinkedObject
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Linked Object.');
  end;

function TRtcAbsArray.GetFloat(index: integer): rtcFloat;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcFloatValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetFloat
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetFloat
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Float.');
  end;

function TRtcAbsArray.GetString(index: integer): RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcStringValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetString
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetString
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsString
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsString
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsString
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to String.');
  end;

function TRtcAbsArray.GetWideString(index: integer): RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcWideStringValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetWideString
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetWideString
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsWideString
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsWideString
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsWideString
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to WideString.');
  end;

function TRtcAbsArray.GetText(index: integer):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcTextValue.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetText
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetText
  else if gobj is TRtcArray then
    Result:=TRtcArray(gobj).GetAsText
  else if gobj is TRtcRecord then
    Result:=TRtcRecord(gobj).GetAsText
  else if gobj is TRtcDataSet then
    Result:=TRtcDataSet(gobj).GetAsText
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Text.');
  end;

function TRtcAbsArray.GetByteArray(index: integer): RtcByteArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcByteArray.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetByteArray
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetByteArray
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to ByteArray.');
  end;

function TRtcAbsArray.GetByteStream(index: integer): TStream;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=TRtcByteStream.NullValue
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetByteStream
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetByteStream
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to ByteStream.');
  end;

function TRtcAbsArray.GetVariant(index: integer): Variant;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=Null
  else if gobj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(gobj).GetVariant
  else if gobj is TRtcValue then
    Result:=TRtcValue(gobj).GetVariant
  else
    raise EConvertError.Create('Can not convert '+gobj.ClassName+' to Variant.');
  end;

function TRtcAbsArray.GetValueType(index: integer): TRtcValueTypes;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=rtc_Null
  else
    Result:=gobj.GetType;
  end;

function TRtcAbsArray.CheckType(const index: integer; typ: TRtcValueTypes): boolean;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=(typ=rtc_Null)
  else
    Result:=gobj.TypeCheck(typ);
  end;

procedure TRtcAbsArray.SetNull(index: integer; const Value: boolean);
  begin
  if Value then
    SetObject(index, nil, True);
  end;

procedure TRtcAbsArray.SetArray(index: integer; const Value: TRtcArray);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsArray.SetRecord(index: integer; const Value: TRtcRecord);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsArray.SetDataSet(index: integer; const Value: TRtcDataSet);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsArray.SetFunctionInfo(index: integer; const Value: TRtcFunctionInfo);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsArray.SetBoolean(index: integer; const Value: boolean);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcBooleanValue) then
    begin
    gobj:=TRtcBooleanValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetBoolean('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcBooleanValue(gobj).SetBoolean(Value);
  end;

procedure TRtcAbsArray.SetCurrency(index: integer; const Value: Currency);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcCurrencyValue) then
    begin
    gobj:=TRtcCurrencyValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetCurrency('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcCurrencyValue(gobj).SetCurrency(Value);
  end;

procedure TRtcAbsArray.SetDateTime(index: integer; const Value: TDateTime);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcDateTimeValue) then
    begin
    gobj:=TRtcDateTimeValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetDateTime('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcDateTimeValue(gobj).SetDateTime(Value);
  end;

procedure TRtcAbsArray.SetException(index: integer; const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcExceptionValue) then
    begin
    gobj:=TRtcExceptionValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetException('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcExceptionValue(gobj).SetException(Value);
  end;

procedure TRtcAbsArray.SetVarName(index: integer; const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcVariableName) then
    begin
    gobj:=TRtcVariableName.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetVarName('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcVariableName(gobj).SetVarName(Value);
  end;

procedure TRtcAbsArray.SetInteger(index: integer; const Value: rtcInteger);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcIntegerValue) then
    begin
    gobj:=TRtcIntegerValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetInteger('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcIntegerValue(gobj).SetInteger(Value);
  end;

procedure TRtcAbsArray.SetCardinal(index: integer; const Value: rtcCardinal);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcCardinalValue) then
    begin
    gobj:=TRtcCardinalValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetCardinal('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcCardinalValue(gobj).SetCardinal(Value);
  end;

procedure TRtcAbsArray.SetLargeInt(index: integer; const Value: rtcLargeInt);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcLargeIntValue) then
    begin
    gobj:=TRtcLargeIntValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetLargeInt('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcLargeIntValue(gobj).SetLargeInt(Value);
  end;

procedure TRtcAbsArray.SetOID(index: integer; const Value: TRtcObjectID);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcOIDValue) then
    begin
    gobj:=TRtcOIDValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetOID('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcOIDValue(gobj).SetOID(Value);
  end;

procedure TRtcAbsArray.SetLinkedObject(index: integer; const Value: TObject);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcOIDValue) then
    begin
    gobj:=TRtcOIDValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetOID('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcOIDValue(gobj).SetLinkedObject(Value);
  end;

procedure TRtcAbsArray.SetFloat(index: integer; const Value: rtcFloat);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcFloatValue) then
    begin
    gobj:=TRtcFloatValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetFloat('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcFloatValue(gobj).SetFloat(Value);
  end;

procedure TRtcAbsArray.SetString(index: integer; const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcStringValue) then
    begin
    gobj:=TRtcStringValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetString('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcStringValue(gobj).SetString(Value);
  end;

procedure TRtcAbsArray.SetWideString(index: integer; const Value: RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcWideStringValue) then
    begin
    gobj:=TRtcWideStringValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetWideString('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcWideStringValue(gobj).SetWideString(Value);
  end;

procedure TRtcAbsArray.SetText(index: integer; const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcTextValue) then
    begin
    gobj:=TRtcTextValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetText('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcTextValue(gobj).SetText(Value);
  end;

procedure TRtcAbsArray.SetByteArray(index: integer; const Value: RtcByteArray);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcByteArray) then
    begin
    gobj:=TRtcByteArray.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetByteArray('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcByteArray(gobj).SetByteArray(Value);
  end;

procedure TRtcAbsArray.SetByteStream(index: integer; const Value: TStream);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcByteStream) then
    begin
    gobj:=TRtcByteStream.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.SetByteStream('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end;
  TRtcByteStream(gobj).SetByteStream(Value);
  end;

procedure TRtcAbsArray.SetVariant(index: integer; const Value: Variant);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if assigned(gobj) then
    if not (gobj is TRtcSimpleValue) then
      raise ERtcInfo.Create('Value already assigned. Set to NULL before assigning another value.')
    else if TRtcSimpleValue(gobj).SetVariant(Value) then
      Exit; // value changed

  gobj:=TRtcValueObject.ObjectFromVariant(Value);
  try
    if gobj<>nil then
      if gobj is TRtcAbsArray then
        TRtcAbsArray(gobj).AutoCreate:=AutoCreate
      else if gobj is TRtcAbsRecord then
        TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
      else if gobj is TRtcAbsValue then
        TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(index,gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsArray.SetVariant('+IntToStr(index)+')',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

procedure TRtcAbsArray.SetAsObject(index: integer; Value: TRtcValueObject);
  begin
  SetObject(index, Value);
  end;

function TRtcAbsArray.NewArray(index: integer): TRtcArray;
  begin
  Result:=TRtcArray.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsArray.NewArray('+IntToStr(index)+')',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsArray.NewRecord(index: integer): TRtcRecord;
  begin
  Result:=TRtcRecord.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsArray.NewRecord('+IntToStr(index)+')',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsArray.NewDataSet(index: integer): TRtcDataSet;
  begin
  Result:=TRtcDataSet.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsArray.NewDataSet('+IntToStr(index)+')',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsArray.NewFunction(index: integer; const func_name:RtcWideString=''): TRtcFunctionInfo;
  begin
  Result:=TRtcFunctionInfo.Create;
  Result.FunctionName:=func_name;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsArray.NewFunction('+IntToStr(index)+',"'+func_name+'")',E,'INFO');
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcAbsArray.NewByteArray(index: integer; InitialSize:Integer=0): RtcByteArray;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcByteArray) then
    begin
    gobj:=TRtcByteArray.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewByteArray('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcByteArray(gobj).SetNull(True);
  Result:=TRtcByteArray(gobj).NewByteArray(InitialSize);
  end;

function TRtcAbsArray.NewBoolean(index: integer): boolean;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcBooleanValue) then
    begin
    gobj:=TRtcBooleanValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewBoolean('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcBooleanValue(gobj).SetNull(True);
  Result:=TRtcBooleanValue(gobj).GetBoolean;
  end;

function TRtcAbsArray.NewCurrency(index: integer): Currency;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcCurrencyValue) then
    begin
    gobj:=TRtcCurrencyValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewCurrency('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcCurrencyValue(gobj).SetNull(True);
  Result:=TRtcCurrencyValue(gobj).GetCurrency;
  end;

function TRtcAbsArray.NewDateTime(index: integer): TDateTime;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcDateTimeValue) then
    begin
    gobj:=TRtcDateTimeValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewDateTime('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcDateTimeValue(gobj).SetNull(True);
  Result:=TRtcDateTimeValue(gobj).GetDateTime;
  end;

function TRtcAbsArray.NewException(index: integer):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcExceptionValue) then
    begin
    gobj:=TRtcExceptionValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewException('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcExceptionValue(gobj).SetNull(True);
  Result:=TRtcExceptionValue(gobj).GetException;
  end;

function TRtcAbsArray.NewVariable(index: integer):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcVariableName) then
    begin
    gobj:=TRtcVariableName.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewVariable('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcVariableName(gobj).SetNull(True);
  Result:=TRtcVariableName(gobj).GetVarName;
  end;

function TRtcAbsArray.NewInteger(index: integer): rtcInteger;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcIntegerValue) then
    begin
    gobj:=TRtcIntegerValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewInteger('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcIntegerValue(gobj).SetNull(True);
  Result:=TRtcIntegerValue(gobj).GetInteger;
  end;

function TRtcAbsArray.NewCardinal(index: integer): rtcCardinal;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcCardinalValue) then
    begin
    gobj:=TRtcCardinalValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewCardinal('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcCardinalValue(gobj).SetNull(True);
  Result:=TRtcCardinalValue(gobj).GetCardinal;
  end;

function TRtcAbsArray.NewLargeInt(index: integer): rtcLargeInt;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcLargeIntValue) then
    begin
    gobj:=TRtcLargeIntValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewLargeInt('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcLargeIntValue(gobj).SetNull(True);
  Result:=TRtcLargeIntValue(gobj).GetLargeInt;
  end;

function TRtcAbsArray.NewFloat(index: integer): rtcFloat;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcFloatValue) then
    begin
    gobj:=TRtcFloatValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewFloat('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcFloatValue(gobj).SetNull(True);
  Result:=TRtcFloatValue(gobj).GetFloat;
  end;

function TRtcAbsArray.NewString(index: integer): RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcStringValue) then
    begin
    gobj:=TRtcStringValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewString('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcStringValue(gobj).SetNull(True);
  Result:=TRtcStringValue(gobj).GetString;
  end;

function TRtcAbsArray.NewWideString(index: integer): RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcWideStringValue) then
    begin
    gobj:=TRtcWideStringValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewWideString('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcWideStringValue(gobj).SetNull(True);
  Result:=TRtcWideStringValue(gobj).GetWideString;
  end;

function TRtcAbsArray.NewText(index: integer):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcTextValue) then
    begin
    gobj:=TRtcTextValue.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewText('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcTextValue(gobj).SetNull(True);
  Result:=TRtcTextValue(gobj).GetText;
  end;

function TRtcAbsArray.NewByteStream(index: integer): TStream;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) or not (gobj is TRtcByteStream) then
    begin
    gobj:=TRtcByteStream.Create;
    try
      SetObject(index, gobj);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcAbsArray.NewByteStream('+IntToStr(index)+')',E,'INFO');
        RtcFreeAndNil(gobj);
        raise;
        end;
      end;
    end
  else
    TRtcByteStream(gobj).SetNull(True);
  Result:=TRtcByteStream(gobj).GetByteStream;
  end;

function TRtcAbsArray.GetCode(index: integer): RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=nullValueCode
  else
    Result:=gobj.toCode;
  end;

procedure TRtcAbsArray.SetCode(index: integer; const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned to field ['+IntToStr(index)+']. Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromCode(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(index,gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsArray.SetCode('+IntToStr(index)+')',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

function TRtcAbsArray.GetJSON(index: integer):RtcWideString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=RtcWideString(nullValueJSON)
  else
    Result:=RtcWideString(gobj.toJSON);
  end;

procedure TRtcAbsArray.SetJSON(index: integer; const Value:RtcWideString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned to field ['+IntToStr(index)+']. Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromJSON(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(index,gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsArray.SetJSON('+IntToStr(index)+')',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

function TRtcAbsArray.GetXMLrpc(index: integer): RtcString;
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if not assigned(gobj) then
    Result:=nullValueXMLrpc
  else
    Result:=gobj.toXMLrpc;
  end;

procedure TRtcAbsArray.SetXMLrpc(index: integer; const Value: RtcString);
  var
    gobj:TRtcValueObject;
  begin
  gobj:=GetObject(index);
  if assigned(gobj) and not (gobj is TRtcSimpleValue) then
    raise ERtcInfo.Create('Value already assigned to field ['+IntToStr(index)+']. Set to NULL before assigning another value.');

  gobj:=TRtcValueObject.ObjectFromXMLrpc(Value);
  try
    if gobj is TRtcAbsArray then
      TRtcAbsArray(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsRecord then
      TRtcAbsRecord(gobj).AutoCreate:=AutoCreate
    else if gobj is TRtcAbsValue then
      TRtcAbsValue(gobj).AutoCreate:=AutoCreate;
    SetObject(index,gobj);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcAbsArray.SetXMLrpc('+IntToStr(index)+')',E,'INFO');
      gobj.Free;
      raise;
      end;
    end;
  end;

{ TRtcArray }

constructor TRtcArray.Create;
  begin
  inherited;
  FValues:=nil;
  end;

destructor TRtcArray.Destroy;
  begin
  try
    Clear;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcArray.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcArray.Clear;
  var
    idx:integer;
    gobj:TObject;
  begin
  if assigned(FValues) then
    begin
    for idx:=0 to FValues.Count-1 do
      begin
      gobj:=TObject(FValues.Items[idx]);
      gobj.Free;
      end;
    RtcFreeAndNil(FValues);
    end;
  end;

function TRtcArray.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Array;
  end;

function TRtcArray.GetFieldCount: integer;
  begin
  if assigned(FValues) then
    Result:=FValues.Count
  else
    Result:=0;
  end;

function TRtcArray.Count: integer;
  begin
  Result:=GetFieldCount;
  end;

function TRtcArray.GetAsString: RtcString;
  var
    res:TRtcHugeString;
    i:integer;
  begin
  res:=TRtcHugeString.Create;
  try
    for i:=0 to Count-1 do
      res.Add(GetString(i));
    Result:=res.Get;
  finally
    res.Free;
    end;
  end;

function TRtcArray.GetAsText:RtcWideString;
  var
    res:TRtcHugeString;
    i:integer;
  begin
  res:=TRtcHugeString.Create;
  try
    for i:=0 to Count-1 do
      res.Add(Utf8Encode(GetText(i)));
    Result:=Utf8Decode(res.Get);
  finally
    res.Free;
    end;
  end;

function TRtcArray.GetAsWideString: RtcWideString;
  var
    res:TRtcHugeString;
    i:integer;
  begin
  res:=TRtcHugeString.Create;
  try
    for i:=0 to Count-1 do
      res.Add(Utf8Encode(GetWideString(i)));
    Result:=Utf8Decode(res.Get);
  finally
    res.Free;
    end;
  end;

function TRtcArray.GetObject(index: integer): TRtcValueObject;
  begin
  if assigned(FValues) then
    begin
    if (index>=0) and (index<FValues.Count) then
      Result:=TRtcValueObject(FValues.Items[index])
    else
      Result:=nil;
    end
  else
    Result:=nil;
  end;

procedure TRtcArray.SetObject(index: integer; pValue: TRtcValueObject; asCopy:boolean=False);
  var
    gobj:TRtcValueObject;
  begin
  if index<0 then
    raise ERtcInfo.Create('TRtcArray.SetObject: index lower than 0 (zero) not allowed.');

  if assigned(FValues) then
    begin
    if (index>=0) and (index<FValues.Count) then
      begin
      gobj:=TRtcValueObject(FValues.Items[index]);
      if gobj<>pValue then
        begin
        if pValue<>nil then
          begin
          if not assigned(gobj) or (gobj is TRtcSimpleValue) then
            begin
            if asCopy then
              FValues.Items[index]:=pValue.copyOf
            else
              FValues.Items[index]:=pValue;
            gobj.Free;
            end
          else if gobj is TRtcValue then
            TRtcValue(gobj).SetObject(pValue,asCopy)
          else
            raise ERtcInfo.Create('Value of type '+gobj.ClassName+' allready assigned at index '+IntToStr(index)+'.'#13#10+
                                   'Set ['+IntToStr(index)+'] to NULL before assigning a different object.')
          end
        else
          begin
          if asCopy then gobj.Free;
          FValues.Items[index]:=nil;
          if (index=FValues.Count-1) then
            FValues.Delete(index);
          end;
        end;
      end
    else if pValue<>nil then
      begin
      while FValues.Count<index+1 do
        FValues.Add(nil);
      if asCopy then
        FValues.Items[index]:=pValue.copyOf
      else
        FValues.Items[index]:=pValue;
      end;
    end
  else if pValue<>nil then
    begin
    FValues:=TObjectList.Create;
    while FValues.Count<index+1 do
      FValues.Add(nil);
    if asCopy then
      FValues.Items[index]:=pValue.copyOf
    else
      FValues.Items[index]:=pValue;
    end;
  end;

class function TRtcArray.NullValue: TRtcArray;
  begin
  Result:=nil;
  end;

procedure TRtcArray.CopyFrom(pValue: TRtcValueObject);
  var
    idx:integer;
    mylist:TObjectList;
    gobj:TRtcValueObject;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Arrays. Data already assigned to this Array.');
  if assigned(TRtcArray(pValue).FValues) then
    begin
    mylist:=TObjectList.Create;
    with TRtcArray(pValue).FValues do
      for idx:=0 to Count-1 do
        begin
        gobj:=TRtcValueObject(Items[idx]);
        if assigned(gobj) then
          gobj:=gobj.copyOf;
        mylist.Add(gobj);
        end;
    end
  else
    mylist:=nil;
  FValues:=mylist;
  end;

function TRtcArray.CopyOf: TRtcValueObject;
  begin
  Result:=TRtcArray.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcArray.from_Code(const s: RtcString; var at:integer);
  var
    data:RtcString;
    idx,cnt:integer;
    gobj:TObject;
    val:TObjectList;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Arrays. TRtcArray is already in use.');

  data:=code_fromShortString(RTC_TYPE2STR_CONV[rtc_Array],s,at);
  try
    if data='' then
      cnt:=0
    else
      cnt:=Str2Int(data);
  except
    raise ERtcInfo.Create('TRtcArray.from_Code: Field Count missing.');
    end;

  val:=TObjectList.Create;
  try
    for idx:=0 to cnt-1 do
      begin
      gobj:=ObjectFromCode(s,at);
      val.Add(gobj);
      end;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcArray.from_Code('+IntToStr(at)+')',E,'INFO');
      for idx:=0 to val.Count-1 do
        begin
        gobj:=TObject(val.Items[idx]);
        gobj.Free;
        end;
      RtcFreeAndNil(val);
      raise;
      end;
    end;

  FValues:=val;
  end;

procedure TRtcArray.to_Code(const Result:TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
  begin
  try
    Result.Add(code_toShortString(RTC_TYPE2STR_CONV[rtc_Array],Int2Str(GetFieldCount)));
    for idx:=0 to GetFieldCount-1 do
      begin
      try
        gobj:=TRtcValueObject(FValues.Items[idx]);
        if assigned(gobj) then
          gobj.to_Code(Result)
        else
          Result.Add(nullValueCode);
      except
        on E:Exception do
          raise ERtcInfo.Create('idx#'+IntToStr(idx)+' '+E.ClassName+': '+E.Message);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcArray.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

class function TRtcArray.FromCode(const data: RtcString; var at:integer): TRtcArray;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcArray.Create;
  try
    Result.from_Code(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcArray.FromCode('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcArray.FromCode(const data: RtcString): TRtcArray;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcArray.FromJSON(const data:RtcWideString; var at: integer): TRtcArray;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcArray.Create;
  try
    Result.from_JSON(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcArray.FromJSON('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcArray.FromJSON(const data:RtcWideString): TRtcArray;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromJSON(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcArray.FromXMLrpc(const data: RtcString; var at: integer): TRtcArray;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcArray.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcArray.FromXMLrpc('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcArray.FromXMLrpc(const data: RtcString): TRtcArray;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLrpc(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcArray.to_JSON(const Result: TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
  begin
  Result.Add('[');
  for idx:=0 to GetFieldCount-1 do
    begin
    if idx>0 then Result.Add(',');
    gobj:=TRtcValueObject(FValues.Items[idx]);
    if assigned(gobj) then
      gobj.to_JSON(Result)
    else
      Result.Add(RtcString(nullValueJSON));
    end;
  Result.Add(']');
  end;

procedure TRtcArray.from_JSON(const s:RtcWideString; var at: integer);
  var
    xval:TRtcValueObject;
    val:TObjectList;
    idx:integer;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Array data. TRtcArray object is already in use.');

  json_skipTag('[',s,at);

  val:=TObjectList.Create;
  try
    repeat
      if json_checkTag(']',s,at,true) then
        Break
      else
        begin
        xval:=TRtcValueObject.ObjectFromJSON(s,at);
        val.Add(xval);

        json_checkTag(',',s,at,true);
        end;
      until False;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcArray.from_JSON('+IntToStr(at)+')',E,'INFO');
      for idx:=0 to val.Count-1 do
        begin
        xval:=TRtcValueObject(val.Items[idx]);
        xval.Free;
        end;
      RtcFreeAndNil(val);
      raise;
      end;
    end;
  FValues:=val;
  end;

procedure TRtcArray.to_XMLRPC(const Result:TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
  begin
  Result.Add('<value><array><data>'#13#10);
  for idx:=0 to GetFieldCount-1 do
    begin
    gobj:=TRtcValueObject(FValues.Items[idx]);
    if assigned(gobj) then
      gobj.to_XMLRPC(Result)
    else
      Result.Add(nullValueXMLrpc);
    Result.Add(#13#10);
    end;
  Result.Add('</data></array></value>');
  end;

procedure TRtcArray.from_XMLrpc(const s: RtcString; var at: integer);
  var
    tags:rtcClosingTagsType;
    c_tag, xtag:RtcString;
    xval:TRtcValueObject;
    val:TObjectList;
    idx:integer;
  begin
  if assigned(FValues) then
    raise ERtcInfo.Create('Can not merge Array data. TRtcArray object is already in use.');

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('ARRAY',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    c_tag:=xmlrpc_FirstCloseTag(tags);

    xtag:=Upper_Case(xmlrpc_checkTag(s,at));

    val:=TObjectList.Create;
    try
      if xtag='DATA' then
        begin
        xmlrpc_skipTag(s,at); // <data>

        xtag:=Upper_Case(xmlrpc_checkTag(s,at));
        while xtag<>'/DATA' do
          begin
          xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
          val.Add(xval);

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          end;

        xmlrpc_readTag(s,at,'/DATA');
        end
      else if xtag<>c_tag then
        begin
        repeat
          xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
          val.Add(xval);

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          until xtag=c_tag;
        end;
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcArray.from_XMLrpc('+IntToStr(at)+')',E,'INFO');
        for idx:=0 to val.Count-1 do
          begin
          xval:=TRtcValueObject(val.Items[idx]);
          xval.Free;
          end;
        RtcFreeAndNil(val);
        raise;
        end;
      end;

    FValues:=val;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcArray.Extract(const index: integer);
  var
    gobj:TRtcValueObject;
  begin
  if index<0 then
    raise ERtcInfo.Create('TRtcArray.SetObject: index lower than 0 (zero) not allowed.');

  if assigned(FValues) then
    begin
    if (index>=0) and (index<FValues.Count) then
      begin
      gobj:=TRtcValueObject(FValues.Items[index]);
      if assigned(gobj) then
        begin
        gobj.Extracted;
        FValues.Items[index]:=nil;
        if (index=FValues.Count-1) then
          FValues.Delete(index);
        end;
      end;
    end;
  end;

function TRtcArray.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_ARRAY_TYPES;
  end;

{ TRtcDataSet }

constructor TRtcDataSet.Create;
  begin
  inherited;
  FNames:=nil;
  FData:=nil;
  New(FTypes);
  New(FSizes);
  New(FRequired);
  SetLength(FTypes^,0);
  SetLength(FSizes^,0);
  SetLength(FRequired^,0);
  FRow:=0;
  end;

destructor TRtcDataSet.Destroy;
  begin
  try
    Clear;
    Dispose(FTypes);
    Dispose(FSizes);
    Dispose(FRequired);
    RtcFreeAndNil(FNames);
    RtcFreeAndNil(FData);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataSet.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

constructor TRtcDataRow.CreateFromRtcDataSet(Source: TRtcDataSet);
  begin
  inherited Create;

  if not assigned(Source) then
    raise ERtcInfo.Create('Can not create TRtcDataRow from NIL.');

  if not assigned(Source.FNames) then
    Source.FNames:=tRtcFastStringObjList.Create;
  if not assigned(Source.FData) then
    Source.FData:=TObjectList.Create;

  FAutoCreate:=Source.FAutoCreate;
  FNames:=Source.FNames;
  FTypes:=Source.FTypes;
  FSizes:=Source.FSizes;
  FRequired:=Source.FRequired;
  FData:=Source.FData;
  FRow:=Source.FRow;
  end;

destructor TRtcDataRow.Destroy;
  begin
  try
    FNames:=nil;
    FTypes:=nil;
    FSizes:=nil;
    FRequired:=nil;
    FData:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataRow.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRow.Clear;
  begin
  raise ERtcInfo.Create('Clear method not available for TRtcDataRow');
  end;

procedure TRtcDataRow.CopyFrom(Value: TRtcValueObject);
  begin
  raise ERtcInfo.Create('CopyFrom method not available for TRtcDataRow');
  end;

function TRtcDataRow.copyOf: TRtcValueObject;
  begin
  {$IFDEF FPC}Result:=nil;{$ENDIF}
  raise ERtcInfo.Create('copyOf method not available for TRtcDataRow');
  end;

function TRtcDataRow.GetType: TRtcValueTypes;
  begin
  {$IFDEF FPC}Result:=rtc_Null;{$ENDIF}
  raise ERtcInfo.Create('GetType method not available for TRtcDataRow');
  end;

procedure TRtcDataRow.to_Code(const Result: TRtcHugeString);
  begin
  raise ERtcInfo.Create('to_Code method not available for TRtcDataRow');
  end;

procedure TRtcDataRow.to_JSON(const Result: TRtcHugeString);
  begin
  raise ERtcInfo.Create('to_JSON method not available for TRtcDataRow');
  end;

procedure TRtcDataRow.to_XMLRPC(const Result: TRtcHugeString);
  begin
  raise ERtcInfo.Create('to_XMLRPC method not available for TRtcDataRow');
  end;

function TRtcDataRow.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  {$IFDEF FPC}Result:=False;{$ENDIF}
  raise ERtcInfo.Create('TypeCheck method not available for TRtcDataRow');
  end;

procedure TRtcDataRow.from_Code(const s: RtcString; var at: integer);
  begin
  raise ERtcInfo.Create('from_Code method not available for TRtcDataRow');
  end;

procedure TRtcDataRow.from_JSON(const s:RtcWideString; var at: integer);
  begin
  raise ERtcInfo.Create('from_JSON method not available for TRtcDataRow');
  end;

procedure TRtcDataRow.from_XMLrpc(const s: RtcString; var at: integer);
  begin
  raise ERtcInfo.Create('from_XMLrpc method not available for TRtcDataRow');
  end;

procedure TRtcDataSet.Clear;
  var
    idx:integer;
  begin
  if assigned(FNames) then
    FNames.Clear;
  if assigned(FData) then
    begin
    for idx:=0 to FData.Count-1 do
      if assigned(FData.Items[idx]) then
        {$IFDEF NEXTGEN} FData.Items[idx]:=nil;
        {$ELSE}  TObject(FData.Items[idx]).Free;
        {$ENDIF}
    FData.Clear;
    end;
  SetLength(FTypes^,0);
  SetLength(FSizes^,0);
  SetLength(FRequired^,0);
  end;

function TRtcDataSet.GetType: TRtcValueTypes;
  begin
  Result:=rtc_DataSet;
  end;

function TRtcDataRow.Get_FieldIndex(const index: RtcString): integer;
  begin
  Result:=GetFieldIndex(RtcWideString(index));
  end;

function TRtcDataRow.Get_FieldName(index: integer): RtcString;
  begin
  Result:=RtcString(GetFieldName(index));
  end;

function TRtcDataRow.Get_FieldRequired(const index: RtcString): boolean;
  begin
  Result:=GetFieldRequired(RtcWideString(index));
  end;

function TRtcDataRow.Get_FieldSize(const index: RtcString): Integer;
  begin
  Result:=GetFieldSize(RtcWideString(index));
  end;

function TRtcDataRow.Get_FieldType(const index: RtcString): TRtcFieldTypes;
  begin
  Result:=GetFieldType(RtcWideString(index));
  end;

function TRtcDataRow.GetFieldCount: integer;
  begin
  if assigned(FNames) then
    Result:=FNames.Count
  else
    Result:=0;
  end;

function TRtcDataRow.GetFieldName(index: integer):RtcWideString;
  begin
  if assigned(FNames) and (index>=0) and (index<FNames.Count) then
    Result:=FNames.Strings[index]
  else
    Result:='';
  end;

procedure TRtcDataRow.SetFieldName(index: integer; const pValue:RtcWideString);
  var
    idx:integer;
  begin
  if pValue='' then
    raise ERtcInfo.Create('Can not set field name to an empty String.');
  if assigned(FNames) and (index>=0) and (index<FNames.Count) then
    begin
    idx:=FNames.Find(pValue);

    if (idx>=0) and (idx<>index) then
      raise ERtcInfo.Create('Another field with name "'+String(pValue)+'" already exists.');

    FNames.Strings[index]:=pValue;
    end
  else
    raise ERtcInfo.Create('No Field at index '+IntToStr(index)+'.');
  end;

function TRtcDataRow.GetFieldType(const index:RtcWideString): TRtcFieldTypes;
  var
    idx:integer;
  begin
  if assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      begin
      if idx<length(FTypes^) then
        Result:=FTypes^[idx]
      else
        Result:=ft_Unknown;
      end
    else
      Result:=ft_Unknown;
    end
  else
    Result:=ft_Unknown;
  end;

procedure TRtcDataRow.SetFieldType(const index:RtcWideString; const pValue: TRtcFieldTypes);
  var
    idx:integer;
  begin
  if index='' then
    raise ERtcInfo.Create('Can not set field type without field name.');

  if not assigned(FNames) then
    FNames:=tRtcFastStringObjList.Create;

  idx:=FNames.Find(index);
  if idx<0 then
    idx:=FNames.Add(index);

  while idx>=length(FTypes^) do
    begin
    SetLength(FTypes^,length(FTypes^)+1);
    FTypes^[length(FTypes^)-1]:=ft_Unknown;
    end;
  FTypes^[idx]:=pValue;
  end;

function TRtcDataRow.GetFieldRequired(const index:RtcWideString): boolean;
  var
    idx:integer;
  begin
  if assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      begin
      if idx<length(FRequired^) then
        Result:=FRequired^[idx]
      else
        Result:=False;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

procedure TRtcDataRow.SetFieldRequired(const index:RtcWideString; const pValue: boolean);
  var
    idx:integer;
  begin
  if index='' then
    raise ERtcInfo.Create('Can not set required property for field without a field name.');

  if not assigned(FNames) then
    FNames:=tRtcFastStringObjList.Create;

  idx:=FNames.Find(index);
  if idx<0 then
    idx:=FNames.Add(index);

  while idx>=length(FRequired^) do
    begin
    SetLength(FRequired^,length(FRequired^)+1);
    FRequired^[length(FRequired^)-1]:=False;
    end;
  FRequired^[idx]:=pValue;
  end;

function TRtcDataRow.GetFieldSize(const index:RtcWideString): Integer;
  var
    idx:integer;
  begin
  if assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      begin
      if idx<length(FSizes^) then
        Result:=FSizes^[idx]
      else
        Result:=0;
      end
    else
      Result:=0;
    end
  else
    Result:=0;
  end;

procedure TRtcDataRow.SetFieldSize(const index:RtcWideString; const pValue: Integer);
  var
    idx:integer;
  begin
  if index='' then
    raise ERtcInfo.Create('Can not set field size without field name.');

  if not assigned(FNames) then
    FNames:=tRtcFastStringObjList.Create;

  idx:=FNames.Find(index);
  if idx<0 then
    idx:=FNames.Add(index);

  while idx>=length(FSizes^) do
    begin
    SetLength(FSizes^,length(FSizes^)+1);
    FSizes^[length(FSizes^)-1]:=0;
    end;
  FSizes^[idx]:=pValue;
  end;

function TRtcDataRow.GetObject(const index:RtcWideString): TRtcValueObject;
  var
    idx:integer;
  begin
  if assigned(FData) and assigned(FNames) and
     (FRow>=0) and (FRow<FData.Count) and
     assigned(FData.Items[FRow]) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      Result:=TRtcArray(FData.Items[FRow]).GetObject(idx)
    else
      Result:=nil;
    end
  else
    Result:=nil;
  end;

function TRtcDataRow.GetFieldIndex(const index:RtcWideString): integer;
  begin
  Result:=FNames.Find(index);
  end;

function TRtcDataSet.GetRowData: TRtcArray;
  begin
  if assigned(FData) and (FRow>=0) and (FRow<FData.Count) and
     assigned(FData.Items[FRow]) then
    Result:=TRtcArray(FData.Items[FRow])
  else
    Result:=nil;
  end;

procedure TRtcDataSet.SetRowData(const pValue: TRtcArray);
  begin
  if assigned(FData) and (FRow>=0) and (FRow<FData.Count) then
    begin
    if TRtcArray(FData.Items[FRow])<>pValue then
      begin
      if assigned(FData.Items[FRow]) then
        {$IFDEF NEXTGEN}  FData.Items[FRow]:=nil;
        {$ELSE} TRtcArray(FData.Items[FRow]).Free;
        {$ENDIF}
      FData.Items[FRow]:=pValue;
      end;
    end
  else
    raise ERtcInfo.Create('Can not assign RowData to non-existing Row.');
  end;

procedure TRtcDataRow.Set_FieldRequired(const index: RtcString; const pValue: boolean);
  begin
  SetFieldRequired(RtcWideString(index),pValue);
  end;

procedure TRtcDataRow.Set_FieldSize(const index: RtcString; const pValue: Integer);
  begin
  SetFieldSize(RtcWideString(index),pValue);
  end;

procedure TRtcDataRow.Set_FieldType(const index: RtcString; const pValue: TRtcFieldTypes);
  begin
  SetFieldType(RtcWideString(index),pValue);
  end;

procedure TRtcDataRow.SetObject(const index:RtcWideString; pValue: TRtcValueObject; asCopy:boolean=False);
  var
    idx:integer;
    myrow:TRtcArray;
  begin
  if FRow<0 then
    raise ERtcInfo.Create('TRtcDataSet.SetObject: Row index beyond Bof.')
  else if index='' then
    raise ERtcInfo.Create('TRtcDataSet.SetObject: Fields without a name not allowed. Set Field Data by using a Field Name.');

  if assigned(FData) and assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then // name found
      begin
      if (FRow>=0) and (FRow<FData.Count) then // row number exists
        begin
        myrow:=TRtcArray(FData.Items[FRow]);
        if assigned(myrow) then
          begin
          if myrow.GetObject(idx)<>pValue then
            myrow.SetObject(idx,pValue,asCopy);
          end
        else if pValue<>nil then
          begin
          myrow:=TRtcArray.Create;
          try
            myrow.SetObject(idx,pValue,asCopy);
          except
            on E:Exception do
              begin
              if LOG_INFO_ERRORS then
                Log('TRtcDataSet.SetObject('+index+')',E,'INFO');
              RtcFreeAndNil(myrow);
              raise;
              end;
            end;
          FData.Items[FRow]:=myrow;
          end;
        end
      else if pValue<>nil then
        begin
        while FData.Count<FRow+1 do
          FData.Add(nil);
        myrow:=TRtcArray.Create;
        try
          myrow.SetObject(idx,pValue,asCopy);
        except
          on E:Exception do
            begin
            if LOG_INFO_ERRORS then
              Log('TRtcDataSet.SetObject 2('+index+')',E,'INFO');
            RtcFreeAndNil(myrow);
            raise;
            end;
          end;
        FData.Items[FRow]:=myrow;
        end;
      end
    else if pValue<>nil then // name not found
      begin
      idx:=FNames.Add(index); // add name

      while FData.Count<FRow+1 do
        FData.Add(nil);
      myrow:=TRtcArray(FData.Items[FRow]);
      if not assigned(myrow) then
        myrow:=TRtcArray.Create;
      try
        myrow.SetObject(idx,pValue,asCopy);
      except
        on E:Exception do
          begin
          if LOG_INFO_ERRORS then
            Log('TRtcDataSet.SetObject 3('+index+')',E,'INFO');
          if not assigned(FData.Items[FRow]) then
            RtcFreeAndNil(myrow);
          raise;
          end;
        end;
      FData.Items[FRow]:=myrow;
      end;
    end
  else if pValue<>nil then // no data yet
    begin
    if not assigned(FNames) then
      FNames:=tRtcFastStringObjList.Create;

    if not assigned(FData) then
      FData:=TObjectList.Create;

    idx:=FNames.Find(index);
    if idx<0 then // name found
      idx:=FNames.Add(index); // add name

    while FData.Count<FRow+1 do
      FData.Add(nil);
    myrow:=TRtcArray.Create;
    try
      myrow.SetObject(idx,pValue,asCopy);
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcDataSet.SetObject 4('+index+')',E,'INFO');
        RtcFreeAndNil(myrow);
        raise;
        end;
      end;
    FData.Items[FRow]:=myrow;
    end;
  // Set Field type, if not defined.
  if assigned(pValue) and (GetFieldType(index)=ft_Unknown) then
    SetFieldType(index,RTC_VALUE2FIELD_TYPES[pValue.GetType]);
  end;

function TRtcDataSet.Empty: boolean;
  begin
  Result:=RowCount=0;
  end;

function TRtcDataSet.GetRowCount: integer;
  begin
  if assigned(FData) then
    Result:=FData.Count
  else
    Result:=0;
  end;

function TRtcDataSet.GetRow: integer;
  begin
  Result:=FRow;
  end;

procedure TRtcDataSet.SetRow(const pValue: integer);
  begin
  if pValue<-1 then
    raise ERtcInfo.Create('Can not move to a Row beyond Bof, index out of bounds.')
  else
    FRow:=pValue;
  end;

function TRtcDataSet.Bof: boolean;
  begin
  Result:=(RowCount=0) or (FRow<0);
  end;

function TRtcDataSet.Eof: boolean;
  begin
  Result:=(RowCount=0) or (FRow>=RowCount);
  end;

procedure TRtcDataSet.First;
  begin
  FRow:=0;
  end;

procedure TRtcDataSet.Last;
  begin
  if RowCount>0 then
    FRow:=RowCount-1
  else
    FRow:=0;
  end;

procedure TRtcDataSet.Append;
  begin
  FRow:=RowCount;
  Insert;
  end;

procedure TRtcDataSet.Insert;
  begin
  if FRow<0 then
    FRow:=0;
  if not assigned(FData) then
    FData:=TObjectList.Create;
  if not assigned(FNames) then
    FNames:=tRtcFastStringObjList.Create;

  if FRow<FData.Count then
    FData.Insert(FRow,nil)
  else
    begin
    while FRow>FData.Count-1 do
      FData.Add(nil);
    end;
  end;

procedure TRtcDataSet.Delete;
  begin
  if (Row<0) then
    raise ERtcInfo.Create('Can not delete a row before first row.')
  else if (Row>=RowCount) then
    raise ERtcInfo.Create('Can not delete a row after last row.');
  if assigned(FData.Items[FRow]) then
    {$IFDEF NEXTGEN}  FData.Items[FRow]:=nil;
    {$ELSE} TRtcArray(FData.Items[FRow]).Free;
    {$ENDIF}
  FData.Delete(FRow);
  if (FRow>=RowCount) and (FRow>0) then
    Dec(FRow);
  end;

procedure TRtcDataSet.Next;
  begin
  if FRow<RowCount then
    Inc(FRow);
  end;

procedure TRtcDataSet.Prior;
  begin
  if FRow>=0 then
    Dec(FRow);
  end;

class function TRtcDataSet.NullValue: TRtcDataSet;
  begin
  Result:=nil;
  end;

procedure TRtcDataSet.to_Code(const Result:TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
    fname:RtcWideString;
  const
    BoolToStr:array[boolean] of RtcString =('F','T');
  begin
  try
    Result.Add(code_toShortString(RTC_TYPE2STR_CONV[rtc_DataSet],Int2Str(GetFieldCount)));
    for idx:=0 to GetFieldCount-1 do
      begin
      try
        fname:=GetFieldName(idx);
        {$IFDEF RtcUpperCaseFieldNames}
          Result.Add(code_toNameString(UpperCaseStr(fname)));
        {$ELSE}
          Result.Add(code_toNameString(fname));
        {$ENDIF}
        Result.Add(code_toMidString(RTC_FIELD2STR_CONV[GetFieldType(fname)]));
        Result.Add(code_toMidString(Int2Str(GetFieldSize(fname))));
        Result.Add(code_toEndString(BoolToStr[GetFieldRequired(fname)]));
      except
        on E:Exception do
          raise ERtcInfo.Create('fld#'+IntToStr(idx)+' '+E.ClassName+' : '+E.Message);
        end;
      end;
    Result.Add(code_toShortString('ROWS',Int2Str(GetRowCount)));
    for idx:=0 to GetRowCount-1 do
      begin
      try
        gobj:=TRtcValueObject(FData.Items[idx]);
        if assigned(gobj) then
          gobj.to_Code(Result)
        else
          Result.Add(nullValueCode);
      except
        on E:Exception do
          raise ERtcInfo.Create('row#'+IntToStr(idx)+' '+E.ClassName+': '+E.Message);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcDataSet.to_Code',E,'INFO');
      raise;
      end;
    end;
  end;

procedure TRtcDataSet.from_Code(const s: RtcString; var at:integer);
  var
    fname:RtcWideString;
    ftype,
    fsize,
    freq,
    data:RtcString;
    idx,cnt:integer;
    gobj:TObject;
  begin
  if assigned(FData) or assigned(FNames) then
    raise ERtcInfo.Create('Can not merge DataSet data. TRtcDataSet object is already in use.');

  data:=code_fromShortString(RTC_TYPE2STR_CONV[rtc_DataSet],s,at);
  try
    if data='' then
      cnt:=0
    else
      cnt:=Str2Int(data);
  except
    raise ERtcInfo.Create('TRtcDataSet.from_Code: Field Count missing.');
    end;

  FNames:=tRtcFastStringObjList.Create;
  try
    for idx:=0 to cnt-1 do
      begin
      fname:=code_fromNameString(s,at);
      ftype:=code_fromMidString(s,at);
      fsize:=code_fromMidString(s,at);
      freq:=code_fromEndString(s,at);
      SetFieldType(fname,StrToFieldType(ftype));
      SetFieldSize(fname,Str2Int(fsize));
      if Upper_Case(freq)='T' then
        SetFieldRequired(fname,True)
      else
        SetFieldRequired(fname,False);
      end;

    data:=code_fromShortString('ROWS',s,at);
    try
      if data='' then
        cnt:=0
      else
        cnt:=Str2Int(data);
    except
      raise ERtcInfo.Create('TRtcDataSet.from_Code: Row Count missing.');
      end;

    FData:=TObjectList.Create;
    try
      for idx:=0 to cnt-1 do
        begin
        gobj:=ObjectFromCode(s,at);
        FData.Add(gobj);
        end;
    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcDataSet.from_Code('+IntToStr(at)+')',E,'INFO');
        for idx:=0 to FData.Count-1 do
          begin
          gobj:=TObject(FData.Items[idx]);
          gobj.Free;
          end;
        RtcFreeAndNil(FData);
        raise;
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcDataSet.from_Code 2('+IntToStr(at)+')',E,'INFO');
      FNames.DestroyObjects;
      RtcFreeAndNil(FNames);
      raise;
      end;
    end;
  end;

procedure TRtcDataSet.CopyFrom(pValue: TRtcValueObject);
  var
    idx:integer;
    fname:RtcWideString;
  begin
  if assigned(FData) or assigned(FNames) then
    raise ERtcInfo.Create('Can not merge DataSets. This DataSet already has data assigned.');

  for idx:=0 to TRtcDataSet(pValue).FieldCount-1 do
    begin
    fname:=TRtcDataSet(pValue).GetFieldName(idx);
    SetFieldType(fname, TRtcDataSet(pValue).GetFieldType(fname));
    SetFieldSize(fname, TRtcDataSet(pValue).GetFieldSize(fname));
    SetFieldRequired(fname, TRtcDataSet(pValue).GetFieldRequired(fname));
    end;

  FData:=TObjectList.Create;

  for idx:=0 to TRtcDataSet(pValue).GetRowCount-1 do
    begin
    if assigned(TRtcDataSet(pValue).FData.Items[idx]) then
      FData.Add(TRtcArray(TRtcDataSet(pValue).FData.Items[idx]).copyOf)
    else
      FData.Add(nil);
    end;
  end;

function TRtcDataSet.copyOf: TRtcValueObject;
  begin
  Result:=TRtcDataSet.Create;
  Result.CopyFrom(self);
  end;

class function TRtcDataSet.FromCode(const data: RtcString; var at:integer): TRtcDataSet;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcDataSet.Create;
  try
    Result.from_Code(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcDataSet.FromCode('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcDataSet.FromCode(const data: RtcString): TRtcDataSet;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcDataSet.FromJSON(const data:RtcWideString; var at: integer): TRtcDataSet;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcDataSet.Create;
  try
    Result.from_JSON(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcDataSet.FromJSON('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcDataSet.FromJSON(const data:RtcWideString): TRtcDataSet;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromJSON(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

class function TRtcDataSet.FromXMLrpc(const data: RtcString; var at: integer): TRtcDataSet;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcDataSet.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcDataSet.FromXMLrpc('+IntToStr(at)+')',E,'INFO');
      at:=oldat;
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

class function TRtcDataSet.FromXMLrpc(const data: RtcString): TRtcDataSet;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLrpc(data,at);
  if at<>length(data) then
    begin
    RtcFreeAndNil(Result);
    raise ERtcInfo.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcDataSet.to_JSON(const Result: TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
    fname:RtcWideString;
    fs:integer;
    fr:boolean;
  const
    BoolToStr:array[boolean] of RtcString =('false','true');
  begin
  Result.Add('{');
  Result.Add(RTC_JSON_DataSetFieldsName);
  Result.Add(':[');
  for idx:=0 to GetFieldCount-1 do
    begin
    if idx>0 then Result.Add(',');
    fname:=GetFieldName(idx);
    Result.Add('{"name":"');
    Result.Add(JSON_EncodeString(fname));
    Result.Add('","type":"');
    Result.Add(RTC_FIELD2STR_CONV[GetFieldType(fname)]);
    Result.Add('"');
    fs:=GetFieldSize(fname);
    if fs<>0 then
      begin
      Result.Add(',"size":');
      Result.Add(Int2Str(fs));
      end;
    fr:=GetFieldRequired(fname);
    if fr then
      begin
      Result.Add(',"req":');
      Result.Add(BoolToStr[fr]);
      end;
    Result.Add('}');
    end;
  Result.Add('],');
  Result.Add(RTC_JSON_DataSetRowsName);
  Result.Add(':[');
  for idx:=0 to GetRowCount-1 do
    begin
    if idx>0 then Result.Add(',');
    gobj:=TRtcValueObject(FData.Items[idx]);
    if assigned(gobj) then
      gobj.to_JSON(Result)
    else
      Result.Add(nullValueJSON);
    end;
  Result.Add(']}');
  end;

procedure TRtcDataSet.from_JSON(const s:RtcWideString; var at: integer);
  var
    fname:RtcWideString;
    a,idx:integer;
    gobj:TRtcValueObject;
    xarr:TRtcArray;
    xrec:TRtcRecord;
  begin
  if assigned(FData) or assigned(FNames) then
    raise ERtcInfo.Create('Can not merge DataSet data. TRtcDataSet object is already in use.');

  json_skipTag('{',s,at);

  try
    repeat
      if json_checkTag(RTC_JSON_DataSetFieldsName,s,at,true) then
        begin
        if assigned(FNames) then
          raise ERtcInfo.Create('JSON Error parsing DataSet: {'+RTC_JSON_DataSetFieldsName+'} already parsed, second Fields definition found.');

        FNames:=tRtcFastStringObjList.Create;

        json_skipTag(':',s,at);

        gobj:=ObjectFromJSON(s,at);
        try
          if assigned(gobj) and (gobj.GetType<>rtc_Array) then
            raise ERtcInfo.Create('JSON Error parsing Dataset: DataSet Fields definitions inside [] expected, different data type found.');

          xarr:=TRtcArray(gobj);
          for a:=0 to xarr.Count-1 do
            begin
            if xarr.isType[a]<>rtc_Record then
              raise ERtcInfo.Create('JSON Error parsing Dataset: Field definition inside {} expected, different data type found.');

            xrec:=xarr.GetRecord(a);
            fname:=xrec.GetText('name');
            SetFieldType(fname,StrToFieldType(xrec.GetString('type')));
            SetFieldSize(fname,xrec.GetInteger('size'));
            SetFieldRequired(fname,xrec.GetBoolean('req'));
            end;
        finally
          gobj.Free;
          end;
        json_checkTag(',',s,at,true);
        end
      else if json_checkTag(RTC_JSON_DataSetRowsName,s,at,true) then
        begin
        if assigned(FData) then
          raise ERtcInfo.Create('JSON Error parsing DataSet: {'+RTC_JSON_DataSetRowsName+'} already parsed, second Rows definition found.');

        json_skipTag(':',s,at);

        gobj:=ObjectFromJSON(s,at);
        try
          if assigned(gobj) and (gobj.GetType<>rtc_Array) then
            raise ERtcInfo.Create('JSON Error parsing Dataset: DataSet Fields definitions inside [] expected, different data type found.');

          xarr:=TRtcArray(gobj);
          FData:=xarr.FValues;
          xarr.FValues:=nil;
        finally
          gobj.Free;
          end;
        json_checkTag(',',s,at,true);
        end
      else if json_checkTag('}',s,at,true) then
        Break
      else if not assigned(FData) and not assigned(FNames) then
        raise ERtcInfo.Create('JSON Error parsing DataSet: '+
                                RTC_JSON_DataSetFieldsName+' or '+
                                RTC_JSON_DataSetRowsName+' in {} expected.')
      else
        raise ERtcInfo.Create('JSON Error parsing DataSet: Missing "}", object structure incomplete.');
      until False;

    if not assigned(FNames) then
      begin
      if not assigned(FData) then
        begin
        FData:=TObjectList.Create;
        FNames:=tRtcFastStringObjList.Create;
        end
      else
        raise ERtcInfo.Create('JSON Error parsing DataSet: Field definitions missing, {'+RTC_JSON_DataSetRowsName+'} not found.');
      end
    else if not assigned(FData) then
      FData:=TObjectList.Create;
  except
    on E:Exception do
      begin
      if LOG_INFO_ERRORS then
        Log('TRtcDataSet.from_JSON('+IntToStr(at)+')',E,'INFO');
      if assigned(FData) then
        begin
        for idx:=0 to FData.Count-1 do
          begin
          gobj:=TRtcValueObject(FData.Items[idx]);
          gobj.Free;
          end;
        RtcFreeAndNil(FData);
        end;
      if assigned(FNames) then
        begin
        FNames.DestroyObjects;
        RtcFreeAndNil(FNames);
        end;
      raise;
      end;
    end;
  end;

procedure TRtcDataSet.to_XMLRPC(const Result:TRtcHugeString);
  var
    idx:integer;
    gobj:TRtcValueObject;
    fname:RtcWideString;
    fs:integer;
    fr:boolean;
  const
    BoolToStr:array[boolean] of RtcString =('0','1');
  begin
  Result.Add('<value><struct><member><name>');
  Result.Add(RTC_XMLRPC_DataSetFieldsName);
  Result.Add('</name>'#13#10'<value><array><data>'#13#10);
  for idx:=0 to GetFieldCount-1 do
    begin
    fname:=GetFieldName(idx);
    Result.Add('<value><struct>'#13#10'<member><name>Name</name><value>');
    Result.Add(xmlrpc_writeNameString(fname));
    Result.Add('</value></member>'#13#10'<member><name>Type</name><value>');
    Result.Add(RTC_FIELD2STR_CONV[GetFieldType(fname)]);
    Result.Add('</value></member>'#13#10);
    fs:=GetFieldSize(fname);
    if fs<>0 then
      begin
      Result.Add('<member><name>Size</name><value><int>');
      Result.Add(Int2Str(fs));
      Result.Add('</int></value></member>'#13#10);
      end;
    fr:=GetFieldRequired(fname);
    if fr then
      begin
      Result.Add('<member><name>Required</name><value><boolean>');
      Result.Add(BoolToStr[fr]);
      Result.Add('</boolean></value></member>'#13#10);
      end;
    Result.Add('</struct></value>'#13#10);
    end;
  Result.Add('</data></array></value></member>'#13#10'<member><name>'+
              RTC_XMLRPC_DataSetRowsName+
              '</name>'#13#10'<value><array><data>'#13#10);
  for idx:=0 to GetRowCount-1 do
    begin
    gobj:=TRtcValueObject(FData.Items[idx]);
    if assigned(gobj) then
      gobj.to_XMLRPC(Result)
    else
      Result.Add(nullValueXMLrpc);
    Result.Add(#13#10);
    end;
  Result.Add('</data></array></value></member></struct></value>');
  end;

function TRtcDataSet.GetAsString: RtcString;
  var
    res:TRtcHugeString;
    oldrow,idx,cnt:integer;
    b:boolean;
    fname:RtcWideString;
  begin
  res:=TRtcHugeString.Create;
  try
    oldrow:=Row;
    try
      First;
      cnt:=GetFieldCount;
      while not Eof do
        begin
        b:=False;
        Res.Add('(');
        for idx:=0 to cnt-1 do
          begin
          fname:=GetFieldName(idx);
          if not GetNull(fname) then
            begin
            if b then Res.Add('; ') else b:=True;
            Res.Add(RtcString(fname));
            Res.Add('=');
            Res.Add(GetString(fname));
            end;
          end;
        Res.Add(')');
        if not Eof then
          Res.Add(#13#10);
        Next;
        end;
    finally
      Row:=oldrow;
      end;
    Result:=res.Get;
  finally
    res.Free;
    end;
  end;

function TRtcDataSet.GetAsText:RtcWideString;
  var
    res:TRtcHugeString;
    oldrow,idx,cnt:integer;
    b:boolean;
    fname:RtcWideString;
  begin
  res:=TRtcHugeString.Create;
  try
    oldrow:=Row;
    try
      First;
      cnt:=GetFieldCount;
      while not Eof do
        begin
        b:=False;
        Res.Add('(');
        for idx:=0 to cnt-1 do
          begin
          fname:=GetFieldName(idx);
          if not GetNull(fname) then
            begin
            if b then Res.Add('; ') else b:=True;
            Res.Add(Utf8Encode(fname));
            Res.Add('=');
            Res.Add(Utf8Encode(GetText(fname)));
            end;
          end;
        Res.Add(')');
        if not Eof then
          Res.Add(#13#10);
        Next;
        end;
    finally
      Row:=oldrow;
      end;
    Result:=Utf8Decode(res.Get);
  finally
    res.Free;
    end;
  end;

function TRtcDataSet.GetAsWideString: RtcWideString;
  var
    res:TRtcHugeString;
    oldrow,idx,cnt:integer;
    b:boolean;
    fname:RtcWideString;
  begin
  res:=TRtcHugeString.Create;
  try
    oldrow:=Row;
    try
      First;
      cnt:=GetFieldCount;
      while not Eof do
        begin
        b:=False;
        Res.Add('(');
        for idx:=0 to cnt-1 do
          begin
          fname:=GetFieldName(idx);
          if not GetNull(fname) then
            begin
            if b then Res.Add('; ') else b:=True;
            Res.Add(Utf8Encode(fname));
            Res.Add('=');
            Res.Add(Utf8Encode(GetWideString(fname)));
            end;
          end;
        Res.Add(')');
        if not Eof then
          Res.Add(#13#10);
        Next;
        end;
    finally
      Row:=oldrow;
      end;
    Result:=Utf8Decode(res.Get);
  finally
    res.Free;
    end;
  end;

procedure TRtcDataSet.from_XMLrpc(const s: RtcString; var at: integer);
  var
    fname:RtcWideString;
    s1:RtcString;
    a,idx:integer;
    gobj:TRtcValueObject;
    xarr:TRtcArray;
    xrec:TRtcRecord;
    tags:rtcClosingTagsType;
    xtag:RtcString;
    o_memb:boolean;
  begin
  if assigned(FData) or assigned(FNames) then
    raise ERtcInfo.Create('Can not merge DataSet data. TRtcDataSet object is already in use.');

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRUCT',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    try
      xtag:=Upper_Case(xmlrpc_checkTag(s,at));
      if (xtag='MEMBER') or (xtag='NAME') then
        begin
        o_memb:=xtag='MEMBER';
        repeat
          if o_memb then xmlrpc_skipTag(s,at); // <member>

          xmlrpc_readTag(s,at,'NAME');
          s1:=Upper_Case(xmlrpc_readTrimValue(s,at));
          xmlrpc_readTag(s,at,'/NAME');

          if s1=RTC_XMLRPC_DataSetFieldsName then
            begin
            if assigned(FNames) then
              raise ERtcInfo.Create('XML-RPC Error parsing DataSet: <member><name>'+RTC_XMLRPC_DataSetFieldsName+'</name> already parsed, second Fields definition found.');

            FNames:=tRtcFastStringObjList.Create;

            gobj:=ObjectFromXMLrpc(s,at);
            try
              if assigned(gobj) and (gobj.GetType<>rtc_Array) then
                raise ERtcInfo.Create('XML-RPC Error parsing Dataset: DataSet Fields definitions inside <array> expected, different data type found.');
              xarr:=TRtcArray(gobj);
              for a:=0 to xarr.Count-1 do
                begin
                if xarr.isType[a]<>rtc_Record then
                  raise ERtcInfo.Create('XML-RPC Error parsing Dataset: Field definition inside <struc> expected, different data type found.');

                xrec:=xarr.GetRecord(a);
                fname:=xrec.GetText('Name');
                SetFieldType(fname,StrToFieldType(xrec.GetString('Type')));
                SetFieldSize(fname,xrec.GetInteger('Size'));
                SetFieldRequired(fname,xrec.GetBoolean('Required'));
                end;
            finally
              gobj.Free;
              end;
            end
          else if s1=RTC_XMLRPC_DataSetRowsName then
            begin
            if assigned(FData) then
              raise ERtcInfo.Create('XML-RPC Error parsing DataSet: <member><name>'+RTC_XMLRPC_DataSetRowsName+'</name> already parsed, second Rows definition found.');

            gobj:=ObjectFromXMLrpc(s,at);
            try
              if assigned(gobj) and (gobj.GetType<>rtc_Array) then
                raise ERtcInfo.Create('XML-RPC Error parsing Dataset: DataSet Fields definitions inside <array> expected, different data type found.');

              xarr:=TRtcArray(gobj);
              FData:=xarr.FValues;
              xarr.FValues:=nil;
            finally
              gobj.Free;
              end;
            end
          else
            raise ERtcInfo.Create('XML-RPC Error parsing DataSet: "'+RTC_XMLRPC_DataSetFieldsName+'" or "'+
                                   RTC_XMLRPC_DataSetRowsName+'" in <name> expected, but "'+String(s1)+'" found.');

          if o_memb then
            xmlrpc_readTag(s,at,'/MEMBER');

          xtag:=Upper_Case(xmlrpc_checkTag(s,at));
          o_memb:=xtag='MEMBER';

          until (xtag<>'MEMBER') and (xtag<>'NAME');
        end;

      if not assigned(FNames) then
        begin
        if not assigned(FData) then
          begin
          FData:=TObjectList.Create;
          FNames:=tRtcFastStringObjList.Create;
          end
        else
          raise ERtcInfo.Create('XML-RPC Error parsing DataSet: Field definitions missing, tags <member><name>'+RTC_XMLRPC_DataSetRowsName+'</name> not found.');
        end
      else if not assigned(FData) then
        FData:=TObjectList.Create;

    except
      on E:Exception do
        begin
        if LOG_INFO_ERRORS then
          Log('TRtcDataSet.from_XMLrpc('+IntToStr(at)+')',E,'INFO');
        if assigned(FData) then
          begin
          for idx:=0 to FData.Count-1 do
            begin
            gobj:=TRtcValueObject(FData.Items[idx]);
            gobj.Free;
            end;
          RtcFreeAndNil(FData);
          end;
        if assigned(FNames) then
          begin
          FNames.DestroyObjects;
          RtcFreeAndNil(FNames);
          end;
        raise;
        end;
      end;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcDataRow.SetField(const FldName:RtcWideString; FldType: TRtcFieldTypes; FldSize: integer; FldRequired: boolean);
  begin
  SetFieldType(FldName,FldType);
  SetFieldSize(FldName,FldSize);
  SetFieldRequired(FldName,FldRequired);
  end;

procedure TRtcDataRow.Set_Field(const FldName: RtcString; FldType: TRtcFieldTypes; FldSize: integer; FldRequired: boolean);
  begin
  SetField(RtcWideString(FldName),FldType,FldSize,FldRequired);
  end;

procedure TRtcDataRow.Set_FieldName(index: integer; const pValue: RtcString);
  begin
  SetFieldName(index,RtcWideString(pValue));
  end;

function TRtcDataRow.FieldByName(const index:RtcWideString): TRtcValue;
  var
    idx:integer;
    myrow:TRtcArray;
    gobj:TRtcValueObject;
  begin
  if assigned(FData) and assigned(FNames) and
     (FRow>=0) and (FRow<FData.Count) and
     assigned(FData.Items[FRow]) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      begin
      myrow:=TRtcArray(FData.Items[FRow]);
      gobj:=myrow.GetObject(idx);
      if gobj=nil then
        begin
        gobj:=TRtcValue.Create;
        myrow.SetObject(idx,gobj);
        end
      else if not (gobj is TRtcValue) then
        begin
        gobj:=TRtcValue.Create;
        TRtcValue(gobj).SetObject(myrow.GetObject(idx)); // store old reference
        myrow.SetObject(idx,nil); // remove old reference
        myrow.SetObject(idx,gobj); // assign new reference
        end;
      Result:=TRtcValue(gobj);
      end
    else
      begin
      gobj:=TRtcValue.Create;
      SetObject(index,gobj);
      Result:=TRtcValue(gobj);
      end;
    end
  else
    begin
    gobj:=TRtcValue.Create;
    SetObject(index,gobj);
    Result:=TRtcValue(gobj);
    end;
  end;

function TRtcDataRow.Field_ByName(const index: RtcString): TRtcValue;
  begin
  Result:=FieldByName(RtcWideString(index));
  end;

procedure TRtcDataRow.Extract(const index:RtcWideString);
  var
    idx:integer;
    myrow:TRtcArray;
  begin
  if FRow<0 then
    raise ERtcInfo.Create('TRtcDataSet.SetObject: Row index beyond Bof.')
  else if index='' then
    raise ERtcInfo.Create('TRtcDataSet.SetObject: Fields without a name not allowed. Set Field Data by using a Field Name.');

  if assigned(FData) and assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then // name found
      begin
      if (FRow>=0) and (FRow<FData.Count) then // row number exists
        begin
        myrow:=TRtcArray(FData.Items[FRow]);
        if assigned(myrow) then
          myrow.Extract(idx);
        end;
      end;
    end;
  end;

procedure TRtcDataRow._Extract(const index:RtcString);
  begin
  Extract(RtcWideString(index));
  end;

function TRtcDataSet.TypeCheck(typ: TRtcValueTypes): boolean;
  begin
  Result:=typ in RTC_DATASET_TYPES;
  end;

{ TRtcDataSetChanges }

constructor TRtcDataSetChanges.Create(Changes: TRtcValueObject);
  begin
  inherited Create;
  if not assigned(Changes) then
    raise ERtcInfo.Create('Can not create TRtcDataSetChanges from NIL');
  FChanges:=TRtcValue.Create;
  FChanges.asObject:=Changes;
  if FChanges.isNull then
    raise ERtcInfo.Create('Can not create TRtcDataSetChanges from NULL');
  if FChanges.isType<>rtc_Array then
    raise ERtcInfo.Create('"Changes" object is invalid (not Array)');
  if FChanges.asArray.isType[0]<>rtc_Array then
    raise ERtcInfo.Create('"Changes" object is invalid (1st parameter is not an Array)');
  if FChanges.asArray.isType[1]<>rtc_DataSet then
    raise ERtcInfo.Create('"Changes" object is invalid (2nd parameter is not a DataSet)');
  FChangeList:=FChanges.asArray.asArray[0];
  FChangeSet:=FChanges.asArray.asDataSet[1];
  if FChangeList.Count<=0 then
    raise ERtcInfo.Create('"Changes" Array is empty');
  if FChangeSet.RowCount<=0 then
    raise ERtcInfo.Create('"Changes" DataSet is empty');
  FPosition:=0;
  FTmpRow:=nil;
  end;

destructor TRtcDataSetChanges.Destroy;
  begin
  RtcFreeAndNil(FTmpRow);
  FChanges.asObject:=nil;
  RtcFreeAndNil(FChanges);
  FChangeSet:=nil;
  FChangeList:=nil;
  inherited;
  end;

function TRtcDataSetChanges.Count: integer;
  begin
  Result:=FChangeList.Count;
  end;

function TRtcDataSetChanges.Bof: boolean;
  begin
  Result:=FPosition<0;
  end;

function TRtcDataSetChanges.Eof: boolean;
  begin
  Result:=FPosition>=FChangeList.Count;
  end;

procedure TRtcDataSetChanges.First;
  begin
  RtcFreeAndNil(FTmpRow);
  FPosition:=0;
  FChangeSet.First;
  end;

procedure TRtcDataSetChanges.Last;
  begin
  RtcFreeAndNil(FTmpRow);
  FPosition:=FChangeList.Count-1;
  FChangeSet.Last;
  if FChangeList.asInteger[FPosition]=Ord(rds_Update) then
    FChangeSet.Prior;
  end;

procedure TRtcDataSetChanges.Next;
  begin
  if FPosition<0 then
    First
  else if FPosition<FChangeList.Count then
    begin
    RtcFreeAndNil(FTmpRow);
    Inc(FPosition);
    if FPosition<FChangeList.Count then
      begin
      if FChangeList.asInteger[FPosition-1]=Ord(rds_Update) then
        FChangeSet.Next;
      FChangeSet.Next;
      end;
    end;
  end;

procedure TRtcDataSetChanges.Prior;
  begin
  if FPosition>=FChangeList.Count then
    Last
  else if FPosition>=0 then
    begin
    RtcFreeAndNil(FTmpRow);
    Dec(FPosition);
    if FPosition>=0 then
      begin
      FChangeSet.Prior;
      if FChangeList.asInteger[FPosition]=Ord(rds_Update) then
        FChangeSet.Prior;
      end;
    end;
  end;

function TRtcDataSetChanges.Action: TRtcDataSetAction;
  begin
  if (FPosition>=0) and (FPosition<FChangeList.Count) then
    Result := TRtcDataSetAction(FChangeList.asInteger[FPosition])
  else
    Result := rds_None;
  end;

function TRtcDataSetChanges.OldRow: TRtcDataRow;
  begin
  if (FPosition>=0) and (FPosition<FChangeList.Count) then
    begin
    case TRtcDataSetAction(FChangeList.asInteger[FPosition]) of
      rds_Update,
      rds_Delete:
        Result:=FChangeSet;
      else
        Result:=nil;
      end;
    end
  else
    Result:=nil;
  end;

function TRtcDataSetChanges.NewRow: TRtcDataRow;
  begin
  if (FPosition>=0) and (FPosition<FChangeList.Count) then
    begin
    case TRtcDataSetAction(FChangeList.asInteger[FPosition]) of
      rds_Insert:
        Result:=FChangeSet;
      rds_Update:
        begin
        if not assigned(FTmpRow) then
          begin
          FChangeSet.Next;
          FTmpRow:=TRtcDataRow.CreateFromRtcDataSet(FChangeSet);
          FChangeSet.Prior;
          end;
        Result:=FTmpRow;
        end;
      else
        Result:=nil;
      end;
    end
  else
    Result:=nil;
  end;

function TRtcDataSetChanges.GetActionSQL(const TableName:RtcWideString):RtcWideString;
  var
    fld:integer;
    fname,s1,s2:RtcWideString;
    nrow,orow:TRtcDataRow;
  begin
  Result:='';
  case self.Action of
    rds_Insert:
      begin
      s1:='';
      s2:='';
      nrow:=self.NewRow;
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if not nrow.isNull[fname] then
          begin
          if s1<>'' then s1:=s1+', ';
          if s2<>'' then s2:=s2+', ';
          s1:=s1+'"'+fname+'"';
          s2:=s2+''''+nrow.asText[fname]+'''';
          end;
        end;
      Result:='INSERT INTO '+TableName+' ('+s1+') VALUES ('+s2+')';
      end;
    rds_Update:
      begin
      s1:='';
      orow:=self.OldRow;
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if s1<>'' then s1:=s1+' AND ';
        if orow.isNull[fname] then
          s1:=s1+'("'+fname+'" is NULL)'
        else
          s1:=s1+'("'+fname+'" = '''+orow.asText[fname]+''')';
        end;
      s2:='';
      nrow:=self.NewRow;
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if orow.asCode[fname]<>nrow.asCode[fname] then
          begin
          if s2<>'' then s2:=s2+', ';
          if nrow.isNull[fname] then
            s2:=s2+'"'+fname+'" = NULL'
          else
            s2:=s2+'"'+fname+'" = '''+nrow.asText[fname]+'''';
          end;
        end;
      Result:='UPDATE '+TableName+' SET '+s2+' WHERE '+s1;
      end;
    rds_Delete:
      begin
      s1:='';
      orow:=self.OldRow;
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if s1<>'' then s1:=s1+' AND ';
        if orow.isNull[fname] then
          s1:=s1+'("'+fname+'" is NULL)'
        else
          s1:=s1+'("'+fname+'" = '''+orow.asText[fname]+''')';
        end;
      Result:='DELETE FROM '+TableName+' WHERE '+s1;
      end;
    end;
  end;

function TRtcDataSetChanges.GetReverseSQL(const TableName:RtcWideString):RtcWideString;
  var
    fld:integer;
    fname,s1,s2:RtcWideString;
    nrow,orow:TRtcDataRow;
  begin
  Result:='';
  case self.Action of
    rds_Delete:
      begin
      s1:='';
      s2:='';
      orow:=self.OldRow;
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if not orow.isNull[fname] then
          begin
          if s1<>'' then s1:=s1+', ';
          if s2<>'' then s2:=s2+', ';
          s1:=s1+'"'+fname+'"';
          s2:=s2+''''+orow.asText[fname]+'''';
          end;
        end;
      Result:='INSERT INTO '+TableName+' ('+s1+') VALUES ('+s2+')';
      end;
    rds_Update:
      begin
      s1:='';
      nrow:=self.NewRow;
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if s1<>'' then s1:=s1+' AND ';
        if nrow.isNull[fname] then
          s1:=s1+'("'+fname+'" is NULL)'
        else
          s1:=s1+'("'+fname+'" = '''+nrow.asText[fname]+''')';
        end;
      s2:='';
      orow:=self.OldRow;
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if orow.asCode[fname]<>nrow.asCode[fname] then
          begin
          if s2<>'' then s2:=s2+', ';
          if orow.isNull[fname] then
            s2:=s2+'"'+fname+'" = NULL'
          else
            s2:=s2+'"'+fname+'" = '''+orow.asText[fname]+'''';
          end;
        end;
      Result:='UPDATE '+TableName+' SET '+s2+' WHERE '+s1;
      end;
    rds_Insert:
      begin
      s1:='';
      nrow:=self.NewRow;
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if s1<>'' then s1:=s1+' AND ';
        if nrow.isNull[fname] then
          s1:=s1+'("'+fname+'" is NULL)'
        else
          s1:=s1+'("'+fname+'" = '''+nrow.asText[fname]+''')';
        end;
      Result:='DELETE FROM '+TableName+' WHERE '+s1;
      end;
    end;
  end;

{ TRtcInfo }

constructor TRtcInfo.Create;
  begin
  inherited;
  ObjList:=nil;
  end;

procedure TRtcInfo.Clear;
  var
    index:RtcWideString;
    ob:TObject;
  begin
  inherited;
  if assigned(ObjList) then
    begin
    while ObjList.Count>0 do
      begin
      ob:=nil;
      index:=ObjList.search_min(ob);
      {$IFDEF RTC_EXTDEBUG}
      if index='' then
        Log('   * ERROR! TRtcInfo.Clear - ObjList.search_min returned empty string.','INFO');
      {$ENDIF}
      ObjList.remove(index);
      try
        if assigned(ob) then
          if ob is TRtcObject then
            TRtcObject(ob).Kill;
      except
        on E:Exception do
          if LOG_INFO_ERRORS then
            Log('TRtcInfo.Clear Obj('+index+')',E,'INFO');
        end;
      end;
    RtcFreeAndNil(ObjList);
    end;
  end;

function TRtcInfo.Get_Object(const index:RtcWideString): TObject;
  begin
  if assigned(ObjList) then
  {$IFDEF UNICODE}
    Result:=ObjList.search(UpperCase(index))
  {$ELSE}
    Result:=ObjList.search(UpperCaseStr(index))
  {$ENDIF}
  else
    Result:=nil;
  end;

procedure TRtcInfo.Set_Object(const index:RtcWideString; pObj: TObject);
  var
    uindex:RtcWideString;
    ob:TObject;
  begin
  if not assigned(ObjList) then
    ObjList:=tStringObjList.Create(8);

{$IFDEF UNICODE}
  uindex:=UpperCase(index);
{$ELSE}
  uindex:=UpperCaseStr(index);
{$ENDIF}
  ob:=ObjList.search(uindex);
  if ob<>nil then
    begin
    if pObj=nil then
      ObjList.remove(uindex)
    else
      begin
      // Kill the old object ONLY if it was directly replaced with another one
      if ob is TRtcObject then
        TRtcObject(ob).Kill;
      ObjList.change(uindex, pObj);
      end;
    end
  else if pObj<>nil then
    ObjList.insert(uindex, pObj);
  end;

function TRtcInfo._Get_ChildInfo(const index: RtcString): TRtcInfo;
  begin
  Result:=Get_ChildInfo(RtcWideString(index));
  end;

function TRtcInfo._Get_Object(const index: RtcString): TObject;
  begin
  Result:=Get_Object(RtcWideString(index));
  end;

function TRtcInfo._NewChild(const index: RtcString): TRtcInfo;
  begin
  Result:=NewChild(RtcWideString(index));
  end;

procedure TRtcInfo._SetNil(const index: RtcString);
  begin
  SetNil(RtcWideString(index));
  end;

procedure TRtcInfo._Set_ChildInfo(const index: RtcString; const pValue: TRtcInfo);
  begin
  Set_ChildInfo(RtcWideString(index),pValue);
  end;

procedure TRtcInfo._Set_Object(const index: RtcString; pObj: TObject);
  begin
  Set_Object(RtcWideString(index),pObj);
  end;

function TRtcInfo.Get_ChildInfo(const index:RtcWideString): TRtcInfo;
  var
    o:TObject;
  begin
  o:=Get_Object(index);
  if (o<>nil) and (o is TRtcInfo) then
    Result:=TRtcInfo(o)
  else if AutoCreate then
    begin
    Result:=TRtcInfo.Create;
    Set_ChildInfo(index,Result);
    end
  else
    Result:=nil;
  end;

procedure TRtcInfo.Set_ChildInfo(const index:RtcWideString; const pValue: TRtcInfo);
  begin
  Set_Object(index, pValue);
  end;

function TRtcInfo.Count: integer;
  begin
  Result:=inherited Count + ObjCount;
  end;

function TRtcInfo.ObjCount: integer;
  begin
  if assigned(ObjList) then
    Result:=ObjList.Count
  else
    Result:=0;
  end;

const
  CRLF=RtcString(#13#10);

var
  CRLF_Bytes:RtcByteArray;

type
  TStringObject = class(TObject)
  public
    value:RtcString;
    end;

  TStringArray = class(TObject)
  private
    data:array of RtcString;

    function GetValue: RtcString;
    procedure SetValue(const Value: RtcString);

    function GetElement(i: integer): RtcString;
    procedure SetElement(i: integer; const Value: RtcString);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count:integer;
    procedure AddValue(const s:RtcString);

    // Setting will add an element to the list, reading will return last element from the list
    property Value:RtcString read GetValue write SetValue;
    property Element[i:integer]:RtcString read GetElement write SetElement;
    end;

  TUploadFileObject = class(TObject)
  public
    filename:RtcString;
    start,count:longint;
    end;

procedure TRtcInfo.SetNil(const index:RtcWideString);
  var
    cname:RtcWideString;
    gobj:TObject;
  begin
  gobj:=Get_Object(index);
  if gobj<>nil then
    begin
    try
      if gobj is TRtcObject then
        TRtcObject(gobj).Kill;
    except
      on E:Exception do
        if LOG_INFO_ERRORS then
          begin
          try
            cname:=gobj.ClassName;
          except
            cname:='?';
            end;
          Log('TRtcInfo.SetNil('+index+':'+cname+')',E,'INFO');
          end;
      end;
    Set_Object(index,nil);
    end;
  end;

function TRtcInfo.NewChild(const index:RtcWideString): TRtcInfo;
  begin
  Result:=TRtcInfo.Create;
  Set_ChildInfo(index,Result);
  end;

{ TRtcHttpHeader }

constructor TRtcHttpHeader.Create;
  begin
  inherited;
  FValues:=tRtcFastStrObjList.Create;
  FCookie:=nil;
  end;

destructor TRtcHttpHeader.Destroy;
  begin
  try
    FValues.DestroyObjects;
    RtcFreeAndNil(FValues);
    RtcFreeAndNil(FCookie);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcHttpHeader.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcHttpHeader.Clear;
  begin
  FValues.DestroyObjects;
  FValues.Clear;
  RtcFreeAndNil(FCookie);
  end;

function TRtcHttpHeader.GetHeaderCS(const index: RtcString): RtcString;
  var
    i:integer;
    obj:TObject;
  begin
  if isCookieNameCS(index) then
    begin
    if not assigned(FCookie) then
      Result:=''
    else
      Result:=FCookie.Text;
    end
  else
    begin
    i:=FValues.FindCS(index);
    if i>=0 then
      begin
      obj:=FValues.Objects[i];
      if not assigned(obj) then
        Result:=''
      else if obj is TStringObject then
        Result:=TStringObject(obj).Value
      else
        Result:='';
      end
    else
      Result:='';
    end;
  end;

procedure TRtcHttpHeader.SetHeaderCS(const index: RtcString; const Value: RtcString);
  var
    i:integer;
    obj:TObject;
  begin
  if isCookieNameCS(index) then
    begin
    if Cookie.Text='' then
      Cookie.Text:=Value
    else
      Cookie.Text:=Cookie.Text+';'+Value;
    end
  else
    begin
    i:=FValues.FindCS(index);
    if i>=0 then
      begin
      obj:=FValues.Objects[i];
      if not assigned(obj) then
        begin
        obj:=TStringObject.Create;
        TStringObject(obj).Value:=Value;
        FValues.Objects[i]:=obj;
        end
      else
        begin
        if not(obj is TStringObject) then
          begin
          obj.Free;
          obj:=TStringObject.Create;
          FValues.Objects[i]:=obj;
          end;
        TStringObject(obj).Value:=Value;
        end;
      end
    else
      begin
      obj:=TStringObject.Create;
      TStringObject(obj).value:=Value;
      FValues.AddCS(index, obj);
      end;
    end;
  end;

function TRtcHttpHeader.GetHeader(const index: RtcString): RtcString;
  var
    i:integer;
    obj:TObject;
  begin
  if isCookieName(index) then
    begin
    if not assigned(FCookie) then
      Result:=''
    else
      Result:=FCookie.Text;
    end
  else
    begin
    i:=FValues.Find(index);
    if i>=0 then
      begin
      obj:=FValues.Objects[i];
      if not assigned(obj) then
        Result:=''
      else if obj is TStringObject then
        Result:=TStringObject(obj).Value
      else
        Result:='';
      end
    else
      Result:='';
    end;
  end;

procedure TRtcHttpHeader.SetHeader(const index, Value: RtcString);
  var
    i:integer;
    obj:TObject;
  begin
  if isCookieName(index) then
    begin
    if Cookie.Text='' then
      Cookie.Text:=Value
    else
      Cookie.Text:=Cookie.Text+';'+Value;
    end
  else
    begin
    i:=FValues.Find(index);
    if i>=0 then
      begin
      obj:=FValues.Objects[i];
      if not assigned(obj) then
        begin
        obj:=TStringObject.Create;
        TStringObject(obj).Value:=Value;
        FValues.Objects[i]:=obj;
        end
      else
        begin
        if not(obj is TStringObject) then
          begin
          obj.Free;
          obj:=TStringObject.Create;
          FValues.Objects[i]:=obj;
          end;
        TStringObject(obj).Value:=Value;
        end;
      end
    else
      begin
      obj:=TStringObject.Create;
      TStringObject(obj).value:=Value;
      FValues.Add(index, obj);
      end;
    end;
  end;

function TRtcHttpHeader.GetHeaderCount: integer;
  begin
  if assigned(FCookie) and (FCookie.Text<>'') then
    Result:=FValues.Count+FCookie.ItemCount
  else
    Result:=FValues.Count;
  end;

function TRtcHttpHeader.GetIHeader(index: integer): RtcString;
  var
    obj:TObject;
  begin
  if index<FValues.Count then
    begin
    obj:=FValues.Objects[index];
    if not assigned(obj) then
      Result:=''
    else if obj is TStringObject then
      Result:=TStringObject(obj).Value
    else
      Result:='';
    end
  else
    begin
    Dec(index,FValues.Count);
    if assigned(FCookie) and (index<FCookie.ItemCount) then
      Result:=FCookie.ItemName[index]+'='+FCookie.ItemValue[index]
    else
      Result:='';
    end;
  end;

procedure TRtcHttpHeader.SetIHeader(index: integer; const Value: RtcString);
  var
    obj:TObject;
  begin
  if index<FValues.Count then
    begin
    obj:=FValues.Objects[index];
    if not assigned(obj) then
      begin
      obj:=TStringObject.Create;
      TStringObject(obj).Value:=Value;
      FValues.Objects[index]:=obj;
      end
    else
      begin
      if not (obj is TStringObject) then
        begin
        obj.Free;
        obj:=TStringObject.Create;
        FValues.Objects[index]:=obj;
        end;
      TStringObject(obj).value:=Value;
      end;
    end
  else if (index=FValues.Count) and assigned(FCookie) then
    Cookie.Text:=Value;
  end;

function TRtcHttpHeader.GetIHeaderName(index: integer): RtcString;
  begin
  if index<FValues.Count then
    Result:=FValues.Strings[index]
  else
    begin
    if assigned(FCookie) and (index-FValues.Count<FCookie.ItemCount) then
      Result:=GetCookieName
    else
      Result:='';
    end;
  end;

procedure TRtcHttpHeader.SetIHeaderName(index: integer; const Value: RtcString);
  begin
  if index<FValues.Count then
    FValues.Strings[index]:=Value;
  end;

function TRtcHttpHeader.GetHeaderText: RtcString;
  var
    a:integer;
    v:RtcString;
  begin
  Result:='';
  for a:=0 to GetHeaderCount-1 do
    begin
    v:=GetIHeader(a);
    if v<>'' then
      Result:=Result+GetIHeaderName(a)+': '+v+CRLF;
    end;
  end;

function TRtcHttpHeader.GetContentLength: int64;
  var
    s:RtcString;
  begin
  s:=GetHeaderCS('CONTENT-LENGTH');
  if s='' then
    Result:=0
  else
    Result:=Str2Int64(Trim(s));
  end;

function TRtcHttpHeader.GetChunkedTransferEncoding: boolean;
  var
    s:RtcString;
  begin
  s:=GetHeaderCS('TRANSFER-ENCODING');
  if s='' then
    Result:=False
  else if Upper_Case(s)='CHUNKED' then
    Result:=True
  else
    Result:=False;
  end;

procedure TRtcHttpHeader.SetChunkedTransferEncoding(const pValue: boolean);
  begin
  if pValue then
    SetHeaderCS('TRANSFER-ENCODING','chunked')
  else
    SetHeaderCS('TRANSFER-ENCODING','');
  end;

procedure TRtcHttpHeader.SetContentLength(const pValue: int64);
  begin
  SetHeaderCS('CONTENT-LENGTH',Int2Str(pValue));
  end;

function TRtcHttpHeader.GetContentType: RtcString;
  begin
  Result:=GetHeaderCS('CONTENT-TYPE');
  end;

procedure TRtcHttpHeader.SetContentType(const pValue: RtcString);
  begin
  SetHeaderCS('CONTENT-TYPE',pValue);
  end;

procedure TRtcHttpHeader.SetHeaderText(const pValue: RtcString);
  var
    MyPos:integer;
    StatusLine,
    HeadStr,
    left:RtcString;
  begin
  if pValue='' then Exit;

  HeadStr:=pValue+#10;

  // Scan for all header attributes ...
  MyPos:=PosEx(#10, HeadStr);
  while (MyPos>1) do // at least 1 character inside line
    begin
    if HeadStr[MyPos-1]=#13 then
      StatusLine:=Copy(HeadStr,1,MyPos-2)
    else
      StatusLine:=Copy(HeadStr,1,MyPos-1);

    Delete(HeadStr,1,MyPos);

    MyPos:=PosEx(':',StatusLine);
    if MyPos>0 then
      begin
      left:=TrimCopy(StatusLine,1,MyPos-1);
      Delete(StatusLine,1,MyPos);
      StatusLine:=Trim(StatusLine);
      SetHeader(left,StatusLine);
      end;
    MyPos:=PosEx(#10, HeadStr);
    end;
  end;

function TRtcHttpHeader.GetCookie: TRtcHttpValues;
  begin
  if not assigned(FCookie) then
    FCookie:=TRtcHttpValues.Create;
  Result:=FCookie;
  end;

{ TRtcRequest }

constructor TRtcRequest.Create;
  begin
  inherited;
  FInfo:=nil;
  FQuery:=nil;
  FParams:=nil;
  FFilePath:=nil;
  end;

destructor TRtcRequest.Destroy;
  begin
  try
    FMethod:='';
    FFileName:='';
    FFullName:='';
    RtcFreeAndNil(FInfo);
    RtcFreeAndNil(FQuery);
    RtcFreeAndNil(FParams);
    RtcFreeAndNil(FFilePath);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcRequest.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRequest.Clear;
  begin
  inherited;
  RtcFreeAndNil(FInfo);
  RtcFreeAndNil(FQuery);
  RtcFreeAndNil(FParams);
  RtcFreeAndNil(FFilePath);
  FMethod:='';
  FFileName:='';
  FFullName:='';
  FClose:=False;
  end;

function TRtcRequest.GetRequestAgent: RtcString;
  begin
  Result:=GetHeaderCS('USER-AGENT');
  end;

function TRtcRequest.GetRequestHost: RtcString;
  begin
  Result:=GetHeaderCS('HOST');
  end;

function TRtcRequest.GetRequestReferer: RtcString;
  begin
  Result:= GetHeaderCS('REFERER');
  end;

procedure TRtcRequest.SetRequestAgent(const pValue: RtcString);
  begin
  SetHeaderCS('USER-AGENT',pValue);
  end;

procedure TRtcRequest.SetRequestHost(const pValue: RtcString);
  begin
  SetHeaderCS('HOST',pValue);
  end;

procedure TRtcRequest.SetRequestReferer(const pValue: RtcString);
  begin
  SetHeaderCS('REFERER',pValue);
  end;

function TRtcRequest.GetURI: RtcString;
  begin
  if Query.Text<>'' then
    Result:=FFullName+'?'+Query.Text
  else
    Result:=FFullName;
  end;

procedure TRtcRequest.SetURI(const pValue: RtcString);
  var
    i:integer;
  begin
  i:=PosEx('?',pValue);
  if i<=0 then
    begin
    if FileName='' then
      FileName:=pValue
    else
      FFullName:=pValue;
    Query.Clear;
    end
  else
    begin
    if FileName='' then
      FileName:=Copy(pValue,1,i-1)
    else
      FFullName:=Copy(pValue,1,i-1);
    Query.Text:=Copy(pValue,i+1,length(pValue)-i);
    end;
  end;

function TRtcRequest.GetURL: RtcString;
  begin
  Result:=Host+URI;
  end;

procedure TRtcRequest.SetHeaderText(const pValue: RtcString);
  var
    MyPos:integer;
    StatusLine,
    HeadStr:RtcString;
  begin
  if pValue='' then Exit;

  HeadStr:=pValue+CRLF;

  // scan HTTP Method, FileName and Params
  MyPos:=PosEx(CRLF,HeadStr,1);
  StatusLine:=Copy(HeadStr,1,MyPos-1);
  if (MyPos>8) and
     ((Copy(StatusLine,MyPos-8,7)='HTTP/1.') or
      (Copy(StatusLine,MyPos-9,8)='HTTPS/1.')) then
    begin
    Delete(HeadStr,1,MyPos+Length(CRLF)-1);

    { Our line probably looks like this:
      GET /xyz HTTP/1.1 }
    MyPos:=PosEx(' ',StatusLine); // first space before FileName
    if MyPos>0 then
      begin
      Method:=Copy(StatusLine,1,MyPos-1); // Request Method
      Delete(StatusLine,1,MyPos);

      MyPos:=PosEx(' ',StatusLine); // space after FileName
      if MyPos>0 then
        begin
        URI:=Copy(StatusLine,1,MyPos-1); // Request URI
        Delete(StatusLine,1,MyPos); // StatusText
        end;
      end;
    end;

  inherited SetHeaderText(HeadStr);
  end;

function TRtcRequest.GetMethod: RtcString;
  begin
  Result:=FMethod;
  end;

procedure TRtcRequest.SetMethod(const pValue: RtcString);
  begin
  if FMethod<>pValue then
    FMethod:=Upper_Case(pValue);
  end;

function TRtcRequest.GetCookieName: RtcString;
  begin
  Result:='COOKIE';
  end;

function TRtcRequest.isCookieName(const Value: RtcString): boolean;
  begin
  Result:=Upper_Case(Value)='COOKIE';
  end;

function TRtcRequest.isCookieNameCS(const Value: RtcString): boolean;
  begin
  Result:= Value='COOKIE';
  end;

function TRtcRequest.GetParams: TRtcHttpValues;
  begin
  if not assigned(FParams) then
    FParams:=TRtcHttpValues.Create;
  Result:=FParams;
  end;

function TRtcRequest.GetQuery: TRtcHttpValues;
  begin
  if not assigned(FQuery) then
    FQuery:=TRtcHttpValues.Create;
  Result:=FQuery;
  end;

function TRtcRequest.GetInfo: TRtcInfo;
  begin
  if not assigned(FInfo) then
    FInfo:=TRtcInfo.Create;
  Result:=FInfo;
  end;

procedure TRtcRequest.SetFileName(const pValue: RtcString);
  begin
  if FFileName=FFullName then
    begin
    FFileName := pValue;
    FFullName := pValue;
    end
  else
    FFileName:=pValue;
  if assigned(FFilePath) then
    FFilePath.UpdateFilePath;
  end;

function TRtcRequest.GetForwardedFor: RtcString;
  begin
  Result:=GetHeaderCS('X-FORWARDED-FOR');
  end;

procedure TRtcRequest.SetForwardedFor(const pValue: RtcString);
  begin
  SetHeaderCS('X-FORWARDED-FOR',pValue);
  end;

function TRtcRequest.GetFilePath: TRtcRequestFilePath;
  begin
  if not assigned(FFilePath) then
    FFilePath:=TRtcRequestFilePath.Create(self);
  Result:=FFilePath;
  end;

procedure TRtcRequest.UpdateFileName;
  var
    a:integer;
    s:RtcString;
    b:boolean;
  begin
  b:= FFileName=FFullName;

  s:='';
  if assigned(FFilePath) then
    begin
    if FFilePath.Count>0 then
      for a:=0 to FFilePath.Count-1 do
        s:=s+'/'+FFilePath[a];
    end;
  if s='' then s:='/';

  FFileName:=s;
  if b then FFullName:=s;
  end;

{ TRtcResponse }

constructor TRtcResponse.Create;
  begin
  inherited;
  end;

destructor TRtcResponse.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcResponse.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcResponse.Clear;
  begin
  inherited;
  end;

function TRtcResponse.GetCookieName: RtcString;
  begin
  Result:='SET-COOKIE';
  end;

function TRtcResponse.isCookieName(const Value: RtcString): boolean;
  begin
  Result:=Upper_Case(Value)='SET-COOKIE';
  end;

function TRtcResponse.isCookieNameCS(const Value: RtcString): boolean;
  begin
  Result:= Value='SET-COOKIE';
  end;

procedure TRtcResponse.SetHeaderText(const pValue: RtcString);
  var
    MyPos:integer;
    StatusLine,
    HeadStr,
    left:RtcString;
  begin
  if pValue='' then Exit;

  HeadStr:=pValue+CRLF;

  if length(HeadStr)>6 then
    begin
    if ((HeadStr[5]='/') and (Upper_Case(Copy(HeadStr,1,4))='HTTP')) or
       ((HeadStr[6]='/') and (Upper_Case(Copy(HeadStr,1,5))='HTTPS')) then
      begin
      MyPos:=Pos(CRLF,HeadStr);
      StatusLine:=Copy(HeadStr,1,MyPos-1);
      Delete(HeadStr,1,MyPos+Length(CRLF)-1);

      { Our line probably looks like this:
        HTTP/1.1 200 OK }
      MyPos:=PosEx(' ',StatusLine); // first space before StatusCode
      if MyPos>0 then
        begin
        StatusCode:=0;
        StatusText:='';

        Delete(StatusLine,1,MyPos);

        MyPos:=PosEx(' ',StatusLine); // space after StatusCode
        if MyPos>0 then
          begin
          left:=Copy(StatusLine,1,MyPos-1); // StatusCode
          Delete(StatusLine,1,MyPos); // StatusText

          if (left<>'') and (StatusLine<>'') then
            begin
            try
              StatusCode:=Str2Int64(left);
              StatusText:=StatusLine;
            except
              // if there is something wrong with this, just ignore the exception
              end;
            end;
          end;
        end;
      end;
    end;

  inherited SetHeaderText(HeadStr);
  end;

{ TRtcHttpValues }

constructor TRtcHttpValues.Create;
  begin
  inherited;
  FValChange:=False;
  FTxtChange:=False;
  FCacheSize:=0;
  FTempFileName:='';
  FTempFileSize:=0;
  FValues:=tRtcFastStrObjList.Create;
  FOrigQuery:='';
  FDelimiter:='';
  end;

destructor TRtcHttpValues.Destroy;
  begin
  try
    FDelimiter:='';
    FOrigQuery:='';
    FValues.DestroyObjects;
    RtcFreeAndNil(FValues);
    if FTempFileName<>'' then
      begin
      Delete_File(FTempFileName);
      FTempFileName:='';
      FTempFileSize:=0;
      end;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcHttpValues.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcHttpValues.Clear;
  begin
  FTxtChange:=False;
  FValChange:=False;

  if FTempFileName<>'' then
    Delete_File(FTempFileName);
  FTempFileSize:=0;
  FTempFileName:='';
  FDelimiter:='';
  FOrigQuery:='';

  FValues.DestroyObjects;
  FValues.Clear;
  end;

function TRtcHttpValues.GetItemCount: integer;
  begin
  if FTxtChange then PrepareValues;

  Result:=FValues.Count;
  end;

function TRtcHttpValues.GetItemName(index: integer): RtcString;
  begin
  if FTxtChange then PrepareValues;

  if index<FValues.Count then
    Result:=FValues.Strings[index]
  else
    Result:='';
  end;

function TRtcHttpValues.GetItemValue(index: integer): RtcString;
  var
    obj:TObject;
  begin
  if FTxtChange then PrepareValues;

  if index<FValues.Count then
    begin
    obj:=FValues.objects[index];
    if not assigned(obj) then
      Result:=''
    else if obj is TStringArray then
      Result:=TStringArray(obj).Value
    else if obj is TUploadFileObject then
      Result:=TUploadFileObject(obj).filename
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else
    Result:='';
  end;

function TRtcHttpValues.GetValueCS(const index: RtcString): RtcString;
  var
    i:integer;
    obj:TObject;
  begin
  if FTxtChange then PrepareValues;

  i:=FValues.FindCS(index);
  if i>=0 then
    begin
    obj:=FValues.objects[i];
    if not assigned(obj) then
      Result:=''
    else if obj is TStringArray then
      Result:=TStringArray(obj).Value
    else if obj is TUploadFileObject then
      Result:=TUploadFileObject(obj).filename
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else
    Result:='';
  end;

function TRtcHttpValues.GetElementCount(const index: RtcString): integer;
  var
    i:integer;
    obj:TObject;
  begin
  if FTxtChange then PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    obj:=FValues.objects[i];
    if not assigned(obj) then
      Result:=0
    else if obj is TStringArray then
      Result:=TStringArray(obj).Count
    else if obj is TUploadFileObject then
      Result:=1
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else
    Result:=0;
  end;

function TRtcHttpValues.GetElement(const index: RtcString; loc: integer): RtcString;
  var
    i:integer;
    obj:TObject;
  begin
  if FTxtChange then PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    obj:=FValues.objects[i];
    if not assigned(obj) then
      Result:=''
    else if obj is TStringArray then
      Result:=TStringArray(obj).GetElement(loc)
    else if obj is TUploadFileObject then
      Result:=TUploadFileObject(obj).filename
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else
    Result:='';
  end;

procedure TRtcHttpValues.SetItemName(index: integer; const Value: RtcString);
  begin
  if FTxtChange then PrepareValues;

  if index<FValues.Count then
    if Value<>FValues.Strings[index] then
      begin
      FValues.Strings[index]:=Value;
      FValChange:=True;
      end;
  end;

procedure TRtcHttpValues.SetItemValue(index: integer; const Value: RtcString);
  var
    obj:TObject;
  begin
  if FTxtChange then PrepareValues;

  if index<FValues.Count then
    begin
    obj:=FValues.Objects[index];
    if not assigned(obj) then
      begin
      if Value<>'' then
        begin
        obj:=TStringArray.Create;
        TStringArray(obj).AddValue(Value);
        FValues.Objects[index]:=obj;
        FValChange:=True;
        end;
      end
    else if obj is TStringArray then
      begin
      TStringArray(obj).Value:=Value;
      if TStringArray(obj).Count=0 then
        begin
        FValues.Objects[index]:=nil;
        obj.Free;
        end;
      FValChange:=True;
      end
    else if obj is TUploadFileObject then
      begin
      if TUploadFileObject(obj).filename<>Value then
        begin
        TUploadFileObject(obj).filename:=Value;
        FValChange:=True;
        end;
      end
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end;
  end;

procedure TRtcHttpValues.SetValueCS(const index: RtcString; const Value: RtcString);
  var
    i:integer;
    vobj:TObject;
  begin
  if FTxtChange then PrepareValues;

  i:=FValues.FindCS(index);
  if i>=0 then
    begin
    vobj:=FValues.Objects[i];
    if not assigned(vobj) then
      begin
      if Value<>'' then
        begin
        vobj:=TStringArray.Create;
        TStringArray(vobj).AddValue(Value);
        FValues.Objects[i]:=vobj;
        FValChange:=True;
        end;
      end
    else if vobj is TStringArray then
      begin
      TStringArray(vobj).Value:=Value;
      if TStringArray(vobj).Count=0 then
        begin
        FValues.Objects[i]:=nil;
        vobj.Free;
        end;
      FValChange:=True;
      end
    else if vobj is TUploadFileObject then
      begin
      if TUploadFileObject(vobj).filename<>Value then
        begin
        if Value='' then
          begin
          FValues.Objects[i]:=nil; // Delete(i);
          vobj.Free;
          end
        else
          TUploadFileObject(vobj).filename:=Value;
        FValChange:=True;
        end;
      end
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else if Value<>'' then
    begin
    vobj:=TStringArray.Create;
    TStringArray(vobj).AddValue(Value);
    FValues.AddCS(index, vobj);
    FValChange:=True;
    end;
  end;

function TRtcHttpValues.GetValue(const index: RtcString): RtcString;
  var
    i:integer;
    obj:TObject;
  begin
  if FTxtChange then PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    obj:=FValues.objects[i];
    if not assigned(obj) then
      Result:=''
    else if obj is TStringArray then
      Result:=TStringArray(obj).Value
    else if obj is TUploadFileObject then
      Result:=TUploadFileObject(obj).filename
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else
    Result:='';
  end;

procedure TRtcHttpValues.SetValue(const index, Value: RtcString);
  var
    i:integer;
    vobj:TObject;
  begin
  if FTxtChange then PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    vobj:=FValues.Objects[i];
    if not assigned(vobj) then
      begin
      if Value<>'' then
        begin
        vobj:=TStringArray.Create;
        TStringArray(vobj).AddValue(Value);
        FValues.Objects[i]:=vobj;
        FValChange:=True;
        end;
      end
    else if vobj is TStringArray then
      begin
      TStringArray(vobj).Value:=Value;
      if TStringArray(vobj).Count=0 then
        begin
        FValues.Objects[i]:=nil;
        vobj.Free;
        end;
      FValChange:=True;
      end
    else if vobj is TUploadFileObject then
      begin
      if TUploadFileObject(vobj).filename<>Value then
        begin
        if Value='' then
          begin
          FValues.Objects[i]:=nil; // Delete(i);
          vobj.Free;
          end
        else
          TUploadFileObject(vobj).filename:=Value;
        FValChange:=True;
        end;
      end
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else if Value<>'' then
    begin
    vobj:=TStringArray.Create;
    TStringArray(vobj).AddValue(Value);
    FValues.Add(index, vobj);
    FValChange:=True;
    end;
  end;

procedure TRtcHttpValues.SetElement(const index: RtcString; loc:integer; const Value: RtcString);
  var
    i:integer;
    vobj:TObject;
    arr:TStringArray;
  begin
  if FTxtChange then PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    vobj:=FValues.Objects[i];
    if not assigned(vobj) then
      begin
      if Value<>'' then
        begin
        arr:=TStringArray.Create;
        TStringArray(vobj).Element[loc]:=Value;
        FValues.Objects[i]:=arr;
        FValChange:=True;
        end;
      end
    else if vobj is TStringArray then
      begin
      arr:=TStringArray(vobj);
      arr.Element[loc]:=Value;
      if arr.Count=0 then
        begin
        FValues.Objects[i]:=nil;
        arr.Free;
        end;
      FValChange:=True;
      end
    else if vobj is TUploadFileObject then
      begin
      if TUploadFileObject(vobj).filename<>Value then
        begin
        if Value='' then
          begin
          FValues.Objects[i]:=nil; // Delete(i);
          vobj.Free;
          end
        else
          TUploadFileObject(vobj).filename:=Value;
        FValChange:=True;
        end;
      end
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else if Value<>'' then
    begin
    arr:=TStringArray.Create;
    arr.Element[loc]:=Value;
    FValues.Add(index, arr);
    FValChange:=True;
    end;
  end;

function TRtcHttpValues.GetDelimiter: RtcString;
  begin
  if FTxtChange then PrepareValues;

  if length(FDelimiter)>0 then
    Result:=FDelimiter
  else
    Result:='&';
  end;

procedure TRtcHttpValues.SetDelimiter(const Value: RtcString);
  begin
  if FTxtChange then PrepareValues;

  if Value<>FDelimiter then
    begin
    FDelimiter:=Value;
    FValChange:=True;
    end;
  end;

function TRtcHttpValues.GetAsText: RtcString;
  begin
  PrepareText;

  if FTempFileName<>'' then
    Result:=Read_File(FTempFileName)
  else
    Result:=FOrigQuery;
  end;

procedure TRtcHttpValues.SetText(const pValue: RtcString);
  begin
  Clear;
  AddText(pValue);
  end;

procedure TRtcHttpValues.AddText(const s: RtcString);
  begin
  PrepareText;

  if FTempFileName<>'' then
    begin
    Write_File(FTempFileName,s,FTempFileSize);
    Inc(FTempFileSize,length(s));
    end
  else if ((FCacheSize=0) and (length(FOrigQuery)+length(s)>=RTC_FORMPOST_CACHESIZE)) then
    begin
    FTempFileName:=GetTempFile;
    if FTempFileName='' then
      raise ERtcInfo.Create('Error creating a temporary file.');

    if length(FOrigQuery)>0 then
      begin
      Write_File(FTempFileName,FOrigQuery,FTempFileSize);
      Inc(FTempFileSize,length(FOrigQuery));
      FOrigQuery:='';
      end;
    Write_File(FTempFileName,s,FTempFileSize);
    Inc(FTempFileSize,length(s));
    end
  else if ((FCacheSize>0) and (length(FOrigQuery)+length(s)>=FCacheSize)) then
    begin
    FTempFileName:=GetTempFile;
    if FTempFileName='' then
      raise ERtcInfo.Create('Error creating a temporary file.');

    if length(FOrigQuery)>0 then
      begin
      Write_File(FTempFileName,FOrigQuery,FTempFileSize);
      Inc(FTempFileSize,length(FOrigQuery));
      FOrigQuery:='';
      end;
    Write_File(FTempFileName,s,FTempFileSize);
    Inc(FTempFileSize,length(s));
    end
  else
    FOrigQuery:=FOrigQuery+s;

  FTxtChange:=True;
  end;

procedure TRtcHttpValues.PrepareText;
  var
    a,i,ic,ec:integer;
    n:RtcString;
    dm:RtcString;
  begin
  if not FValChange then Exit;
  FValChange:=False;

  dm:=Delimiter;

  ic:=GetItemCount;
  if ic=0 then
    FOrigQuery:=''
  else if length(dm)>1 then
    begin
    FOrigQuery:='';
    for a:=0 to ic-1 do
      begin
      n:=GetItemName(a);
      ec:=GetElementCount(n);
      if ec>1 then
        begin
        for i:=0 to ec-1 do
          begin
          FOrigQuery:=FOrigQuery+
                  '--'+dm+CRLF+
                  'Content-Disposition: form-data; name="'+n+'"'+CRLF+
                  CRLF+
                  GetElement(n,i)+CRLF;
          end;
        end
      else
        begin
        FOrigQuery:=FOrigQuery+
                '--'+dm+CRLF+
                'Content-Disposition: form-data; name="'+n+'"'+CRLF+
                CRLF+
                GetItemValue(a)+CRLF;
        end;
      end;
    FOrigQuery:=FOrigQuery+'--'+dm+'--'+CRLF;
    end
  else
    begin
    FOrigQuery:='';
    ic:=GetItemCount;
    if ic>0 then
      for a:=0 to ic-1 do
        begin
        n:=GetItemName(a);
        ec:=GetElementCount(n);
        if ec>1 then
          begin
          for i:=0 to ec-1 do
            begin
            if FOrigQuery<>'' then FOrigQuery:=FOrigQuery+dm;
            FOrigQuery:=FOrigQuery+n+'='+GetElement(n,i);
            end;
          end
        else
          begin
          if FOrigQuery<>'' then FOrigQuery:=FOrigQuery+dm;
          FOrigQuery:=FOrigQuery+n+'='+GetItemValue(a);
          end;
        end;
    end;
  end;

procedure TRtcHttpValues.PrepareValues;
  const
    BUFFER:integer=32000;

  var
    MyPos,MyPos2:integer;

    dm,xxdm,
    HeadStr,
    StatusLine,
    left,right:RtcString;

    at,i:integer;
    obj2:TUploadFileObject;
    obj3:TStringArray;

  begin
  FTxtChange:=False;

  if (FOrigQuery='') and (FTempFileName='') then
    begin
    Clear;
    Exit;
    end;

  if (length(FDelimiter)<=1) and (FTempFileName<>'') then
    begin
    FOrigQuery:=Read_File(FTempFileName);
    FTempFileName:='';
    FTempFileSize:=0;
    end;

  if (FTempFileName<>'') then
    begin // MULTIPART/FORM-DATA
    // Jump to first Delimiter (ignore preamble)
    if FDelimiter<>'' then
      dm:=FDelimiter
    else
      dm:='&';
    at:=Scan_File(FTempFileName,RtcStringToBytes(dm),BUFFER,0,FTempFileSize)-2;
    if at>=0 then while Read_File(FTempFileName,at+2,length(dm))=dm do
      begin
      HeadStr:=Read_File(FTempFileName,at,2+length(dm)+2);
      if length(HeadStr)<4+length(dm) then
        Break; // end of file

      if Copy(HeadStr,1,2)<>'--' then
        raise ERtcInfo.Create('Missing "--" at the beginning of a new Multipart data');
      Inc(at,2);

      // skip delimiter
      Inc(at,length(dm));
      if Copy(HeadStr,2+length(dm)+1,2)='--' then
        Break
      else if Copy(HeadStr,2+length(dm)+1,2)<>CRLF then
        raise ERtcInfo.Create('Missing <CRLF> after Delimiter in Multipart text');
      Inc(at,2);

      // Split Content-disposition line from the rest
      MyPos:=Scan_File(FTempFileName,CRLF_Bytes,BUFFER,at,FTempFileSize);
      if MyPos<0 then
        raise ERtcInfo.Create('Missing <CRLF> in Multipart text');
      StatusLine:=Read_File(FTempFileName,at,MyPos-at);
      at:=MyPos+2;

      if StatusLine<>'' then
        begin
        // Remove "Content-disposition:"
        MyPos:=PosEx(':',StatusLine);
        if MyPos<=0 then
          raise ERtcInfo.Create('Missing ":" (after "Content-disposition"?) in Multipart text');
        if Upper_Case(TrimCopy(StatusLine,1,MyPos-1))<>'CONTENT-DISPOSITION' then
          raise ERtcInfo.Create('Missing "Content-disposition" in Multipart data');
        Delete(StatusLine,1,MyPos);

        // Remove "FORM-DATA;"
        MyPos:=PosEx(';',StatusLine);
        if MyPos<=0 then
          raise ERtcInfo.Create('Missing ";" (after "form-data"?) in Multipart text.');
        if Upper_Case(TrimCopy(StatusLine,1,MyPos-1))<>'FORM-DATA' then
          raise ERtcInfo.Create('Invalid disposition type "'+String(Copy(StatusLine,1,MyPos-1))+'", expecting "FORM-DATA" in Multipart text');
        Delete(StatusLine,1,MyPos);

        // Remove "name="
        MyPos:=PosEx('=',StatusLine);
        if MyPos<=0 then
          raise ERtcInfo.Create('Missing "=" (after "name"?) in Multipart text.');
        if Upper_Case(TrimCopy(StatusLine,1,MyPos-1))<>'NAME' then
          raise ERtcInfo.Create('Invalid disposition param "'+String(Copy(StatusLine,1,MyPos-1))+'", expecting "NAME" in Multipart text');
        Delete(StatusLine,1,MyPos);

        // Remove opening <">
        MyPos:=PosEx('"',StatusLine);
        if MyPos<=0 then
          raise ERtcInfo.Create('Parameter names have to be in quotes. Missing opening <"> in Multipart text');
        Delete(StatusLine,1,MyPos);

        // Remove closing <">, get the parameter name and check if rest is clear
        MyPos:=PosEx('"',StatusLine);
        if MyPos<=0 then
          raise ERtcInfo.Create('Parameter names have to be in quotes. Missing closing <"> in Multipart text');

        left:=Copy(StatusLine,1,MyPos-1);

        Delete(StatusLine,1,MyPos);
        StatusLine:=Trim(StatusLine);
        if StatusLine<>'' then
          begin
          // Remove ";" before "filename="
          MyPos:=PosEx(';',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Error. Data after content disposition param: "'+String(StatusLine)+'".');
          Delete(StatusLine,1,MyPos);

          // Remove "filename="
          MyPos:=PosEx('=',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Missing "=" (after "filename"?) in Multipart text.');
          if Upper_Case(TrimCopy(StatusLine,1,MyPos-1))<>'FILENAME' then
            raise ERtcInfo.Create('Invalid disposition param "'+String(Copy(StatusLine,1,MyPos-1))+'", expecting "FILENAME" in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove opening <">
          MyPos:=PosEx('"',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Parameter names have to be in quotes. Missing opening <"> in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove closing <">, get the parameter name and check if rest is clear
          MyPos:=PosEx('"',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Parameter names have to be in quotes. Missing closing <"> in Multipart text');

          right:=Copy(StatusLine,1,MyPos-1);

          Delete(StatusLine,1,MyPos);
          StatusLine:=Trim(StatusLine);

          if StatusLine<>'' then
            raise ERtcInfo.Create('Error. Data after content disposition param: "'+String(StatusLine)+'".');
          end
        else
          right:='';
        end
      else
        raise ERtcInfo.Create('Missing field name in Multipart text.');

      xxdm:='--'+dm;

      StatusLine:=Read_File(FTempFileName,at,2+length(dm));
      if StatusLine=xxdm then
        Continue;

      // Split Content-disposition line from the rest
      MyPos:=Scan_File(FTempFileName,CRLF_Bytes,BUFFER,at,FTempFileSize);
      if MyPos<0 then
        raise ERtcInfo.Create('Missing <CRLF> after content disposition param.');

      while MyPos>at do
        begin
        at:=MyPos+2;
        StatusLine:=Read_File(FTempFileName,at,2+length(dm));
        if StatusLine=xxdm then
          begin
          MyPos:=-1;
          Break;
          end
        else
          begin
          MyPos:=Scan_File(FTempFileName,CRLF_Bytes,BUFFER,at,FTempFileSize);
          if MyPos<=0 then
            raise ERtcInfo.Create('Missing <CRLF> after content header.');
          end;
        end;

      if MyPos<0 then
        Continue;

      Inc(at,2);
      // Split content data part from the rest
      StatusLine:=Read_File(FTempFileName,at,2+length(dm));
      if StatusLine=xxdm then
        Continue;

      MyPos:=Scan_File(FTempFileName,RtcStringToBytes(CRLF+xxdm),BUFFER,at,FTempFileSize);
      if MyPos<0 then
        raise ERtcInfo.Create('Delimiter missing after Multipart data.');

      if right='' then
        begin
        StatusLine:=Read_File(FTempFileName,at,MyPos-at);
        at:=MyPos+2;

        // Save
        i:=FValues.Find(left);
        if i>=0 then
          begin
          if not assigned(FValues.Objects[i]) then
            begin
            obj3:=TStringArray.Create;
            obj3.AddValue(StatusLine);
            FValues.Objects[i]:=obj3;
            end
          else if FValues.Objects[i] is TStringArray then
            begin
            obj3:=TStringArray(FValues.Objects[i]);
            obj3.AddValue(StatusLine);
            end
          else
            raise ERtcInfo.Create('Duplicate value for "'+String(left)+'" in Multipart text');
          end
        else
          begin
          obj3:=TStringArray.Create;
          obj3.AddValue(StatusLine);
          FValues.Add(left, obj3);
          end;
        end
      else
        begin
        // Save
        i:=FValues.Find(left);
        if i>=0 then
          begin
          if FValues.Objects[i]=nil then
            begin
            obj2:=TUploadFileObject.Create;
            obj2.filename:=right;
            obj2.start:=at;
            obj2.count:=MyPos-at;
            FValues.Objects[i]:=obj2;
            end
          else if FValues.Objects[i] is TUploadFileObject then
            begin
            obj2:=TUploadFileObject(FValues.Objects[i]);
            obj2.filename:=right;
            obj2.start:=at;
            obj2.count:=MyPos-at;
            end
          else
            raise ERtcInfo.Create('Duplicate value for "'+String(left)+'" in Multipart text');
          end
        else
          begin
          obj2:=TUploadFileObject.Create;
          obj2.filename:=right;
          obj2.start:=at;
          obj2.count:=MyPos-at;
          FValues.Add(left, obj2);
          end;
        at:=MyPos+2;
        end;
      end;
    end
  else
    begin
    if FDelimiter='' then // Try to recognize the delimiter used in the Query
      begin
      MyPos:=PosEx(';',FOrigQuery);
      MyPos2:=PosEx('&',FOrigQuery);
      if (MyPos>0) and (MyPos2>0) then
        begin
        if MyPos2<MyPos then
          FDelimiter:='&'
        else
          FDelimiter:=';';
        end
      else if (MyPos>0) then
        FDelimiter:=';'
      else if (MyPos2>0) then
        FDelimiter:='&';
      end;

    if FDelimiter<>'' then
      dm:=FDelimiter
    else
      dm:='&';

    if length(dm)>1 then
      begin // MULTIPART/FORM-DATA

      // Jump to first Delmiter (ignore preamble)
      at:=Pos(dm,FOrigQuery)-2;
      if at>0 then while Copy(FOrigQuery,at+2,length(dm))=dm do
        begin
        if at+length(dm)+2>=length(FOrigQuery) then
          Break; // no more data

        if Copy(FOrigQuery,at,2)<>'--' then
          raise ERtcInfo.Create('Missing "--" at the beginning of a new Multipart data');
        Inc(at,2);

        // skip delimiter
        Inc(at,length(dm));
        if Copy(FOrigQuery,at,2)='--' then
          Break
        else if Copy(FOrigQuery,at,2)<>CRLF then
          raise ERtcInfo.Create('Missing <CRLF> after Delimiter in Multipart text');
        Inc(at,2);

        // Split Content-disposition line from the rest
        MyPos:=PosEx(CRLF,FOrigQuery,at);
        if MyPos<=0 then
          raise ERtcInfo.Create('Missing <CRLF> in Multipart text');
        StatusLine:=Copy(FOrigQuery,at,MyPos-at);
        at:=MyPos+2;

        if StatusLine<>'' then
          begin
          // Remove "Content-disposition:"
          MyPos:=PosEx(':',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Missing ":" (after "Content-disposition"?) in Multipart text');
          if Upper_Case(TrimCopy(StatusLine,1,MyPos-1))<>'CONTENT-DISPOSITION' then
            raise ERtcInfo.Create('Missing "Content-disposition" in Multipart data');
          Delete(StatusLine,1,MyPos);

          // Remove "FORM-DATA;"
          MyPos:=PosEx(';',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Missing ";" (after "form-data"?) in Multipart text.');
          if Upper_Case(TrimCopy(StatusLine,1,MyPos-1))<>'FORM-DATA' then
            raise ERtcInfo.Create('Invalid disposition type "'+String(Copy(StatusLine,1,MyPos-1))+'", expecting "FORM-DATA" in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove "name="
          MyPos:=PosEx('=',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Missing "=" (after "name"?) in Multipart text.');
          if Upper_Case(TrimCopy(StatusLine,1,MyPos-1))<>'NAME' then
            raise ERtcInfo.Create('Invalid disposition param "'+String(Copy(StatusLine,1,MyPos-1))+'", expecting "NAME" in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove opening <">
          MyPos:=PosEx('"',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Parameter names have to be in quotes. Missing opening <"> in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove closing <">, get the parameter name and check if rest is clear
          MyPos:=PosEx('"',StatusLine);
          if MyPos<=0 then
            raise ERtcInfo.Create('Parameter names have to be in quotes. Missing closing <"> in Multipart text');

          left:=Copy(StatusLine,1,MyPos-1);

          Delete(StatusLine,1,MyPos);
          StatusLine:=Trim(StatusLine);
          if StatusLine<>'' then
            begin
            // Remove ";" before "filename="
            MyPos:=PosEx(';',StatusLine);
            if MyPos<=0 then
              raise ERtcInfo.Create('Error. Data after content disposition param: "'+String(StatusLine)+'".');
            Delete(StatusLine,1,MyPos);

            // Remove "filename="
            MyPos:=PosEx('=',StatusLine);
            if MyPos<=0 then
              raise ERtcInfo.Create('Missing "=" (after "filename"?) in Multipart text.');
            if Upper_Case(TrimCopy(StatusLine,1,MyPos-1))<>'FILENAME' then
              raise ERtcInfo.Create('Invalid disposition param "'+String(Copy(StatusLine,1,MyPos-1))+'", expecting "FILENAME" in Multipart text');
            Delete(StatusLine,1,MyPos);

            // Remove opening <">
            MyPos:=PosEx('"',StatusLine);
            if MyPos<=0 then
              raise ERtcInfo.Create('Parameter names have to be in quotes. Missing opening <"> in Multipart text');
            Delete(StatusLine,1,MyPos);

            // Remove closing <">, get the parameter name and check if rest is clear
            MyPos:=PosEx('"',StatusLine);
            if MyPos<=0 then
              raise ERtcInfo.Create('Parameter names have to be in quotes. Missing closing <"> in Multipart text');

            right:=Copy(StatusLine,1,MyPos-1);

            Delete(StatusLine,1,MyPos);
            StatusLine:=Trim(StatusLine);

            if StatusLine<>'' then
              raise ERtcInfo.Create('Error. Data after content disposition param: "'+String(StatusLine)+'".');
            end
          else
            right:='';
          end
        else
          raise ERtcInfo.Create('Missing field name in Multipart text.');

        xxdm:='--'+dm;
        StatusLine:=Copy(FOrigQuery,at,2+length(dm));
        if StatusLine=xxdm then
          Continue;

        // Split Content-disposition line from the rest
        MyPos:=PosEx(CRLF, FOrigQuery, at);
        if MyPos<=0 then
          raise ERtcInfo.Create('Missing <CRLF> after content disposition param.');

        while MyPos>at do
          begin
          at:=MyPos+2;
          StatusLine:=Copy(FOrigQuery,at,2+length(dm));
          if StatusLine=xxdm then
            begin
            MyPos:=-1;
            Break;
            end
          else
            begin
            MyPos:=PosEx(CRLF,FOrigQuery,at);
            if MyPos<=0 then
              raise ERtcInfo.Create('Missing <CRLF> after content header.');
            end;
          end;

        if MyPos<0 then
          Continue;

        Inc(at,2);
        // Split content data part from the rest
        StatusLine:=Copy(FOrigQuery,at,4+length(dm));
        if StatusLine=xxdm then
          Continue;

        MyPos:=PosEx(CRLF+xxdm, FOrigQuery, at);
        if MyPos<=0 then
          raise ERtcInfo.Create('Delimiter missing after Multipart data.');

        if right='' then
          begin
          StatusLine:=Copy(FOrigQuery,at,MyPos-at);
          at:=MyPos+2;

          // Save
          i:=FValues.Find(left);
          if i>=0 then
            begin
            if not assigned(FValues.Objects[i]) then
              begin
              obj3:=TStringArray.Create;
              obj3.AddValue(StatusLine);
              FValues.Objects[i]:=obj3;
              end
            else if FValues.Objects[i] is TStringArray then
              begin
              obj3:=TStringArray(FValues.Objects[i]);
              obj3.AddValue(StatusLine);
              end
            else
              raise ERtcInfo.Create('Duplicate value for "'+String(left)+'" in Multipart text');
            end
          else
            begin
            obj3:=TStringArray.Create;
            obj3.AddValue(StatusLine);
            FValues.Add(left,obj3);
            end;
          end
        else
          begin
          // Save
          i:=FValues.Find(left);
          if i>=0 then
            begin
            if FValues.Objects[i]=nil then
              begin
              obj2:=TUploadFileObject.Create;
              obj2.filename:=right;
              obj2.start:=at;
              obj2.count:=MyPos-at;
              FValues.Objects[i]:=obj2;
              end
            else if FValues.Objects[i] is TUploadFileObject then
              begin
              obj2:=TUploadFileObject(FValues.Objects[i]);
              obj2.filename:=right;
              obj2.start:=at;
              obj2.count:=MyPos-at;
              end
            else
              raise ERtcInfo.Create('Duplicate value for "'+String(left)+'" in Multipart text');
            end
          else
            begin
            obj2:=TUploadFileObject.Create;
            obj2.filename:=right;
            obj2.start:=at;
            obj2.count:=MyPos-at;
            FValues.Add(left,obj2);
            end;
          at:=MyPos+2;
          end;
        end;
      // ignore epilogue
      end
    else // URLEncoded
      begin
      HeadStr:=FOrigQuery+dm;
      // Scan for all header attributes ...
      MyPos:=Pos(dm, HeadStr);
      while (MyPos>=1) do // at least 1 character inside line
        begin
        if MyPos=1 then
          Delete(HeadStr,1,length(dm))
        else
          begin
          StatusLine:=Copy(HeadStr,1,MyPos-1);
          Delete(HeadStr,1,MyPos+Length(dm)-1);

          MyPos:=PosEx('=',StatusLine);
          if MyPos>0 then
            begin
            left:=TrimCopy(StatusLine,1,MyPos-1);

            Delete(StatusLine,1,MyPos);
            // Save
            i:=FValues.Find(left);
            if i>=0 then
              begin
              if not assigned(FValues.Objects[i]) then
                begin
                obj3:=TStringArray.Create;
                FValues.Objects[i]:=obj3;
                obj3.AddValue(StatusLine);
                end
              else
                begin
                obj3:=TStringArray(FValues.Objects[i]);
                obj3.AddValue(StatusLine);
                end;
              end
            else
              begin
              obj3:=TStringArray.Create;
              obj3.AddValue(StatusLine);
              FValues.Add(left, obj3);
              end;
            end;
          end;
        MyPos:=Pos(dm, HeadStr);
        end;
      end;
    end;
  end;

function TRtcHttpValues.GetFile(const index: RtcString; const LocalFileName:RtcWideString): boolean;
  var
    i,start,count:integer;
  begin
  Result:=False;
  if FTxtChange then PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    if not assigned(FValues.Objects[i]) then
      begin
      Delete_File(LocalFileName);
      end
    else if FValues.objects[i] is TStringArray then
      begin
      Delete_File(LocalFileName);
      Write_File(LocalFileName, TStringArray(FValues.Objects[i]).Value );
      end
    else if FValues.Objects[i] is TUploadFileObject then
      begin
      Delete_File(LocalFileName);

      start:=TUploadFileObject(FValues.Objects[i]).start;
      count:=TUploadFileObject(FValues.Objects[i]).count;

      if FTempFileName<>'' then
        Write_FileEx(LocalFileName, Read_FileEx(FTempFileName,start,count) )
      else
        Write_File(LocalFileName, Copy(FOrigQuery,start,count) );

      Result:=True;
      end
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end;
  end;

function TRtcHttpValues.IsFile(const index: RtcString): Boolean;
  var
    i: Integer;
  begin
  Result := False;
  i := FValues.Find(index);
  if i >= 0 then
    Result := FValues.Objects[i] is TUploadFileObject;
  end;

function TRtcHttpValues.GetFile(const index: RtcString; stream:TStream):boolean;
  var
    i, Start, Count: Integer;
  begin
  Result := False;
  if FTxtChange then PrepareValues;

  i := FValues.Find(index);
  if i >= 0 then
    begin
    if not Assigned(FValues.Objects[i]) then
      begin
      // Do nothing
      end
    else if FValues.Objects[i] is TStringArray then
      begin
      Count := Length(TStringArray(FValues.Objects[i]).Value);
    {$IFDEF RTC_BYTESTRING}
      stream.Write( TStringArray(FValues.Objects[i]).Value[1], Count);
    {$ELSE}
      stream.Write(RtcStringToBytes(TStringArray(FValues.Objects[i]).Value,1,Count)[0], Count);
    {$ENDIF}
      end
    else if FValues.Objects[i] is TUploadFileObject then
      begin
      Start := TUploadFileObject(FValues.Objects[i]).Start;
      Count := TUploadFileObject(FValues.Objects[i]).Count;

      if FTempFileName = '' then
      {$IFDEF RTC_BYTESTRING}
        stream.Write(FOrigQuery[Start], Count)
      {$ELSE}
        stream.Write(RtcStringToBytes(FOrigQuery, Start, Count)[0], Count)
      {$ENDIF}
      else
        stream.Write(Read_FileEx(FTempFileName, Start, Count)[0], Count);

      Result := True;
      end
    else
      raise ERtcInfo.Create('Fatal error! Wrong object type in HttpValues!');
    end;
  end;

{ TRtcSession }

constructor TRtcSession.Create;
  begin
  inherited;
  FID:=''; FPeerAddr:='';
  end;

destructor TRtcSession.Destroy;
  begin
  try
    // {$IFDEF RTC_EXTDEBUG}Log('TRtcSession.Destroy "'+FID+'" ('+FPeerAddr+')','DEBUG');{$ENDIF}
    FID:=''; FPeerAddr:='';
    inherited;
    // {$IFDEF RTC_EXTDEBUG}Log('TRtcSession.Destroyed.','DEBUG');{$ENDIF}
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSession.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcRequestFilePath }

constructor TRtcRequestFilePath.Create(Parent: TRtcRequest);
  begin
  inherited Create;
  FRequest:=Parent;
  SetLength(FValues,0);
  FUpdating:=0;
  UpdateFilePath;
  end;

destructor TRtcRequestFilePath.Destroy;
  var
    a:integer;
  begin
  try
    Inc(FUpdating);
    try
      for a:=0 to Length(FValues)-1 do
        FValues[a]:='';
      SetLength(FValues,0);
    finally
      Dec(FUpdating);
      end;
    FRequest:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcRequestFilePath.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcRequestFilePath.Equal(index: integer; const pValue: RtcString): boolean;
  begin
  {$IFDEF UNICODE}
    Result:=Same_Text(GetValue(index),pValue);
  {$ELSE}
    Result:=SameText(GetValue(index),pValue);
  {$ENDIF}
  end;

function TRtcRequestFilePath.GetCount: integer;
  begin
  Result:=Length(FValues);
  end;

function TRtcRequestFilePath.GetValue(index: integer): RtcString;
  begin
  if Length(FValues)>index then
    Result:=FValues[index]
  else
    Result:='';
  end;

procedure TRtcRequestFilePath.SetCount(const pValue: integer);
  var
    idx,len:integer;
  begin
  len:=length(FValues);
  if len<>pValue then
    begin
    if len>pValue then
      for idx:=pValue to len-1 do
        FValues[idx]:='';

    SetLength(FValues,pValue);

    if len<pValue then
      for idx:=len to pValue-1 do
        FValues[idx]:='';
    end;
  if FUpdating=0 then
    FRequest.UpdateFileName;
  end;

procedure TRtcRequestFilePath.SetValue(index: integer; const pValue: RtcString);
  begin
  if PosEx('/',pValue)>0 then
    raise ERtcInfo.Create('Do NOT use "/" inside FilePath elements!');

  Inc(FUpdating);
  try
    if Length(FValues)>index then
      begin
      if pValue='' then // set to '' to remove the last element
        Count:=index
      else
        FValues[index]:=pValue;
      end
    else if pValue<>'' then // always ignore setting a value to '', as it would only add one more trailing "/"
      begin
      Count:=index+1;
      FValues[index]:=pValue;
      end;
  finally
    Dec(FUpdating);
    end;
  if FUpdating=0 then
    FRequest.UpdateFileName;
  end;

procedure TRtcRequestFilePath.UpdateFilePath;
  var
    i,a,len:integer;
    s,x:RtcString;
  begin
  Inc(FUpdating);
  try
    s:=FRequest.FileName;
    x:='';
    i:=-1;
    len:=length(s);
    for a:=1 to len do
      begin
      if s[a]='/' then
        begin
        Inc(i);
        if i>0 then
          SetValue(i-1,x);
        x:='';
        end
      else
        x:=x+s[a];
      end;
    if length(x)>0 then
      begin
      if i>=0 then
        SetValue(i,x);
      x:='';
      end;
    SetCount(i+1);
  finally
    Dec(FUpdating);
    end;
  end;

{ TStringArray }

constructor TStringArray.Create;
  begin
  inherited;
  SetLength(Data,0);
  end;

destructor TStringArray.Destroy;
  begin
  try
    Clear;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TStringArray.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TStringArray.Count: integer;
  begin
  Result:=length(Data);
  end;

function TStringArray.GetValue: RtcString;
  begin
  if length(Data)>0 then
    Result:=Data[length(Data)-1]
  else
    Result:='';
  end;

procedure TStringArray.SetValue(const Value: RtcString);
  begin
  if Value<>'' then
    begin
    if length(Data)>0 then
      Data[length(Data)-1]:=Value
    else
      begin
      SetLength(Data,1);
      Data[0]:=Value;
      end;
    end
  else if length(Data)>0 then
    SetLength(Data,Length(Data)-1)
  end;

procedure TStringArray.Clear;
  var
    a:integer;
  begin
  if length(Data)>0 then
    for a:=0 to length(Data)-1 do
      Data[a]:='';
  SetLength(Data,0);
  end;

function TStringArray.GetElement(i: integer): RtcString;
  begin
  if (i>=0) and (i<length(Data)) then
    Result:=Data[i]
  else
    Result:='';
  end;

procedure TStringArray.SetElement(i: integer; const Value: RtcString);
  begin
  if i>=0 then
    begin
    if Value='' then
      begin
      if i=length(Data)-1 then
        SetLength(Data,Length(Data)-1)
      else if i<length(Data) then
        Data[i]:=Value;
      end
    else
      begin
      if i<length(Data) then
        Data[i]:=Value
      else
        begin
        SetLength(Data,i+1);
        Data[i]:=Value;
        end;
      end;
    end;
  end;

procedure TStringArray.AddValue(const s: RtcString);
  begin
  SetLength(Data,length(Data)+1);
  Data[length(Data)-1]:=s;
  end;

initialization
AppFileName:=ExpandUNCFileName(ParamStr(0));

{$IFDEF RTC_FORMATSET}
  {$IFDEF FPC}
    RtcFormatSettings:=DefaultFormatSettings;
  {$ELSE}
    RtcFormatSettings:=TFormatSettings.Create;
  {$ENDIF}
{$ENDIF}

CRLF_Bytes:=RtcStringToBytes(CRLF);

fObjConstructors:=TStringPtrList.Create(128);
fObjManagers:=TObjList.Create(128);
fObjManCS:=TRtcCritSec.Create;
finalization
RtcFreeAndNil(fObjManCS);
RtcFreeAndNil(fObjManagers);
RtcFreeAndNil(fObjConstructors);

SetLength(CRLF_Bytes,0);

AppFileName:='';
end.

