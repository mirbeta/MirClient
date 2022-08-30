unit cxADOMDInt;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 4/24/2008 5:03:28 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\Common Files\System\ado\msadomd.dll (1)
// LIBID: {22813728-8BD3-11D0-B4EF-00A0C9138CA4}
// LCID: 0
// Helpfile: C:\Program Files\Common Files\System\ado\ado270.chm
// HelpString: Microsoft ActiveX Data Objects (Multi-dimensional) 2.8 Library
// DepndLst:
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
//   (2) v2.8 ADODB, (C:\Program Files\Common Files\System\ado\msado15.dll)
// Errors:
//   Hint: Symbol 'Type' renamed to 'type_'
//   Error creating palette bitmap of (TCatalog) : Server C:\Program Files\Common Files\System\ado\msadomd.dll contains no icons
//   Error creating palette bitmap of (TCellset) : Server C:\Program Files\Common Files\System\ado\msadomd.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties
// which return objects that may need to be explicitly created via a function
// call prior to any access via the property. These items have been disabled
// in order to prevent accidental use from within the object inspector. You
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively
// removing them from the $IFDEF blocks. However, such items must still be
// programmatically created via a method of the appropriate CoClass before
// they can be used.
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, AdoInt, Classes, Graphics, OleServer, StdVCL, Variants;



// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ADOMDMajorVersion = 2;
  ADOMDMinorVersion = 8;

  LIBID_ADOMD: TGUID = '{22813728-8BD3-11D0-B4EF-00A0C9138CA4}';

  IID_ICatalog: TGUID = '{228136B1-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_MD_Collection: TGUID = '{22813751-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_CubeDefs: TGUID = '{2281375D-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_CubeDef25: TGUID = '{2281373E-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_CubeDef: TGUID = '{DA16A34A-7B7A-46FD-AD9D-66DF1E699FA1}';
  IID_Dimensions: TGUID = '{2281375C-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Dimension: TGUID = '{22813742-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Hierarchies: TGUID = '{2281375B-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Hierarchy: TGUID = '{22813746-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Levels: TGUID = '{22813758-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Level: TGUID = '{2281373A-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Members: TGUID = '{22813757-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Member: TGUID = '{22813736-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_ICellset: TGUID = '{2281372A-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Cell: TGUID = '{2281372E-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Positions: TGUID = '{2281375A-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Position: TGUID = '{22813734-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Axes: TGUID = '{22813759-8BD3-11D0-B4EF-00A0C9138CA4}';
  IID_Axis: TGUID = '{22813732-8BD3-11D0-B4EF-00A0C9138CA4}';
  CLASS_Catalog: TGUID = '{228136B0-8BD3-11D0-B4EF-00A0C9138CA4}';
  CLASS_Cellset: TGUID = '{228136B8-8BD3-11D0-B4EF-00A0C9138CA4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library
// *********************************************************************//
// Constants for enum MemberTypeEnum
type
  MemberTypeEnum = TOleEnum;
const
  adMemberUnknown = $00000000;
  adMemberRegular = $00000001;
  adMemberAll = $00000002;
  adMemberMeasure = $00000003;
  adMemberFormula = $00000004;

// Constants for enum SchemaObjectTypeEnum
type
  SchemaObjectTypeEnum = TOleEnum;
const
  adObjectTypeDimension = $00000001;
  adObjectTypeHierarchy = $00000002;
  adObjectTypeLevel = $00000003;
  adObjectTypeMember = $00000004;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  ICatalog = interface;
  MD_Collection = interface;
  CubeDefs = interface;
  CubeDef25 = interface;
  CubeDef = interface;
  Dimensions = interface;
  Dimension = interface;
  Hierarchies = interface;
  Hierarchy = interface;
  Levels = interface;
  Level = interface;
  Members = interface;
  Member = interface;
  ICellset = interface;
  Cell = interface;
  Positions = interface;
  Position = interface;
  Axes = interface;
  Axis = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  Catalog = ICatalog;
  Cellset = ICellset;


// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//
  PPSafeArray1 = ^PSafeArray; {*}


// *********************************************************************//
// Interface: ICatalog
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {228136B1-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  ICatalog = interface(IDispatch)
    ['{228136B1-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Name: WideString; safecall;
    procedure _Set_ActiveConnection(const ppConn: IDispatch); safecall;
    procedure Set_ActiveConnection(const ppConn: WideString); safecall;
    function Get_ActiveConnection: IDispatch; safecall;
    function Get_CubeDefs: CubeDefs; safecall;
    property Name: WideString read Get_Name;
    property CubeDefs: CubeDefs read Get_CubeDefs;
  end;

// *********************************************************************//
// Interface: MD_Collection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813751-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  MD_Collection = interface(IDispatch)
    ['{22813751-8BD3-11D0-B4EF-00A0C9138CA4}']
    procedure Refresh; safecall;
    function _NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// Interface: CubeDefs
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2281375D-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  CubeDefs = interface(MD_Collection)
    ['{2281375D-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Item(Index: OleVariant): CubeDef; safecall;
    property Item[Index: OleVariant]: CubeDef read Get_Item; default;
  end;

// *********************************************************************//
// Interface: CubeDef25
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2281373E-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  CubeDef25 = interface(IDispatch)
    ['{2281373E-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_Properties: Properties; safecall;
    function Get_Dimensions: Dimensions; safecall;
    property Name: WideString read Get_Name;
    property Description: WideString read Get_Description;
    property Properties: Properties read Get_Properties;
    property Dimensions: Dimensions read Get_Dimensions;
  end;

// *********************************************************************//
// Interface: CubeDef
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA16A34A-7B7A-46FD-AD9D-66DF1E699FA1}
// *********************************************************************//
  CubeDef = interface(CubeDef25)
    ['{DA16A34A-7B7A-46FD-AD9D-66DF1E699FA1}']
    function GetSchemaObject(eObjType: SchemaObjectTypeEnum; const bsUniqueName: WideString): IDispatch; safecall;
  end;

// *********************************************************************//
// Interface: Dimensions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2281375C-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Dimensions = interface(MD_Collection)
    ['{2281375C-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Item(Index: OleVariant): Dimension; safecall;
    property Item[Index: OleVariant]: Dimension read Get_Item; default;
  end;

// *********************************************************************//
// Interface: Dimension
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813742-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Dimension = interface(IDispatch)
    ['{22813742-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Name: WideString; safecall;
    function Get_UniqueName: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_Properties: Properties; safecall;
    function Get_Hierarchies: Hierarchies; safecall;
    property Name: WideString read Get_Name;
    property UniqueName: WideString read Get_UniqueName;
    property Description: WideString read Get_Description;
    property Properties: Properties read Get_Properties;
    property Hierarchies: Hierarchies read Get_Hierarchies;
  end;

// *********************************************************************//
// Interface: Hierarchies
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2281375B-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Hierarchies = interface(MD_Collection)
    ['{2281375B-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Item(Index: OleVariant): Hierarchy; safecall;
    property Item[Index: OleVariant]: Hierarchy read Get_Item; default;
  end;

// *********************************************************************//
// Interface: Hierarchy
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813746-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Hierarchy = interface(IDispatch)
    ['{22813746-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Name: WideString; safecall;
    function Get_UniqueName: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_Properties: Properties; safecall;
    function Get_Levels: Levels; safecall;
    property Name: WideString read Get_Name;
    property UniqueName: WideString read Get_UniqueName;
    property Description: WideString read Get_Description;
    property Properties: Properties read Get_Properties;
    property Levels: Levels read Get_Levels;
  end;

// *********************************************************************//
// Interface: Levels
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813758-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Levels = interface(MD_Collection)
    ['{22813758-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Item(Index: OleVariant): Level; safecall;
    property Item[Index: OleVariant]: Level read Get_Item; default;
  end;

// *********************************************************************//
// Interface: Level
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2281373A-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Level = interface(IDispatch)
    ['{2281373A-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Name: WideString; safecall;
    function Get_UniqueName: WideString; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_Depth: Smallint; safecall;
    function Get_Properties: Properties; safecall;
    function Get_Members: Members; safecall;
    property Name: WideString read Get_Name;
    property UniqueName: WideString read Get_UniqueName;
    property Caption: WideString read Get_Caption;
    property Description: WideString read Get_Description;
    property Depth: Smallint read Get_Depth;
    property Properties: Properties read Get_Properties;
    property Members: Members read Get_Members;
  end;

// *********************************************************************//
// Interface: Members
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813757-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Members = interface(MD_Collection)
    ['{22813757-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Item(Index: OleVariant): Member; safecall;
    property Item[Index: OleVariant]: Member read Get_Item; default;
  end;

// *********************************************************************//
// Interface: Member
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813736-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Member = interface(IDispatch)
    ['{22813736-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Name: WideString; safecall;
    function Get_UniqueName: WideString; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_Parent: Member; safecall;
    function Get_LevelDepth: Integer; safecall;
    function Get_LevelName: WideString; safecall;
    function Get_Properties: Properties; safecall;
    function Get_type_: MemberTypeEnum; safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_DrilledDown: WordBool; safecall;
    function Get_ParentSameAsPrev: WordBool; safecall;
    function Get_Children: Members; safecall;
    property Name: WideString read Get_Name;
    property UniqueName: WideString read Get_UniqueName;
    property Caption: WideString read Get_Caption;
    property Description: WideString read Get_Description;
    property Parent: Member read Get_Parent;
    property LevelDepth: Integer read Get_LevelDepth;
    property LevelName: WideString read Get_LevelName;
    property Properties: Properties read Get_Properties;
    property type_: MemberTypeEnum read Get_type_;
    property ChildCount: Integer read Get_ChildCount;
    property DrilledDown: WordBool read Get_DrilledDown;
    property ParentSameAsPrev: WordBool read Get_ParentSameAsPrev;
    property Children: Members read Get_Children;
  end;

// *********************************************************************//
// Interface: ICellset
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2281372A-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  ICellset = interface(IDispatch)
    ['{2281372A-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Item(var idx: PSafeArray): Cell; safecall;
    procedure Open(DataSource: OleVariant; ActiveConnection: OleVariant); safecall;
    procedure Close; safecall;
    procedure _Set_Source(const pvSource: IDispatch); safecall;
    procedure Set_Source(const pvSource: WideString); safecall;
    function Get_Source: OleVariant; safecall;
    procedure _Set_ActiveConnection(const ppConn: IDispatch); safecall;
    procedure Set_ActiveConnection(const ppConn: WideString); safecall;
    function Get_ActiveConnection: IDispatch; safecall;
    function Get_State: Integer; safecall;
    function Get_Axes: Axes; safecall;
    function Get_FilterAxis: Axis; safecall;
    function Get_Properties: Properties; safecall;
    property Item[var idx: PSafeArray]: Cell read Get_Item;
    property State: Integer read Get_State;
    property Axes: Axes read Get_Axes;
    property FilterAxis: Axis read Get_FilterAxis;
    property Properties: Properties read Get_Properties;
  end;

// *********************************************************************//
// Interface: Cell
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2281372E-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Cell = interface(IDispatch)
    ['{2281372E-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_Positions: Positions; safecall;
    function Get_Properties: Properties; safecall;
    function Get_FormattedValue: WideString; safecall;
    procedure Set_FormattedValue(const pbstr: WideString); safecall;
    function Get_Ordinal: Integer; safecall;
    property Value: OleVariant read Get_Value write Set_Value;
    property Positions: Positions read Get_Positions;
    property Properties: Properties read Get_Properties;
    property FormattedValue: WideString read Get_FormattedValue write Set_FormattedValue;
    property Ordinal: Integer read Get_Ordinal;
  end;

// *********************************************************************//
// Interface: Positions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2281375A-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Positions = interface(MD_Collection)
    ['{2281375A-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Item(Index: OleVariant): Position; safecall;
    property Item[Index: OleVariant]: Position read Get_Item; default;
  end;

// *********************************************************************//
// Interface: Position
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813734-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Position = interface(IDispatch)
    ['{22813734-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Ordinal: Integer; safecall;
    function Get_Members: Members; safecall;
    property Ordinal: Integer read Get_Ordinal;
    property Members: Members read Get_Members;
  end;

// *********************************************************************//
// Interface: Axes
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813759-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Axes = interface(MD_Collection)
    ['{22813759-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Item(Index: OleVariant): Axis; safecall;
    property Item[Index: OleVariant]: Axis read Get_Item; default;
  end;

// *********************************************************************//
// Interface: Axis
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22813732-8BD3-11D0-B4EF-00A0C9138CA4}
// *********************************************************************//
  Axis = interface(IDispatch)
    ['{22813732-8BD3-11D0-B4EF-00A0C9138CA4}']
    function Get_Name: WideString; safecall;
    function Get_DimensionCount: Integer; safecall;
    function Get_Positions: Positions; safecall;
    function Get_Properties: Properties; safecall;
    property Name: WideString read Get_Name;
    property DimensionCount: Integer read Get_DimensionCount;
    property Positions: Positions read Get_Positions;
    property Properties: Properties read Get_Properties;
  end;

// *********************************************************************//
// The Class CoCatalog provides a Create and CreateRemote method to
// create instances of the default interface ICatalog exposed by
// the CoClass Catalog. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCatalog = class
    class function Create: ICatalog;
    class function CreateRemote(const MachineName: string): ICatalog;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCatalog
// Help String      : ADOMD Catalog Class
// Default Interface: ICatalog
// Def. Intf. DISP? : No
// Event   Interface:
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCatalogProperties= class;
{$ENDIF}
  TCatalog = class(TOleServer)
  private
    FIntf:        ICatalog;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TCatalogProperties;
    function      GetServerProperties: TCatalogProperties;
{$ENDIF}
    function      GetDefaultInterface: ICatalog;
  protected
    procedure InitServerData; override;
    function Get_Name: WideString;
    procedure _Set_ActiveConnection(const ppConn: IDispatch);
    procedure Set_ActiveConnection(const ppConn: WideString);
    function Get_ActiveConnection: IDispatch;
    function Get_CubeDefs: CubeDefs;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICatalog);
    procedure Disconnect; override;
    property DefaultInterface: ICatalog read GetDefaultInterface;
    property Name: WideString read Get_Name;
    property CubeDefs: CubeDefs read Get_CubeDefs;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCatalogProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCatalog
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCatalogProperties = class(TPersistent)
  private
    FServer:    TCatalog;
    function    GetDefaultInterface: ICatalog;
    constructor Create(AServer: TCatalog);
  protected
    function Get_Name: WideString;
    procedure _Set_ActiveConnection(const ppConn: IDispatch);
    procedure Set_ActiveConnection(const ppConn: WideString);
    function Get_ActiveConnection: IDispatch;
    function Get_CubeDefs: CubeDefs;
  public
    property DefaultInterface: ICatalog read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCellset provides a Create and CreateRemote method to
// create instances of the default interface ICellset exposed by
// the CoClass Cellset. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCellset = class
    class function Create: ICellset;
    class function CreateRemote(const MachineName: string): ICellset;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCellset
// Help String      : ADOMD Cellset Class
// Default Interface: ICellset
// Def. Intf. DISP? : No
// Event   Interface:
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCellsetProperties= class;
{$ENDIF}
  TCellset = class(TOleServer)
  private
    FIntf:        ICellset;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TCellsetProperties;
    function      GetServerProperties: TCellsetProperties;
{$ENDIF}
    function      GetDefaultInterface: ICellset;
  protected
    procedure InitServerData; override;
    function Get_Item(var idx: PSafeArray): Cell;
    procedure _Set_Source(const pvSource: IDispatch);
    procedure Set_Source(const pvSource: WideString);
    function Get_Source: OleVariant;
    procedure _Set_ActiveConnection(const ppConn: IDispatch);
    procedure Set_ActiveConnection(const ppConn: WideString);
    function Get_ActiveConnection: IDispatch;
    function Get_State: Integer;
    function Get_Axes: Axes;
    function Get_FilterAxis: Axis;
    function Get_Properties: Properties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICellset);
    procedure Disconnect; override;
    procedure Open; overload;
    procedure Open(DataSource: OleVariant); overload;
    procedure Open(DataSource: OleVariant; ActiveConnection: OleVariant); overload;
    procedure Close;
    property DefaultInterface: ICellset read GetDefaultInterface;
    property Item[var idx: PSafeArray]: Cell read Get_Item;
    property State: Integer read Get_State;
    property Axes: Axes read Get_Axes;
    property FilterAxis: Axis read Get_FilterAxis;
    property Properties: Properties read Get_Properties;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCellsetProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCellset
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCellsetProperties = class(TPersistent)
  private
    FServer:    TCellset;
    function    GetDefaultInterface: ICellset;
    constructor Create(AServer: TCellset);
  protected
    function Get_Item(var idx: PSafeArray): Cell;
    procedure _Set_Source(const pvSource: IDispatch);
    procedure Set_Source(const pvSource: WideString);
    function Get_Source: OleVariant;
    procedure _Set_ActiveConnection(const ppConn: IDispatch);
    procedure Set_ActiveConnection(const ppConn: WideString);
    function Get_ActiveConnection: IDispatch;
    function Get_State: Integer;
    function Get_Axes: Axes;
    function Get_FilterAxis: Axis;
    function Get_Properties: Properties;
  public
    property DefaultInterface: ICellset read GetDefaultInterface;
  published
  end;
{$ENDIF}


resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoCatalog.Create: ICatalog;
begin
  Result := CreateComObject(CLASS_Catalog) as ICatalog;
end;

class function CoCatalog.CreateRemote(const MachineName: string): ICatalog;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Catalog) as ICatalog;
end;

procedure TCatalog.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{228136B0-8BD3-11D0-B4EF-00A0C9138CA4}';
    IntfIID:   '{228136B1-8BD3-11D0-B4EF-00A0C9138CA4}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCatalog.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ICatalog;
  end;
end;

procedure TCatalog.ConnectTo(svrIntf: ICatalog);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCatalog.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCatalog.GetDefaultInterface: ICatalog;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCatalog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCatalogProperties.Create(Self);
{$ENDIF}
end;

destructor TCatalog.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCatalog.GetServerProperties: TCatalogProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCatalog.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TCatalog._Set_ActiveConnection(const ppConn: IDispatch);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := ppConn;
end;

procedure TCatalog.Set_ActiveConnection(const ppConn: WideString);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := ppConn;
end;

function TCatalog.Get_ActiveConnection: IDispatch;
begin
  Result := DefaultInterface.Get_ActiveConnection;
end;

function TCatalog.Get_CubeDefs: CubeDefs;
begin
    Result := DefaultInterface.CubeDefs;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCatalogProperties.Create(AServer: TCatalog);
begin
  inherited Create;
  FServer := AServer;
end;

function TCatalogProperties.GetDefaultInterface: ICatalog;
begin
  Result := FServer.DefaultInterface;
end;

function TCatalogProperties.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TCatalogProperties._Set_ActiveConnection(const ppConn: IDispatch);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := ppConn;
end;

procedure TCatalogProperties.Set_ActiveConnection(const ppConn: WideString);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := ppConn;
end;

function TCatalogProperties.Get_ActiveConnection: IDispatch;
begin
  Result := DefaultInterface.Get_ActiveConnection;
end;

function TCatalogProperties.Get_CubeDefs: CubeDefs;
begin
    Result := DefaultInterface.CubeDefs;
end;

{$ENDIF}

class function CoCellset.Create: ICellset;
begin
  Result := CreateComObject(CLASS_Cellset) as ICellset;
end;

class function CoCellset.CreateRemote(const MachineName: string): ICellset;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Cellset) as ICellset;
end;

procedure TCellset.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{228136B8-8BD3-11D0-B4EF-00A0C9138CA4}';
    IntfIID:   '{2281372A-8BD3-11D0-B4EF-00A0C9138CA4}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCellset.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ICellset;
  end;
end;

procedure TCellset.ConnectTo(svrIntf: ICellset);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCellset.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCellset.GetDefaultInterface: ICellset;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCellset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCellsetProperties.Create(Self);
{$ENDIF}
end;

destructor TCellset.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCellset.GetServerProperties: TCellsetProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCellset.Get_Item(var idx: PSafeArray): Cell;
begin
    Result := DefaultInterface.Item[idx];
end;

procedure TCellset._Set_Source(const pvSource: IDispatch);
  { Warning: The property Source has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Source := pvSource;
end;

procedure TCellset.Set_Source(const pvSource: WideString);
  { Warning: The property Source has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Source := pvSource;
end;

function TCellset.Get_Source: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Source;
end;

procedure TCellset._Set_ActiveConnection(const ppConn: IDispatch);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := ppConn;
end;

procedure TCellset.Set_ActiveConnection(const ppConn: WideString);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := ppConn;
end;

function TCellset.Get_ActiveConnection: IDispatch;
begin
  Result := DefaultInterface.Get_ActiveConnection;
end;

function TCellset.Get_State: Integer;
begin
    Result := DefaultInterface.State;
end;

function TCellset.Get_Axes: Axes;
begin
    Result := DefaultInterface.Axes;
end;

function TCellset.Get_FilterAxis: Axis;
begin
    Result := DefaultInterface.FilterAxis;
end;

function TCellset.Get_Properties: Properties;
begin
    Result := DefaultInterface.Properties;
end;

procedure TCellset.Open;
begin
  DefaultInterface.Open(EmptyParam, EmptyParam);
end;

procedure TCellset.Open(DataSource: OleVariant);
begin
  DefaultInterface.Open(DataSource, EmptyParam);
end;

procedure TCellset.Open(DataSource: OleVariant; ActiveConnection: OleVariant);
begin
  DefaultInterface.Open(DataSource, ActiveConnection);
end;

procedure TCellset.Close;
begin
  DefaultInterface.Close;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCellsetProperties.Create(AServer: TCellset);
begin
  inherited Create;
  FServer := AServer;
end;

function TCellsetProperties.GetDefaultInterface: ICellset;
begin
  Result := FServer.DefaultInterface;
end;

function TCellsetProperties.Get_Item(var idx: PSafeArray): Cell;
begin
    Result := DefaultInterface.Item[idx];
end;

procedure TCellsetProperties._Set_Source(const pvSource: IDispatch);
  { Warning: The property Source has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Source := pvSource;
end;

procedure TCellsetProperties.Set_Source(const pvSource: WideString);
  { Warning: The property Source has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Source := pvSource;
end;

function TCellsetProperties.Get_Source: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Source;
end;

procedure TCellsetProperties._Set_ActiveConnection(const ppConn: IDispatch);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := ppConn;
end;

procedure TCellsetProperties.Set_ActiveConnection(const ppConn: WideString);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := ppConn;
end;

function TCellsetProperties.Get_ActiveConnection: IDispatch;
begin
  Result := DefaultInterface.Get_ActiveConnection;
end;

function TCellsetProperties.Get_State: Integer;
begin
    Result := DefaultInterface.State;
end;

function TCellsetProperties.Get_Axes: Axes;
begin
    Result := DefaultInterface.Axes;
end;

function TCellsetProperties.Get_FilterAxis: Axis;
begin
    Result := DefaultInterface.FilterAxis;
end;

function TCellsetProperties.Get_Properties: Properties;
begin
    Result := DefaultInterface.Properties;
end;

{$ENDIF}

end.
