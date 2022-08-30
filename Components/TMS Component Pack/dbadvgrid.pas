{***************************************************************************}
{ TDBAdvGrid component                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1996 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit DBAdvGrid;

interface

{$IFNDEF DELPHIXE2_LVL}
  {$DEFINE TMSUSETQUERY}
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
  {$IFDEF WIN32}
//  {$DEFINE TMSUSETQUERY}
  {$ENDIF}
{$ENDIF}


uses
  BaseGrid, Windows, Graphics, SysUtils, Messages, Classes, Controls, DB, DBCtrls,
  Dialogs, AdvGrid, Menus, Grids, AdvUtil, INIFiles, AdvObj, ImgList, Forms, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFNDEF TMSNOADO}
  , ADODB
  {$ENDIF}
  , JPeg
  {$IFDEF DELPHI2006_LVL}
  , WideStrings
  {$ENDIF}
  {$IFDEF DELPHI2010_LVL}
  , GIFImg, PNGImage
  {$ENDIF}
  {$IFDEF TMSUSETQUERY}
  , DBTables
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 8; // Release nr.
  BLD_VER = 5; // Build nr.

  s_QuickConfig = 'Quick config';
  s_AddAllFIelds = 'Add all DB fields';
  s_RemoveAllFields = 'Remove all DB fields';
  s_RemoveAllColumns = 'Remove all columns';

  // version history
  // 2.2.0.0 : New : Support for ImageList per column
  // 2.2.0.1 : Fixed : Issue with checkboxfields
  // 2.3.0.0 : New : OnGetHTMLTemplate event added to dynamically set the template
  //         : Improved : Handling of PageUp/PageDn keys
  //         : Fixed : Issue with export of graphic cells
  //         : Fixed : Issue with mousewheel handling
  // 2.3.0.1 : Fixed : Issue with RowCount when FixedRows > 1
  // 2.3.0.2 : Fixed : Issue with DisableControls/EnableControls
  // 2.3.0.3 : Fixed : Issue with FloatingFooter enabled
  // 2.3.0.4 : Fixed : Issue with EditPostMode = epRow and AdvanceOnEnter = true
  // 2.3.0.5 : Fixed : Handling of click on DB indicator for partially visible row
  // 2.3.0.6 : Fixed : Issue with AdvanceInsert and tab key handling
  //         : Fixed : Issue with OnSelectionChanged
  //         : Fixed : Issue with grouping & refresh from datasource
  //         : Improved : Disabling of mouse wheel handling
  // 2.3.0.7 : Fixed : Issue with AppendOnArrowDown
  //         : Fixed : Issue with multiselection
  // 2.3.0.8 : Fixed : Issue with OnGetRecordCount use when FloatingFooter is enabled
  // 2.3.0.9 : Fixed : Issue with modified indication when cell is cleared
  // 2.3.0.10: Fixed : Issue with defined columns, AutoRemoveColumns & dataset change
  // 2.3.0.11: Fixed : Issue with editing with ClientDataSets
  // 2.3.0.12: Fixed : Issue with cell validation
  // 2.3.0.13: Fixed : Issue with column hiding & moving columns
  //         : Fixed : Issue with CDS active record
  // 2.3.0.14: Fixed : Issue with grid not connected to a dataset
  // 2.3.0.15: Fixed : Issue with override of OnGetCellType
  // 2.3.0.16: Fixed : Issue with GetSelectedField & hidden columns
  // 2.3.0.17: Fixed : Issue with incorrect scroll with PageMode = true
  // 2.3.1.0 : New : Added 64bit support for TDBAdvGrid
  // 2.3.1.1 : Fixed : Issue with mouse clicks & FloatingFooter fsColumnPreview style
  // 2.3.1.2 : Fixed : Issue with floating footer & checkbox field
  //         : Fixed : Issue with alignment & grouping
  // 2.3.1.3 : Fixed : Issue with OnSelectionChange with vert. scrollbar
  // 2.3.2.0 : Improved : Handling color value editing bound to DB numeric fields
  // 2.3.2.1 : Fixed : Issue with FloatingFooter fsColumnPreview / fsCustomPaint types
  // 2.3.2.2 : Fixed : Issue with CheckAllCheck = true and CheckBoxfields
  // 2.3.3.0 : Improved : Automatic update of combobox index value when combobox inplace editor is used with ftInteger, ftSmallInt, ftWord field types
  // 2.3.3.1 : Fixed : Issue with fsColumnPreview FloatingFooter and checkboxes
  // 2.3.3.2 : Fixed : Issue with updating fkLookup fields
  // 2.3.3.3 : Fixed : Issue with cell access per column in grouped grids
  // 2.3.3.4 : Fixed : Issue with AutoRemoveColumns = false and swapping datasets
  // 2.3.3.5 : Fixed : Issue with EditPostMode = epCell and PageMode = false
  // 2.3.3.6 : Fixed : Issue with Columns FloatFormat setting and grouping
  // 2.3.3.7 : Fixed : Issue with editing combobox values and using tab/arrow keys
  // 2.3.3.8 : Fixed : Issue with OnGetRecordCount & floating footers
  // 2.3.3.9 : Fixed : Issue with scrolling & floating footer
  // 2.3.4.0 : New : Support for displaying PNG blob fields added
  // 2.3.4.1 : Fixed : Issue with DataSource.AutoEdit = false and checkboxes
  // 2.3.4.2 : Fixed : Issue with display of GIF blob fields
  // 2.3.5.0 : New : grid.Columns[].UseComboObjectValue added to force use of combobox object values to persist value in ftSmallInt,ftInteger,ftWord fields
  // 2.3.5.1 : Fixed : edDataCheckBox editor type applicable for both PageMode = true or false
  // 2.3.5.2 : Fixed : Issue with data image fields of type ftByte
  // 2.3.5.3 : Fixed : Issue with cell validation and AlwaysValidate=false
  // 2.3.6.0 : New : Automatic display of memo fields in HTML template columns
  // 2.3.6.1 : Fixed : Issue with lookup editing
  // 2.3.6.2 : Fixed:  Issue with setting DropDownAlwaysVisible = true
  // 2.3.6.3 : Fixed : Issue with ReadOnly columns and hiding columns
  //         : Improved : Handling of filtering with checkbox columns
  // 2.3.6.4 : Fixed : Issue with DataImageField and empty datasets
  // 2.3.6.5 : Fixed : Issue with runtime creation of component
  // 2.3.6.6 : Fixed : Issue with old BLOB field graphic handling
  // 2.3.6.7 : Fixed : Issue with DataImageField and last rows in small datasets
  // 2.3.6.8 : Fixed : Issue with MinSize/MaxSize control when hidden columns are used
  // 2.3.6.9 : Fixed : Issue with OnGetHTMLTemplate and PageMode = false
  //         : Fixed : Issue with data image displayed for empty DB values
  // 2.3.6.10 : Fixed : Issue with forcing columns to not display DB fields on an active dataset
  // 2.3.6.11 : Improved : Behavior with AppendOnArrowDown & FloatingFooter.Visible
  // 2.3.6.12 : Fixed : Issue with updating regular text fields from a combobox inplace editor
  // 2.3.6.13 : Fixed : Issue with rendering when DataSetType = dtSequenced
  // 2.3.6.14 : Fixed : Issue with OLE drag & drop
  // 2.3.6.15 : Fixed : Issue with unbound checkbox column
  // 2.3.6.16 : Fixed : Issue with mouse wheel scrolling for dtNonSequenced dataset
  // 2.3.6.17 : Fixed : Issue with design-time removing of dataset fields
  // 2.3.6.18 : Fixed : Issue with Zoom for grid with PageMode = true
  // 2.3.6.19 : Fixed : Issue with Find when no more records are found
  // 2.3.6.20 : Fixed : Issue with search footer and visualizing records
  //          : Fixed : Issue with rowselect
  // 2.3.6.21 : Fixed : Workaround for regression in XE7 C++Builder
  // 2.3.6.22 : Fixed : Issue with tab key handling on last row
  // 2.3.6.23 : Fixed : Issue with mousewheel scroll when PageMode = false
  // 2.3.6.24 : Fixed : Issue with AutoRemoveColumns
  // 2.3.6.25 : Fixed : Issue with goTabs = true and tab at last column
  // 2.3.6.26 : Fixed : Issue with hidden columns and AutoCreateColumns/AutoRemoveColumns = true
  // 2.3.6.27 : Fixed : Issue with column hiding & editing
  // 2.3.7.0  : Improved : Checkbox based record selection
  // 2.3.7.1  : Fixed : Issue with persisting columns when AutoCreateColumns / AutoRemoveColumns = true
  // 2.3.7.2  : Fixed : Issue with MouseAction.DirectEdit = true and mouse wheel handling
  //          : Fixed : Issue with Cell validation
  // 2.3.7.3  : Fixed : Issue with column width handling when using grid.Columns.Insert()
  // 2.3.8.0  : New : Use of FindField in HTML template handling instead of FieldByName
  //          : Fixed : Issue with inplace editing when AdvanceOnEnter = true
  //          : Fixed : Issue with hide/unhide columns
  // 2.3.8.1  : Fixed : Issue with RefreshOnDelete = true on empty datasets
  // 2.3.8.2  : Fixed : Issue with key handling for TDBAdvGrid with no datasource or active dataset assigned
  // 2.3.8.3  : Fixed : Issue with lookup fields & hidden columns
  // 2.3.8.4  : Fixed : Issue with combining filter edit and displaying DB field names in column header
  // 2.3.8.5  : Fixed : Issue with OnCellClick in specific circumstances

type
  TDBAdvGrid = class;

  TAdvGridDataLink = class(TDataLink)
  private
    FGrid: TDBAdvGrid;
    FFieldCount: Integer;
    FFieldMap: array of Integer;
    FModified: Boolean;
    FInUpdateData: Boolean;
    FSparseMap: Boolean;
    FLockEffects: Boolean;
    function GetDefaultFields: Boolean;
    function GetFields(I: Integer): TField;
  protected
    procedure ActiveChanged; override;
    procedure BuildAggMap;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure EditingChanged; override;
    function IsAggRow(Value: Integer): Boolean; virtual;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    function GetMappedIndex(ColIndex: Integer): Integer;
  public
    constructor Create(AGrid: TDBAdvGrid);
    destructor Destroy; override;
    function AddMapping(const FieldName: string): Boolean;
    procedure ClearMapping;
    procedure Modified;
    procedure Reset;
    property DefaultFields: Boolean read GetDefaultFields;
    property FieldCount: Integer read FFieldCount;
    property Fields[I: Integer]: TField read GetFields;
    property SparseMap: Boolean read FSparseMap write FSparseMap;
    property Grid: TDBAdvGrid read FGrid;
  end;

  TColumnPopupType = (cpFixedCellsRClick, cpFixedCellsLClick,
    cpNormalCellsRClick, cpNormalCellsLClick, cpAllCellsRClick, cpAllCellsLClick);

  TDBGridColumnItem = class(TCollectionItem)
  private
    FField: TField;
    FFieldName: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    FColumnHeader: string;
    FSortStyle: TSortStyle;
    FSortPrefix: string;
    FSortSuffix: string;
    FEditMask: string;
    FEditLength: Integer;
    FEditLink: TEditLink;
    FFont: TFont;
    FColor: TColor;
    FEditorType: TEditorType;
    FFixed: Boolean;
    FReadOnly: Boolean;
    FComboItems: TStringList;
    FSpinMin: Integer;
    FSpinMax: Integer;
    FSpinStep: Integer;
    FPassword: Boolean;
    FPrintFont: TFont;
    FPrintColor: TColor;
    FBorders: TCellBorders;
    FBorderPen: TPen;
    FPrintBorders: TCellBorders;
    FPrintBorderPen: TPen;
    FTag: Integer;
    FDefIdx: Integer;
    FName: string;
    FCheckFalse: string;
    FCheckTrue: string;
    FShowBands: Boolean;
    FFilter: TStringList;
    FMaxSize: Integer;
    FMinSize: Integer;
    FAutoMinSize: Integer;
    FAutoMaxSize: Integer;
    FColumnPopupType: TColumnPopupType;
    FColumnPopup: TPopupMenu;
    FFilterCaseSensitive: Boolean;
    FFloatFormat: string;
    FPictureField: Boolean;
    FCheckBoxField: Boolean;
    FProgressField: Boolean;
    FDataImageField: boolean;
    FProgressColor: TColor;
    FProgressBKColor: TColor;
    FProgressTextColor: TColor;
    FProgressTextBKColor: TColor;
    FHTMLTemplate: string;
    FHeaderAlignment: TAlignment;
    FHeaderFont: TFont;
    FShowUnicode: boolean;
    FPictureStretch: TStretchMode;
    FUseLookupEditor: boolean;
    FImages: TCustomImageList;
    FUseComboObjectValue: boolean;
    FAutoCreated: boolean;
    procedure SetWidth(const Value: Integer);
    function GetWidth: Integer;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetHeaderAlignment(const Value: TAlignment);
    procedure SetColumnHeader(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetFixed(const Value: Boolean);
    procedure SetPassword(const Value: Boolean);
    procedure SetComboItems(const Value: TStringList);
    procedure FontChanged(Sender: TObject);
    procedure PenChanged(Sender: TObject);
    procedure SetBorders(const Value: TCellBorders);
    procedure SetBorderPen(const Value: TPen);
    function GetRows(idx: Integer): string;
    procedure SetRows(idx: Integer; const Value: string);
    function GetDates(idx: Integer): TDateTime;
    function GetFloats(idx: Integer): Double;
    function GetInts(idx: Integer): Integer;
    procedure SetDates(idx: Integer; const Value: TDateTime);
    procedure SetFloats(idx: Integer; const Value: Double);
    procedure SetInts(idx: Integer; const Value: Integer);
    function GetTimes(idx: Integer): TDateTime;
    procedure SetTimes(idx: Integer; const Value: TDateTime);
    procedure SetEditorType(const Value: TEditorType);
    procedure SetShowBands(const Value: Boolean);
    procedure SetFilter(const Value: TStringList);
    procedure FilterChanged(Sender: TObject);
    procedure SetFloatFormat(const Value: string);
    function GetField: TField;
    procedure SetField(Value: TField); virtual;
    procedure SetFieldName(const Value: string);
    procedure SetPictureField(const Value: Boolean);
    procedure SetCheckBoxField(const Value: Boolean);
    procedure SetProgressField(const Value: Boolean);
    procedure SetProgressColor(const Value: TColor);
    procedure SetProgressBKColor(const Value: TColor);
    procedure SetProgressTextColor(const Value: TColor);
    procedure SetProgressTextBKColor(const Value: TColor);
    procedure SetDataImageField(const Value: boolean);
    procedure SetHTMLTemplate(const Value: string);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetPrintFont(const Value: TFont);
    function GetReadOnly: boolean;
  protected
    function GetDisplayName: string; override;
    function GetGrid: TDBAdvGrid;
    procedure SetIndex(Value: Integer); override;
    function GridColIndex: integer;
    property AutoCreated: boolean read FAutoCreated write FAutoCreated;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignVisuals(Source: TPersistent);
    property DefIdx: Integer read FDefIdx write FDefIdx;
    property Rows[idx: Integer]: string read GetRows write SetRows;
    property Ints[idx: Integer]: Integer read GetInts write SetInts;
    property Floats[idx: Integer]: Double read GetFloats write SetFloats;
    property Dates[idx: Integer]: TDateTime read GetDates write SetDates;
    property Times[idx: Integer]: TDateTime read GetTimes write SetTimes;
    property Field: TField read GetField write SetField;
  published
    property AutoMinSize: Integer read FAutoMinSize write FAutoMinSize default 0;
    property AutoMaxSize: Integer read FAutoMaxSize write FAutoMaxSize default 0;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Borders: TCellBorders read FBorders write SetBorders;
    property BorderPen: TPen read FBorderPen write SetBorderPen;
    property CheckBoxField: Boolean read FCheckBoxField write SetCheckBoxField default false;
    property CheckFalse: string read FCheckFalse write FCheckFalse;
    property CheckTrue: string read FCheckTrue write FCheckTrue;
    property Color: TColor read FColor write SetColor;
    property ColumnPopup: TPopupMenu read FColumnPopup write FColumnPopup;
    property ColumnPopupType: TColumnPopupType read FColumnPopupType write FColumnPopupType default cpFixedCellsRClick;
    property ComboItems: TStringList read FComboItems write SetComboItems;
    property UseComboObjectValue: boolean read FUseComboObjectValue write FUseComboObjectValue default false;
    property DataImageField: boolean read FDataImageField write SetDataImageField default false;
    property EditLength: Integer read FEditLength write FEditLength default 0;
    property EditLink: TEditLink read FEditLink write FEditLink;
    property EditMask: string read FEditMask write FEditMask;
    property Editor: TEditorType read FEditorType write SetEditorType default edNormal;
    property FieldName: string read FFieldName write SetFieldName;
    property Filter: TStringList read FFilter write SetFilter;
    property FilterCaseSensitive: Boolean read FFilterCaseSensitive write FFilterCaseSensitive default false;
    property Fixed: Boolean read FFixed write SetFixed default false;
    property FloatFormat: string read FFloatFormat write SetFloatFormat;
    property Font : TFont read FFont write SetFont;
    property Header: string read FColumnHeader write SetColumnHeader;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property HeaderAlignment: TAlignment read FHeaderAlignment write SetHeaderAlignment default taLeftJustify;
    property HTMLTemplate: string read FHTMLTemplate write SetHTMLTemplate;
    property Images: TCustomImageList read FImages write FImages;
    property MinSize: Integer read FMinSize write FMinSize default 0;
    property MaxSize: Integer read FMaxSize write FMaxSize default 0;
    property Name: string read FName write FName;
    property Password: Boolean read FPassword write SetPassword default false;
    property PictureField: Boolean read FPictureField write SetPictureField default false;
    property PictureStretch: TStretchMode read FPictureStretch write FPictureStretch default StretchWithAspectRatio; 
    property PrintBorders: TCellBorders read FPrintBorders write FPrintBorders;
    property PrintBorderPen: TPen read fPrintBorderPen write FPrintBorderPen;
    property PrintColor: TColor read FPrintColor write FPrintColor default clWhite;
    property PrintFont: TFont read FPrintFont write SetPrintFont;
    property ProgressColor: TColor read FProgressColor write SetProgressColor default clRed;
    property ProgressBKColor: TColor read FProgressBKColor write SetProgressBKColor default clWhite;
    property ProgressTextColor: TColor read FProgressTextColor write SetProgressTextColor default clWhite;
    property ProgressTextBKColor: TColor read FProgressTextBKColor write SetProgressTextBKColor default clRed;
    property ProgressField: Boolean read FProgressField write SetProgressField default false;
    property ReadOnly: Boolean read GetReadOnly write FReadOnly default false;
    property ShowBands: Boolean read FShowBands write SetShowBands default false;
    property ShowUnicode: Boolean read FShowUnicode write FShowUnicode default false;
    property SortStyle: TSortStyle read FSortStyle write FSortStyle default ssAutomatic;
    property SortPrefix: string read FSortPrefix write FSortPrefix;
    property SortSuffix: string read FSortSuffix write FSortSuffix;
    property SpinMax: Integer read FSpinMax write FSpinMax default 0;
    property SpinMin: Integer read FSpinMin write FSpinMin default 0;
    property SpinStep: Integer read FSpinStep write FSpinStep default 1;
    property Tag: Integer read FTag write FTag default 0;
    property UseLookupEditor: boolean read FUseLookupEditor write FUseLookupEditor default True;
    property Width: Integer read GetWidth write SetWidth default 50;
  end;

  TDBGridColumnCollection = class(TCollection)
  private
    FOwner: TDBAdvGrid;
    FNoRecursiveUpdate: Boolean;
    function GetItem(Index: Integer): TDBGridColumnItem;
    procedure SetItem(Index: Integer; const Value: TDBGridColumnItem);
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
    property Grid: TDBAdvGrid read FOwner;
  public
    function HasFieldsDefined: boolean;
    function GetItemClass: TCollectionItemClass; virtual;
    function Add: TDBGridColumnItem;
    function Insert(index: Integer): TDBGridColumnItem;
    property Items[Index: Integer]: TDBGridColumnItem read GetItem write SetItem; default;
    constructor Create(AOwner: TDBAdvGrid);
    procedure Delete(Index: integer);
    procedure SetOrganization;
    procedure ResetOrganization;
  end;

  TSetQueryOrderEvent = procedure(Sender: TObject; var SQL: string) of object;
  TGetRecordCountEvent = procedure(Sender: TObject; var Count: integer) of object;
  TGetHTMLTemplateDataEvent = procedure(Sender: TObject; ACol, ARow: integer; Fieldname: string; var Data: string) of object;
  TGetHTMLTemplateEvent = procedure(Sender: TObject; ACol, ARow: integer; var HTMLTemplate: string; Fields: TFields) of object;
  TDBColumnPopupEvent = procedure(Sender: TObject; ACol, ARow: Integer; PopupMenu: TPopupMenu) of object;
  TInvalidPictureEvent = procedure(Sender: TObject; ACol, ARow: integer) of object;
  TFieldToStreamEvent = procedure(Sender: TObject; ACol,ARow: integer; DBField: TField; MS: TMemoryStream) of object;
  TDataSetType = (dtSequenced, dtNonSequenced);
  TEditPostMode = (epCell, epRow);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvGrid = class(TAdvStringGrid)
  private
    FColumnCollection: TDBGridColumnCollection;
    FDataLink: TAdvGridDataLink;
    FInternalCall: boolean;
    FDoNotUpdateMe: Boolean;
    FCellGraphic: TCellGraphic;
    FOnRowChanged: TRowChangedEvent;
    FShowMemoFields: Boolean;
    FOnCellValidate: TCellValidateEvent;
    FOnCellValidateWide: TCellValidateWideEvent;
    FOnCanSort: TCanSortEvent;
    FOnSetQueryOrder: TSetQueryOrderEvent;
    FOldState: TDataSetState;
    FRecordChanged: Boolean;
    FShowDefinedFields: Boolean;
    FInternalInsert: boolean;
    FShowDBIndicator: Boolean;
    FOldPosition: TBookMark;
    FEmptyDataSet: Boolean;
    FOldIsBOF: Boolean;
    FOldIsEOF: Boolean;
    FCancelEditReturn: Boolean;
    FDoNotCountRow: Boolean;
    FOnGetRecordCount: TGetRecordCountEvent;
    FShowBooleanFields: Boolean;
    FShowPictureFields: Boolean;
    FNotDeletionUpdate: Boolean;
    FPicture: TPicture;
    FPageMode: Boolean;
    FPictureList: TList;
    FRefreshOnDelete: Boolean;
    FUpdateCancel: boolean;
    FNewRecord: Boolean;
    FRefreshOnInsert: Boolean;
    FLookupKeys: TStringList;
    FVisibleFieldCount: integer;
    FDataSetType: TDataSetType;
    FExportRow: integer;
    FSelExport: Boolean;
    FSelImport: Boolean;
    FSelRow: integer;
    FImportPos: TBookMark;
    FNewAppendRecord: boolean;
    FOnGetHTMLTemplateData: TGetHTMLTemplateDataEvent;
    FOnGetHTMLTemplate: TGetHTMLTemplateEvent;
    FKeyDownAppend: boolean;
    FMouseWheelScrolling: boolean;
    FMouseWheelScrolled: Boolean;
    FLastDesignChoice: Integer;
    FOnRowChanging: TRowChangingEvent;
    FFilteredDataSet: Boolean;
    FOldDataSetType: TDataSetType;
    FOnGetEditText: TGetEditEvent;
    FShowUnicode: Boolean;
    FInternalSelection: Boolean;
    FDatasetTypeAuto: Boolean;
    FEditPostMode: TEditPostMode;
    FEditRec: integer;
    FEditRecData: TStringList;
    FOldEditingState: TDataSetState;
    //FEditRecBm: TBookMark;
    FEditUpdating: Boolean;
    FEditText: string;
    FEditWideText: widestring;
    FDoNotCallSelect: Boolean;
    FDoNotBounsBack: Boolean;
    FOldAR: Integer;   // OldActiveRecord for Export Notification
    FOldTopRow: Integer;
    FAppending: Boolean;
    FAppendOperation: Boolean;
    FExporting: Boolean;
    FEditEnding: Boolean;
    FOldFoaterEnableCalc: Boolean;
    FAllowRowChange: Boolean;
    FMustEnableControls: integer;
    FAutoCreateColumns: Boolean;
    FAutoRemoveColumns: Boolean;
    FOnColumnPopup: TDBColumnPopupEvent;
    FInternalMove: Boolean;
    FBlockCallBack: Boolean;
    FShouldNotPostChanges: Boolean;
    FInvalidPicture: TPicture;
    FOnInvalidPicture: TInvalidPictureEvent;
    FOnFieldToStream: TFieldToStreamEvent;
    FShowBlankRow: Boolean;
    FExportStartRow: Integer;
    FUseDBFieldWidths: boolean;
    FTabInsertion: Boolean;
    FColWith0Width: Boolean;
    FWasEditing: Boolean;
    procedure DataUpdate;
    procedure DataChange;
    procedure ActiveChange(Value: Boolean);
    procedure RecordChanged(Field: TField);
    procedure EditingChanged;
    procedure UpdateRowCount;
    function CheckDataSet: Boolean;
    //procedure UpdateActive;
    procedure UpdateScrollBar(distance: integer);
    procedure UpdateVisibleFields;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
    //function RowsInDataSet: integer;
    procedure SyncActiveRec;
    procedure WMVScroll(var WMScroll: TWMScroll); message WM_VSCROLL;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Msg:TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure GetEditTextEvent(Sender: TObject; ACol, ARow: Longint; var Value: string);
    procedure RowChangingEvent(Sender: TObject; OldRow, NewRow: Integer; var Allow: boolean);
    procedure RowUpdateEvent(Sender: TObject; OldRow, NewRow: Integer);
    function GetFieldCount: Integer;
    function GetFields(FieldIndex: Integer): TField;
    procedure SynchColumns;
    procedure SetColumnCollection(const Value: TDBGridColumnCollection);
    function GetColumnCollection: TDBGridColumnCollection;
    function GetColCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetShowMemoFields(const Value: Boolean);
    procedure CellValidateEvent(Sender: TObject; ACol, ARow: Integer; var Value: string; var Valid: Boolean);
    procedure CellValidateWideEvent(Sender: TObject; ACol, ARow: Integer; var Value: widestring; var Valid: Boolean);
    procedure CanSortEvent(Sender: TObject; ACol: Integer; var DoSort: Boolean);
    {$IFDEF TMSUSETQUERY}
    procedure DoTQuerySort(ACol: integer);
    {$ENDIF}
    {$IFNDEF TMSNOADO}
    procedure DoADOSort(ACol: integer);
    {$ENDIF}
    procedure SetShowDBIndicator(const Value: Boolean);
    procedure SetShowBooleanFields(const Value: Boolean);
    procedure SetShowPictureFields(const Value: Boolean);
    function GetDBFieldAtColumn(ACol: integer): TField;
    function GetSelectedField: TField;
    function GetDBFieldIndexAtColumn(ACol: integer): integer;
    function GetFixedRowsEx: Integer;
    procedure SetFixedRowsEx(const Value: Integer);
    function HTMLDBReplace(s: string; dataset: Tdataset; ACol, ARow: integer): string;
    procedure LoadFromDataSet;
    procedure RemoveAllPictures;
    procedure RemoveAllBooleanCheckBoxes;
    procedure RemoveAllStringCheckBoxes;
    procedure RemoveAllGraphics;
    procedure RemoveAllCellValues;
    procedure SetPageMode(const Value: Boolean);
    procedure SetDataSetType(const Value: TDataSetType);
    function GetDBRow: Integer;
    procedure SetDBRow(const Value: Integer);
    function MouseOverDesignChoice(X, Y: Integer): integer;
    procedure DesignerUpdate;
    function GetColumnByName(AValue: string): TDBGridColumnItem;
    function GetColumnByFieldName(AValue: string): TDBGridColumnItem;
    procedure SetDatasetTypeAuto(const Value: Boolean);
    procedure SetEditPostMode(const Value: TEditPostMode);
    procedure SetAutoCreateColumns(const Value: Boolean);
    procedure SetAutoRemoveColumns(const Value: Boolean);
    procedure SetInvalidPicture(const Value: TPicture);
    procedure SetShowBlankRow(const Value: Boolean);
    function CanShowBlankRow: Boolean;
    function GetBuffercount: integer;
    //procedure PostEditRecData;
  protected
    procedure InitValidate(ACol,ARow: Integer); override;
    function CanEditShow: Boolean; override;
    function CanEditModify: Boolean; override;
    procedure WndProc(var Message: TMessage); override;
    procedure DoKeyDown(Key: word; Shift: TShiftState);
    procedure DoF2Edit; override;
    function IsDSEdit: boolean; override;
    function HasColumnsProp: boolean; override;
    function DBWheelUp: boolean;
    function DBWheelDown: boolean;
    function GetColumnCheckTrue(ACol: Integer): string; override;
    function GetColumnCheckFalse(ACol: Integer): string; override;
    procedure DirectWheelChange(delta: integer; var SuppressMsg: Boolean); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoCanEditCell(ACol,ARow: Integer; var CanEdit: boolean); override;
    procedure DoCheckBoxClick(ACol,ARow: integer; AState: boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function SelectCell(ACol, ARow: longint): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanModify: Boolean; virtual;
    function CanTabToNextRow: Boolean; override;
    function GetRecordCount: Integer; virtual;
    function GetCurrentCell: string; override;
    procedure GetDisplText(c, r: Integer; var Value: string); override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;
    procedure Scroll(Distance: Integer); virtual;
    procedure TopLeftChanged; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure Loaded; override;
    procedure DoAutoRemoveColumns(removedefined: boolean); virtual;
    function CreateColumns: TDBGridColumnCollection; virtual;
    procedure ColWidthsChanged; override;
    function GetEditCell(i,j: integer): string; override;
    procedure GetCellAlign(ACol, ARow: Integer; var HAlign: TAlignment; var VAlign: TVAlignment); override;
    procedure GetCellBorder(ACol, ARow: Integer; APen: TPen; var borders: TCellBorders); override;
    procedure GetCellPrintBorder(ACol, ARow: Integer; APen: TPen; var borders: TCellBorders); override;
    procedure GetCellPrintColor(ACol, ARow: Integer; AState: TGridDrawState; ABrush: TBrush; AFont: TFont); override;
    procedure GetCellColor(ACol, ARow: Integer; AState: TGridDrawState; ABrush: TBrush; AFont: TFont); override;
    function HasCombo(ACol, ARow: Integer; AEditor: TEditorType = edNone): Boolean; override;
    procedure GetStaticCellEditor(ACol, ARow: Integer; var AEditor: TEditorType); override;
    procedure GetCellEditor(ACol, ARow: Integer; var AEditor: TEditorType); override;
    function GetEditorNone(ACol,ARow: Integer): boolean; override;
    procedure GetCellFixed(ACol, ARow: Integer; var IsFixed: Boolean); override;
    procedure GetCellPassword(ACol, ARow: Integer; var IsPassword: Boolean); override;
   // procedure GetCellReadOnly(ACol,ARow: Integer;var IsReadOnly: Boolean); override;
    procedure GetColFormat(ACol: Integer; var ASortStyle: TSortStyle; var aPrefix, aSuffix: string); override;
    function GetEditLimit: Integer; override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetCellGraphic(ACol, ARow: Integer): TCellGraphic; override;
    function GetCellType(ACol, ARow: Integer): TCellType; override;
    function GetCheckFalse(ACol, ARow: Integer): string; override;
    function GetCheckTrue(ACol, ARow: Integer): string; override;
    function GetFilter(ACol: Integer; Disp: boolean = false): Boolean; override;
    procedure UpdateColSize(ACol: Integer; var NewWidth: Integer); override;
    procedure UpdateAutoColSize(ACol: Integer; var NewWidth: Integer); override;
    procedure UpdateColHeaders; override;
    function GetFormattedCell(ACol, ARow: Integer): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure GetCellReadOnly(ACol, ARow: Integer; var IsReadOnly: Boolean); override;
    function ToggleCheck(ACol, ARow: Integer; FromEdit: Boolean): Boolean; override;
    procedure PasteInCell(ACol, ARow: Integer; Value: string); override;
    procedure DoAppendRow; override;
    //procedure UpdateForPageModeForCheckBoxColumn;
    procedure RemoveStringCheckBox(ACol: integer);
    procedure AddStringCheckBox(ACol: integer);
    procedure QueryAddRow(var AllowAdd: Boolean); override;
    procedure LoadLookupList(fld: TField; list: TStrings);
    {$IFDEF DELPHI2006_LVL}
    procedure LoadWideLookupList(fld: TField; list: TWideStrings);
    {$ENDIF}
    function GetLookupKey(i: Integer): string;
    property DataLink: TAdvGridDataLink read FDataLink;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    procedure UpdateOnSelection(var GR: TGridRect); override;
    procedure UpdateSelectionRect(var GR: TGridRect); override;
    procedure BlobFieldToStream(DBField: TBlobField; var size: tpoint; Col, Row: integer);
    procedure TabToNextRowAtEnd; override;
    procedure OnMouseActionsChanged(Sender: TObject); override;
    procedure EditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Edit_WMKeyDown(var Msg: TWMKeydown); override;
    procedure OnNavigationChanged(Sender: TObject); override;
    function DoAllowFmtPaste: boolean; override;
    procedure DoInvalidPicture(Col,Row: integer); virtual;
    procedure PaintDesigner; override;
    procedure HandleDesignChoice(X,Y: Integer); override;
    function GetCellImageList(ACol,ARow: integer): TCustomImageList; override;
    procedure DoGetEditorProp(ACol,ARow: integer; EditLink: TEditLink); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignCells(Source: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetVersionNr: Integer; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property ColumnByName[AValue:string]: TDBGridColumnItem read GetColumnByName;
    property ColumnByFieldName[AValue:string]: TDBGridColumnItem read GetColumnByFieldName;
    function ValidateCell(const NewValue: string): Boolean; override;
    procedure ExportNotification(state: TGridExportState; ARow: Integer); override;
    procedure ImportNotification(AState: TGridImportState; ARow: Integer); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property FieldCount: Integer read GetFieldCount;
    property Fields[FieldIndex: Integer]: TField read GetFields;
    procedure AddAllFields;
    procedure RemoveAllFields;
    procedure RemoveAllColumns;
    procedure SaveColumnPositions(Key,Section: string);
    procedure LoadColumnPositions(Key,Section: string);
    procedure LoadColPositions; override;
    procedure SaveColPositions; override;
    procedure Zoom(x: Integer); override;
    procedure UpdateActive;
    procedure Reload;
    procedure UpdateDisplay;
    property Row: Integer read GetDBRow write SetDBRow;
    property SelectedField: TField read GetSelectedField;
    property FieldAtColumn[ACol: Integer]: TField read GetDBFieldAtColumn;
    property FieldIndexAtColumn[Acol: Integer]: Integer read GetDBFieldIndexAtColumn;
    property ShowBlankRow: Boolean read FShowBlankRow write SetShowBlankRow default True;
  published
    property AutoCreateColumns: Boolean read FAutoCreateColumns write SetAutoCreateColumns;
    property AutoRemoveColumns: Boolean read FAutoRemoveColumns write SetAutoRemoveColumns;
    property Columns: TDBGridColumnCollection read GetColumnCollection write SetColumnCollection;
    property ColCount: Integer read GetColCount write SetColCount;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DatasetTypeAuto: Boolean read FDatasetTypeAuto write SetDatasetTypeAuto default true;
    property DataSetType: TDataSetType read FDataSetType write SetDataSetType default dtSequenced;
    property EditPostMode: TEditPostMode read FEditPostMode write SetEditPostMode default epCell;
    property FixedRows: Integer read GetFixedRowsEx write SetFixedRowsEx;
    property PageMode: Boolean read FPageMode write SetPageMode default true;
    property InvalidPicture: TPicture read FInvalidPicture write SetInvalidPicture;
    property RefreshOnDelete: Boolean read FRefreshOnDelete write FRefreshOnDelete default false;
    property RefreshOnInsert: Boolean read FRefreshOnInsert write FRefreshOnInsert default false;
    property ShowDBIndicator: Boolean read FShowDBIndicator write SetShowDBIndicator default true;
    property ShowMemoFields: Boolean read FShowMemoFields write SetShowMemoFields default False;
    property ShowBooleanFields: Boolean read FShowBooleanFields write SetShowBooleanFields default False;
    property ShowPictureFields: Boolean read FShowPictureFields write SetShowPictureFields default False;
    property ShowUnicode: Boolean read FShowUnicode write FShowUnicode;
    property UseDBFieldWidths: boolean read FUseDBFieldWidths write FUseDBFieldWidths default False;

    property OnRowChanging:TRowChangingEvent read FOnRowChanging write FOnRowChanging;
    property OnRowUpdate: TRowChangedEvent read FOnRowChanged write FOnRowChanged;
    property OnCellValidate: TCellValidateEvent read FOnCellValidate write FOnCellValidate;
    property OnCellValidateWide: TCellValidateWideEvent read FOnCellValidateWide write FOnCellValidateWide;
    property OnCanSort: TCanSortEvent read FOnCanSort write FOnCanSort;
    property OnColumnPopup: TDBColumnPopupEvent read FOnColumnPopup write FOnColumnPopup;
    property OnSetQueryOrder: TSetQueryOrderEvent read FOnSetQueryOrder write FOnSetQueryOrder;
    property OnGetEditText: TGetEditEvent read FOnGetEditText write FOnGetEditText;
    property OnGetRecordCount: TGetRecordCountEvent read FOnGetRecordCount write FOnGetRecordCount;
    property OnGetHTMLTemplateData: TGetHTMLTemplateDataEvent read FOnGetHTMLTemplateData write FOnGetHTMLTemplateData;
    property OnGetHTMLTemplate: TGetHTMLTemplateEvent read FOnGetHTMLTemplate write FOnGetHTMLTemplate;
    property OnInvalidPicture: TInvalidPictureEvent read FOnInvalidPicture write FOnInvalidPicture;
    property OnFieldToStream: TFieldToStreamEvent read FOnFieldToStream write FOnFieldToStream;
  end;

implementation

uses
  Variants, StrUtils
{$IFDEF DELPHIXE4_LVL}
  , AnsiStrings
{$ENDIF}
  ;

const
  MinGraphicSize = 44;

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


function FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;
  Result := False;
  if BufferSize < MinGraphicSize then Exit;
  case Words[0] of
    $4D42: GraphicClass := TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    {$IFDEF DELPHIXE_LVL}
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
    {$ENDIF}
  else
    {$IFDEF DELPHI2010_LVL}
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else
    {$ENDIF}
    if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    {$IFDEF DELPHI2010_LVL}
    {$IFDEF DELPHIXE4_LVL}
    else if AnsiStrings.StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ENDIF}
      GraphicClass := TGIFImage
    {$ENDIF}
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  Result := (GraphicClass <> nil);
end;

function FindGraphicClass(Stream: TStream;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  Buffer: PByte;
  CurPos: Int64;
  BytesRead: Integer;
begin
  if Stream is TCustomMemoryStream then
  begin
    Buffer := TCustomMemoryStream(Stream).Memory;
    CurPos := Stream.Position;
    Inc(Buffer, CurPos);
    Result := FindGraphicClass(Buffer^, Stream.Size - CurPos, GraphicClass);
    Exit;
  end;
  GetMem(Buffer, MinGraphicSize);
  try
    BytesRead := Stream.Read(Buffer^, MinGraphicSize);
    Stream.Seek(-BytesRead, soCurrent);
    Result := FindGraphicClass(Buffer^, BytesRead, GraphicClass);
  finally
    FreeMem(Buffer);
  end;
end;

{ TAdvGridDataLink }

constructor TAdvGridDataLink.Create(AGrid: TDBAdvGrid);
begin
  inherited Create;
  FGrid := AGrid;
  VisualControl := True;
end;

//------------------------------------------------------------------------------

destructor TAdvGridDataLink.Destroy;
begin
  ClearMapping;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TAdvGridDataLink.GetDefaultFields: Boolean;
var
  I: Integer;
begin
  Result := True;
  if DataSet <> nil then
  begin
  {$IFDEF DELPHIXE6_LVL}
    Result := lcAutomatic in DataSet.Fields.LifeCycles;
  {$ELSE}
    Result := DataSet.DefaultFields;
  {$ENDIF}
  end;

  if Result and SparseMap then
    for I := 0 to FFieldCount - 1 do
      if FFieldMap[I] < 0 then
      begin
        Result := False;
        Exit;
      end;
end;

//------------------------------------------------------------------------------

function TAdvGridDataLink.GetFields(I: Integer): TField;
begin
  if (0 <= I) and (I < FFieldCount) and (FFieldMap[I] >= 0) then
    Result := DataSet.FieldList[FFieldMap[I]]
  else
  begin
    if (0 <= I) and (I < FFieldCount) then
      Result := DataSet.FieldList[I]
    else
      Result := nil;
  end;
end;

//------------------------------------------------------------------------------

function TAdvGridDataLink.AddMapping(const FieldName: string): Boolean;
{var
  Field: TField;
  NewSize: Integer; }
begin
  Result := True; {
  if FFieldCount >= MaxMapSize then RaiseGridError(STooManyColumns);
  if SparseMap then
    Field := DataSet.FindField(FieldName)
  else
    Field := DataSet.FieldByName(FieldName);

  if FFieldCount = Length(FFieldMap) then
  begin
    NewSize := Length(FFieldMap);
    if NewSize = 0 then
      NewSize := 8
    else
      Inc(NewSize, NewSize);
    if (NewSize < FFieldCount) then
      NewSize := FFieldCount + 1;
    if (NewSize > MaxMapSize) then
      NewSize := MaxMapSize;
    SetLength(FFieldMap, NewSize);
  end;
  if Assigned(Field) then
  begin
    FFieldMap[FFieldCount] := Dataset.FieldList.IndexOfObject(Field);
    Field.FreeNotification(FGrid);
  end
  else
    FFieldMap[FFieldCount] := -1;
  Inc(FFieldCount);  }
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.ActiveChanged;
begin
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        raise exception.Create('UniDirectional DataSet');

  if not FLockEffects then
    FGrid.ActiveChange(Active);
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.ClearMapping;
begin
  FFieldMap := nil;
  FFieldCount := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.Modified;
begin
  FModified := True;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.DataSetChanged;
begin
  if not FLockEffects then
    FGrid.DataChange;
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.DataSetScrolled(Distance: Integer);
begin
  if not FLockEffects then
    FGrid.Scroll(Distance);
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.LayoutChanged;
{var
  SaveState: Boolean;}
begin
  { FLayoutFromDataset determines whether default column width is forced to
    be at least wide enough for the column title.  }
{  SaveState := FGrid.FLayoutFromDataset;
  FGrid.FLayoutFromDataset := True;
  try
    FGrid.LayoutChanged;
  finally
    FGrid.FLayoutFromDataset := SaveState;
  end; }
  inherited LayoutChanged;
end;

//------------------------------------------------------------------------------
procedure TAdvGridDataLink.FocusControl(Field: TFieldRef);
begin
  if Assigned(Field) and Assigned(Field^) then
  begin
  {  FGrid.SelectedField := Field^;
    if (FGrid.SelectedField = Field^) and FGrid.AcquireFocus then
    begin
      Field^ := nil;
      FGrid.ShowEditor;
    end; }
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.EditingChanged;
begin
  if not FLockEffects then
    FGrid.EditingChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.RecordChanged(Field: TField);
begin
  if not FLockEffects then
    FGrid.RecordChanged(Field);
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified and not FLockEffects then
      FGrid.DataUpdate;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

//------------------------------------------------------------------------------

function TAdvGridDataLink.GetMappedIndex(ColIndex: Integer): Integer;
begin
  if (0 <= ColIndex) and (ColIndex < FFieldCount) then
    Result := FFieldMap[ColIndex]
  else
    Result := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.Reset;
begin
  if FModified then
    RecordChanged(nil)
  else
    Dataset.Cancel;
end;

//------------------------------------------------------------------------------

function TAdvGridDataLink.IsAggRow(Value: Integer): Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

procedure TAdvGridDataLink.BuildAggMap;
begin
end;

//------------------------------------------------------------------------------

{ TDBGridColumnItem }

constructor TDBGridColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FWidth := 50;
  FFont := TFont.Create;
  FHeaderFont := TFont.Create;
  FPrintFont := TFont.Create;
  FBorderPen := TPen.Create;
  FBorderPen.Width := 1;
  FBorderPen.Color := clSilver;
  Editor := edNormal;
  FUseLookupEditor := true;
  FAutoCreated := false;

  FPrintBorders := [cbTop, cbLeft, cbRight, cbBottom];
  FPrintBorderPen := TPen.Create;
  FPrintBorderPen.Width := 1;
  FPrintBorderPen.Color := clBlack;

  if Assigned(Collection) then
  begin
    FFont.Assign((TDBGridColumnCollection(Collection).FOwner).Font);
    FHeaderFont.Assign((TDBGridColumnCollection(Collection).FOwner).Font);
    FPrintFont.Assign((TDBGridColumnCollection(Collection).FOwner).Font);
    FColor := TDBGridColumnCollection(Collection).FOwner.Color;
  end;
    
  FPrintColor := clWhite;
  FFont.OnChange := FontChanged;
  FHeaderFont.OnChange := FontChanged;
  FBorderPen.OnChange := PenChanged;
  FComboItems := TStringList.Create;
  FCheckBoxField := false;
  FProgressField := false;
  FProgressColor := clRed;
  FProgressBKColor := clWhite;
  FProgressTextColor := clWhite;
  FProgressTextBKColor := clRed;
  FCheckTrue := 'Y';
  FCheckFalse := 'N';
  FSpinStep := 1;
  FUseComboObjectValue := false;

  FPictureField := false;
  FPictureStretch := StretchWithAspectRatio;
  FDataImageField := false;
  FHTMLTemplate := '';

  FMinSize := 0;
  FMaxSize := 0;
  FFilter := TStringList.Create;
  FFilter.OnChange := FilterChanged;
end;

//------------------------------------------------------------------------------

destructor TDBGridColumnItem.Destroy;
begin
  FFilter.Free;
  FFont.Free;
  FPrintFont.Free;
  FComboItems.Free;
  FBorderPen.Free;
  FPrintBorderPen.Free;
  FHeaderFont.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.FontChanged(Sender: TObject);
begin
  TDBGridColumnCollection(Collection).Update(self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.PenChanged(Sender: TObject);
begin
  TDBGridColumnCollection(Collection).Update(self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetWidth(const Value: Integer);
var
  Grid: TDBAdvGrid;
begin
  FWidth := Value;
  TDBGridColumnCollection(Collection).FOwner.AllColWidths[Index] := FWidth;
  if (FWidth = 0) then
  begin
    Grid := GetGrid;
    if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) then
      Grid.FColWith0Width := True;
  end;
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetWidth: Integer;
begin
  Result := TDBGridColumnCollection(Collection).FOwner.AllColWidths[Index];
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GridColIndex: integer;
var
  colgrp: integer;
begin
  colgrp := (Collection as TDBGridColumnCollection).FOwner.GroupColumn;

  Result := Index;

  if not (Collection as TDBGridColumnCollection).FOwner.PageMode and (colgrp <> - 1) then
  begin
    if Index > colgrp then
      Result := Index - 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetHeaderAlignment(const Value: TAlignment);
begin
  FHeaderAlignment := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetColumnHeader(const Value: string);
//var
//  i: Integer;
begin
  FColumnHeader := Value;
 { if (TDBGridColumnCollection(Collection).FOwner.FixedRows > 0) and
    not TDBGridColumnCollection(Collection).FOwner.FColMoving then
  begin
    TDBGridColumnCollection(Collection).FOwner.Cells[Index,0] := Value;
    TDBGridColumnCollection(Collection).Update(Self);
  end;
 }
 {
  TDBGridColumnCollection(Collection).FOwner.ColumnHeaders.Clear;

  for i := 1 to TDBGridColumnCollection(Collection).Count do
  begin
    TDBGridColumnCollection(Collection).FOwner.ColumnHeaders.Add(
      TDBGridColumnCollection(Collection).Items[i - 1].Header);
  end;
  }

  if TDBGridColumnCollection(Collection).FOwner.PageMode then
    TDBGridColumnCollection(Collection).FOwner.Invalidate
  else
    TDBGridColumnCollection(Collection).FOwner.Cells[Index,0] := value;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetFont(const value: TFont);
begin
  FFont.Assign(value);
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetColor(const Value: TColor);
begin
  FColor := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetShowBands(const Value: Boolean);
begin
  FShowBands := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetFixed(const Value: Boolean);
begin
  FFixed := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.Assign(Source: TPersistent);
begin
  if Source is TDBGridColumnItem then
  begin
    FieldName := TDBGridColumnItem(Source).FieldName;
    FAlignment := TDBGridColumnItem(Source).Alignment;
    FBorderPen.Assign(TDBGridColumnItem(Source).BorderPen);
    FBorders := TDBGridColumnItem(Source).Borders;
    FCheckBoxField := TDBGridColumnItem(Source).CheckBoxField;
    FCheckFalse := TDBGridColumnItem(Source).CheckFalse;
    FCheckTrue := TDBGridColumnItem(Source).CheckTrue;
    FColor := TDBGridColumnItem(Source).Color;
    FComboItems.Assign(TDBGridColumnItem(Source).ComboItems);
    FEditLength := TDBGridColumnItem(Source).EditLength;
    FEditLink := TDBGridColumnItem(Source).EditLink;
    FEditMask := TDBGridColumnItem(Source).EditMask;
    FEditorType := TDBGridColumnItem(Source).Editor;
    FFixed := TDBGridColumnItem(Source).Fixed;
    FFont.Assign(TDBGridColumnItem(Source).Font);
    FColumnHeader := TDBGridColumnItem(Source).Header;
    FHeaderFont.Assign(TDBGridColumnItem(Source).HeaderFont);
    FHeaderAlignment := TDBGridColumnItem(Source).HeaderAlignment;
    FName := TDBGridColumnItem(Source).Name;
    FPassword := TDBGridColumnItem(Source).Password;
    FPrintBorderPen.Assign(TDBGridColumnItem(Source).PrintBorderPen);
    FPrintBorders := TDBGridColumnItem(Source).PrintBorders;
    FPrintColor := TDBGridColumnItem(Source).PrintColor;
    FPrintFont.Assign(TDBGridColumnItem(Source).PrintFont);
    FReadOnly := TDBGridColumnItem(Source).ReadOnly;
    FSortPrefix := TDBGridColumnItem(Source).SortPrefix;
    FSortStyle := TDBGridColumnItem(Source).SortStyle;
    FSortSuffix := TDBGridColumnItem(Source).SortSuffix;
    FSpinMax := TDBGridColumnItem(Source).SpinMax;
    FSpinMin := TDBGridColumnItem(Source).SpinMin;
    FSpinStep := TDBGridColumnItem(Source).SpinStep;
    FColumnPopup := TDBGridColumnItem(Source).ColumnPopup;
    FTag := TDBGridColumnItem(Source).Tag;
    FWidth := TDBGridColumnItem(Source).Width;
    FShowBands := TDBGridColumnItem(Source).ShowBands;
    FMinSize := TDBGridColumnItem(Source).MinSize;
    FMaxSize := TDBGridColumnItem(Source).MaxSize;
    FAutoMinSize := TDBGridColumnItem(Source).AutoMinSize;
    FAutoMaxSize := TDBGridColumnItem(Source).AutoMaxSize;
    FFilter.Assign(TDBGridColumnItem(Source).Filter);
    FFilterCaseSensitive := TDBGridColumnItem(Source).FilterCaseSensitive;
    FDefIdx := TDBGridColumnItem(Source).DefIdx;
    FFloatFormat := TDBGridColumnItem(Source).FloatFormat;
    FSortStyle := TDBGridColumnItem(Source).SortStyle;
    FSortPrefix := TDBGridColumnItem(Source).SortPrefix;
    FSortSuffix := TDBGridColumnItem(Source).SortSuffix;
    FDataImageField := TDBGridColumnItem(Source).DataImageField;
    FShowUnicode := TDBGridColumnItem(Source).ShowUnicode;
    FProgressField := TDBGridColumnItem(Source).ProgressField;
    FProgressColor := TDBGridColumnItem(Source).ProgressColor;
    FProgressBKColor := TDBGridColumnItem(Source).ProgressBKColor;
    FProgressTextColor := TDBGridColumnItem(Source).ProgressTextColor;
    FProgressTextBKColor := TDBGridColumnItem(Source).ProgressTextBKColor;
    FHTMLTemplate := TDBGridColumnItem(Source).HTMLTemplate;
    FPictureField := TDBGridColumnItem(Source).PictureField;
    FPictureStretch := TDBGridColumnItem(Source).PictureStretch;
    FUseComboObjectValue := TDBGridColumnItem(Source).UseComboObjectValue;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.AssignVisuals(Source: TPersistent);
begin
  if Source is TDBGridColumnItem then
  begin
    FAlignment := TDBGridColumnItem(Source).Alignment;
    FBorderPen.Assign(TDBGridColumnItem(Source).BorderPen);
    FBorders := TDBGridColumnItem(Source).Borders;
    FCheckFalse := TDBGridColumnItem(Source).CheckFalse;
    FCheckTrue := TDBGridColumnItem(Source).CheckTrue;
    FColor := TDBAdvGrid(Collection.Owner).Color;
    FComboItems.Assign(TDBGridColumnItem(Source).ComboItems);
    FEditLength := TDBGridColumnItem(Source).EditLength;
    FEditLink := TDBGridColumnItem(Source).EditLink;
    FEditMask := TDBGridColumnItem(Source).EditMask;
    FEditorType := TDBGridColumnItem(Source).Editor;
    FFixed := TDBGridColumnItem(Source).Fixed;
    FFont.Assign(TDBAdvGrid(Collection.Owner).Font);
    FHeaderFont.Assign(TDBGridColumnItem(Source).HeaderFont);
    FHeaderAlignment := TDBGridColumnItem(source).HeaderAlignment;
    FPassword := TDBGridColumnItem(Source).Password;
    FPrintBorderPen.Assign(TDBGridColumnItem(Source).PrintBorderPen);
    FPrintBorders := TDBGridColumnItem(Source).PrintBorders;
    FPrintColor := TDBGridColumnItem(Source).PrintColor;
    FPrintFont.Assign(TDBGridColumnItem(Source).PrintFont);
    FReadOnly := TDBGridColumnItem(Source).ReadOnly;
    FSortPrefix := TDBGridColumnItem(Source).SortPrefix;
    FSortStyle := TDBGridColumnItem(Source).SortStyle;
    FSortSuffix := TDBGridColumnItem(Source).SortSuffix;
    FSpinMax := TDBGridColumnItem(Source).SpinMax;
    FSpinMin := TDBGridColumnItem(Source).SpinMin;
    FSpinStep := TDBGridColumnItem(Source).SpinStep;
    FColumnPopup := TDBGridColumnItem(Source).ColumnPopup;
    FWidth := TDBGridColumnItem(Source).Width;
    FShowBands := TDBGridColumnItem(Source).ShowBands;
    FMinSize := TDBGridColumnItem(Source).MinSize;
    FMaxSize := TDBGridColumnItem(Source).MaxSize;
    FAutoMinSize := TDBGridColumnItem(Source).AutoMinSize;
    FAutoMaxSize := TDBGridColumnItem(Source).AutoMaxSize;
    FFilter.Assign(TDBGridColumnItem(Source).Filter);
    FFilterCaseSensitive := TDBGridColumnItem(Source).FilterCaseSensitive;
    FDefIdx := TDBGridColumnItem(Source).DefIdx;
    FFloatFormat := TDBGridColumnItem(Source).FloatFormat;
    FSortStyle := TDBGridColumnItem(Source).SortStyle;
    FSortPrefix := TDBGridColumnItem(Source).SortPrefix;
    FSortSuffix := TDBGridColumnItem(Source).SortSuffix;
  	FHTMLTemplate := TDBGridColumnItem(Source).HTMLTemplate;
  end;
end;


//------------------------------------------------------------------------------

function TDBGridColumnItem.GetDisplayName: string;
begin
  if FieldName = '' then
    Result := 'Column ' + Inttostr(Index)
  else
    Result := FieldName;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetComboItems(const Value: TStringList);
begin
  FComboItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetPassword(const Value: Boolean);
begin
  FPassword := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetBorderPen(const Value: TPen);
begin
  FBorderPen := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetBorders(const Value: TCellBorders);
begin
  FBorders := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetReadOnly: boolean;
begin
  Result := FReadOnly;
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetRows(idx: integer): string;
begin
  Result := (Collection as TDBGridColumnCollection).FOwner.Cells[GridColIndex, idx];
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetRows(idx: integer; const Value: string);
begin
  (Collection as TDBGridColumnCollection).FOwner.Cells[GridColIndex, idx] := Value;
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetDates(idx: Integer): TDateTime;
begin
  Result := (Collection as TDBGridColumnCollection).FOwner.Dates[GridColIndex, idx];
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetFloats(idx: Integer): Double;
begin
  Result := (Collection as TDBGridColumnCollection).FOwner.Floats[GridColIndex, idx];
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetInts(idx: Integer): Integer;
begin
  Result := (Collection as TDBGridColumnCollection).FOwner.Ints[GridColIndex, idx];
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetDates(idx: Integer; const Value: TDateTime);
begin
  (Collection as TDBGridColumnCollection).FOwner.Dates[GridColIndex, idx] := Value;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetFloats(idx: Integer; const Value: Double);
begin
  (Collection as TDBGridColumnCollection).FOwner.Floats[GridColIndex, idx] := Value;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetIndex(Value: Integer);
var
  OldIndex, TmpWidth: Integer;

begin
  OldIndex := Index;
  inherited SetIndex(Value);

  if (csDesigning in (Collection as TDBGridColumnCollection).FOwner.ComponentState) then
  begin
    TmpWidth := TDBGridColumnCollection(Collection).Items[OldIndex].Width;
    TDBGridColumnCollection(Collection).Items[OldIndex].Width := TDBGridColumnCollection(Collection).Items[Index].Width;
    TDBGridColumnCollection(Collection).Items[Index].Width := TmpWidth;
  end;
end;

procedure TDBGridColumnItem.SetInts(idx: Integer; const Value: Integer);
begin
  (Collection as TDBGridColumnCollection).FOwner.Ints[GridColIndex, idx] := Value;
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetTimes(idx: Integer): TDateTime;
begin
  Result := (Collection as TDBGridColumnCollection).FOwner.Times[GridColIndex, idx];
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetTimes(idx: Integer; const Value: TDateTime);
begin
  (Collection as TDBGridColumnCollection).FOwner.Times[GridColIndex, idx] := Value;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetEditorType(const Value: TEditorType);
var
  UpdateFlg: Boolean;
begin
  if FEditorType <> Value then
  begin
    UpdateFlg := (FEditorType = edDataCheckBox) or (Value = edDataCheckBox);
    FEditorType := Value;
    if UpdateFlg then
      (Collection as TDBGridColumnCollection).FOwner.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetFilter(const Value: TStringList);
begin
  FFilter.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.FilterChanged(Sender: TObject);
begin
  (Collection as TDBGridColumnCollection).FOwner.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetFloatFormat(const Value: string);
begin
  FFloatFormat := Value;
  (Collection as TDBGridColumnCollection).FOwner.Invalidate;
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetField: TField;
var
  Grid: TDBAdvGrid;
begin { Returns Nil if FieldName can't be found in dataset }
  Grid := GetGrid;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(Grid) and
    Assigned(Grid.DataLink.DataSet) then
    with Grid.Datalink.Dataset do
    {$IFDEF DELPHIXE6_LVL}
      if Active or not (lcAutomatic in Fields.LifeCycles) then
    {$ELSE}
      if Active or (not DefaultFields) then
    {$ENDIF}
        SetField(FindField(FieldName));
  Result := FField;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetField(Value: TField);
var
  Grid: TDBAdvGrid;
begin
  if FField = Value then
    Exit;

  if Assigned(Value) and (csDestroying in Value.ComponentState) then
    Value := nil; // don't acquire references to fields being destroyed

  Grid := GetGrid;

  if Assigned(FField) and Assigned(Grid) then
    FField.RemoveFreeNotification(Grid);

  FField := Value;
  if Assigned(Value) then
  begin
    if Assigned(Grid) then
      FField.FreeNotification(Grid);
    FFieldName := Value.FullName;
  end;

  Changed(False);
end;

//------------------------------------------------------------------------------

function TDBGridColumnItem.GetGrid: TDBAdvGrid;
begin
  if Assigned(Collection) and (Collection is TDBGridColumnCollection) then
    Result := TDBGridColumnCollection(Collection).Grid
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetFieldName(const Value: string);
var
  AField: TField;
  Grid: TDBAdvGrid;
begin
  AField := nil;
  Grid := GetGrid;
  if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) and
    not (csLoading in Grid.ComponentState) and (Length(Value) > 0) then
    AField := Grid.DataLink.DataSet.FindField(Value); { no exceptions }
  FFieldName := Value;
  if Value <> '' then
    SetField(AField);
  Changed(False);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetPictureField(const Value: Boolean);
begin
  if Value and (FCheckBoxField or FProgressField) then
    raise exception.Create('Can not assign true when CheckBoxField or ProgressField is also true.');

  FPictureField := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetCheckBoxField(const Value: Boolean);
var
  Grid: TDBAdvGrid;
begin
  if Value and (FPictureField or FProgressField) then
    raise exception.Create('Can not assign true when PictureField or ProgressField is also true.');

  if FCheckBoxField <> Value then
  begin
    Grid := GetGrid;

    if Assigned(Grid) then
    begin
      if not Value then
        Grid.RemoveStringCheckBox(index)
      else
      begin
        FCheckBoxField := Value;
        Grid.AddStringCheckBox(Index);
      end;
    end;

    FCheckBoxField := Value;

    TDBGridColumnCollection(Collection).Update(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetProgressField(const Value: Boolean);
begin
  if Value and (FCheckBoxField or FPictureField) then
    raise Exception.Create('Can not assign true when CheckBoxField or PictureField is also true.');
    
  if FProgressField <> Value then
  begin
    FProgressField := Value;
    TDBGridColumnCollection(Collection).Update(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetProgressColor(const Value: TColor);
begin
  FProgressColor := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;
//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetProgressBKColor(const Value: TColor);
begin
  FProgressBKColor := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;
//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetProgressTextColor(const Value: TColor);
begin
  FProgressTextColor := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;
//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetProgressTextBKColor(const Value: TColor);
begin
  FProgressTextBKColor := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetDataImageField(const Value: boolean);
begin
  FDataImageField := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetHTMLTemplate(const Value: string);
begin
  FHTMLTemplate := Value;
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnItem.SetPrintFont(const Value: TFont);
begin
  FPrintFont.Assign(Value);
  TDBGridColumnCollection(Collection).Update(Self);
end;

//------------------------------------------------------------------------------

{ TDBGridColumnCollection }

function TDBGridColumnCollection.Add: TDBGridColumnItem;
begin
  Result := TDBGridColumnItem(inherited Add);

  if Count > 1 then
    Result.AssignVisuals(Items[Count - 2]);
end;

//------------------------------------------------------------------------------

  constructor TDBGridColumnCollection.Create(AOwner: TDBAdvGrid);
  begin
    inherited Create(GetItemClass);

  FOwner := AOwner;
  FNoRecursiveUpdate := False;
end;

//------------------------------------------------------------------------------

function TDBGridColumnCollection.GetItem(Index: Integer): TDBGridColumnItem;
begin
  Result := TDBGridColumnItem(inherited GetItem(Index));
end;

//------------------------------------------------------------------------------

function TDBGridColumnCollection.GetItemClass: TCollectionItemClass;
begin
  Result := TDBGridColumnItem;
end;

//------------------------------------------------------------------------------

function TDBGridColumnCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnCollection.Delete(Index: integer);
var
  il: TIntList;
  i: integer;
begin
  il := TIntList.Create(-1,-1);

  for i := 0 to Count - 1 do
    il.Add(Items[i].Width);

  il.Delete(Index);

  inherited Delete(Index);

  for i := 0 to Count - 1 do
    Items[i].Width := il.Items[i];
  il.Free;
end;

//------------------------------------------------------------------------------

function TDBGridColumnCollection.Insert(Index: Integer): TDBGridColumnItem;
var
  i: integer;
begin
  inherited Add;
  for i := Count - 1 downto Index + 1 do
    TDBGridColumnItem(Items[i]).Assign(TDBGridColumnItem(Items[i - 1]));
  Result := TDBGridColumnItem(Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  if not FNoRecursiveUpdate and ((Action = cnDeleting) or (Action = cnExtracting)) then
    Grid.UnHideColumnsAll;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnCollection.ResetOrganization;
var
  i: Integer;
  rst: Boolean;
begin
  rst := False;
  while not rst do
  begin
    rst := True;
    for i := 1 to Count do
    begin
      if i - 1 > Items[i - 1].DefIdx then
      begin
        rst := False;
        FOwner.MoveColumn(i - 1, Items[i - 1].DefIdx);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnCollection.SetItem(Index: Integer;
  const Value: TDBGridColumnItem);
begin
  inherited SetItem(Index, Value);
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnCollection.SetOrganization;
var
  i: Integer;
begin
  FNoRecursiveUpdate := True;
  FOwner.SynchColumns;
  FNoRecursiveUpdate := False;

  for i := 1 to Count do
    Items[i - 1].DefIdx := i - 1;

{$IFDEF TMSDEBUG}
  for i := 1 to Count do
    outputdebugstring(pchar(inttostr(Items[i - 1].DefIdx)));
{$ENDIF}
end;

//------------------------------------------------------------------------------

function TDBGridColumnCollection.HasFieldsDefined: boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Count do
  begin
    if (Items[i - 1].FieldName <> '') and not (Items[i - 1].AutoCreated) then
      Result := true;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBGridColumnCollection.Update(Item: TCollectionItem);
var
  VisCols: Integer;
begin
  inherited Update(Item);
//reflect changes
  if not FNoRecursiveUpdate then
  begin
    VisCols := Count - FOwner.NumHiddenColumns;
//if (csDesigning in fOwner.ComponentState) then
    if (VisCols <> FOwner.ColCount) and (VisCols > FOwner.FixedCols) then
    begin
      FOwner.ColCount := VisCols;
      FOwner.ColWidthsChanged;
    end;
    FOwner.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

{ TDBAdvGrid }

constructor TDBAdvGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsDBVersion := True;
  FShowBlankRow := True;
  FDataLink := TAdvGridDataLink.Create(self);
  FColumnCollection := CreateColumns;
  FColumnCollection.FNoRecursiveUpdate := True;
  SynchColumns;
  FColumnCollection.FNoRecursiveUpdate := False;
  AlwaysValidate := true;
  FLastDesignChoice := -1;
  FOldPosition := nil;
  FCellGraphic := TCellGraphic.Create;
  FShowMemoFields := false;
  FShowDefinedFields := false;
  FShowDBIndicator := true;
  FEmptyDataSet := false;
  FShowBooleanFields := false;
  FShowPictureFields := false;
  FPicture := TPicture.Create;
  FLookupKeys := TStringList.Create;
  FUseDBFieldWidths := false;
  FColWith0Width := False;

  inherited OnRowChanging:= RowChangingEvent;
  inherited OnRowUpdate := RowUpdateEvent;
  inherited OnCellValidate := CellValidateEvent;
  inherited OnCanSort := CanSortEvent;
  inherited OnGetEditText:= GetEditTextEvent;
  inherited OnCellValidateWide := CellValidateWideEvent;

  FPageMode := true;
  FPictureList := TList.Create;
//  DoubleBuffered := true;
  ColCount := 2;
  RowCount := 2;

  FRefreshOnDelete := false;
  RefreshOnInsert := false;

  FVisibleFieldCount := 0;
  FDataSetType := dtSequenced;
  FExportRow := -1;
  FSelRow := 0;

  FOldDataSetType := dtSequenced;
  FDatasetTypeAuto := true;
  FEditRec := -1;
  FOldEditingState := dsBrowse;
  FEditText := '';
  FEditWideText := '';
  FEditRecData := TStringList.Create;
  FEditPostMode := epCell;

  FOldAR := -1;
  FOldTopRow := -1;

  FAutoCreateColumns := True;
  FAutoRemoveColumns := True;
  FInvalidPicture := TPicture.Create;
  FInvalidPicture.Icon.Handle := LoadIcon(0, IDI_ERROR);

  DefaultRowHeight := 22;
  FixedColWidth := 20;
  FExportStartRow := -1;
end;

//------------------------------------------------------------------------------

destructor TDBAdvGrid.Destroy;
var
  i: integer;
begin
  if Assigned(FPictureList) then
  begin
    for i := 0 to FPictureList.Count - 1 do
      TPicture(FPictureList.Items[i]).Free;
    FPictureList.Free;
  end;

  FEditRecData.Free;
  FLookupKeys.Free;
  FDataLink.Free;
  FDataLink := nil;
  FColumnCollection.Free;
  FCellGraphic.Free;
  FPicture.Free;
  FInvalidPicture.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.DoAutoRemoveColumns(removedefined: boolean);
var
  DoUpdate,CanRemove: Boolean;
  i: integer;
begin
  if FAutoRemoveColumns then
  begin
    DoUpdate := true;

    CanRemove := (not Columns.HasFieldsDefined or removedefined);

    if (csDesigning in ComponentState) and not (csLoading in ComponentState) and CanRemove then
    begin
      DoUpdate := MessageDlg('Automatically remove columns ?',mtConfirmation, [mbYes, mbNo],0) = mrYes;
    end;

    if (csLoading in ComponentState) then
      DoUpdate := false;

    // remove any previous field references
    for i := 0 to FColumnCollection.Count - 1 do
      FColumnCollection.Items[i].FField := nil;

    if CanRemove and DoUpdate then
    begin
      ColCount := 2;
      Columns[0].FieldName := '';
      Columns[1].FieldName := '';
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.ActiveChange(Value: Boolean);
var
  i, j, rc, ColC: integer;
  aField: TField;
  {$IFNDEF DELPHIXE3_LVL}
  {$IFNDEF DELPHI2006_LVL}
  sl: TStringList;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  sl: TWideStringList;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  sl: TStringList;
  {$ENDIF}
  fldidx: integer;
  dw: integer;
  TM: TTextMetric;
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active then
        FShowDefinedFields := false;

      //Text := '';
      if FDataLink.DataSet.Active then
      begin
        FShowDefinedFields := false;

        // detect if there is a preset DB field column
        for i := 0 to FColumnCollection.Count - 1 do
        begin
          if FColumnCollection.Items[i].FieldName <> '' then
          begin
            FShowDefinedFields := true;
            Break;
          end;
        end;

        if FShowDefinedFields and FAutoRemoveColumns then
        begin
          i := 0;
          {$IFNDEF DELPHIXE3_LVL}

          {$IFNDEF DELPHI2006_LVL}
          sl := TStringList.Create;
          {$ENDIF}
          {$IFDEF DELPHI2006_LVL}
          sl := TWideStringList.Create;
          {$ENDIF}

          {$ENDIF}

          {$IFDEF DELPHIXE3_LVL}
          sl := TStringList.Create;
          {$ENDIF}

          try
            DataSource.DataSet.GetFieldNames(sl);
            while (i <= FColumnCollection.Count - 1) do
            begin
              if FColumnCollection.Items[i].FieldName <> '' then
              begin
                if (sl.IndexOf(FColumnCollection.Items[i].FieldName) < 0) then
                begin
                  FColumnCollection.Items[i].FieldName := '';
                  FColumnCollection.Items[i].Header := '';
                  FColumnCollection.Items[i].Free;
                  FShowDefinedFields := false;
                  i := -1;
                end;
              end;
              inc(i);
            end;
          finally
            sl.free;
          end;
        end;

        // count visible fields in dataset
        FVisibleFieldCount := 0;

        for i := 0 to FDataLink.DataSet.FieldCount - 1 do
        begin
          if FDataLink.DataSet.Fields[i].Visible then
            inc(FVisibleFieldCount);
        end;

        // do auto creation of all visible fields when no predefined fields used
        if not FShowDefinedFields and FAutoCreateColumns then
        begin
          if FShowDBIndicator and (FixedCols > 0) then
          begin
            ColC := FVisibleFieldCount + FixedCols - NumHiddenColumns;
            if ColC >= 0 then
              ColCount := ColC;
          end
          else
          begin
            ColC := FVisibleFieldCount + FixedCols - NumHiddenColumns;
            if ColC >= 0 then
              ColCount := ColC;
          end;
        end;

        for i := 0 to FColumnCollection.Count - 1 do
        begin
          if not ((i = 0) and FShowDBIndicator and (FixedCols > 0)) then
          begin
         { j := 1;
          if not FShowDBIndicator or (FixedCols <= 0) then
            j := 0; }

            aField := nil;
            if FColumnCollection.Items[i].FieldName <> '' then
            begin
              fldidx := FDataLink.DataSet.FieldList.IndexOf(FColumnCollection.Items[i].FieldName);

              if fldidx >= 0 then
                aField := FDataLink.DataSet.Fields[fldidx];

              //aField := FDataLink.DataSet.Fieldbyname(FColumnCollection.Items[i].FieldName)
            end
            else
              if not FShowDefinedFields then
              begin
                j := GetDBFieldIndexAtColumn(i);
                if (j <> -1) then
                  aField := FDataLink.DataSet.Fields[j];
              end;

            if Assigned(aField) then
            begin
              if FColumnCollection.Items[i].FieldName = '' then
              begin
                if FAutoCreateColumns then
                begin
                  FColumnCollection.Items[i].FieldName := aField.FieldName;
                  FColumnCollection.Items[i].AutoCreated := true;
                end;

                if not (FColumnCollection.Items[i].Editor in [edCheckBox, edDataCheckBox]) then
                  FColumnCollection.Items[i].Alignment := aField.Alignment;

                FColumnCollection.Items[i].EditMask := aField.EditMask;

                if (aField.DisplayWidth > 0) and FUseDBFieldWidths then
                begin
                  Canvas.Font.Assign(FColumnCollection.Items[i].Font);
                  GetTextMetrics(Canvas.Handle, TM);
                  dw := aField.DisplayWidth * (Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
                  if (dw < ClientWidth) then
                    FColumnCollection.Items[i].Width := dw
                  else
                    FColumnCollection.Items[i].Width := DefaultColWidth;
                end;

                if (aField.DataType in [ftString, ftWideString]) then
                  FColumnCollection.Items[i].EditLength := aField.Size
                else
                  FColumnCollection.Items[i].EditLength := 0;
              end;
            end;
          end;
        end;

      //self.Clear;
        RowCount := 1000;
        FDataLink.BufferCount := GetBufferCount;

        rc := GetRecordCount + FixedRows;  //FDataLink.DataSet.RecordCount+1;
        if rc < FixedRows + 1 then
        begin
          if CanShowBlankRow then
            rc := FixedRows + 1
          else
            rc := FixedRows;

          if (rc > 0) then
            for i := 0 to ColCount - 1 do
              Cells[i, rc - 1] := '';
        end;

        self.RowCount := rc;

        FColWith0Width := False;
        for i := 0 to FColumnCollection.Count - 1 do
        begin
          if (FColumnCollection.Items[i].Width = 0) then
          begin
            FColWith0Width := True;
            Break;
          end;
        end;

  {  if not FShowDefinedFields then
    begin
      if FShowDBIndicator and (FixedCols > 0) then
        self.ColCount:= FDataLink.DataSet.FieldCount+1
      else
        self.ColCount:= FDataLink.DataSet.FieldCount;
    end;    }
   // FDataLink.BufferCount:= self.VisibleRowCount+1;
        if not PageMode then
        begin
          if Self.FloatingFooter.Visible then
            self.RowCount := self.RowCount + 1;
          LoadFromDataSet;
        end;

        {
        if DataSetType = dtSequenced then
        begin
          if (not FDataLink.DataSet.IsSequenced) or (FDataLink.DataSet.RecNo < 0) then
          begin
            FDataSetType := dtNonSequenced;
            if not (FDataLink.DataSet.Bof and FDataLink.DataSet.Eof) then
              showmessage('Can not use Non Sequenced Dataset with Sequenced DataSetType.');
          end;
        end
        else // DataSetType = dtNonSequenced
        begin
        end;

        }
        if FDatasetTypeAuto then
        begin
          if (DataSetType = dtSequenced) then
          begin
            if (not FDataLink.DataSet.IsSequenced) or ((FDataLink.DataSet.RecNo < 0) and not FDataLink.DataSet.IsEmpty) then
              FDataSetType := dtNonSequenced;
          end
        end
        else
        begin
          // do nothing
        end;

        FFilteredDataSet := FDataLink.DataSet.Filtered;

        UpdateScrollBar(1);

        InvalidateGrid;
      end
      else
      begin
        if HasNodes or (GroupColumn >= FixedCols) then
        begin
          ExpandAll;
          UnGroup;
        end;

        ClearAll;

        RowCount := FixedRows + 1;

        DoAutoRemoveColumns(false);

        for rc := 0 to RowCount - 1 do
        begin
          for i := 0 to ColCount - 1 do
            Cells[i, rc] := '';
        end;

      end;
    end
    else
    begin
      DoAutoRemoveColumns(true);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.RecordChanged(Field: TField);
var
  I: Integer;
begin
  FRecordChanged := true;

  if not HandleAllocated then
    Exit;

  if Field = nil then
    Invalidate
  else
  begin
    for I := 0 to Columns.Count - 1 do
    begin
      if GetDBFieldAtColumn(I) = Field then
        InvalidateCell(I, Row);
    end;
  end;
end;

procedure TDBAdvGrid.Reload;
begin
  if not PageMode then
    if CheckDataSet then
    begin
      ActiveChange(true);
      LoadFromDataSet;
    end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.EditingChanged;
var
  OldV: Boolean;
  i: integer;
  //aField: TField;
  //bm: TBookMark;
begin
//InvalidateCol(0);

(*  if FEditPostMode = epRow then
  begin
    if FEditUpdating then
      exit;

    if FDataLink.DataSet.State = dsBrowse then
    begin
      if (FEditRec >=0) and false then
      begin
        if FOldEditingState = dsEdit then
        begin
          FEditUpdating:= true;
          //bm := FDataLink.DataSet.GetBookMark;
          FDataLink.DataSet.DisableControls;
          FDataLink.DataSet.Edit;

          for i:= 0 to FEditRecData.Count-1 do
          begin
            aField:= GetDBFieldAtColumn(i);
            if (aField <> nil) and (integer(FEditRecData.Objects[i]) = 1 ) then
            begin
              if aField.FieldKind = fkLookup then
              begin
                FDataLink.DataSet.FieldByName(aField.KeyFields).AsString := FEditRecData[i];
              end
              else
              begin
                aField.AsString:= FEditRecData[i];
               { if (aField.IsBlob) and not ShowMemoFields then
                  TempValue := '(MEMO)'
                else
                  TempValue := Value;  }
              end;

            end;
          end;

          FDataLink.DataSet.Post;
          FEditRec:= -1;
          FEditRecData.Clear;
          //FDataLink.DataSet.GotoBookMark(bm);
          //FDataLink.DataSet.FreeBookMark(bm);
          FDataLink.DataSet.EnableControls;
          FEditUpdating:= false;
        end
        else if FOldEditingState = dsInsert then
        begin
        end;
      end;

    end;
  end;    *)
  //PostEditRecData;
  if FDataLink.DataSet.State in [dsEdit, dsInsert] then
  begin
    FOldEditingState := FDataLink.DataSet.State;

    FWasEditing := True;

    FOldFoaterEnableCalc := FloatingFooter.EnableCalculation;
    FloatingFooter.EnableCalculation := False;

    //if FDataLink.DataSet.State = dsEdit then
      //Cells[RealColIndex(Col), Row] := Cells[RealColIndex(Col), Row];
  end
  else
  begin
    if (FloatingFooter.EnableCalculation <> FOldFoaterEnableCalc) and ({FOldState}FOldEditingState in [dsEdit, dsInsert]) then
    begin
      FloatingFooter.EnableCalculation := FOldFoaterEnableCalc;

      if FloatingFooter.Visible and (EditPostMode = epRow) then
      begin
        OldV := FDoNotCallSelect;
        FDoNotCallSelect := False;
        for i:= FixedCols to ColCount -1 do
          CalcFooter(i);
        FDoNotCallSelect := OldV
      end;
    end;
  end;
  {
  if FOldEditingState in [dsEdit, dsInsert] then
    if (FDataLink.DataSet.State = dsBrowse) and not FEditUpdating and (FEditRec >=0 ) then
      FEditUpdating:= true;
  }

  FNotDeletionUpdate := true;
{
  if (DataSetType = dtSequenced) and (FDataLink.DataSet.State = dsInsert) then
  begin
    if FDataLink.DataSet.Eof then  // may be Append
    begin
      FAppending := True;
      OldV := FInternalCall;
      FInternalCall := True;
      Row := RowCount-1;
      FInternalCall := OldV;
      FAppending := False;
    end;
  end;
}
  FAppendOperation := FDataLink.DataSet.Eof and (FDataLink.DataSet.State = dsInsert) and not FInternalInsert and not FEmptyDataSet;

  InvalidateCell(0, Row);
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.CanModify: Boolean;
begin
//FDataLink.OnDataChange := nil;
  Result := false;
  if PageMode and CheckDataSet then
  begin
    Result := FDataLink.Edit;
  end;
//FDataLink.OnDataChange := DataChange;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.DataChange;
var
  TempState: TDataSetState;
begin
  if not Assigned(FDataLink.DataSet) or not PageMode or FEditUpdating then
    Exit;

  UpdateVisibleFields;

  UpdateRowCount;

  UpdateScrollBar(1);

  TempState := FOldState;
  FOldState := FDataLink.DataSet.state;

  if (TempState = dsInsert) and (FDataLink.DataSet.state = dsBrowse) {and (EditPostMode = epRow)} then
  begin
    FEditEnding := True;
    InvalidateEditor; //HideEditor;    // RF:PInsertSecondRecIss
    FEditEnding := False;
  end;

  FNotDeletionUpdate := false;

  if FFilteredDataSet <> FDataLink.DataSet.Filtered then
  begin
    if FDataLink.DataSet.Filtered then
    begin
      if (DataSetType = dtSequenced) then
      begin
        FOldDataSetType:= DataSetType;
        DataSetType:= dtNonSequenced;
      end;
    end
    else //not FDataLink.DataSet.Filtered then
    begin
      if (DataSetType <> FOldDataSetType) and (FOldDataSetType = dtSequenced) then
      begin
        DataSetType:= dtSequenced;
        //FOldDataSetType:=
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.UpdateVisibleFields;
var
  i, j, NewVisibleFieldCount, ColC: integer;
  aField: TField;
  TM: TTextMetric;
  dw: integer;
begin
  NewVisibleFieldCount := 0;

  for i := 0 to FDataLink.DataSet.FieldCount - 1 do
  begin
    if FDataLink.DataSet.Fields[i].Visible then
    begin
      NewVisibleFieldCount := NewVisibleFieldCount + 1;
    end;
  end;

  if NewVisibleFieldCount <> FVisibleFieldCount then
  begin
    FVisibleFieldCount := NewVisibleFieldCount;

    if not FShowDefinedFields then
    begin
      if FShowDBIndicator and (FixedCols > 0) then
      begin
        ColC := ({FDataLink.DataSet.FieldCount} FVisibleFieldCount + FixedCols) - NumHiddenColumns;
        if ColC >= 0 then
          self.ColCount := ColC;
      end
      else
      begin
        ColC := {FDataLink.DataSet.FieldCount} FVisibleFieldCount + FixedCols - NumHiddenColumns;
        if ColC >= 0 then
          self.ColCount := ColC;
      end;
    end;

    if not FShowDefinedFields then
    begin
      for i := 0 to FColumnCollection.Count - 1 do
        FColumnCollection.Items[i].FieldName := '';
    end;

    for i := 0 to FColumnCollection.Count - 1 do
    begin
      if not ((i = 0) and FShowDBIndicator and (FixedCols > 0)) then
      begin
     { j := 1;
      if not FShowDBIndicator or (FixedCols <= 0) then
        j := 0; }

        aField := nil;
        if FColumnCollection.Items[i].FieldName <> '' then
          aField := FDataLink.DataSet.FindField(FColumnCollection.Items[i].FieldName)
        else
          if not FShowDefinedFields then
          begin
            j := GetDBFieldIndexAtColumn(i);
            if j <> -1 then
              aField := FDataLink.DataSet.Fields[j]
            else
              aField := nil;
          end;

        if Assigned(aField) then
        begin
          if not FShowDefinedFields and (FColumnCollection.Items[i].FieldName = '') then
            FColumnCollection.Items[i].FieldName := aField.FieldName;


          if not (FColumnCollection.Items[i].Editor in [edCheckBox, edDataCheckBox]) then
            FColumnCollection.Items[i].Alignment := aField.Alignment;

          FColumnCollection.Items[i].EditMask := aField.EditMask;

          if (aField.DisplayWidth > 0) and FUseDBFieldWidths then
          begin
            Canvas.Font.Assign(FColumnCollection.Items[i].Font);
            GetTextMetrics(Canvas.Handle, TM);
            dw := aField.DisplayWidth * (Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
            if (dw < ClientWidth) then
              FColumnCollection.Items[i].Width := dw
            else
              FColumnCollection.Items[i].Width := DefaultColWidth;
          end;


          if (aField.DataType in [ftString, ftWideString]) then
            FColumnCollection.Items[i].EditLength := aField.Size
          else
            FColumnCollection.Items[i].EditLength := 0;
        end;
      end;
    end;

    InvalidateGrid;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.DataUpdate;
var
  Field: TField;
  v: integer;
  Editor: TEditorType;
  rci: integer;
  idx: integer;
begin
  rci := RealColIndex(Col);

  idx := combobox.ItemIndex;

  Field := GetDBFieldAtColumn(rci); //SelectedField;

  if Assigned(Field) and (FDataLink.DataSet.State in [dsEdit, dsInsert]) then
  begin
    if (Field.IsBlob) then
    begin
      if (Field.DataType <> ftGraphic) then
      begin  // Memo Field
        Field.AsString := FEditText;
      end;
    end
    else if Field.FieldKind = fkLookup then
    begin
      if idx >= 0 then
      begin
        v := integer(ComboBox.Items.Objects[idx]);
        FDataLink.DataSet.FieldByName(Field.KeyFields).AsString := GetLookupKey(v);
      end;
    end
    else
    begin
      GetCellEditor(rci, Row, Editor);

      if (Editor = edComboList) and (idx >= 0) and (Field.DataType in [ftSmallint, ftInteger, ftWord]) then
      begin
        if Columns[rci].UseComboObjectValue then
        begin
          v := integer(ComboBox.Items.Objects[idx]);
          Field.AsInteger := v;
        end
        else
        begin
          Field.Text := ComboBox.Items[idx];
        end;
      end
      else
      begin
        if Field.DataType = ftWideString then
        begin
          TWideStringField(Field).Value := FEditWideText;
        end
        else
          Field.Text := FEditText;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBAdvGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);

  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = FDataLink.DataSource) then
  begin
    DataSource := nil;
  end;

  if (Operation = opRemove) and (AComponent is TCustomImageList) then
  begin
    for I := 0 to FColumnCollection.Count - 1 do
    begin
      if FColumnCollection.Items[I].Images = AComponent then
        FColumnCollection.Items[I].FImages := nil;
    end;
  end;

  if (Operation = opRemove) and (AComponent is TField) and FAutoRemoveColumns then
  begin
    for I := 0 to FColumnCollection.Count - 1 do
    begin
      if not ((I = 0) and FShowDBIndicator and (FixedCols > 0)) then
      begin
        if FColumnCollection.Items[I].FieldName <> '' then
        begin
          if UpperCase(TField(AComponent).DisplayName) = UpperCase(FColumnCollection.Items[I].FieldName) then
          begin
            if Assigned(DataSource) and Assigned(DataSource.DataSet) and (TField(AComponent).DataSet = DataSource.DataSet) then
              FColumnCollection.Delete(I);
            Break;
          end;
        end
        else if not FShowDefinedFields then
        begin
          if (GetDBFieldAtColumn(I) = AComponent) then
          begin
            FColumnCollection.Delete(I);
            Break;
          end;
        end;

      end;
     { if (GetDBFieldAtColumn(I) = AComponent) then
      begin
        FColumnCollection.Delete(I);
        Break;
      end; }
    end;
  end;

  if (Operation = opRemove) and (AComponent is TPopupMenu) then
    for i := 0 to FColumnCollection.Count - 1 do
      if FColumnCollection.Items[I].ColumnPopup = AComponent then
        FColumnCollection.Items[I].ColumnPopup := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetDataSource(const Value: TDataSource);
var
  rc, i: Integer;
begin
  if Value = FDatalink.Datasource then
    Exit;

  FDataLink.DataSource := Value;

  if not Assigned(FDataLink.DataSource) then
  begin
    RowCount := FixedRows + 1;

    if AutoRemoveColumns then
    begin
      ColCount := 2;
      FColumnCollection.Items[0].FieldName := '';
      FColumnCollection.Items[1].FieldName := '';
    end;

    rc := RowCount;
    if (rc > 0) then
      for i := 0 to ColCount - 1 do
        Cells[i, rc-1] := '';
  end;

  if Value <> nil then
    Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetReadOnly(const Value: Boolean);
begin
  FDataLink.Readonly := Value;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.HTMLDBReplace(s: string; dataset: Tdataset; ACol, ARow: integer): string;
var
  beforetag, aftertag, fld, dbfld: string;
  i, j: integer;
  AField: TField;

begin
  beforetag := '';

  while Pos('<#', s) > 0 do
  begin
    i := pos('<#', s);
    beforetag := beforetag + copy(s, 1, i - 1); //part prior to the tag
    aftertag := copy(s, i, length(s)); //part after the tag
    j := pos('>', aftertag);
    fld := copy(aftertag, 1, j - 1);
    Delete(fld, 1, 2);
    Delete(s, 1, i + j - 1);

    dbfld := '';
    if Assigned(DataSet) then
    begin
      if DataSet.Active then
      begin
        AField := DataSet.FindField(fld);

        if Assigned(AField) then
        begin
          if afield.IsBlob then
          begin
            if not ShowMemoFields then
            begin
              dbfld := '(MEMO)'
            end
            else
            begin
              if Assigned(AField.OnGetText) then
                dbfld := AField.DisplayText
              else
                dbfld := AField.AsString;
            end;
          end
          else
           dbfld := AField.DisplayText;
        end;

        if Assigned(FOnGetHTMLTemplateData) then
          FOnGetHTMLTemplateData(Self, ACol, ARow, fld, dbfld);
      end
      else
        dbfld := '(' + fld + ')';
    end
    else dbfld := '(' + fld + ')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + s;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetDisplText(c, r: Integer; var Value: string);
var
  OldActiveRecord, i,j: integer;
  AField: TField;
  HTMLTemplate: string;
  fldidx: integer;
  frd: integer;
begin
  //inherited;

  if Assigned(FDataLink) and (c < FColumnCollection.Count) and PageMode then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if FDataLink.DataSet.Active then
      begin
       { i := 1;
       if not FShowDBIndicator or (FixedCols <= 0) then
         i := 0; }

        frd := 1;
        if FilterEdit.Enabled then
        begin
          if FilterEdit.Row = FixedRows - 1 then
            frd := 2;
        end;

        if (r = FixedRows - frd)  then
        begin
          if (((not FShowDBIndicator or (FixedCols <= 0)) and (c = 0)) or (c > 0)) then // Field Names
          begin
            if (FColumnCollection.Items[c].Header <> '') then
              Value := FColumnCollection.Items[c].Header
            else
              if (GridCells[c,r] <> '') then
                Value := GridCells[c,r]
              else
                if (FColumnCollection.Items[c].FieldName <> '') then
                begin
                  fldidx := FDataLink.DataSet.FieldList.IndexOf(FColumnCollection.Items[c].FieldName);
                  if fldidx > -1 then
                    aField := FDataLink.DataSet.Fields[fldidx]
                  else
                    aField := nil;

                  if Assigned(aField) and (aField.DisplayLabel <> '') then
                    Value := aField.DisplayLabel
                  else
                    Value := FColumnCollection.Items[c].FieldName;
                end
                else
                  if {not FShowDefinedFields} AutoCreateColumns then
                  begin
                    i := GetDBFieldIndexAtColumn(c);
                    if (i <> -1) then
                    begin
                      if (FDataLink.DataSet.Fields[i].DisplayLabel <> '') then
                        Value := FDataLink.DataSet.Fields[i].DisplayLabel
                      else
                        Value := FDataLink.DataSet.Fields[i].DisplayName;
                  end;
            end;
         end;
        end
        else // Field Data
        begin
          if (c = 0) and (FixedCols > 0) and FShowDBIndicator {or FDataLink.DataSet.Eof or FDataLink.DataSet.Bof} then
            Exit;
          if (r <= FixedRows - 1) or (FloatingFooter.Visible and (r >= RowCount - 1) and (FloatingFooter.FooterStyle = fsFixedLastRow)) then
          begin
            Exit;
          end;

          AField := GetDBFieldAtColumn(c);

          if (ShowBooleanFields and (aField <> nil) and (aField.DataType = ftBoolean))
            or (ShowPictureFields and (aField <> nil) and (aField.DataType = ftGraphic))
            or (FColumnCollection.Items[c].PictureField) or FColumnCollection.Items[c].CheckBoxField then
            Exit;

          OldActiveRecord := FDataLink.ActiveRecord;
          //FDataLink.ActiveRecord:= r-1;

          if DataSetType = dtSequenced then
          begin
            if (FDataLink.DataSet.State = dsInsert) or (FDataLink.DataSet.RecNo < 0) then
            begin
              if FDataLink.Eof and (FDataLink.ActiveRecord < r - (self.Row - FDataLink.ActiveRecord)) then
                FDataLink.ActiveRecord := FDataLink.BufferCount   // Set to exit;
              else
                FDataLink.ActiveRecord := r - (self.Row - FDataLink.ActiveRecord) //TopRow;
            end
            else
            begin
              FDataLink.ActiveRecord := r - ({FDataLink.DataSet.RecNo} self.Row - FDataLink.ActiveRecord);
            end;
          end
          else // DataSetType = dtNonSequenced
          begin
            if FExportRow = -1 then
            begin
              if FNewAppendRecord then
                FDataLink.ActiveRecord := r - {2}FixedRows-1
              else
                FDataLink.ActiveRecord := r - {1}FixedRows;
            end;
          end;

          if (FDataLink.ActiveRecord < 0) or ((FDataLink.ActiveRecord >= FDataLink.BufferCount) and not((FDataLink.ActiveRecord = FDataLink.BufferCount) and (DataSetType = dtNonSequenced) and FExporting) ) then
          begin
            if DataSetType = dtNonSequenced then
              Value := '';
            FDataLink.ActiveRecord := OldActiveRecord;
            Exit;
          end;

          try
            if FColumnCollection.Items[c].HTMLTemplate <> '' then
            begin
              // query template
              HTMLTemplate := FColumnCollection.Items[c].HTMLTemplate;

              if Assigned(OnGetHTMLTemplate) then
                OnGetHTMLTemplate(Self, c, r, HTMLTemplate, FDataLink.DataSet.Fields);

              Value := HTMLDBReplace(HTMLTemplate, FDataLink.DataSet, c, r);
            end
            else
            begin
              if FColumnCollection.Items[c].FieldName <> '' then
              begin
                fldidx := FDataLink.DataSet.FieldList.IndexOf(FColumnCollection.Items[c].FieldName);

                AField := nil;

                if fldidx > -1 then
                   AField := FDataLink.DataSet.Fields[fldidx];

                if Assigned(AField) and (AField.IsBlob) then
                begin
                  if (AField.DataType = ftGraphic)
                    or (FColumnCollection.Items[c].PictureField) then
                    Value := '(GRAPHIC)'
                  else
                  begin
                    if not ShowMemoFields then
                    begin
                      Value := '(MEMO)'
                    end
                    else
                    begin
                      if Assigned(aField.OnGetText) then
                        Value := AField.DisplayText
                      else
                        Value := AField.AsString;
                    end;
                  end;
                end
                else
                begin
                  if Assigned(AField) then
                  begin
                    if (AField.DataType = ftWideString) and (FColumnCollection.Items[c].ShowUnicode) then
                      Value := EncodeWideStr(TWideStringField(aField).Value)
                    else
                      Value := aField.DisplayText;
                  end;
                end;
              end
              else if AutoCreateColumns {not FShowDefinedFields} then
              begin
                j := GetDBFieldIndexAtColumn(c);

                if (j <> -1) then
                  aField := FDataLink.DataSet.Fields[j]
                else
                  aField := nil;

                if Assigned(aField) then
                begin
                  if aField.IsBlob then
                  begin
                    if (aField.DataType = ftGraphic)
                    or (FColumnCollection.Items[c].PictureField) then
                      Value := '(GRAPHIC)'
                    else
                    begin
                      if not ShowMemoFields then
                        Value := '(MEMO)'
                      else
                      begin
                        if Assigned(aField.OnGetText) then
                          Value := aField.DisplayText
                        else
                          Value := AField.AsString;
                      end;
                    end;
                  end
                  else
                  begin
                    if (aField.DataType = ftWideString) and ShowUnicode then
                      Value := EncodeWideStr(TWideStringField(aField).Value)
                    else
                      Value := AField.DisplayText;
                  end;
                end;
              end;
              {
              if (FEditPostMode = epRow) and (FEditRec = r) and (FEditRec >= 0) then
              begin
                if FEditRecData.Count > c then
                  if integer(FEditRecData.Objects[c]) = 1 then
                    Value:= FEditRecData[c];
              end; }

            end;
          finally
            FDataLink.ActiveRecord := OldActiveRecord;
          end;
        end;
      end
      else
      begin
        if (r = {0}FixedRows-1) then
        begin
          if (r = {0}FixedRows-1) and (((not FShowDBIndicator or (FixedCols <= 0)) and (c = 0)) or (c > 0)) then // Field Names
          begin
            if (FColumnCollection.Items[c].Header <> '') then
              Value := FColumnCollection.Items[c].Header
            else
              if (FColumnCollection.Items[c].FieldName <> '') then
                Value := FColumnCollection.Items[c].FieldName
          end;
        end;
      end;
    end;
  end;
  
  if Assigned(OnGetDisplText) then
    OnGetDisplText(Self, c, r, Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SyncActiveRec;
var
  i, j, AR, RC: Integer;
begin
  if PageMode then
  begin
    j := Row - TopRow;
    RC := FDataLink.RecordCount;
    AR := FDataLink.ActiveRecord;
    if (j <> AR) then
    begin
      FDataLink.FLockEffects := True;
      if (j > AR) then
      begin
        i := FDataLink.DataSet.MoveBy(-AR - 1);
        FDataLink.DataSet.MoveBy(-i);
      end
      else //if (j < AR) then
      begin
        i := FDataLink.DataSet.MoveBy(RC - AR);
        FDataLink.DataSet.MoveBy(-i);
      end;
      FDataLink.FLockEffects := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.WMVScroll(var WMScroll: TWMScroll);
var
  SI: TScrollInfo;
  a, OldTopRow: integer;
  RC, AR, j: Integer;
  OldLeftCol, ds: integer;
  FMoveSelection: TGridRect;
begin
  if (csDesigning in ComponentState) then
    Exit;

  if not Assigned(DataSource) then
    Exit;

  if (Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active and DataSource.DataSet.ControlsDisabled and PageMode) then
    Exit;

  FMoveSelection := Selection;
  FDoNotCountRow := true;

  OldTopRow := TopRow;
  OldLeftCol := LeftCol;

  if EditMode then
  begin
    HideInplaceEdit;
    SetFocus;
  end;


//if WMScroll.ScrollCode <> SB_THUMBPOSITION then

  if ((DataSetType = dtSequenced) and (not (WMScroll.ScrollCode in [SB_PAGEDOWN, SB_PAGEUP, SB_THUMBPOSITION, SB_THUMBTRACK]) or (not FloatingFooter.Visible))) or not PageMode then
    inherited;

  if not PageMode then
    Exit;

  //if Navigation.KeepHorizScroll then
  //  BeginUpdate;

  ds := 0;
  try
    if PageMode then
    begin
      RC := FDatalink.RecordCount;
      AR := FDataLink.ActiveRecord;

      with WMScroll, FDataLink.DataSet do
        case ScrollCode of
          SB_LINEUP:
            begin
              if (AR > 0) and (AR = FDataLink.BufferCount - 1) then
                AR := AR - 1;
              ds := FDataLink.DataSet.MoveBy(-AR - 1);
            end;
          SB_LINEDOWN:
            begin
              FDoNotUpdateMe := true;
              a := FDataLink.DataSet.MoveBy(RC - AR);
              FDoNotUpdateMe := false;
              if a = 0 then
              begin
                FDataLink.DataSet.MoveBy(1);
                if DataSetType = dtNonSequenced then
                  UpdateScrollBar(1);
              end
              else if a > 1 then
                ds := FDataLink.DataSet.MoveBy(-1)
              else
                UpdateScrollBar(1);
        //FDataLink.DataSet.MoveBy(-1);
            end;
          SB_PAGEUP:
            begin
              if (DataSetType = dtSequenced) then
              begin
                if FloatingFooter.Visible and (FloatingFooter.FooterStyle = fsFixedLastRow) then
                  ds := FDataLink.DataSet.MoveBy(Min(2 - VisibleRowCount, (-AR - 1) + (TopRow - OldTopRow)))
                else
                  ds := FDataLink.DataSet.MoveBy((-AR - 1) + (TopRow - OldTopRow));
              end
              else
                ds := FDataLink.DataSet.MoveBy(-VisibleRowCount);
              //FDataLink.DataSet.MoveBy(-(VisibleRowCount div 2));
            end;
          SB_PAGEDOWN:
            begin
              if (DataSetType = dtSequenced) then
              begin
                if FloatingFooter.Visible and (FloatingFooter.FooterStyle = fsFixedLastRow) then
                begin
                  //ds := FDataLink.DataSet.MoveBy((RC - AR) + (TopRow - OldTopRow));
                  ds := FDataLink.DataSet.MoveBy(Max(VisibleRowCount - 2, (RC - AR) + (TopRow - OldTopRow)));
                end
                else
                begin
                  ds := FDataLink.DataSet.MoveBy((RC - AR) + (TopRow - OldTopRow));

                  // moving the cache by one so that partial visible row can show data
                  FDataLink.FLockEffects := True;
                  j := FDataLink.DataSet.MoveBy(1);
                  FDataLink.DataSet.MoveBy(-j);
                  FDataLink.FLockEffects := False;
                end;
              end
              else
                ds := FDataLink.DataSet.MoveBy(VisibleRowCount);
              //FDataLink.DataSet.MoveBy(VisibleRowCount div 2);
            end;
          SB_THUMBPOSITION:
            begin
              if IsSequenced and (DataSetType = dtSequenced) then
              begin
            //FDoNotUpdateMe:= true;
                a := TopRow - OldTopRow;
                if FloatingFooter.Visible then
                begin
                  SI.cbSize := sizeof(SI);
                  SI.fMask := SIF_ALL;
                  GetScrollInfo(Self.Handle, SB_VERT, SI);
                  if SI.nTrackPos <= 1 then First
                  else
                    if SI.nTrackPos >= RecordCount then
                      FDataLink.DataSet.MoveBy(RecordCount)
                  else
                    Row := SI.nTrackPos;

                  SyncActiveRec;
                end
                else
                begin
                if (a > 0) then
                begin
                  ds := FDataLink.DataSet.MoveBy((RC - AR - 1) + a);
              //FDataLink.DataSet.MoveBy(-j);
                end
                else if (a < 0) then
                begin
                  ds := FDataLink.DataSet.MoveBy(-AR + a);
                end;
                end;

            //FDoNotUpdateMe:= false;
          {
            SI.cbSize := sizeof(SI);
            SI.fMask := SIF_ALL;
            GetScrollInfo(Self.Handle, SB_VERT, SI);
            if SI.nTrackPos <= 1 then First
            else if SI.nTrackPos >= RecordCount then Last
            else RecNo := SI.nTrackPos;
            }
              end
              else
                case Pos of
                  0: First;
                  1: FDataLink.MoveBy(-VisibleRowCount);
                  2: Exit;
                  3: FDataLink.MoveBy(VisibleRowCount);
                  4: Last;
                end;
            end;
          SB_THUMBTRACK:
            begin
              if (DataSetType = dtSequenced) and IsSequenced then
              begin
                a := TopRow - OldTopRow;
                if FloatingFooter.Visible then
                begin
                end
                else
                begin
                  if (a > 0) then
                  begin
                    FDataLink.DataSet.MoveBy((RC - AR - 1) + a);
                  end
                  else if (a < 0) then
                  begin
                    FDataLink.DataSet.MoveBy(-AR + a);
                  end;
                end;
              end
              else
                case Pos of
                  0: First;
                  1: FDataLink.MoveBy(-VisibleRowCount);
                  2: Exit;
                  3: FDataLink.MoveBy(VisibleRowCount);
                  4: Last;
                end;

            end;
          SB_BOTTOM: Last;
          SB_TOP: First;
        end;
    end;
  finally
    if Navigation.KeepHorizScroll then
      LeftCol := OldLeftCol;
    //EndUpdate;
    
    if (DataSetType = dtNonSequenced) and CheckDataSet and (ds <> 0) then
      Invalidate;
  end;

  if not EqualRect(TRect(FMoveSelection), TRect(Selection)) then
    SelectionChanged(Selection.Left, Selection.Top, Selection.Right, Selection.Bottom);


{  if (WMScroll.ScrollCode <> SB_ENDSCROLL) then
begin
FDataLink.BufferCount:= self.VisibleRowCount +1+ GetScrollPos(Handle,SB_VERT);
end;   }
  FDoNotCountRow := false;

end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetRecordCount: integer;

  function RowsInDataset: Integer;
  var
    cb: TBookMark;
    iseof, isbof: Boolean;
  begin
    Result := -1;
    if not CheckDataSet then
      Exit;

    FDataLink.DataSet.DisableControls;

    iseof := FDataLink.DataSet.Eof;
    isbof := FDataLink.DataSet.Bof;

    if isbof and iseof then
    begin
      Result := 0;
  //FMaxRows := 0;
    end
    else
      with FDataLink.DataSet do
      begin
        cb := GetBookMark;
        First;
        if (csDesigning in ComponentState) then
          Result := MoveBy(100)
        else
          Result := MoveBy($7FFFFFFF) + 1;

        GotoBookMark(cb);
        FreeBookMark(cb);

    //FMaxRows := Result;
      end;

    if iseof then
      FDataLink.DataSet.Next;

    if isbof then
      FDataLink.DataSet.Prior;

    if FDataLink.DataSet.State = dsInsert then
      Result := Result + 1;

    FEmptyDataSet := Result = 0;
    FDataLink.DataSet.EnableControls;
  end;

//var
//  OldRecNo: integer;
begin
  Result := 0;
  if Assigned(FDataLink) then
    if Assigned(FDataLink.DataSet) then
      if FDataLink.DataSet.Active then
      begin
        FDoNotUpdateMe := true;

        if Assigned(FOnGetRecordCount) then
        begin
          FDataLink.DataSet.DisableControls;
          FOnGetRecordCount(self, Result);
          FEmptyDataSet := Result = 0;
          if (FloatingFooter.Visible) and PageMode and (FloatingFooter.FooterStyle = fsFixedLastRow) then
          begin
            if (Result = 0) then
              Inc(Result, 2)
            else
              Inc(Result);
          end;
          FDataLink.DataSet.EnableControls;
        end
        else
        begin
          Result := RowsInDataSet;
          if (FloatingFooter.Visible) and PageMode and (FloatingFooter.FooterStyle = fsFixedLastRow) then
          begin
            if (Result = 0) then
              Inc(Result, 2)
            else
              Inc(Result);
          end;
        end;
   { OldRecNo:= FDataLink.DataSet.RecNo;
    FDataLink.dataset.First;
    Result := FDataLink.dataset.MoveBy($FFFF);
    FDataLink.DataSet.RecNo:= OldRecNo; }
        FDoNotUpdateMe := false;
      end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.Scroll(Distance: Integer);
begin
  if not HandleAllocated then
    Exit;

  UpdateScrollBar(Distance);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.UpdateScrollBar(Distance: integer);
var
  SIOld, SINew: TScrollInfo;
  gdr1, gdr2, gdr3, R: TRect;
  i, j, nr: integer;
  RC, OldRow, OldTopRow: Integer;
begin

  if FDatalink.Active and HandleAllocated and not FDoNotUpdateMe and PageMode then
    with FDatalink.DataSet do
    begin
      OldRow := Row;
      OldTopRow := TopRow;

      if (DataSetType = dtNonSequenced) then
      begin
        FInternalCall := true;

       if FDataLink.BufferCount <> GetBufferCount then
         FDataLink.BufferCount := GetBufferCount;

       (* if TopRow > FixedRows+1 then
          TopRow := FixedRows+1;
          Change for scrolling issue when FloatingFooter.Visible *)

        if TopRow > FixedRows + 1 then
        begin
          if FloatingFooter.Visible or SearchFooter.Visible then
          begin
            //if (TopRow > FixedRows + 2) then
            //  TopRow := FixedRows + 1
          end
          else
          begin
            TopRow := FixedRows + 1;
          end;
        end;

        if FloatingFooter.Visible then
        begin
          R := CellRect(Col,Row);
          if (R.Bottom > Height - FloatingFooter.Height) then
            TopRow := TopRow + 1;
        end;

        if (Row <> (FdataLink.ActiveRecord + FixedRows)) then
        begin
          if (FDatalink.DataSet.State <> dsInsert) then
          begin
            if (Row > (FdataLink.ActiveRecord + FixedRows)) then
            begin
              FBlockCallBack := True;
              Row := min(FdataLink.ActiveRecord + FixedRows, RowCount-1);
              FBlockCallBack := False;
            end
            else
            begin
              if SearchFooter.Visible or FloatingFooter.Visible then
              begin
                Row := min(FdataLink.ActiveRecord + FixedRows, RowCount - 1);
              end
              else
              begin
                Row := min(FdataLink.ActiveRecord + FixedRows, RowCount - 1);
              end;
            end;
          end
          else
          begin
            if TopRow < FixedRows + 1 then
            begin
              FInternalSelection:= true;
              Row := min(FdataLink.ActiveRecord + {1}FixedRows, RowCount - 1);
              FInternalSelection:= false;
            end;
          end;
        end;

        if (VisibleRowCount + FixedRows <> RowCount) then
        begin
          SIOld.cbSize := SizeOf(SIOld);
          SIOld.nMax := -1;
          SIOld.fMask := SIF_ALL;

          GetScrollInfo(Self.Handle, SB_VERT, SIOld);
          SINew := SIOld;
          SINew.nMin := 0;
          SINew.nPage := 0;
          SINew.nMax := 4;
          if FDataLink.BOF then SINew.nPos := 0
          else if FDataLink.EOF then SINew.nPos := 4
          else SINew.nPos := 2;
          if ((SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
            (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos))
            and (SIOld.nMax > 0) then
            SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
        end;

        FInternalCall := false;
      end
      else
      begin
        RC := RecNo + (FixedRows - 1);

        if RC < 0 then
          Exit;
        //RC := FDataLink.ActiveRecord + FixedRows;

        FInternalCall := true;

        if FloatingFooter.Visible and (FloatingFooter.FooterStyle = fsFixedLastRow) then
          nr := Min(RC, RowCount - 2)
        else
          nr := Min(RC, RowCount - 1);

        Row := nr;

        i := Row - TopRow;

        if (i > FDataLink.ActiveRecord) then // when selected row position is greator than Active Record
        begin
          j := VisibleRowCount - FDataLink.ActiveRecord - 1; //-j
          if (Row + j) < RowCount then
          begin
            Row := Row + j;
            Row := Row - j;
          end;
        end
        else if i < FDataLink.ActiveRecord then // when selected row position is less than Active Record
        begin
          if FDataLink.ActiveRecord >= VisibleRowCount then
          begin

            if (Row >= VisibleRowCount) and (TopRow <> (Row - VisibleRowCount + 1)) then
            begin
              if (FDataLink.RecordCount - FDataLink.ActiveRecord - 1) < (VisibleRowCount - i - 1) then  // FF: issue with AutoSizeRow, scroll then click cell
              begin
                if SearchFooter.Visible then
                  TopRow := Row - VisibleRowCount + 3
                else
                  TopRow := Row - VisibleRowCount + 1;
              end;
            end;
          end
          else
          begin
            if FUpdateCancel and (Row - FDataLink.ActiveRecord >= 0) {(1 < 0)} then
            begin
              j := FDataLink.ActiveRecord;
              Row := Row - j;
              Row := min(Row + j, RowCount-1);
            end;
          end;
        end;

        FInternalCall := false;
      end;

      FKeyDownAppend := false;

      if (Distance <> 0) then
      begin
      gdr1 := CellRect(FixedCols, TopRow);
      gdr2 := CellRect(VisibleColCount + FixedCols, TopRow + VisibleRowCount);

      UnionRect(gdr3, gdr1, gdr2);
      InvalidateRect(Handle, @gdr3, False);
      end;

      if ShowDBIndicator and ((OldTopRow <> TopRow) or (OldRow <> Row)) then
      begin
        InvalidateCell(0,OldRow);
        InvalidateCell(0,Row);
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.Loaded;
begin
  inherited;

  if ScrollSynch and PageMode then
    ScrollSynch := False;
  Columns.SetOrganization;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.TopLeftChanged;
begin
  inherited;

  if DataSetType = dtNonSequenced then
  begin
    if not FBlockCallBack then
      UpdateScrollBar(1);
  end;
end;

//------------------------------------------------------------------------------
procedure TDBAdvGrid.ColumnMoved(FromIndex, ToIndex: Integer);
var
  CN: TDBGridColumnItem;
  tr: Integer;
  RFI,RTI: Integer;
begin
  FColumnCollection.FNoRecursiveUpdate := True;

  SynchColumns;

  tr := TopRow;

  RFI := RealColIndex(FromIndex);
  RTI := RealColIndex(ToIndex);

  BeginUpdate;

  TDBGridColumnItem(FColumnCollection.Add).Assign(TDBGridColumnItem(FColumnCollection.Items[RFI]));
  TDBGridColumnItem(FColumnCollection.Items[RFI]).Free;
  CN := TDBGridColumnItem(FColumnCollection.Insert(RTI));
  CN.Assign(FColumnCollection.Items[FColumnCollection.Count - 1]);
  FColumnCollection.Items[FColumnCollection.Count - 1].Free;
  TopRow := tr;

  EndUpdate;

  FColumnCollection.FNoRecursiveUpdate := False;

  inherited;
end;

procedure TDBAdvGrid.DoKeyDown(Key: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.WMKeyDown(var Msg: TWMKeydown);
var
  OldRowCount, i, OldCol, j: integer;
  IsCtrl, IsShift, OldValue, OldV2: Boolean;
  Key:word;
  SS: TShiftState;
  Form: TCustomForm;
  R: TRect;
  CanInsert, CanDelete: boolean;
  lc: integer;
  CallBU: Boolean;
  FMoveSelection: TGridRect;
begin
  if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active and DataSource.DataSet.ControlsDisabled and PageMode
     and not (Msg.CharCode in [VK_LEFT, VK_RIGHT]) then
    Exit;

  lc := LeftCol;
  if not PageMode then
  begin
    inherited;
    Exit;
  end;

  FMoveSelection := Selection;

  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;
  IsShift := GetKeyState(VK_SHIFT) and $8000 = $8000;
  CallBU := LeftCol > FixedCols;

  FDoNotCountRow := true;

  Key := Msg.CharCode;
  SS := KeyDataToShiftState(Msg.KeyData);

  OldCol:= Col;

  case Msg.CharCode of
    VK_UP:
      begin
        if CallBU then
          BeginUpdate;
        try
          FNewAppendRecord := false;

          if CheckDataSet then
            if not (FDataLink.DataSet.State = dsInsert) then
              FDataLink.DataSet.MoveBy(-1)
            else
              FDataLink.DataSet.MoveBy(0);

          Select;
          if Navigation.KeepHorizScroll then
            LeftCol := lc;
        finally
          if CallBU then
            EndUpdate;
        end;
      end;

    VK_DOWN:
      begin
        OldV2 := FInternalMove;

        if not CheckDataSet then
        begin
          inherited;
          Exit;
        end;

        if CallBU then
          BeginUpdate;
        try

          if FKeyDownAppend and (FDataLink.DataSet.State in [dsEdit, dsInsert]) and FRecordChanged then
          begin
            FNewAppendRecord := false;
            i := FDataLink.DataSet.MoveBy(-1);
            FDataLink.DataSet.MoveBy(-i);
          end;

          if not FKeyDownAppend then
          begin
            if (DataSetType = dtNonSequenced) and (SearchFooter.Visible or FloatingFooter.Visible) then
              FInternalMove := True;

            i := FDataLink.DataSet.MoveBy(1);

            if (DataSetType = dtSequenced) and (FDataLink.ActiveRecord = FDataLink.BufferCount - 1) then
            begin  // moving the cache by one so that partial visible row can show data
              FDataLink.FLockEffects := True;
              j := FDataLink.DataSet.MoveBy(1);
              FDataLink.DataSet.MoveBy(-j);
              FDataLink.FLockEffects := False;
            end;

            if (i = 0) and Navigation.AppendOnArrowDown then
            begin
              if not IsCtrl and not IsShift and (GetKeystate(VK_MENU) and $8000 = 0) then
              begin
              //FInternalInsert := true;
                FDataLink.DataSet.Append;
                FNewRecord := true;
                FKeyDownAppend := true;

                if DataSetType = dtSequenced then
                begin
                  OldValue := FInternalCall;
                  FInternalCall:= true;
                  FInternalSelection:= true;
                  Row := min(Row + 1, RowCount-1);      // FF: Index out of bound
                  FInternalSelection:= false;
                  FInternalCall:= OldValue;
                end;

                //FInternalInsert := false;
                if Assigned(OnAutoAddRow) then
                  OnAutoAddRow(Self, RowCount - 1 - FixedFooters);
              end;
            end
            else
            begin
              if not Navigation.AppendOnArrowDown and (i = 0) and FDataLink.DataSet.Eof and not FDataLink.DataSet.Bof
                and (DataSetType <> dtNonSequenced) then
              begin
                i := FDataLink.DataSet.MoveBy(-1);
                FDataLink.DataSet.MoveBy(abs(i));
              end;
            end;
          end;

          Select;

          if Navigation.KeepHorizScroll then
            LeftCol := lc;
        finally
          if CallBU then
            EndUpdate;
          FInternalMove := OldV2;
        end;

      end;
    VK_NEXT:
      begin
        if not ((FDataLink.DataSet.Eof) and (Row = FDatalink.BufferCount)) then
        begin
          BeginUpdate;
          try
            FDataLink.DataSet.MoveBy(FDataLink.BufferCount);
            Select;
            if Navigation.KeepHorizScroll then
              LeftCol := lc;
          finally
            EndUpdate;
          end;
        end;
      end;
    VK_PRIOR:
      begin
        if not ((FDataLink.DataSet.Bof) and (Row = FixedRows)) then
        begin
          BeginUpdate;
          try
            FDataLink.DataSet.MoveBy(-FDataLink.BufferCount);
            Select;
            if Navigation.KeepHorizScroll then
              LeftCol := lc;
          finally
            EndUpdate;
          end;
        end;
      end;
    VK_HOME:
      begin
        if Navigation.HomeEndKey = heFirstLastRow then
        begin
          BeginUpdate;
          try
            FDataLink.DataSet.First;

            DoKeyDown(Key, SS);

            Select;
            if Navigation.KeepHorizScroll then
              LeftCol := lc;
          finally
            EndUpdate;
          end;
        end
        else
          inherited;
      end;
    VK_END:
      begin
        if Navigation.HomeEndKey = heFirstLastRow then
        begin
          BeginUpdate;
          try
            FDataLink.DataSet.Last;
            if FloatingFooter.Visible then
            begin
              R := CellRect(Col,Row);
              if R.Bottom > Height - FloatingFooter.Height then
                TopRow := TopRow + 1;
            end;

            DoKeyDown(Key, SS);

            Select;
          
            if Navigation.KeepHorizScroll then
              LeftCol := lc;
          finally
            EndUpdate;
          end;  
        end
        else
          inherited;
          //.MoveBy($7FFFFFFF);
  //FDataLink.DataSet.MoveBy(-FDataLink.BufferCount);
      end;
    VK_INSERT:
      begin
        if Navigation.AllowInsertRow and not IsCtrl and not IsShift and (GetKeystate(VK_MENU) and $8000 = 0) then
        begin
          CanInsert := true;
          if Assigned(OnCanInsertRow) then
            OnCanInsertRow(Self, Row, CanInsert);

          if not CheckDataSet or not FDataLink.DataSet.CanModify then
            CanInsert := false;

          if CanInsert then
          begin
            OldRowCount := self.RowCount;
            if not FEmptyDataSet then
              inherited;
            if (OldRowCount < self.RowCount) or FEmptyDataSet then
            begin
              FInternalInsert := true;
              FDataLink.DataSet.Insert;
              FNewRecord := true;
              FInternalInsert := false;
            end;
          end;
        end;
      end;
    VK_DELETE:
      begin
        if (Navigation.AllowDeleteRow) and (GetKeystate(VK_MENU) and $8000 = 0) then
        begin
          CanDelete := true;
          if Assigned(OnCanDeleteRow) then
            OnCanDeleteRow(Self, Row, CanDelete);

          if not CheckDataSet or not FDataLink.DataSet.CanModify then
            CanDelete := false;

          if CanDelete then
          begin
            if CheckDataSet and ((RowCount - FixedFooters - FixedRows >= 1) or
              ((RowCount - FixedFooters - FixedRows = 1) and FixedRowAlways)) then
            begin
        //OldRowCount:= self.RowCount;
        //inherited;
        //if OldRowCount > self.RowCount then
              begin
                if not FDataLink.DataSet.IsEmpty then
                begin
                  //Allow := True;
                  //if Assigned(OnCanDeleteRow) then
                  //  OnCanDeleteRow(self, Row, Allow);
                  //if not Allow then Exit;

                  FDoNotCountRow := false;
                  FDataLink.DataSet.Delete;
                  InvalidateRow(Row);
                end;
              end;
            end;
            Msg.CharCode := 0;
            Exit;
          end;
        end;
      end;
    VK_RETURN:
      begin
        if CheckDataSet and (FDataLink.DataSet.State = dsInsert) and ((not FRecordChanged) and (FEditPostMode = epCell)) then
        begin
          FCancelEditReturn := true;
          FNewRecord := false;
          FDataLink.DataSet.Cancel;
          FCancelEditReturn := false;
          FNewAppendRecord := false;
          FKeyDownAppend := false;

          DoKeyDown(Key, SS);
        end
        else
          inherited;
      end;
      
    VK_ESCAPE:
    begin
      if Assigned(DataSource) then
        FDatalink.Reset;

      Form := GetParentForm(Self);
      if (Form <> nil) and Form.KeyPreview then
        TWinControl(Form).WindowProc(TMessage(Msg));

    end;
  else
  begin
    if not(Key in [VK_ESCAPE, VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_INSERT]) and
      (FOldState = dsBrowse) and (FDataLink.DataSet.State = dsBrowse) then 
      FDoNotCountRow := false;  
    inherited;
  end;

    if ActiveCellShow then
    begin
      InvalidateCell(OldCol, 0);
      InvalidateCell(Col, 0);
    end;
  end;

  if (Key in [VK_ESCAPE, VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_INSERT, VK_DELETE]) then
    DoKeyDown(Key, SS);

  //if not EqualRect(TRect(FMoveSelection), TRect(Selection)) then
  //  SelectionChanged(Selection.Left, Selection.Top, Selection.Right, Selection.Bottom);


  FDoNotCountRow := false;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.TabToNextRowAtEnd;
var
  OldV2, OldValue: Boolean;
  i: Integer;
begin
  if Assigned(DataLink.DataSet) and not (DataLink.DataSet.State in [dsEdit, dsInsert]) and (DataSetType = dtSequenced) and not FKeyDownAppend then
  begin
    inherited;
    Exit;
  end;

  OldV2 := FInternalMove;
  if not FKeyDownAppend then
  begin
    if (DataSetType = dtNonSequenced) and (SearchFooter.Visible or FloatingFooter.Visible) then
      FInternalMove := True;
    i := FDataLink.DataSet.MoveBy(1);
    if (i = 0) {and (goTabs in Options)} then
    begin
      if {not IsCtrl and not IsShift and} (GetKeystate(VK_MENU) and $8000 = 0) then
      begin
        FDataLink.DataSet.append;
        FNewRecord := true;
        FKeyDownAppend := true;

        if DataSetType = dtSequenced then
        begin
          OldValue := FInternalCall;
          FInternalCall := true;
          FInternalSelection:= true;
          Row := min(Row + 1, RowCount-1);
          FInternalSelection := false;
          FInternalCall := OldValue;
        end;
      end;
    end
    else
    begin
      if not Navigation.AppendOnArrowDown and (i = 0) and FDataLink.DataSet.Eof and not FDataLink.DataSet.Bof then
      begin
        i := FDataLink.DataSet.MoveBy(-1);
        FDataLink.DataSet.MoveBy(abs(i));
      end;
    end;
  end;
  Select;
  FInternalMove := OldV2;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.SelectCell(ACol, ARow: LongInt): Boolean;
var
  a, i: integer;
  OldRow, NewRow, OldAR, OldCol: LongInt;
  NewSelection: Boolean;
  DataSetOK: boolean;

begin
  OldAR := 0;
  if FEditPostMode = epCell then
    NewSelection := (ARow <> Row) or (ACol <> Col)
  else
    NewSelection := false; //(ARow <> Row);

  if (FEditPostMode = epRow) and ((ARow <> Row) or (ACol <> Col)) and PageMode then
  begin
    FDataLink.UpdateData;
  end;

  if FDoNotCallSelect{FDoNotCountRow} then  // This avoids first time Editing of a non editable cell
  begin
    Result := true;
    Exit;
  end;

  DataSetOK := CheckDataSet;

  // FF: MouseClick Issue after arrow Key Down Append
  if DataSetOK and (FDataLink.DataSet.State = dsInsert) and NewSelection and not FInternalSelection
     and FKeyDownAppend and (DataSetType = dtSequenced) and not ((FRecordChanged or (FEditText <> '')) and (FEditPostMode = epCell)) then
    ARow := ARow + 1;

  FAllowRowChange := True;
  OldRow := Row;
  OldCol := Col;
  Result := inherited SelectCell(ACol, ARow);
  NewRow := ARow;

  if (FEditPostMode = epRow) and ((ARow <> OldRow) or (ACol <> OldCol)) and PageMode then
  begin
    FEditText := Cells[ACol, ARow];
  end;

  if not PageMode then
    Exit;

  if DataSetOK and (FDataLink.DataSet.State = dsInsert) and NewSelection and not FInternalSelection then
  begin
    if FRecordChanged and (FEditPostMode = epCell) then
    begin
      FNewRecord := false;
      FDataLink.DataSet.Post;
      FNewAppendRecord := false;
      FKeyDownAppend := false;
    end
    else
    begin
      if not FTabInsertion then
      begin
        FNewRecord := false;
        FUpdateCancel := true;
        if FKeyDownAppend and Navigation.AppendOnArrowDown and (EditPostMode = epCell) and (DataSetType = dtSequenced) then
          Result := False;
        FDataLink.DataSet.Cancel;
        FUpdateCancel := false;
        FNewAppendRecord := false;
        FKeyDownAppend := false;
      end;
    end;
  end;
//FDataLink.DataSet.RecNo:= NewRow;
  if not FAllowRowChange then
    Exit;
                            // Change for Exporting While dtNonSequenced

  if not FInternalCall and ((DataSetType = dtSequenced) or ((DataSetType = dtNonSequenced)and not FExporting)) then
  begin
{    if Assigned(FOnRowChanged) then
      FOnRowChanged(self, OldRow, NewRow);   }

//FDataLink.ActiveRecord := SIOld.nPos; //NewRow;
//FDataLink.DataSet.RecNo:= FDataLink.DataSet.RecNo;
  FTabInsertion := False;

  if DataSetOK then
    OldAR := FDataLink.ActiveRecord;

    a := NewRow - OldRow;
    FDoNotCountRow := true;
    FDoNotCallSelect := true;
    try
      if DataSetOK and (a <> 0) then
      begin

        if FAppendOperation and (a = -1) and not FDataLink.DataSet.Modified and (DataSetType = dtSequenced) then
          i := FDataLink.DataSet.MoveBy(0)
        else if not FInternalMove then
        begin
          i := FDataLink.DataSet.MoveBy(a);
          if FWasEditing then
          begin
            if (i > 0) and (OldAR = FDataLink.ActiveRecord) and (FDataLink.ActiveRecord <> FDataLink.BufferCount - 1) then // FF: CDS edit iss with idx
              FDataLink.DataSet.MoveBy(a);

            if FColWith0Width then
            begin
              FWasEditing := False;
              Invalidate;
            end;
          end;
        end
        else
        begin
          i := -1;
        end;

        if FMouseWheelScrolling and (i <> 0) then
          FMouseWheelScrolled := True;

       { if (i <> a) and (a = 1) and (i = 0)
          and Navigation.AdvanceOnEnter and Navigation.AdvanceInsert then }
        if (i <> a) and (a = 1) and (i = 0) then
        begin
          if (Navigation.AdvanceOnEnter or (goTabs in Options)) and Navigation.AdvanceInsert then
          begin
            FInternalInsert := true;
            FDataLink.DataSet.Append; // .Insert;
            FNewRecord := true;
            FInternalInsert := false;
            if Navigation.AdvanceOnEnter then
              FNewAppendRecord := true;
            if (goTabs in Options) and (Col <> FixedCols) then
              FTabInsertion := True;
          end
          else
          begin

          end;
        end;

        // change for insert case that is Insert then edit then go row below cause empty grid.
        if {(i <> a) and (i = 0) and} (DataSetType = dtSequenced) and (FEditPostMode = epRow)
           and (Row <> OldRow) and (Row <> NewRow) and (FDataLink.DataSet.State <> dsInsert) then
          Result := False;
      end;
    finally
      FDoNotCountRow := false;
      FDoNotCallSelect := false;
    end;
  end;

  //if CheckDataSet and (FDataLink.DataSet.State = dsEdit) and (FEditPostMode = epRow) and (ACol <> Col) then
    //Cells[RealColIndex(ACol), Row] := Cells[RealColIndex(ACol), Row];   // To restore OldValue when escape
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.RowUpdateEvent(Sender: TObject; OldRow, NewRow: Integer);
{var
  a: integer}
begin {
if CheckDataSet and (FDataLink.DataSet.State = dsInsert) then
begin
FDataLink.DataSet.Cancel;
end; }
//FDataLink.DataSet.RecNo:= NewRow;
  if not FInternalCall then
  begin
{    if Assigned(FOnRowChanged) then
      FOnRowChanged(self, OldRow, NewRow);  }

//FDataLink.ActiveRecord := SIOld.nPos; //NewRow;
//FDataLink.DataSet.RecNo:= FDataLink.DataSet.RecNo;
{  a:= NewRow - OldRow;
if Assigned(FDataLink.DataSet) and (FDataLink.DataSet.Active) then
  FDataLink.DataSet.MoveBy(a); }



  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetFieldCount: Integer;
begin
  Result := FDatalink.FieldCount;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetFields(FieldIndex: Integer): TField;
begin
  Result := FDatalink.Fields[FieldIndex];
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SynchColumns;
var
  i{, j}: Integer;
begin
  while FColumnCollection.Count < ColCount + NumHiddenColumns do
  begin
    FColumnCollection.Add;
  end;

  while FColumnCollection.Count > ColCount + NumHiddenColumns do
  begin

    FColumnCollection.Items[FColumnCollection.Count - 1].Free;
  end;

{  if csDesigning in ComponentState then
for i := 1 to ColCount do
  Cells[i - 1,FixedRows] := 'Column ' + IntToStr(i - 1);
}
  if FixedRows > 0 then
    for i := 1 to ColCount do
    begin
      //j := RealColIndex(i - 1);
      //FColumnCollection.Items[j].FColumnHeader := Cells[j, 0];
    end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.CreateColumns: TDBGridColumnCollection;
begin
  Result := TDBGridColumnCollection.Create(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetColumnCollection(
  const Value: TDBGridColumnCollection);
begin
  FColumnCollection.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.ColWidthsChanged;
var
  i, j: Integer;
begin
  inherited;

  for i := 1 to ColCount do
  begin
    j := RealColIndex(i - 1);
    if j < FColumnCollection.Count then
      TDBGridColumnItem(FColumnCollection.Items[j]).FWidth := ColWidths[i - 1];
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetBuffercount: Integer;
begin
  Result := VisibleRowcount + 1;
end;

procedure TDBAdvGrid.GetCellAlign(ACol, ARow: Integer;
  var HAlign: TAlignment; var VAlign: TVAlignment);
var
  c: integer;
begin
  c := ACol;
  if (GroupColumn >= 0) and (ACol >= GroupColumn) then
    inc(c);

  HAlign := taRightJustify;

  if FColumnCollection.Count > c then
  begin
    if ARow < FixedRows then
      HAlign := TDBGridColumnItem(FColumnCollection.Items[c]).HeaderAlignment
    else
      HAlign := TDBGridColumnItem(FColumnCollection.Items[c]).Alignment;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetCellBorder(ACol, ARow: Integer; APen: TPen;
  var borders: TCellBorders);
begin
  if FColumnCollection.Count > Acol then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      Borders := TDBGridColumnItem(FColumnCollection.Items[ACol]).Borders;
      APen.Assign(TDBGridColumnItem(FColumnCollection.Items[ACol]).BorderPen);
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetCellPrintBorder(ACol, ARow: Integer;
  APen: TPen; var borders: TCellBorders);
begin
  if FColumnCollection.Count > Acol then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      Borders := TDBGridColumnItem(FColumnCollection.Items[ACol]).PrintBorders;
      APen.Assign(TDBGridColumnItem(FColumnCollection.Items[ACol]).PrintBorderPen);
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetCellPrintColor(ACol, ARow: Integer;
  AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
begin
  if not HasCellProperties(ACol,ARow) then
  begin
    if (FColumnCollection.Count > ACol) then
    begin
      if (ACol >= FixedCols) and (Arow >= FixedRows) and
        (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
        (ARow < RowCount - FixedFooters) then
      begin
        if TDBGridColumnItem(FColumnCollection.Items[ACol]).Fixed then
          ABrush.Color := FixedColor
        else
          if not (TDBGridColumnItem(FColumnCollection.Items[ACol]).ShowBands and Bands.Active and Bands.Print) then
            ABrush.Color := TDBGridColumnItem(FColumnCollection.Items[ACol]).PrintColor;

        AFont.Assign(TDBGridColumnItem(FColumnCollection.Items[ACol]).PrintFont);
      end
      else
      begin
        AFont.Assign(PrintSettings.FixedFont);
      end;
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------
{
procedure TDBAdvGrid.GetCellColor(ACol, ARow: Integer;
  AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
begin
  if FColumnCollection.Count > ACol then
  begin
    if (ACol >= FixedCols) and
      (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
      (ARow < RowCount - FixedFooters) then
    begin
      if TDBGridColumnItem(FColumnCollection.Items[ACol]).Fixed then
        ABrush.Color := FixedColor
      else
        if not (TDBGridColumnItem(FColumnCollection.Items[ACol]).ShowBands and Bands.Active) then
          ABrush.Color := TDBGridColumnItem(FColumnCollection.Items[ACol]).Color;

      if ARow < FixedRows then
        AFont.Assign(TDBGridColumnItem(FColumnCollection.Items[ACol]).HeaderFont)
      else
        AFont.Assign(TDBGridColumnItem(FColumnCollection.Items[ACol]).Font);
    end;
  end;
  inherited;
end;
}
procedure TDBAdvGrid.GetCellColor(ACol, ARow: Integer;
  AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
begin

  if FColumnCollection.Count > ACol then
  begin
    if (ACol >= FixedCols) and
      (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
      (ARow < RowCount - FixedFooters) then
    begin
      if TDBGridColumnItem(FColumnCollection.Items[ACol]).Fixed then
        ABrush.Color := FixedColor
      else
        if not (TDBGridColumnItem(FColumnCollection.Items[ACol]).ShowBands and Bands.Active) and (ARow >= FixedRows) then
          ABrush.Color := TDBGridColumnItem(FColumnCollection.Items[ACol]).Color;


      if ARow < FixedRows then
        AFont.Assign(TDBGridColumnItem(FColumnCollection.Items[ACol]).HeaderFont)
      else
        AFont.Assign(TDBGridColumnItem(FColumnCollection.Items[ACol]).Font);
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.HasColumnsProp: boolean;
begin
  Result := true;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.HasCombo(ACol, ARow: Integer; AEditor: TEditorType = edNone): Boolean;
begin
  Result := inherited HasCombo(Acol,ARow);
  if FColumnCollection.Count > ACol then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      Result := Result or (TDBGridColumnItem(FColumnCollection.Items[acol]).Editor in [edComboEdit, edComboList]);
    end;
  end;
  if Assigned(OnHasComboBox) then
    OnHasComboBox(Self, ACol, ARow, Result);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetStaticCellEditor(ACol, ARow: Integer; var AEditor: TEditorType);
begin
  AEditor := TDBGridColumnItem(FColumnCollection.Items[ACol]).Editor;
  inherited; 
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetEditorNone(ACol,ARow: Integer): boolean;
begin
  Result := false;

  if (ACol >= FixedCols) and (Arow >= FixedRows) then
  begin
    if FShowDefinedFields and (FColumnCollection.Items[ACol].FieldName = '') then
    begin
      if not FColumnCollection.Items[ACol].ReadOnly then
      begin
        Result := TDBGridColumnItem(FColumnCollection.Items[acol]).Editor = edNone;
      end;
    end;
  end;

end;
//------------------------------------------------------------------------------

procedure TDBAdvGrid.DoGetEditorProp(ACol, ARow: integer; EditLink: TEditLink);
var
  i, RCol: Integer;
  readOnlyCell: Boolean;
  lookupCell: Boolean;
  fld: TField;
  sl: TStringList;
  {$IFDEF DELPHI2006_LVL}
  slw: TWideStringList;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  {$IFNDEF DELPHI_UNICODE}
  isWide: boolean;
  {$ENDIF}
  {$ENDIF}
begin
  readOnlyCell := false;
  lookupCell := false;

  RCol := RealColIndex(ACol);

  if not Assigned(FDataLink) or not Assigned(FDataLink.DataSet) or
	   not FDataLink.DataSet.Active then
  	Exit;

  if (Columns.Count > RCol) and PageMode then
  begin
    if Assigned(FDataLink) then
    begin
      if not CheckDataSet or not FDataLink.DataSet.CanModify then
      begin
        Exit;
      end;
    end;

    if (RCol >= FixedCols) and (Arow >= FixedRows) then
    begin
      if FShowDefinedFields and (Columns[RCol].FieldName = '') then
      begin
        if not Columns[RCol].ReadOnly then
        begin
          EditLink := Columns[RCol].EditLink;
        end
        else
        begin
          readOnlyCell:= true;
        end;
      end
      else
      begin
        fld := nil;
        sl := TStringList.Create;
        {$IFDEF DELPHI2006_LVL}
        slw := TWideStringList.Create;
        {$ENDIF}
        try
        if Columns[ACol].FieldName <> '' then
        begin
          outputdebugstring(pchar(Columns[ACol].FieldName));
          fld := FDataLink.DataSet.Fieldbyname(Columns[ACol].FieldName);
          readOnlyCell := readOnlyCell or fld.ReadOnly;
          lookupCell := (fld.FieldKind = fkLookup) and (Columns[ACol].UseLookupEditor);
          {$IFDEF DELPHI2006_LVL}
          {$IFNDEF DELPHI_UNICODE}
          isWide := fld.DataType = ftWideString;
          {$ENDIF}
          {$ENDIF}

          if lookupCell then
          begin
            {$IFDEF DELPHI2006_LVL}
            {$IFNDEF DELPHI_UNICODE}
            if isWide then
              LoadWideLookupList(fld, slw)
            else
            {$ENDIF}
            {$ENDIF}
              LoadLookupList(fld, sl);
           end;
        end
        else if not FShowDefinedFields then
        begin
          i := GetDBFieldIndexAtColumn(RCol);
          if (i <> -1) then
            fld := FDataLink.DataSet.Fields[i]
          else
            fld := nil;

          if Assigned(fld) then
          begin
            readOnlyCell := readOnlyCell or fld.ReadOnly;
            lookupCell := (fld.FieldKind = fkLookup) and (Columns[RCol].UseLookupEditor);
            {$IFDEF DELPHI2006_LVL}
            {$IFNDEF DELPHI_UNICODE}
            isWide := fld.DataType = ftWideString;
            {$ENDIF}
            {$ENDIF}

            if lookupCell then
            begin
              {$IFDEF DELPHI2006_LVL}
              {$IFNDEF DELPHI_UNICODE}
              if isWide then
                LoadWideLookupList(fld, slw)
              else
              {$ENDIF}
              {$ENDIF}
                LoadLookupList(fld, sl);
            end;
          end;
        end;

        if Assigned(fld) then
        begin
          if fld.DataType in [ftString, ftWideString] then
            MaxEditLength := fld.Size
          else
            MaxEditLength := 0;
        end;

        readOnlyCell := readOnlyCell or Columns[RCol].ReadOnly;

        if not readOnlyCell then
        begin
          if CanModify then
          begin
            ComboBox.Items.Assign(Columns[RCol].ComboItems);

            if lookupCell then
            begin
              {$IFDEF DELPHI2006_LVL}
              {$IFNDEF DELPHI_UNICODE}
              if (fld.DataType = ftWideString) then
              begin
                UniCombo.Items.Assign(TWideStrings(slw));
              end
              else
              {$ENDIF}
              {$ENDIF}
              begin
                ComboBox.Items.Assign(sl);
              end;
            end;

            SpinEdit.MinValue := Columns[RCol].SpinMin;
            SpinEdit.MaxValue := Columns[RCol].SpinMax;
            SpinEdit.Increment := Columns[RCol].SpinStep;
            EditLink := Columns[RCol].EditLink;
          end;
        end;
        finally
          sl.Free;
          {$IFDEF DELPHI2006_LVL}
          slw.Free;
          {$ENDIF}
        end;
      end;
    end;
  end;

  if not readOnlyCell then
    inherited;
end;
//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetCellEditor(ACol, ARow: Integer; var AEditor: TEditorType);
var
  i: integer;
  readOnlyCell: boolean;
  lookupCell: boolean;
  fld: TField;
  PEditor: TEditorType;
  {$IFDEF DELPHI2006_LVL}
  {$IFNDEF DELPHI_UNICODE}
  isWide: boolean;
  {$ENDIF}
  {$ENDIF}
begin
  readOnlyCell := false;
  lookupCell := false;

  PEditor := edNone;

  {$IFDEF DELPHI2006_LVL}
  {$IFNDEF DELPHI_UNICODE}
  isWide := false;
  {$ENDIF}
  {$ENDIF}


  if not Assigned(FDataLink) or not Assigned(FDataLink.DataSet) or
   	not FDataLink.DataSet.Active then
  begin
    inherited;
  	Exit;
  end;

  if (FColumnCollection.Count > ACol) and PageMode then
  begin
    if Assigned(FDataLink) then
    begin
      if not CheckDataSet or not FDataLink.DataSet.CanModify then
      begin
        AEditor := edNone;
        Exit;
      end;
    end;

    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      if FShowDefinedFields and (FColumnCollection.Items[ACol].FieldName = '') then
      begin
        if not FColumnCollection.Items[ACol].ReadOnly then
        begin
          AEditor := TDBGridColumnItem(FColumnCollection.Items[acol]).Editor;
          EditLink := TDBGridColumnItem(FColumnCollection.Items[ACol]).EditLink;
        end
        else
        begin
          readOnlyCell:= true;
          AEditor := edNone;
        end;
      end
      else
      begin
        fld := nil;

        if FColumnCollection.Items[ACol].FieldName <> '' then
        begin
          fld := FDataLink.DataSet.FieldByName(FColumnCollection.Items[ACol].FieldName);
          readOnlyCell := readOnlyCell or fld.ReadOnly;
          lookupCell := (fld.FieldKind = fkLookup) and (FColumnCollection.Items[ACol].UseLookupEditor);
          {$IFDEF DELPHI2006_LVL}
          {$IFNDEF DELPHI_UNICODE}
          iswide := fld.DataType = ftWideString;
          {$ENDIF}
          {$ENDIF}
        end
        else if not FShowDefinedFields then
        begin
          i := GetDBFieldIndexAtColumn(ACol);
          if (i <> -1) then
            fld := FDataLink.DataSet.Fields[i]
          else
            fld := nil;

          if Assigned(fld) then
          begin
            readOnlyCell := readOnlyCell or fld.ReadOnly;
            lookupCell := (fld.FieldKind = fkLookup) and (FColumnCollection.Items[ACol].UseLookupEditor);
            {$IFDEF DELPHI2006_LVL}
            {$IFNDEF DELPHI_UNICODE}
            iswide := fld.DataType = ftWideString;
            {$ENDIF}
            {$ENDIF}
          end;
        end;

        if Assigned(fld) then
        begin
          case fld.DataType of
            ftString, ftFixedChar: PEditor := edNormal;
            ftSmallint, ftInteger, ftWord, ftLargeint: PEditor := edNumeric;
            ftFloat, ftCurrency: PEditor := edFloat;
            ftDate: PEditor := edDateEdit;
            ftTime: PEditor := edTimeEdit;
            ftAutoInc: readOnlyCell := true;
            ftMemo: PEditor := edNormal;
            ftWideString: PEditor := edNormal;
          end;
        end;

        readOnlyCell := readOnlyCell or FColumnCollection.Items[ACol].ReadOnly;

        if not readOnlyCell then
        begin
          //if CanModify then
          //begin
          //  ComboBox.Items.Assign(TDBGridColumnItem(FColumnCollection.Items[ACol]).ComboItems);

            if lookupCell then
            begin
              {$IFDEF DELPHI2006_LVL}
              {$IFNDEF DELPHI_UNICODE}
              if isWide then
                AEditor := edUniComboList
              else
              {$ENDIF}
              {$ENDIF}
                AEditor := edComboList;
            end
            else
            begin
              AEditor := TDBGridColumnItem(FColumnCollection.Items[acol]).Editor;
              if (AEditor = edNormal) and (PEditor <> edNone) then
                AEditor := PEditor;
            end;

            EditLink := TDBGridColumnItem(FColumnCollection.Items[ACol]).EditLink;
         // end;
        end
        else // if ReadOnly
          AEditor := edNone;
      end;
    end;
  end;

  if not readOnlyCell then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetCellFixed(ACol, ARow: Integer;
  var IsFixed: Boolean);
begin
  if FColumnCollection.Count > Acol then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      IsFixed := TDBGridColumnItem(FColumnCollection.Items[ACol]).Fixed;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetCellPassword(ACol, ARow: Integer;
  var IsPassword: Boolean);
begin
  if (FColumnCollection.Count > Acol) and (ARow >= FixedRows) then
  begin
    IsPassword := TDBGridColumnItem(FColumnCollection.Items[ACol]).Password;
  end;
  inherited;
end;

{
procedure TDBAdvGrid.GetCellReadOnly(ACol, ARow: Integer;
var IsReadOnly: Boolean);
var
BC: TPoint;
begin
IsReadOnly := False;

if not (csLoading in ComponentState) then
begin
if FColumnCollection.Count > ACol then
begin
  if (ACol >= FixedCols) and (ARow >= FixedRows) then
  begin
    IsReadOnly := not TDBGridColumnItem(FColumnCollection.Items[ACol]).ReadOnly and
                  not TDBGridColumnItem(FColumnCollection.Items[ACol]).Fixed;
  end;
end;
end;

BC := BaseCell(ACol,ARow);

if HasCellProperties(ACol,ARow) and IsReadOnly then
begin
IsReadOnly := not (inherited ReadOnly[ACol,ARow]);
end;

if Assigned(OnCanEditCell) then
OnCanEditCell(Self,BC.Y,BC.X,IsReadOnly);
end;
}

//------------------------------------------------------------------------------

function TDBAdvGrid.GetColCount: Integer;
begin
  Result := inherited ColCount;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetColCount(const Value: Integer);
begin
  inherited ColCount := Value;
  FColumnCollection.FNoRecursiveUpdate := True;
  SynchColumns;
  FColumnCollection.FNoRecursiveUpdate := False;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetColFormat(ACol: Integer;
  var ASortStyle: TSortStyle; var aPrefix, aSuffix: string);
begin
  if FColumnCollection.Count > ACol then
  begin
    ASortStyle := Columns[ACol].SortStyle;
    APrefix := Columns[ACol].SortPrefix;
    ASuffix := Columns[ACol].SortSuffix;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetEditCell(i, j: integer): string;
begin
  Result := Cells[i,j];
end;

function TDBAdvGrid.GetEditLimit: Integer;
begin
  Result := inherited GetEditLimit;

  if Columns.Count > Col then
  begin
    if Col >= FixedCols then
      Result := Columns[RealColIndex(Col)].EditLength;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetEditMask(ACol, ARow: Integer): string;
var
  msk: string;
begin
  if Columns.Count > Acol then
  begin
    if ACol >= FixedCols then
      Result := Columns[RealColIndex(ACol)].EditMask;
  end;

  msk := inherited GetEditMask(ACol, ARow);
  if msk <> '' then
    Result := msk;
end;


procedure TDBAdvGrid.BlobFieldToStream(DBField: TBlobField; var size: tpoint; Col, Row: integer);
var
  s, ms: TMemoryStream;
  sig: word;
  b: byte;
  Ljpg: TJPEGImage;
  {$IFDEF DELPHI2010_LVL}
  LGif: TGIFImage;
  LPng: TPNGImage;
  {$ENDIF}
  ABitmap: TBitmap;
  APicture: TPicture;
  oletype: integer;
  oleoffset: integer;
  i: Integer;

begin
//Result := '';
  size.X := 0;
  size.Y := 0;

  if (TBlobField(DBField).BlobType = ftGraphic) or (1 > 0) then
  begin
    s := TMemoryStream.Create;
    try
      if Assigned(OnFieldToStream) then
      begin
        OnFieldToStream(Self,Col,Row, DBField, S); 
      end
      else
        DBField.SaveToStream(S);       

      oletype := -1;
      oleoffset := 0;

      if s.Size > 2 then
      begin
    // find file type
        S.Position := 0;
        S.Read(sig, 2);

        case sig of
          $1C15: // OLE storage
            begin
              i := 0;
              while (i < 512) do
              begin
                S.Read(b, 1);
                inc(i);
                if (b = $FF) then
                begin
                  S.Read(b, 1);
                  inc(i);
                  if b = $D8 then
                  begin
                    oletype := 1;
                    oleoffset := i;
                    break;
                  end;
                end;
                if (b = $47) then
                begin
                  S.Read(b, 1);
                  inc(i);
                  if b = $49 then
                  begin
                    oletype := 2;
                    oleoffset := i;
                    break;
                  end;
                end;
                if (b = ord('B')) then
                begin
                  S.Read(b, 1);
                  inc(i);
                  if (b = ord('M')) then
                  begin
                    oletype := 0;
                    oleoffset := i;
                    Break;
                  end;
                end;
              end;

              S.Seek(oleoffset, 0);

              case oletype of
                0:
                  begin
                    ABitmap := TBitmap.Create;
                    ABitmap.LoadFromStream(S);
                    if not ABitmap.Empty then
                    begin
                      FPicture.Assign(ABitmap);
                      size.X := ABitmap.Width;
                      size.Y := ABitmap.Height;
                    end;
                    ABitmap.Free;
                  end;
                1:
                  begin
                    LJPg := TJPEGImage.Create;
                    try
                      LJpg.LoadFromStream(S);
                      APicture := TPicture.Create;
                      APicture.Assign(LJpg);

                      if not APicture.Graphic.Empty then
                      begin
                        FPicture.Assign(APicture);
                        size.X := APicture.Graphic.Width;
                        size.Y := APicture.Graphic.Height;
                      end;
                      APicture.Free;
                    finally
                      FreeAndNil(LJpg);
                    end;
                  end;
                2:
                  begin
                    ms := TMemoryStream.Create;
                    ms.CopyFrom(s, s.Size - s.Position);
                    FPicture.Graphic.LoadFromStream(ms);
                    ms.Free;
                  end;
              end;
            end;
          $4947: //gif signature
            begin
             {$IFDEF DELPHI2010_LVL}
             S.Position := 0;
             LGif := TGIFImage.Create;
             try
               LGif.LoadFromStream(S);
               APicture := TPicture.Create;
               APicture.Assign(LGif);
               if not APicture.Graphic.Empty then
               begin
                 FPicture.Assign(APicture);
                 size.X := APicture.Graphic.Width;
                 size.Y := APicture.Graphic.Height;
               end;
               APicture.Free;
             finally
               FreeAndNil(LGif);
             end;
             {$ENDIF}
            end;
          $D8FF: //jpeg signature
            begin
              S.Position := 0;
              LJPg := TJPEGImage.Create;
              try
                LJpg.LoadFromStream(S);
                APicture := TPicture.Create;
                APicture.Assign(LJpg);

                if not APicture.Graphic.Empty then
                begin
                  FPicture.Assign(APicture);

                  size.X := APicture.Graphic.Width;
                  size.Y := APicture.Graphic.Height;
                end;
                APicture.Free;
              finally
                FreeAndNil(LJpg);
              end;
            end;
          $5089: // png signature
            begin
              {$IFDEF DELPHI2010_LVL}
              S.Position := 0;
              LPng := TPNGImage.Create;
              try
                LPng.LoadFromStream(S);
                APicture := TPicture.Create;
                APicture.Assign(LPng);

                if not APicture.Graphic.Empty then
                begin
                  FPicture.Assign(APicture);

                  size.X := APicture.Graphic.Width;
                  size.Y := APicture.Graphic.Height;
                end;
                APicture.Free;
              finally
                FreeAndNil(LPng);
              end;
              {$ENDIF}
            end
        else
          begin
            APicture := TPicture.Create;
            APicture.Assign(DBField);
            if not APicture.Graphic.Empty then
            begin
              FPicture.Assign(APicture);

              size.X := APicture.Graphic.Width;
              size.Y := APicture.Graphic.Height;
              APicture.Free;
            end;
          end;
        end;
      end;
      FreeAndNil(S);
    except
      DoInvalidPicture(Col,Row);
      if (S <> nil) then
        FreeAndNil(S);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetCellGraphic(ACol, ARow: Integer): TCellGraphic;


  procedure LoadPictureFromBlobField(Field: TBlobField; Dest: TPicture);
  var
    Graphic: TGraphic;
    GraphicClass: TGraphicClass;
    Stream: TMemoryStream;
  begin
    Graphic := nil;
    Stream := TMemoryStream.Create;
    try
      Field.SaveToStream(Stream);
      if Stream.Size = 0 then
      begin
        Dest.Assign(nil);
        Exit;
      end;
      if not FindGraphicClass(Stream.Memory^, Stream.Size, GraphicClass) then
      begin
        FPicture.Assign(TPicture(Field));
      end
      else
      begin
        Graphic := GraphicClass.Create;
        Stream.Position := 0;
        Graphic.LoadFromStream(Stream);
        Dest.Assign(Graphic);
      end;
    finally
      Stream.Free;
      Graphic.Free;
    end;
  end;

  procedure LoadPictureFromField(Field: TField);
  begin
    try
      LoadPictureFromBlobField(Field as TBlobField, FPicture);
    except
      DoInvalidPicture(ACol,ARow);
    end;
  end;

var
  Fld: TField;
  OldActiveRecord: integer;
  sp: TPoint;
  IndCol: integer;
  dt: string;

begin
  Result := inherited GetCellGraphic(ACol, ARow);

  if not Assigned(FColumnCollection) then
    Exit;

  if (csDestroying in ComponentState) or (ACol >= FColumnCollection.Count) then
    Exit;

  if (FColumnCollection.Count > Acol) and (Result = nil) then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) and
      (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
      (ARow < RowCount - FixedFooters) then
    begin
      if TDBGridColumnItem(FColumnCollection.Items[ACol]).Editor = edDataCheckBox then
      begin
        FCellGraphic.CellType := ctVirtCheckBox;
        FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;
        Result := FCellGraphic;
      end;
    end;
  end;


  if not PageMode then
  begin

{
  if ShowBooleanFields and (ACol >= FixedCols) and (ARow >= FixedRows) then
  begin
    Fld := GetDBFieldAtColumn(ACol);

    if Fld <> nil then
    begin
      if Fld.DataType = ftBoolean then
      begin
        if (ACol = 0) and (FixedCols > 0) and FShowDBIndicator then
          exit;

        FCellGraphic.CellType := ctCheckBox;
        FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;
        FCellGraphic.CellBoolean := uppercase(Cells[ACol, ARow]) = 'TRUE';
        Result := FCellGraphic;
      end;
    end;
  end;   }

    Exit;
  end;


  if FShowDBIndicator and (FixedCols > 0) then
    IndCol := 1
  else
    IndCol := 0;

// CheckBox for Boolean fields
  if ShowBooleanFields and (ACol >= IndCol) and (ARow >= FixedRows) and
     not ((ARow = RowCount - 1) and (FloatingFooter.Visible)) then
  begin
    Fld := GetDBFieldAtColumn(ACol);

    if Assigned(Fld) then
    begin
      if Fld.DataType = ftBoolean then
      begin
        OldActiveRecord := FDataLink.ActiveRecord;

        if not FExporting then
        begin
          if (FDataLink.DataSet.State = dsInsert) or (FDataLink.DataSet.RecNo < 0) then
          begin
            FDataLink.ActiveRecord := ARow - (self.Row - FDataLink.ActiveRecord);
          end
          else
            FDataLink.ActiveRecord := ARow - ({FDataLink.DataSet.RecNo} self.Row - FDataLink.ActiveRecord);

          if (FDataLink.ActiveRecord < 0) or (FDataLink.ActiveRecord >= FDataLink.BufferCount) then
          begin
            FDataLink.ActiveRecord := OldActiveRecord;
            Exit;
          end;
        end;

        try
          FCellGraphic.CellType := ctCheckBox;
          FCellGraphic.CellAngle := 0;
          FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;
          FCellGraphic.CellBoolean := Fld.AsBoolean;

          case self.VAlignment of
            vtaTop: FCellGraphic.CellVAlign := vaTop;
            vtaCenter: FCellGraphic.CellVAlign := vaCenter;
            vtaBottom: FCellGraphic.CellVAlign := vaBottom;
          end;

          case FColumnCollection.Items[ACol].Alignment of
            taLeftJustify: FCellGraphic.CellHAlign := haLeft;
            taRightJustify: FCellGraphic.CellHAlign := haRight;
            taCenter: FCellGraphic.CellHAlign := haCenter;
          end;

          Result := FCellGraphic;
        finally
          if not FExporting then
            FDataLink.ActiveRecord := OldActiveRecord;
        end;

   { FCellGraphic.CellType := ctCheckBox;
    FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;
    FCellGraphic.CellBoolean := Fld.AsBoolean;
    Result := FCellGraphic;  }
      end;
    end;
  end;


  if (Result = nil) and (FColumnCollection.Items[ACol].CheckBoxField) and (ARow >= FixedRows) and
     (Columns[ACol].FieldName ='') and not ((ARow = RowCount - 1) and (FloatingFooter.Visible)) then
  begin
    FCellGraphic.CellType := ctCheckBox;
    FCellGraphic.CellAngle := 0;
    FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;
    FCellGraphic.CellBoolean := false;
    case self.VAlignment of
      vtaTop: FCellGraphic.CellVAlign := vaTop;
      vtaCenter: FCellGraphic.CellVAlign := vaCenter;
      vtaBottom: FCellGraphic.CellVAlign := vaBottom;
    end;
    case FColumnCollection.Items[ACol].Alignment of
      taLeftJustify: FCellGraphic.CellHAlign := haLeft;
      taRightJustify: FCellGraphic.CellHAlign := haRight;
      taCenter: FCellGraphic.CellHAlign := haCenter;
    end;
    Result := FCellGraphic;
  end;


  if (Result = nil) and (FColumnCollection.Items[ACol].ProgressField) and (ARow >= FixedRows) and
   not ((ARow = RowCount - 1) and (FloatingFooter.Visible)) then
  begin
    FCellGraphic.CellType := ctProgress;

    FCellgraphic.CellBitmap := TBitmap(FColumnCollection.Items[ACol].ProgressColor);
    FCellgraphic.CellIcon := TIcon(FColumnCollection.Items[ACol].ProgressBKColor);
    FCellgraphic.CellIndex := TColor(FColumnCollection.Items[ACol].ProgressTextColor);
    FCellgraphic.CellAngle := TColor(FColumnCollection.Items[ACol].ProgressTextBKColor);

    FCellgraphic.CellErrFrom := 0;
    FCellgraphic.CellErrLen := 100;
    FCellgraphic.CellBoolean := true;

    Result := FCellGraphic;
  end;

  if (Result = nil) and (FColumnCollection.Items[ACol].CheckBoxField) and (ARow = 0) and (MouseActions.CheckAllCheck) then
  begin
    FCellGraphic.CellType := ctCheckBox;
    FCellGraphic.CellAngle := 0;
    FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;

        case self.VAlignment of
          vtaTop: FCellGraphic.CellVAlign := vaTop;
          vtaCenter: FCellGraphic.CellVAlign := vaCenter;
          vtaBottom: FCellGraphic.CellVAlign := vaBottom;
        end;
        case FColumnCollection.Items[ACol].HeaderAlignment of
          taLeftJustify: FCellGraphic.CellHAlign := haLeft;
          taRightJustify: FCellGraphic.CellHAlign := haRight;
          taCenter: FCellGraphic.CellHAlign := haCenter;
        end;

    Result := FCellGraphic;
    Exit;
  end;

// CheckBox for String Fields
  if (Result = nil) and (FColumnCollection.Items[ACol].CheckBoxField) and (ARow >= FixedRows) and (ACol >= IndCol) and
     not ((ARow = RowCount - 1) and (FloatingFooter.Visible and (FloatingFooter.FooterStyle = fsFixedLastRow))) then
  begin
    Fld := GetDBFieldAtColumn(ACol);

    if Fld <> nil then
    begin
      OldActiveRecord := FDataLink.ActiveRecord;

      if (FDataLink.DataSet.State = dsInsert) or (FDataLink.DataSet.RecNo < 0) then
      begin
        FDataLink.ActiveRecord := ARow - (self.Row - FDataLink.ActiveRecord);
      end
      else
        FDataLink.ActiveRecord := ARow - ({FDataLink.DataSet.RecNo} self.Row - FDataLink.ActiveRecord);

      if (FDataLink.ActiveRecord < 0) or (FDataLink.ActiveRecord >= FDataLink.BufferCount) then
      begin
        FDataLink.ActiveRecord := OldActiveRecord;
        Exit;
      end;

      try
        FCellGraphic.CellType := ctCheckBox;
        FCellGraphic.CellAngle := 0;
        FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;

        dt := Fld.DisplayText;

        if Assigned(OnGetDisplText) then
          OnGetDisplText(Self, ACol, ARow, dt);

        FCellGraphic.CellBoolean := UpperCase(dt) = UpperCase(FColumnCollection.Items[ACol].CheckTrue);

        case self.VAlignment of
          vtaTop: FCellGraphic.CellVAlign := vaTop;
          vtaCenter: FCellGraphic.CellVAlign := vaCenter;
          vtaBottom: FCellGraphic.CellVAlign := vaBottom;
        end;
        case FColumnCollection.Items[ACol].Alignment of
          taLeftJustify: FCellGraphic.CellHAlign := haLeft;
          taRightJustify: FCellGraphic.CellHAlign := haRight;
          taCenter: FCellGraphic.CellHAlign := haCenter;
        end;

        Result := FCellGraphic;
      finally
        FDataLink.ActiveRecord := OldActiveRecord;
      end;
    end;

  end;

// Show picture when ShowPictureFields = true
  if (Result = nil) and ShowPictureFields and (ACol >= IndCol) and (ARow >= FixedRows) and
     not ((ARow = RowCount - 1) and (FloatingFooter.Visible)) then
  begin
    Fld := GetDBFieldAtColumn(ACol);
    if Fld <> nil then
    begin
      if Fld.DataType = ftGraphic then
      begin
        OldActiveRecord := FDataLink.ActiveRecord;

        if (FDataLink.DataSet.State = dsInsert) or (FDataLink.DataSet.RecNo < 0) then
        begin
          FDataLink.ActiveRecord := ARow - (self.Row - FDataLink.ActiveRecord);
        end
        else
          FDataLink.ActiveRecord := ARow - ({FDataLink.DataSet.RecNo} self.Row - FDataLink.ActiveRecord);

        if (FDataLink.ActiveRecord < 0) or (FDataLink.ActiveRecord >= FDataLink.BufferCount) then
        begin
          FDataLink.ActiveRecord := OldActiveRecord;
          Exit;
        end;

        try
          FCellGraphic.CellType := ctPicture;
          LoadPictureFromField(fld);  // sets FPicture
          FCellGraphic.CellBitmap := TBitmap(FPicture);
          FCellGraphic.CellHAlign := haCenter;
          FCellGraphic.CellVAlign := vaCenter;
          FCellGraphic.CellTransparent := True;
          FCellGraphic.CellAngle := Integer(FColumnCollection.Items[ACol].PictureStretch);
          FCellGraphic.CellIndex := 4;
          Result := FCellGraphic;
        finally
          FDataLink.ActiveRecord := OldActiveRecord;
        end;
      end;
    end;
  end;

// Extract Picture from Blob fields when Column.PictureField = true
  if (Result = nil) and FColumnCollection.Items[ACol].PictureField and (ACol >= IndCol) and (ARow >= FixedRows) then
  begin
    Fld := GetDBFieldAtColumn(ACol);
    if Fld <> nil then
    begin
      if true {Fld.IsBlob} then
      begin
        OldActiveRecord := FDataLink.ActiveRecord;

        if (FDataLink.DataSet.State = dsInsert) or (FDataLink.DataSet.RecNo < 0) then
        begin
          FDataLink.ActiveRecord := ARow - (self.Row - FDataLink.ActiveRecord);
        end
        else
          FDataLink.ActiveRecord := ARow - ({FDataLink.DataSet.RecNo} self.Row - FDataLink.ActiveRecord);

        if (FDataLink.ActiveRecord < 0) or (FDataLink.ActiveRecord >= FDataLink.BufferCount) then
        begin
          FDataLink.ActiveRecord := OldActiveRecord;
          Exit;
        end;

        try
          FCellGraphic.CellType := ctPicture;
          FPicture.Assign(nil);
          BlobFieldToStream(TBlobField(Fld), sp, ACol, ARow); // Sets FPicture
          FCellGraphic.CellBitmap := TBitmap(FPicture);
          FCellGraphic.CellHAlign := haCenter;
          FCellGraphic.CellVAlign := vaCenter;
          FCellGraphic.CellTransparent := True;
          FCellGraphic.CellAngle := Integer(FColumnCollection.Items[ACol].PictureStretch);
          FCellGraphic.CellIndex := 4;
          Result := FCellGraphic;
        finally
          FDataLink.ActiveRecord := OldActiveRecord;
        end;
      end;
    end;
  end;

// DataImageField
  if (Result = nil) and FColumnCollection.Items[ACol].DataImageField and (ACol >= IndCol) and (ARow >= FixedRows) then
  begin
    Fld := GetDBFieldAtColumn(ACol);
    if Fld <> nil then
    begin
      if Fld.DataType in [ftSmallInt, ftInteger, ftWord {$IFDEF DELPHI_UNICODE}, ftByte, ftLargeInt {$ENDIF}] then
      begin
      (*
        OldActiveRecord := FDataLink.ActiveRecord;

        if (FDataLink.DataSet.State = dsInsert) or (FDataLink.DataSet.RecNo < 0) then
        begin
          FDataLink.ActiveRecord := ARow - (self.Row - FDataLink.ActiveRecord);
        end
        else
          FDataLink.ActiveRecord := ARow - ({FDataLink.DataSet.RecNo} self.Row - FDataLink.ActiveRecord);

        if (FDataLink.DataSet.Eof) then
          Exit;

        if (FDataLink.ActiveRecord < 0) or (FDataLink.ActiveRecord >= FDataLink.BufferCount) then
        begin
          FDataLink.ActiveRecord := OldActiveRecord;
          Exit;
        end;
      *)
        FCellGraphic.CellType := ctDataImage;
        FCellGraphic.CellHAlign := haCenter;
        FCellGraphic.CellVAlign := vaCenter;
        FCellGraphic.CellTransparent := True;
        FCellGraphic.CellAngle := Integer(StretchWithAspectRatio);
        FCellGraphic.CellIndex := 0;
        Result := FCellGraphic;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetCellImageList(ACol, ARow: integer): TCustomImageList;
begin
  Result := GridImages;
  if Assigned(FColumnCollection) and (FColumnCollection.Count > Acol) then
  begin
    if Assigned(FColumnCollection.Items[ACol].Images) then
      Result := FColumnCollection.Items[ACol].Images
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetCellType(ACol, ARow: Integer): TCellType;
var
  Fld: TField;
begin
  Result := inherited GetCellType(ACol, ARow);

  if (ARow < 0) or (ACol < 0) then
    Exit;

  if Assigned(FColumnCollection) and CheckDataSet and (FColumnCollection.Count > Acol) then
  begin
    if ShowBooleanFields and (ACol >= FixedCols) and (ARow >= FixedRows) {and FPageMode} then
    begin
      Fld := GetDBFieldAtColumn(ACol);
      if Assigned(Fld) then
        if Fld.DataType = ftBoolean then
          Result := ctCheckBox;
    end;

    if (FColumnCollection.Items[ACol].CheckBoxField) and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      Result := ctCheckBox;
    end;

    if (Result = ctEmpty) and FColumnCollection.Items[ACol].DataImageField and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      Fld := GetDBFieldAtColumn(ACol);
      if Assigned(Fld) then
        if Fld.DataType in [ftSmallInt, ftInteger] then
        begin
          Result := ctDataImage;
        end;
    end;

    if (FColumnCollection.Count > Acol) and (Result = ctEmpty) then
    begin
      if (ACol >= FixedCols) and (Arow >= FixedRows) and
        (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
        (ARow < RowCount - FixedFooters) then
      begin
        if TDBGridColumnItem(FColumnCollection.Items[ACol]).Editor = edDataCheckBox then
        begin
          Result := ctDataCheckBox;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetCheckFalse(ACol, ARow: Integer): string;
begin
  Result := inherited GetCheckFalse(ACol, ARow);

  if (FColumnCollection.Count > ACol) then
  begin
    if (ACol >= FixedCols) and (ARow >= FixedRows) and
      (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
      (ARow < RowCount - FixedFooters) then
    begin
      if (GroupColumn >= FixedCols) then
        if (ACol >= GroupColumn) then
          Inc(ACol);

      Result := TDBGridColumnItem(FColumnCollection.Items[ACol]).CheckFalse;
    end;
  end;

end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetCheckTrue(ACol, ARow: Integer): string;
begin
  Result := inherited GetCheckTrue(ACol, ARow);

  if (FColumnCollection.Count > Acol) then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) and
      (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
      (ARow < RowCount - FixedFooters) then
    begin
      if (GroupColumn >= FixedCols) then
        if (ACol >= GroupColumn) then
          Inc(ACol);

      Result := TDBGridColumnItem(FColumnCollection.Items[ACol]).CheckTrue;
    end;
  end;

end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetFilter(ACol: Integer; Disp: boolean = false): Boolean;
var
  i: integer;
begin
  FilterList.Clear;

  if not PageMode and FilterDropDownAuto then
    inherited GetFilter(Acol);

  if (FColumnCollection.Count > ACol) then
  begin
    Result := FColumnCollection.Items[ACol].Filter.Count > 0;

    if Result then
    begin
      for i := 0 to FColumnCollection.Items[ACol].Filter.Count - 1 do
      begin
        FilterList.Add(FColumnCollection.Items[ACol].Filter.Strings[i]);
      end;
    end;
  end;

  if Assigned(OnGetColumnFilter) then
    OnGetColumnFilter(Self, ACol, FilterList);

  Result := FilterList.Count > 0;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.UpdateColSize(ACol: Integer;
  var NewWidth: Integer);
var
  RCol: integer;
begin
  RCol := RealColIndex(ACol);
  if (FColumnCollection.Count > RCol) and (RCol >= 0) then
  begin
    if (FColumnCollection.Items[RCol].MinSize > 0) then
    begin
      if NewWidth < FColumnCollection.Items[RCol].MinSize then
        NewWidth := FColumnCollection.Items[RCol].MinSize;
    end;

    if (FColumnCollection.Items[RCol].MaxSize > 0) then
    begin
      if NewWidth > FColumnCollection.Items[RCol].MaxSize then
        NewWidth := FColumnCollection.Items[RCol].MaxSize;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.UpdateAutoColSize(ACol: Integer;
  var NewWidth: Integer);
var
  RCol: integer;
begin
  RCol := RealColIndex(ACol);
  if (FColumnCollection.Count > RCol) and (RCol >= 0) then
  begin
    if (FColumnCollection.Items[RCol].AutoMinSize > 0) then
    begin
      if NewWidth < FColumnCollection.Items[RCol].AutoMinSize then
        NewWidth := FColumnCollection.Items[RCol].AutoMinSize;
    end;

    if (FColumnCollection.Items[RCol].AutoMaxSize > 0) then
    begin
      if NewWidth > FColumnCollection.Items[RCol].AutoMaxSize then
        NewWidth := FColumnCollection.Items[RCol].AutoMaxSize;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.UpdateColHeaders;
var
  i: Integer;
begin
  for i := 1 to Columns.Count do
  begin
    if i < ColumnHeaders.Count then
    begin
      Columns.Items[i - 1].FColumnHeader := ColumnHeaders.Strings[i - 1];
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetFormattedCell(ACol, ARow: Integer): string;
var
  fmt: string;
  IsFloat: Boolean;
  s: string;
  c: integer;
begin
  c := ACol;
  if (GroupColumn >= 0) and (ACol >= GroupColumn) then
    inc(c);

  if (FColumnCollection.Count > c) then
  begin
    s := Cells[ACol, ARow];
    fmt := TDBGridColumnItem(FColumnCollection.Items[c]).FloatFormat;

    IsFloat := IsType(s) in [atNumeric, atFloat];

    if Assigned(OnGetFloatFormat) then
      OnGetFloatFormat(Self, ACol, ARow, IsFloat, Fmt);

    if (fmt <> '') and IsFloat then
    begin
      Result := Format(fmt, [Floats[ACol, ARow]]);
    end
    else
      Result := s;

  end
  else
    Result := inherited GetFormattedCell(ACol, ARow);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetShowMemoFields(const Value: Boolean);
begin
  if FShowMemoFields <> value then
  begin
    FShowMemoFields := Value;
    if not PageMode and CheckDataSet then
      ActiveChange(true);
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.CellValidateEvent(Sender: TObject; ACol,
  ARow: Integer; var Value: string; var Valid: Boolean);
var
  OldV: boolean;
  v, i: integer;
  aField: TField;
  ShouldUpdateData: Boolean;
  WasEditable: Boolean;
begin

  if CheckDataSet and Assigned(FColumnCollection) and (FColumnCollection.Count > Acol) and PageMode then
  begin
    if (pos('|\', Value) = 1) then
      Value := DecodeWideStr(Value);

    if Assigned(FOnCellValidate) then
      FOnCellValidate(Sender, ACol, ARow, Value, Valid);

    if not (DataSource.DataSet.State in [dsInsert, dsEdit]) and (FEditPostMode = epCell) then
      Exit;

    if (csDestroying in ComponentState) then
      Exit;


  {
  i := 1;
  if not FShowDBIndicator or (FixedCols <= 0) then
  i := 0;
  }

    aField := GetDBFieldAtColumn(ACol);
    if (ShowBooleanFields and (aField <> nil) and (aField.DataType = ftBoolean))
      or (ShowPictureFields and (aField <> nil) and (aField.DataType = ftGraphic))
      or (FColumnCollection.Items[ACol].PictureField) or FColumnCollection.Items[ACol].CheckBoxField then
      Exit;

    if Valid then
    begin
      ShouldUpdateData := True;
      if FShouldNotPostChanges then
      begin
        FShouldNotPostChanges := False;
        if (Value = '') then
          ShouldUpdateData := False;
      end;

      if FEditPostMode = epCell then
      begin
        if ShouldUpdateData and CanModify then
        try
          if FColumnCollection.Items[ACol].FieldName <> '' then
          begin
            aField := FDataLink.DataSet.FieldByName(FColumnCollection.Items[ACol].FieldName);

            if (FColumnCollection.Items[ACol].Editor = edColorPickerDropDown) and not (aField.DataType in [ftString, ftWideString, ftMemo, {$IFDEF DELPHI2007_LVL} ftWideMemo, {$ENDIF} ftBytes, ftVarBytes]) then
            begin
              Value := IntToStr(integer(ColorPickerDropDown.SelectedColor));
            end;

            if AField.FieldKind = fkLookup then
            begin
              if (Combobox.ItemIndex >= 0) then
              begin
                v := integer(ComboBox.Items.Objects[ComboBox.ItemIndex]);
                FDataLink.DataSet.FieldByName(aField.KeyFields).AsString := GetLookupKey(v); ;
              end;
            end
            else
            begin
              if (aField.IsBlob) and not ShowMemoFields then
                AField.AsString := '(MEMO)'
              else
              begin
                if Assigned(aField.OnSetText) then
                  AField.OnSetText(AField, Value)
                else
                  AField.AsString := Value;
              end;    
            end;
          end
          else
            if not FShowDefinedFields then
            begin
              i := GetDBFieldIndexAtColumn(ACol);

              if i <> -1 then
                aField := FDataLink.DataSet.Fields[i]
              else
                aField := nil;

              if Assigned(aField) then
              begin
                if aField.FieldKind = fkLookup then
                begin
                  v := integer(ComboBox.Items.Objects[ComboBox.ItemIndex]);
                  FDataLink.DataSet.FieldByName(aField.KeyFields).AsString := GetLookupKey(v); ;
                end
                else
                begin
                  if (aField.IsBlob) and not ShowMemoFields then
                    AField.AsString := '(MEMO)'
                  else
                    AField.AsString := Value;
                end;
              end;
            end;
          if FShowDefinedFields and (FColumnCollection.Items[ACol].FieldName = '') then
            Value := '';
        except{finally}   // FF: issue: no more editing if exception occured while editing
          Valid := False;
        end;

        OldV := FCancelEditReturn;
        WasEditable := goEditing in Options;
        if DataSetType = dtSequenced then
          FCancelEditReturn := true
        else
        begin

        end;

        try
          if ShouldUpdateData then
            FDataLink.DataSet.Post;
        except
          on E: Exception do
          begin
            FRecordChanged := false;
            FDataLink.DataSet.Cancel;
            //do not re-raise exception 
            //raise exception.Create(E.message);
          end;
        end;

        if FNewRecord then
        begin
          if FRefreshOnInsert then
            FDataLink.DataSet.Refresh;
          FNewRecord := false;
        end;
        if DataSetType = dtNonSequenced then
        begin
          if FNewAppendRecord then
          begin
            if TopRow > {2}(FixedRows+1) then
            begin
              TopRow := {2}(FixedRows+1);
            //Options:= Options + [goEditing];
              InvalidateGrid;
            end;
            FNewAppendRecord := false;
          end;
        end;
        FCancelEditReturn := OldV;
        if (DataSetType = dtSequenced) then
        begin
          if WasEditable and not(goEditing in Options) then
            Options := Options + [goEditing];
        end;

        if not DataLink.DataSet.Filtered then
          FEmptyDataSet := false;
        FKeyDownAppend := false;
      end
      else // FEditPostMode = epRow
      begin
        if CheckDataSet then
        begin
          // setting FEditText for ComBoBox
          case FColumnCollection.Items[ACol].Editor of
            edComboEdit,edComboList, edEditBtn, edDateEdit, edTimeEdit, edNormal, edSpinEdit, edFloatSpinEdit, edFloat
            , edTrackbarDropDown, edMemoDropDown, edCalculatorDropDown
            , edTimePickerDropDown, edDetailDropDown, edGridDropDown, edColorPickerDropDown
            , edImagePickerDropDown, edAdvGridDropDown
            {$IFNDEF DELPHI_UNICODE}
            , edUniComboEdit,edUniComboList, edUniEdit, edUniMemo
            {$ENDIF}
            :
            begin
              if (FColumnCollection.Items[ACol].Editor = edColorPickerDropDown) and not (aField.DataType in [ftString, ftWideString, ftMemo, {$IFDEF DELPHI2007_LVL} ftWideMemo, {$ENDIF} ftBytes, ftVarBytes]) then
              begin
                Value := IntToStr(integer(ColorPickerDropDown.SelectedColor));
              end;

              if (FEditText <> Value) and (aField.FieldKind <> fkLookup) then
                FEditText := Value;

              if (FEditWideText <> Value) and (aField.FieldKind <> fkLookup) and (aField.DataType = ftWideString) then
                FEditWideText := Value;
            end;
          end;

          if (aField.Text <> Value) and not FDatalink.FModified and (FDataLink.DataSet.State in [dsEdit, dsInsert]) then
            FDatalink.Modified;

          FDataLink.UpdateData;
        end;

        Exit;
        
        (*
        if FColumnCollection.Items[ACol].FieldName <> '' then
        begin
          aField := FDataLink.DataSet.fieldbyname(FColumnCollection.Items[ACol].FieldName);

          if aField.FieldKind = fkLookup then
          begin
            v := integer(ComboBox.Items.Objects[ComboBox.ItemIndex]);
            //FDataLink.DataSet.FieldByName(aField.KeyFields).AsString := GetLookupKey(v);
            TempValue := GetLookupKey(v);
          end
          else
          begin
            if (aField.IsBlob) and not ShowMemoFields then
              TempValue := '(MEMO)'
            else
              TempValue := Value;
          end;
        end
        else if not FShowDefinedFields then
        begin
          aField := FDataLink.DataSet.Fields[GetDBFieldIndexAtColumn(ACol) {ACol - i}];

          if aField.FieldKind = fkLookup then
          begin
            v := integer(ComboBox.Items.Objects[ComboBox.ItemIndex]);
            //FDataLink.DataSet.FieldByName(aField.KeyFields).AsString := GetLookupKey(v);
            TempValue := GetLookupKey(v);
          end
          else
          begin
            if (aField.IsBlob) and not ShowMemoFields then
              TempValue := '(MEMO)'
            else
              TempValue := Value;
          end;
        end;
        if FShowDefinedFields and (FColumnCollection.Items[ACol].FieldName = '') then
          TempValue := '';

        for i:= FEditRecData.Count to ACol do
        begin
          FEditRecData.AddObject('', pointer(0)); // Invalid Value
        end;

        FEditRecData[ACol]:= TempValue;
        FEditRecData.Objects[ACol]:= pointer(1);  // Value Value

        if FEditRec < 0 then
        begin
          FEditRec := ARow;
          FEditRecBm:= FDataLink.DataSet.GetBookmark;
          //FDataLink.DataSet.UpdateRecord;
        end
        else if FEditRec <> ARow then
          raise exception.Create('Invalid Data');
        *)
      end;

    end;
  end
  else
    if Assigned(FOnCellValidate) then
      FOnCellValidate(Sender, ACol, ARow, Value, Valid);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.CellValidateWideEvent(Sender: TObject; ACol, ARow: Integer; var Value: widestring; var Valid: Boolean);
{$IFNDEF DELPHI_UNICODE}
var
  OldV: boolean;
  v,i: integer;
  aField: TField;
  WasEditable: Boolean;
{$ENDIF}  
begin
  {$IFNDEF DELPHI_UNICODE}
  if CheckDataSet and Assigned(FColumnCollection) and (FColumnCollection.Count > Acol) and PageMode then
  begin
    if (pos('|\', Value) = 1) then
      Value := DecodeWideStr(Value);

    if Assigned(FOnCellValidateWide) then
      FOnCellValidateWide(Sender, ACol, ARow, Value, Valid);

    aField := GetDBFieldAtColumn(ACol);
    if (ShowBooleanFields and (aField <> nil) and (aField.DataType = ftBoolean))
      or (ShowPictureFields and (aField <> nil) and (aField.DataType = ftGraphic))
      or (FColumnCollection.Items[ACol].PictureField) or FColumnCollection.Items[ACol].CheckBoxField then
      Exit;

    if Valid then
    begin
      if FEditPostMode = epCell then
      begin
        if self.CanModify then
        try
          if FColumnCollection.Items[ACol].FieldName <> '' then
          begin
            aField := FDataLink.DataSet.fieldbyname(FColumnCollection.Items[ACol].FieldName);

            if aField.FieldKind = fkLookup then
            begin
              {$IFDEF DELPHI2006_LVL}
              if aField.DataType = ftWideString then
                v := integer(UniCombo.Items.Objects[UniCombo.ItemIndex])
              else
              {$ENDIF}
                v := integer(ComboBox.Items.Objects[ComboBox.ItemIndex]);
              FDataLink.DataSet.FieldByName(aField.KeyFields).AsString := GetLookupKey(v); ;
            end
            else
            begin
              if (aField.IsBlob) and not ShowMemoFields then
                AField.AsString := '(MEMO)'
              else
              begin
                if Assigned(aField.OnSetText) then
                  AField.OnSetText(aField, Value)
                else
                begin
                  if AField.DataType = ftWideString then
                  begin
                    TWideStringField(aField).Value := Value;
                  end
                  else
                    AField.AsString := Value;
                end;
              end;
            end;
          end
          else
            if not FShowDefinedFields then
            begin
              i := GetDBFieldIndexAtColumn(ACol);
              if (i <> -1) then
                AField := FDataLink.DataSet.Fields[i]
              else
                AField := nil;

              if Assigned(aField) then
              begin
                if AField.FieldKind = fkLookup then
                begin
                  v := integer(ComboBox.Items.Objects[ComboBox.ItemIndex]);
                  FDataLink.DataSet.FieldByName(aField.KeyFields).AsString := GetLookupKey(v); ;
                end
                else
                begin
                  if (AField.IsBlob) and not ShowMemoFields then
                    AField.AsString := '(MEMO)'
                  else
                    TWideStringField(aField).Value := Value;
                end;
              end;
            end;
          if FShowDefinedFields and (FColumnCollection.Items[ACol].FieldName = '') then
            Value := '';
        except{finally}   // FF: issue: no more editing if exception occured while editing
          Valid := False;
        end;

        OldV := FCancelEditReturn;
        WasEditable := goEditing in Options;
        if DataSetType = dtSequenced then
          FCancelEditReturn := true
        else
        begin

        end;

        try
          FDataLink.DataSet.Post;
        except
          on E: Exception do
          begin
            FRecordChanged := false;
            FDataLink.DataSet.Cancel;
            //do not re-raise exception
            //raise exception.Create(E.message);
          end;
        end;

        if FNewRecord then
        begin
          if FRefreshOnInsert then
            FDataLink.DataSet.Refresh;
          FNewRecord := false;
        end;
        if DataSetType = dtNonSequenced then
        begin
          if FNewAppendRecord then
          begin
            if TopRow > (FixedRows+1) then
            begin
              TopRow := (FixedRows+1);
              InvalidateGrid;
            end;
            FNewAppendRecord := false;
          end;
        end;
        FCancelEditReturn := OldV;
        if (DataSetType = dtSequenced) then
        begin
          if WasEditable and not(goEditing in Options) then
            Options := Options + [goEditing];
        end;
        
        if not DataLink.DataSet.Filtered then
          FEmptyDataSet := false;
        FKeyDownAppend := false;
      end
      else // FEditPostMode = epRow
      begin
        if CheckDataSet then
        begin
          // setting FEditText for ComboBox
          case FColumnCollection.Items[ACol].Editor of
            edUniComboEdit,edUniComboList, edUniEdit, edUniMemo, edUniEditBtn:
            begin
              if (FEditWideText <> Value) and (aField.FieldKind <> fkLookup) then
                FEditWideText := Value;
            end;
          end;

          if (aField.Text <> Value) and not FDatalink.FModified and (FDataLink.DataSet.State in [dsEdit, dsInsert]) then
            FDatalink.Modified;
          FDataLink.UpdateData;
        end;
        Exit;
      end;
    end;
  end;
  {$ENDIF}
end;


//------------------------------------------------------------------------------

{$IFDEF TMSUSETQUERY}
procedure TDBAdvGrid.DoTQuerySort(ACol: Integer);
var
  fldname, SQLText: string;
  i: integer;
begin
  if SortSettings.Direction = sdAscending then
    SortSettings.Direction := sdDescending
  else
    SortSettings.Direction := sdAscending;

  if FColumnCollection.Items[ACol].FieldName <> '' then
    fldname := FColumnCollection.Items[ACol].FieldName
  else
    fldname := TQuery(FDataLink.DataSet).FieldList.Fields[ACol - 1].FieldName;

  if (pos(' ', fldname) > 0) and not Assigned(FOnSetQueryOrder) then
    Exit;

  i := pos('ORDER BY', UpperCase(TQuery(FDataLink.DataSet).SQL.Text));
  if i > 0 then
    TQuery(FDataLink.DataSet).SQL.Text := copy(TQuery(FDataLink.DataSet).SQL.Text, 0, i - 1);

  TQuery(FDataLink.DataSet).SQL.Text := TQuery(FDataLink.DataSet).SQL.Text + ' ORDER BY ' + fldname;

  if SortSettings.Direction = sdDescending then
    TQuery(FDataLink.DataSet).SQL.Text := TQuery(FDataLink.DataSet).SQL.Text + ' DESC';

  SQLText := TQuery(FDataLink.DataSet).SQL.Text;

  if Assigned(FOnSetQueryOrder) then
    FOnSetQueryOrder(self, SQLText);

  TQuery(FDataLink.DataSet).SQL.Text := SQLText;

  TQuery(FDataLink.DataSet).Active := true;
  SortSettings.Column := ACol;

end;
{$ENDIF}

//------------------------------------------------------------------------------
{$IFNDEF TMSNOADO}
procedure TDBAdvGrid.DoADOSort(ACol: Integer);
var
  fldname, SQLText: string;
  i: integer;
begin
  if SortSettings.Direction = sdAscending then
    SortSettings.Direction := sdDescending
  else
    SortSettings.Direction := sdAscending;

  if FColumnCollection.Items[ACol].FieldName <> '' then
    fldname := FColumnCollection.Items[ACol].FieldName
  else
    fldname := TADOQuery(FDataLink.DataSet).FieldList.Fields[ACol - 1].FieldName;

  if (pos(' ', fldname) > 0) and not Assigned(FOnSetQueryOrder) then
    exit;

  i := pos('ORDER BY', UpperCase(TADOQuery(FDataLink.DataSet).SQL.Text));
  if i > 0 then
    TADOQuery(FDataLink.DataSet).SQL.Text := copy(TADOQuery(FDataLink.DataSet).SQL.Text, 0, i - 1);

  TADOQuery(FDataLink.DataSet).SQL.Text := TADOQuery(FDataLink.DataSet).SQL.Text + ' ORDER BY ' + fldname;

  if self.SortSettings.Direction = sdDescending then
    TADOQuery(FDataLink.DataSet).SQL.Text := TADOQuery(FDataLink.DataSet).SQL.Text + ' DESC';

  SQLText := TADOQuery(FDataLink.DataSet).SQL.Text;
  if Assigned(FOnSetQueryOrder) then
    FOnSetQueryOrder(self, SQLText);
  TADOQuery(FDataLink.DataSet).SQL.Text := SQLText;

  TADOQuery(FDataLink.DataSet).Active := true;
  SortSettings.Column := ACol;

end;
{$ENDIF}
//------------------------------------------------------------------------------

procedure TDBAdvGrid.CanSortEvent(Sender: TObject; ACol: Integer; var DoSort: Boolean);

begin
  if not CheckDataSet or not Assigned(FColumnCollection) or (ACol >= FColumnCollection.Count) or not PageMode then
    Exit;

  if Assigned(FOnCanSort) then
    FOnCanSort(Sender, ACol, DoSort);

  {$IFDEF TMSUSETQUERY}
  if DoSort and (FDataLink.DataSet is TQuery) then
  begin
    DoSort := False; // disable internal sort
    DoTQuerySort(ACol);
  end
  else
  {$ENDIF}
  {$IFNDEF TMSNOADO}
  if DoSort and (FDataLink.DataSet is TADOQuery) then
  begin
    DoSort := False; // disable internal sort
    DoADOSort(ACol);
  end;
  {$ENDIF}

end;

function TDBAdvGrid.CanTabToNextRow: Boolean;
var
  i: integer;
begin
  Result := inherited CanTabToNextRow;

  if CheckDataSet then
  begin
    i := FDataLink.DataSet.MoveBy(1);
    Result := i <> 0;
    if Result then
      FDataLink.DataSet.MoveBy(-1);
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetEditText(ACol, ARow: Integer): string;
var
  j: integer;
  aField: TField;
begin
  Result := inherited GetEditText(ACol, ARow);

  if not Assigned(FDataLink) or not Assigned(FDataLink.DataSet) or
	not FDataLink.DataSet.Active then
	Exit;

  ACol := RealColIndex(ACol);

  if (FColumnCollection.Count > Acol) and PageMode then
  begin
    aField := GetDBFieldAtColumn(ACol);
    if (ShowBooleanFields and (aField <> nil) and (aField.DataType = ftBoolean))
      or (ShowPictureFields and (aField <> nil) and (aField.DataType = ftGraphic))
      or (FColumnCollection.Items[ACol].PictureField) or FColumnCollection.Items[ACol].CheckBoxField then
      Exit;

    if FColumnCollection.Items[ACol].FieldName <> '' then
    begin
      //Result := FDataLink.DataSet.fieldbyname(FColumnCollection.Items[ACol].FieldName).AsString
      aField:= FDataLink.DataSet.fieldbyname(FColumnCollection.Items[ACol].FieldName);

      if (aField.IsBlob) then
        Result:= AField.AsString
      else
        Result:= AField.Text;
    end
    else if not FShowDefinedFields then
    begin
   { j := 1;
    if not FShowDBIndicator or (FixedCols <= 0) then
      j := 0; }
  //Result:= FDataLink.DataSet.Fields[ACol-j].AsString;
   // j := ACol - j;

      //Result := FDataLink.DataSet.Fields[GetDBFieldIndexAtColumn(ACol) {j}].AsString;

      j := GetDBFieldIndexAtColumn(ACol);
      if (j <> -1) then
        aField:= FDataLink.DataSet.Fields[j]
      else
        aField := nil;

      if Assigned(aField) then
      begin
        if (aField.IsBlob) then
          Result:= aField.AsString
        else
          Result:= aField.Text;
      end;

    end;
  end;

  if Assigned(FOnGetEditText) then
    FOnGetEditText(self, ACol, ARow, Result);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.UpdateRowCount;
var
  OldRowCount, NewRowCount: Integer;
  ShouldRepaint, OldDoNotCountRow, OldValue, OldInternalCall, OldSel, ReCountedRows: boolean;
  OldRow, br: integer;
begin
  OldRowCount := RowCount;
  if CanShowBlankRow then
    br := 1
  else
    br := 0;

  if RowCount <= FixedRows then
    RowCount := FixedRows + br;

//FixedRows := FTitleOffset;
  ShouldRepaint := false;

  OldRow := Row;

  if not PageMode then
    Exit;

  ReCountedRows := False;

  with FDataLink do
    if not Active {or (RecordCount = 0)} or not HandleAllocated then
      RowCount := {2}(FixedRows+1)
    else
    begin                                                             // FF: Field prop changing while inserting
      if (FDataLink.DataSet.State = dsInsert) and not FInternalInsert and (FOldState <> dsInsert) then
      begin
        if not FEmptyDataSet then
        begin
          InsertRows(row, 1);

          if FDataLink.DataSet.Eof then   // Append
          begin
            if (DataSetType = dtSequenced) then
            begin
              FAppending := True;
              OldValue := FInternalCall;
              FInternalCall := True;

              OldSel := FInternalSelection;
              FInternalSelection := True;

              Row := RowCount - 1;

              FInternalSelection := OldSel;

              FInternalCall := OldValue;
              FAppending := False;
            end
            else
            begin
              FAppending := True;
              OldValue := FInternalCall;
              FInternalCall := True;

              OldSel := FInternalSelection;
              FInternalSelection := True;

              Row := min(VisibleRowCount+1, RowCount-1);

              FInternalSelection := OldSel;
              FInternalCall := OldValue;
              FAppending := False;
            end;
          end;

        end;
      end
      else if FDataLink.DataSet.State = dsBrowse then
      begin                                                            // Change to avoid StackOverFlow
        if (FOldState = dsInsert) and FRecordChanged and FEmptyDataSet {}and not FInternalCall{} then
        begin
          FInternalCall := true;
          NewRowCount := self.GetRecordCount;
          if NewRowCount < FixedRows then
            RowCount := (FixedRows + br)
          else
            RowCount := NewRowCount + FixedRows;

          FInternalCall := false;
          ShouldRepaint := true;

          ReCountedRows := True;
        end;

        if (FOldState = dsInsert) and ( ((not FRecordChanged) and (FEditPostMode = epCell)) or (FEditPostMode = epRow) ) then
        begin
          if RowCount > {2}(FixedRows+1) then
          begin
            if (FEditPostMode = epCell) then
            begin
              OldValue := FCancelEditReturn;
              FCancelEditReturn:= false;

              if FInternalCall then
              begin
                OldInternalCall:= FInternalCall;
                FInternalCall := true;
                self.DeleteRow(RowCount - 1);    // when Delete Row it moves Row to RowCount-1
                FInternalCall := OldInternalCall;
              end;
              FCancelEditReturn:= OldValue;
            end;

            if not FInternalCall then
            begin
              FInternalCall := True;
              NewRowCount := self.GetRecordCount;
              if NewRowCount < FixedRows then
                RowCount := (FixedRows + br)
              else
                RowCount := NewRowCount + FixedRows;

              FInternalCall := false;
              ShouldRepaint := true;

              ReCountedRows := True;
            end;

          end;
        end
        else if (FOldState = dsBrowse) and (FDataLink.DataSet.State = dsBrowse) and not FInternalCall
          and not FCancelEditReturn and not FDoNotCountRow and not FNotDeletionUpdate then
        begin
        //FDataLink.DataSet.DisableControls;
          FInternalCall := True;
          NewRowCount := self.GetRecordCount; // .RowsInDataSet;
          if NewRowCount < FixedRows then
            RowCount := FixedRows + br
          else
            RowCount := NewRowCount + FixedRows;

          FInternalCall := false;
          ShouldRepaint := true;
          ReCountedRows := True;
      //FDataLink.DataSet.EnableControls;
        end;

        if (FOldState = dsInsert) and (FEditPostMode = epRow) and FEmptyDataSet
           and not ShouldRepaint and not FDoNotBounsBack then
        begin
          FInternalCall := true;
          FDoNotBounsBack := True;
          NewRowCount := self.GetRecordCount;
          if NewRowCount < FixedRows then
            RowCount := (FixedRows + br)
          else
            RowCount := NewRowCount + FixedRows;

          FInternalCall := false;
          ShouldRepaint := true;
          FDoNotBounsBack := False;
          ReCountedRows := True;
        end;

        if not ReCountedRows and (FOldState = dsInsert) and (FRecordChanged and (FEditPostMode = epCell)) then
        begin
          if not FInternalCall then
          begin
            FInternalCall := true;
            NewRowCount := self.GetRecordCount;
            if NewRowCount < FixedRows then
              RowCount := (FixedRows + br)
            else
              RowCount := NewRowCount + FixedRows;

            FInternalCall := false;
            ShouldRepaint := true;
          end;
        end;
   { RowCount := 1000;
    FDataLink.BufferCount := VisibleRowCount+1; //+1
    self.RowCount:= self.GetRecordCount+2; }
      end;
  //if dgRowSelect in Options then TopRow := FixedRows;
  //UpdateActive;
    end;

  FRecordChanged := false;

  if (OldRowCount <> RowCount) or ShouldRepaint then
  begin
    if (OldRowCount > RowCount) and FRefreshOnDelete and ShouldRepaint then
    begin
      OldDoNotCountRow := FDoNotCountRow;
    //OldRow:= Row;
      FDoNotCountRow := true;
    //FDataLink.DataSet.DisableControls;
      if not FDataLink.DataSet.IsEmpty then
        FDataLink.DataSet.Refresh;

      //Row := OldRow;
      Row:= Min(OldRow, RowCount - 1);
    //FDataLink.DataSet.EnableControls;

      FDoNotCountRow := OldDoNotCountRow;
    end;

    if (FDataLink.BufferCount <> GetBuffercount) and (DataSetType = dtSequenced) then
      FDataLink.BufferCount := GetBuffercount;

    Invalidate;
    InvalidateRow(Row);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if Assigned(FDataLink) and PageMode then
    if Assigned(FDataLink.DataSet) then
      if FDataLink.DataSet.Active then
      begin
        FDataLink.BufferCount := GetBufferCount;
        UpdateScrollBar(1);
      end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetShowDBIndicator(const Value: Boolean);
begin
  FShowDBIndicator := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
const
  INSERT_INDIC: array[0..5] of TPoint = ((x: 0; y: 4), (x: 6; y: 4),
    (x: 1; y: 2), (x: 5; y: 7),
    (x: 1; y: 6), (x: 5; y: 1));
  EDIT_INDIC: array[0..11] of TPoint = ((x: 2; y: 1), (x: 2; y: 9),
    (x: 3; y: 1), (x: 3; y: 9),
    (x: 0; y: 0), (x: 2; y: 0),
    (x: 4; y: 0), (x: 6; y: 0),
    (x: 0; y: 9), (x: 2; y: 9),
    (x: 4; y: 9), (x: 6; y: 9));
  BROWSE_INDIC: array[0..2] of TPoint = ((x: 0; y: 0),
    (x: 0; y: 10),
    (x: 5; y: 5));
var
  x_offs, y_offs: Integer;
  i: Integer;
  BrowseIndic: array[0..2] of TPoint;
begin
  inherited;


  if CheckDataSet and FShowDBIndicator and RowIndicator.Empty and PageMode then
  begin
    if (ACol = 0) and (ARow = Row) and (FixedCols > 0) and (RowCount > FixedRows) then
    begin
      Canvas.Brush.Color := clBlack;
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Width := 1;

      y_offs := 0;

      case VAlignment of
        vtaCenter: y_offs := (RowHeights[ARow] - 11) shr 1;
        vtaBottom: y_offs := RowHeights[ARow] - 10 - 3;
        vtaTop: y_offs := 1;
      end;
      ARect.Top := ARect.Top + y_offs;

      x_offs := 0;

      if ACol > Columns.Count then
      begin
        case Columns[ACol].Alignment of
          taLeftJustify: x_offs := 3;
          taCenter: x_offs := (ColWidths[ACol] - 5) shr 1;
          taRightJustify: x_offs := ColWidths[ACol] - 5 - 5;
        end;
      end
      else
        x_offs := 3;


      ARect.Left := ARect.Left + x_offs;

      case DataSource.DataSet.State of
        dsInsert:
          begin
            i := 0;
            while i <= High(INSERT_INDIC) do
            begin
              Canvas.MoveTo(ARect.Left + INSERT_INDIC[i + 0].X, ARect.Top + INSERT_INDIC[i + 0].Y);
              Canvas.LineTo(ARect.Left + INSERT_INDIC[i + 1].X, ARect.Top + INSERT_INDIC[i + 1].Y);

              Inc(i, 2);
            end;
          end;
        dsEdit:
          begin
            i := 0;
            while i <= High(EDIT_INDIC) do
            begin
              Canvas.MoveTo(ARect.Left + EDIT_INDIC[i + 0].X, ARect.Top + EDIT_INDIC[i + 0].Y);
              Canvas.LineTo(ARect.Left + EDIT_INDIC[i + 1].X, ARect.Top + EDIT_INDIC[i + 1].Y);

              Inc(i, 2);
            end;
          end;
      else
        begin
          for i := Low(BROWSE_INDIC) to High(BROWSE_INDIC) do
          begin
            BrowseIndic[i].X := ARect.Left + BROWSE_INDIC[i].X;
            BrowseIndic[i].Y := ARect.Top + BROWSE_INDIC[i].Y;
          end;
          Canvas.Polygon(BrowseIndic);
        end;
      end;
    end;
  end;
end;

function TDBAdvGrid.IsDSEdit: boolean;
begin
  Result := false;
  if CheckDataSet then
    Result := (FDataLink.DataSet.State in [dsEdit, dsInsert]);
end;

{
procedure TDBAdvGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
AState: TGridDrawState);
var
y_offs: Integer;
begin
inherited;
if CheckDataSet and FShowDBIndicator and PageMode then
begin
  if (ACol = 0) and (ARow = Row) and (FixedCols > 0) then
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;

    y_offs := 0;

    case VAlignment of
    vtaCenter: y_offs := (RowHeights[ARow] - 11) shr 1;
    vtaBottom: y_offs := RowHeights[ARow] - 11;
    end;
    ARect.Top := ARect.Top + y_offs;

    case DataSource.DataSet.State of
      dsInsert:
        begin
          Canvas.MoveTo(ARect.Left + 3, ARect.Top + 8);
          Canvas.LineTo(ARect.Left + 9, ARect.Top + 8);
          Canvas.MoveTo(ARect.Left + 4, ARect.Top + 6);
          Canvas.LineTo(ARect.Left + 8, ARect.Top + 11);
          Canvas.MoveTo(ARect.Left + 4, ARect.Top + 10);
          Canvas.LineTo(ARect.Left + 8, ARect.Top + 5);
        end;
      dsEdit:
        begin
          Canvas.MoveTo(ARect.Left + 5, ARect.Top + 3);
          Canvas.LineTo(ARect.Left + 5, ARect.Top + 12);
          Canvas.MoveTo(ARect.Left + 6, ARect.Top + 3);
          Canvas.LineTo(ARect.Left + 6, ARect.Top + 12);
          Canvas.MoveTo(ARect.Left + 3, ARect.Top + 2);
          Canvas.LineTo(ARect.Left + 5, ARect.Top + 2);
          Canvas.MoveTo(ARect.Left + 7, ARect.Top + 2);
          Canvas.LineTo(ARect.Left + 9, ARect.Top + 2);

          Canvas.MoveTo(ARect.Left + 3, ARect.Top + 12);
          Canvas.LineTo(ARect.Left + 5, ARect.Top + 12);
          Canvas.MoveTo(ARect.Left + 7, ARect.Top + 12);
          Canvas.LineTo(ARect.Left + 9, ARect.Top + 12);
        end;
    else
      begin
        Canvas.Polygon([Point(ARect.Left + 3, ARect.Top + 3),
          Point(ARect.Left + 3, ARect.Top + 13), Point(ARect.Left + 8, ARect.Top + 8)]);
      end;
    end;
  end;
end;
end;
}

//------------------------------------------------------------------------------

function TDBAdvGrid.CheckDataSet: Boolean;
begin
  Result := Assigned(FDataLink) and Assigned(FDataLink.DataSet) and FDataLink.DataSet.Active;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.ExportNotification(state: TGridExportState;
  ARow: Integer);
var
  df: integer;
  OldV: Boolean;
begin
  inherited;

  if not CheckDataSet or not PageMode then
    Exit;

  case State of
    esExportStart:
      begin
        FExporting := True;
        FSelExport := false;
        FOldIsEOF := FDataLink.DataSet.Eof;
        FOldIsBOF := FDataLink.DataSet.Bof;

//        if FDataLink.DataSet.Eof and FDataLink.DataSet.Bof then // empty dataset
//          Exit;

        FDataLink.DataSet.DisableControls;
        inc(FMustEnableControls);
        BeginUpdate;
        FOldAR := FDataLink.ActiveRecord;
        FOldPosition := FDataLink.DataSet.GetBookMark;
        FOldTopRow := TopRow;

        if (ARow >= 0) and (DataSetType = dtNonSequenced) then
        begin
          FExportStartRow := ARow;
          FDataLink.DataSet.First;
          if (ARow - FixedRows) > 0 then
          begin
            FDataLink.DataSet.MoveBy(ARow - FixedRows);
          end;
        end;
  //FDataLink.DataSet.First;
      end;
    esExportNewRow, esExportSelRow:
      begin
        if DataSetType = dtSequenced then
        begin
          Row := ARow;
        end
        else //DataSetType = dtNonSequenced
        begin
          if State = esExportSelRow then
            FSelExport := true;

          FExportRow := ARow;

          if FSelExport then
          begin
            FDataLink.DataSet.GotoBookMark(FOldPosition);
            if (ARow - Row) > 0 then
              FDataLink.DataSet.MoveBy(ARow - Row);
          end
          else
          begin
            FDataLink.DataSet.First;
            if (ARow - FixedRows) > 0 then
            begin
              FDataLink.DataSet.MoveBy(ARow - FixedRows);
            end;
          end;
        end;
     (* r := ARow - {Row;//} FDataLink.DataSet.recno;
      FDataLink.DataSet.MoveBy(r); *)
      end;
    esExportNextRow:
    begin
      if DataSetType = dtSequenced then
      begin
        Row := ARow;
      end
      else //DataSetType = dtNonSequenced
      begin
        FExportRow := ARow;
        if (ARow <> FExportStartRow) and ((ARow - FixedRows) > 0) then
        begin
          FDataLink.DataSet.MoveBy(1);
        end;
      end;
    end;
    esExportDone:
      begin
        FExporting := False;
        FExportRow := -1;
        FSelExport := false;
        FExportStartRow := -1;

        if Assigned(FOldPosition) then
        begin    // Go back to original position
          if (FOldTopRow > -1) then
            TopRow := FOldTopRow;

          FDataLink.DataSet.GotoBookMark(FOldPosition);
          FDataLink.DataSet.FreeBookMark(FOldPosition);
          FOldPosition := nil;
          if (FOldAR <> FDataLink.ActiveRecord) then
          begin
            // RePosition ActiveRecord in Cache
            if (FOldAR < FDataLink.ActiveRecord) then
            begin
              df := (FDataLink.BufferCount - FDataLink.ActiveRecord) + FDataLink.ActiveRecord - FOldAR;
              df := FDataLink.DataSet.MoveBy(df);
              FDataLink.DataSet.MoveBy(-df);
            end
            else
            begin
              df := FDataLink.ActiveRecord + (FOldAR - FDataLink.ActiveRecord);
              df := FDataLink.DataSet.MoveBy(-df);
              FDataLink.DataSet.MoveBy(abs(df));
            end;
          end;

          if FOldIsEOF then
            FDataLink.DataSet.Next;

          if FOldIsBOF then
            FDataLink.DataSet.Prior;

          OldV := FDoNotCountRow;
          FDoNotCountRow := True;
          dec(FMustEnableControls);
          FDataLink.DataSet.EnableControls;  // This will call UpdateScrollBar which sets Row
          FDoNotCountRow := OldV;
        end;

        if (FMustEnableControls > 0) and (FDataLink.DataSet.ControlsDisabled) then
          FDataLink.DataSet.EnableControls;

        EndUpdate;
      end;
    esExportFail:
      begin  // Do not go back to original position
        FExporting := False;
        FExportRow := -1;
        FSelExport := false;
        FExportStartRow := -1;
        if Assigned(FOldPosition) then
        begin
          //FDataLink.DataSet.GotoBookMark(FOldPosition);
          FDataLink.DataSet.FreeBookMark(FOldPosition);
          FOldPosition := nil;
         { if FOldIsEOF then
            FDataLink.DataSet.Next;

          if FOldIsBOF then
            FDataLink.DataSet.Prior;
          }
          FDataLink.DataSet.EnableControls;
          EndUpdate;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.ImportNotification(AState: TGridImportState;
  ARow: Integer);
begin
  inherited;

  if not CheckDataSet or not PageMode then
    Exit;

  if DataSetType = dtSequenced then
  begin

  end
  else // DataSetType = dtNonSequenced
  begin

    case AState of
      isImportStart:
        begin
          FSelImport := false;
       { FOldIsEOF := FDataLink.DataSet.Eof;
        FOldIsBOF := FDataLink.DataSet.Bof;
        FDataLink.DataSet.DisableControls;
        BeginUpdate;                                 }
          FSelRow := Row;
          FImportPos := FDataLink.DataSet.GetBookMark;
        end;
      isImportNewRow, isImportSelRow:
        begin
          if AState = isImportSelRow then
            FSelImport := true;

          if FSelImport then
          begin
            FDataLink.DataSet.GotoBookMark(FImportPos);
            if (ARow - FSelRow {Row}) > 0 then
              FDataLink.DataSet.MoveBy(ARow - FSelRow {Row});
          end
          else
          begin
            FDataLink.DataSet.First;
            if (ARow - {1}FixedRows) > 0 then
              FDataLink.DataSet.MoveBy(ARow - {1}FixedRows);
          end;
        end;
      isImportDone:
        begin
          FSelImport := false;
       { if Assigned(FImportPos) then
        begin
          FDataLink.DataSet.GotoBookMark(FOldPosition);
          FDataLink.DataSet.FreeBookMark(FOldPosition);
          if FOldIsEOF then
            FDataLink.DataSet.Next;

          if FOldIsBOF then
            FDataLink.DataSet.Prior;

          FDataLink.DataSet.EnableControls;
          EndUpdate;
        end;         }
        end;
    end;


  end;
end;

procedure TDBAdvGrid.InitValidate(ACol, ARow: Integer);
begin
  inherited;
  //OldCellText := GetEditText(RealColIndex(ACol), ARow);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetCellReadOnly(ACol, ARow: Integer;
  var IsReadOnly: Boolean); // IsReadOnly means CanEdit
var
  i,rc: integer;
  isFixed: Boolean;
begin
  inherited;

  if FCancelEditReturn then
    IsReadOnly := false;

  if Assigned(FDataLink) then
  begin
    if FDataLink.Active and Assigned(FDataLink.DataSet) then
      if FDataLink.DataSet.Active and not FDataLink.DataSet.CanModify then
      begin
        if (ACol < FColumnCollection.Count) and (Columns[ACol].FieldName <> '') then
        begin
          IsReadOnly := false;
          Exit;
        end;
      end;
  end;

  if (ACol >= FColumnCollection.Count) {or not PageMode} then
  begin
    IsReadOnly := false;
    Exit;
  end;

  rc := ACol;

  isFixed := false;
  GetCellFixed(rc, ARow, isFixed);

  if not isFixed and (FColumnCollection.Items[rc].HTMLTemplate <> '') then
  begin
    IsReadOnly := false;
    exit;
  end;

  if (FColumnCollection.Items[rc].Editor = edNone) then
  begin
    IsReadOnly := false;
    Exit;
  end;

  if IsReadOnly and CheckDataSet and not isFixed then
  begin

{ if FDataLink.DataSet.IsEmpty and (FDataLink.DataSet.State = dsBrowse) then
begin
  IsReadOnly:= false;
  exit;
end;
}
 { i := 1;
  if not FShowDBIndicator or (FixedCols <= 0) then
    i := 0; }
    if FColumnCollection.Items[rc].FieldName <> '' then
    begin
      IsReadOnly := not ({FDataLink.DataSet.fieldbyname(FColumnCollection.Items[ACol].FieldName).IsIndexField}
        FDataLink.DataSet.FieldByName(FColumnCollection.Items[ACol].FieldName).ReadOnly);
    end
    else if not FShowDefinedFields then
    begin
      i := GetDBFieldIndexAtColumn(rc);

      if i <> -1 then
        IsReadOnly := not (FDataLink.DataSet.Fields[i].ReadOnly);
    end;
    //else if FColumnCollection.Items[ACol].ReadOnly then
      //IsReadOnly:= not FColumnCollection.Items[ACol].ReadOnly;

    // Commented due making false when user set true
    //if IsReadOnly then
      //IsReadOnly := (goEditing in Options);
  end;

  // Fix for Spin escap issue
  //if IsReadOnly and (FDataLink.DataSet.State = dsBrowse) and not EditMode and not EditActive then
    //Cells[RealColIndex(Col), Row] := Cells[RealColIndex(Col), Row];
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.ValidateCell(const NewValue: string): Boolean;
begin
  Result := inherited ValidateCell(NewValue);

  if not Result then
    CurrentCell := NewValue;

  if CheckDataSet and PageMode then
  begin
    if (FDataLink.DataSet.State = dsEdit) then
    begin
      if not Navigation.AdvanceOnEnter then
      begin
        if FEditPostMode = epCell then
        begin
          FInternalCall := true;
          FDataLink.DataSet.Post;
          FInternalCall := false;
        end;
      end
      else
        CurrentCell := NewValue;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.CanShowBlankRow: Boolean;
begin
  Result := FShowBlankRow or (goEditing in Options) or not FixedRowAlways;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetShowBlankRow(const Value: Boolean);
begin
  if (FShowBlankRow <> Value) then
  begin
    FShowBlankRow := Value;
    UpdateRowCount;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetShowBooleanFields(const Value: Boolean);
begin
  if FShowBooleanFields <> Value then
  begin
    FShowBooleanFields := Value;
    if not PageMode and CheckDataSet then
    begin
      if FShowBooleanFields then
        ActiveChange(true)
      else
        RemoveAllBooleanCheckBoxes;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetShowPictureFields(const Value: Boolean);
begin
  if FShowPictureFields <> Value then
  begin
    FShowPictureFields := Value;
    if not PageMode and CheckDataSet then
    begin
      if FShowPictureFields then
        ActiveChange(true)
      else
        RemoveAllPictures;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetSelectedField: TField;
begin
  Result := FieldAtColumn[RealColIndex(Col)];
end;

function TDBAdvGrid.GetDBFieldAtColumn(ACol: integer): TField;
var
  j: integer;
  fldidx: integer;
begin
  Result := nil;
  if not CheckDataSet or (ACol >= FColumnCollection.Count) then
    Exit;

  if (FColumnCollection.Items[ACol].HTMLTemplate <> '') then
    Exit;

  if FShowDefinedFields then
  begin
    if FColumnCollection.Items[ACol].FieldName <> '' then
    begin
      fldidx := FDataLink.DataSet.FieldList.IndexOf(FColumnCollection.Items[ACol].FieldName);
      if fldidx > -1 then
        Result := FDataLink.DataSet.Fields[fldidx];
    end;
  end
  else
  begin
    if not ((ACol = 0) and FShowDBIndicator and (FixedCols > 0)) then
    begin
    {j := 1;
    if not FShowDBIndicator or (FixedCols <= 0) then
      j := 0; }

      if FColumnCollection.Items[ACol].FieldName <> '' then
        Result := FDataLink.DataSet.Fieldbyname(FColumnCollection.Items[ACol].FieldName)
      else
      begin
        j := GetDBFieldIndexAtColumn(ACol);
        if (j <> -1) then
          Result := FDataLink.DataSet.Fields[j]
        else
          Result := nil;  
      end;

    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetDBFieldIndexAtColumn(ACol: integer): integer;
var
  j, i, fieldIdx: integer;
begin
  Result := -1;
  if not CheckDataSet or (ACol >= FColumnCollection.Count) then
    Exit;

  if (FColumnCollection.Items[ACol].HTMLTemplate <> '') then
    Exit;

  if FShowDefinedFields then
  begin
    if FColumnCollection.Items[ACol].FieldName <> '' then
      Result := FDataLink.DataSet.FieldList.IndexOfName(FColumnCollection.Items[ACol].FieldName);
  end
  else
  begin
    if not ((ACol < FixedCols) and FShowDBIndicator and (FixedCols > 0)) then
    begin
      j := FixedCols;
      if not FShowDBIndicator or (FixedCols <= 0) then
        j := 0;

      if FColumnCollection.Items[ACol].FieldName <> '' then
        Result := FDataLink.DataSet.FieldList.IndexOfName(FColumnCollection.Items[ACol].FieldName)
      else
      begin
        fieldIdx := 0;
        for i := 0 to FDataLink.DataSet.FieldCount - 1 do
        begin
          if FDataLink.DataSet.Fields[i].Visible then
          begin
            if fieldIdx >= (ACol - j) then
            begin
              Result := i;
              break;
            end;
            fieldIdx := fieldIdx + 1;
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.ToggleCheck(ACol, ARow: Integer;
  FromEdit: Boolean): Boolean;
var
  Fld: TField;
//OldActiveRecord:  Integer;
  OldV: Boolean;
  WasEditable: Boolean;
  ae: boolean;
begin
  if Row <> ARow then
  begin
    Row := ARow;
  end;

  Result := inherited ToggleCheck(ACol, ARow, FromEdit);

  if not Result then
    Exit;

  if (ACol >= FColumnCollection.Count) or not PageMode then
    Exit;

  if CheckDataSet then
  begin
    ae := DataSource.AutoEdit or (dsEdit = FDataLink.DataSet.State);
    if FDataLink.DataSet.CanModify and ae then
    begin
  //Row := ARow;

  //OldActive := FDataLink.ActiveRecord;
  //FDataLink.ActiveRecord := ARow - FixedRows;

      Fld := GetDBFieldAtColumn(ACol); // ColumnField RealColIndex()
      if Assigned(Fld) and (not FColumnCollection.Items[ACol].ReadOnly) and (goEditing in options) then
      begin
    {
    if (ACol = 0) and (FixedCols > 0) and FShowDBIndicator then
      exit;

    OldActiveRecord:= FDataLink.ActiveRecord;

    if (FDataLink.DataSet.State = dsInsert) or (FDataLink.DataSet.RecNo < 0)then
    begin
      FDataLink.ActiveRecord:= ARow - (self.Row - FDataLink.ActiveRecord);
    end
    else
      FDataLink.ActiveRecord:= ARow -(FDataLink.DataSet.RecNo - FDataLink.ActiveRecord);

    if (FDataLink.ActiveRecord <0) or (FDataLink.ActiveRecord >= FDataLink.BufferCount) then
    begin
      FDataLink.ActiveRecord:= OldActiveRecord;
      exit;
    end;
    }
      // FDataLink.DataSet.Edit;
        OldV := FCancelEditReturn;
        WasEditable := goEditing in Options;
        FInternalCall := true;
        FCancelEditReturn := true;
        CanModify;

        if Fld.DataType = ftBoolean then
        begin
          Fld.AsBoolean := not Fld.AsBoolean;
          SetCheckBoxState(ACol, ARow, Fld.AsBoolean);
        end
        else if FColumnCollection.Items[ACol].CheckBoxField then
        begin
          if UpperCase(FColumnCollection.Items[ACol].CheckTrue) = UpperCase(Fld.AsString) then
          begin
            Fld.AsString := FColumnCollection.Items[ACol].CheckFalse;
            SetCheckBoxState(ACol, ARow, false);
          end
          else
          begin
            Fld.AsString := FColumnCollection.Items[ACol].CheckTrue;
            SetCheckBoxState(ACol, ARow, true);
          end;
        end
        else if FColumnCollection.Items[ACol].Editor = edDataCheckBox then
        begin
          if UpperCase(FColumnCollection.Items[ACol].CheckTrue) = UpperCase(Fld.AsString) then
          begin
            Fld.AsString := FColumnCollection.Items[ACol].CheckFalse;
            SetCheckBoxState(ACol, ARow, false);
          end
          else
          begin
            Fld.AsString := FColumnCollection.Items[ACol].CheckTrue;
            SetCheckBoxState(ACol, ARow, true);
          end;
        end;
       {
    else
    begin
      Flg := Fld.Value;
      Flg := not Flg;
      Fld.Value := Integer(Flg);
      SetCheckBoxState(ACol,ARow,flg);
    end;
    }
        if EditPostMode = epCell then
        begin
          FDataLink.DataSet.Post;
        end
        else // EditPostMode = epRow
        begin

        end;

        FCancelEditReturn := OldV;
        FInternalCall := false;

        if WasEditable and not(goEditing in Options) then
          Options := Options + [goEditing];

        Result := True;
    //FDataLink.ActiveRecord := OldActiveRecord;
      end
      else
      begin
        //GetCheckBoxState(ACol, ARow, flg);
        //SetCheckBoxState(ACol, ARow, not flg);
      end;

  //FDataLink.ActiveRecord := OldActiveRecord;
    end
    else
      Result := not Result;

  end;

end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetFixedRowsEx: Integer;
begin
  Result := inherited FixedRows;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetFixedRowsEx(const Value: Integer);
begin
  if Value <> FixedRows then
  begin
    if (csDesigning in ComponentState) then
      if (Value > (inherited FixedRows)) and (Value >= RowCount) then
        if not CheckDataSet then
          RowCount := Value + 1;

    if (Value >= 1) then
      inherited FixedRows := Value;

    if CheckDataSet then
      ActiveChange(FDataLink.DataSet.Active);
  end;
end;

//------------------------------------------------------------------------------
procedure TDBAdvGrid.DoInvalidPicture(Col, Row: integer);
begin
  FPicture.Assign(FInvalidPicture);
  if Assigned(OnInvalidPicture) then
    OnInvalidPicture(Self, Col, Row);
end;
//------------------------------------------------------------------------------

function TDBAdvGrid.DoAllowFmtPaste: boolean;
begin
  Result := false;
end;

procedure TDBAdvGrid.DoAppendRow;
begin
end;

procedure TDBAdvGrid.DoCanEditCell(ACol, ARow: Integer; var CanEdit: boolean);
begin
  inherited;

  if (ACol >= Columns.Count) then
    Exit;

  if Columns[ACol].ReadOnly then
    CanEdit := false;
end;

procedure TDBAdvGrid.DoCheckBoxClick(ACol, ARow: integer; AState: boolean);
begin
  inherited;
  RepaintRow(ARow);
end;

procedure TDBAdvGrid.DoF2Edit;
begin
  inherited;
  ShowInplaceEdit;
end;


procedure TDBAdvGrid.PasteInCell(ACol, ARow: Integer; Value: string);
var
  rc {, r}: Integer;
  Fld: TField;
  OldV: Boolean;
  aField: TField;
  CanEdit: Boolean;
begin
//inherited;
  if not PageMode then
  begin
    inherited PasteInCell(ACol,ARow,Value);
    Exit;
  end;

  rc := RealColIndex(ACol);

  if (rc >= FColumnCollection.Count) then
    Exit;

  aField := GetDBFieldAtColumn(rc);
  if (ShowBooleanFields and (aField <> nil) and (aField.DataType = ftBoolean))
    or (ShowPictureFields and (aField <> nil) and (aField.DataType = ftGraphic))
    or (FColumnCollection.Items[rc].PictureField) or FColumnCollection.Items[rc].CheckBoxField then
    exit;

  Fld := GetDBFieldAtColumn(rc);

  if Fld <> nil then
  begin
    if (rc = 0) and (FixedCols > 0) and FShowDBIndicator then
      exit;

    if FShowDefinedFields and (FColumnCollection.Items[rc].FieldName = '') then
      Exit;

    if DataSetType = dtSequenced then
    begin
      Row := ARow;
    end
    else // DataSetType = dtNonSequenced
    begin
    //Row := ARow;
   { FDataLink.DataSet.First;
    if (ARow -1) > 0 then
      FDataLink.DataSet.MoveBy(ARow-1);
    }
    end;

 { r := ARow - FDataLink.DataSet.recno;
  FDataLink.DataSet.MoveBy(r);
  }
    CanEdit := FDataLink.DataSet.State in [dsEdit, dsInsert];

    if CanEdit or self.CanModify then
    begin
      try
      {if (Fld.IsBlob) and not ShowMemoFields then
        Fld.AsString := '(MEMO)'
      else }
        Fld.AsString := Value;

      finally
      end;

      if (EditPostMode = epCell) then
      begin
        OldV := FCancelEditReturn;
        FCancelEditReturn := true;
        FDataLink.DataSet.Post;
        FCancelEditReturn := OldV;
        FEmptyDataSet := false;
      end;
    end;
  end;

// take hidden cells into account
  if rc <> ACol then
    RepaintCell(ACol, ARow);
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.RemoveAllCellValues;
var
  c, r: integer;
begin
  for r := 1 to RowCount - 1 do
  begin
    for c := 0 to FColumnCollection.Count - 1 do
      Cells[c, r] := '';
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetPageMode(const Value: Boolean);
var
  i: Integer;
begin
  if FPageMode <> Value then
  begin
    FPageMode := Value;
    if not FPageMode then
      RemoveAllGraphics
    else
      RemoveAllCellValues;
    if CheckDataSet then
      ActiveChange(FDataLink.DataSet.Active);

    if FloatingFooter.Visible and not ((csDesigning in ComponentState) or (csLoading in ComponentState)) then
    begin
      for i:= FixedCols to ColCount -1 do
        CalcFooter(i);
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.LoadFromDataSet;
var
  c, r, i: integer;
  aField: TField;
  value, HTMLTemplate: string;
  aPicture: TPicture;
  sp: TPoint;
  cb: boolean;
  bm: TBookMark;
begin
  if CheckDataSet and not PageMode and Assigned(FColumnCollection) and (FColumnCollection.Count > 0) then
  begin
    if Assigned(FPictureList) then
    begin
      for i := 0 to FPictureList.Count - 1 do
        TPicture(FPictureList.Items[i]).Free;
      FPictureList.Clear;
    end;

    i := 1;
    if not FShowDBIndicator or (FixedCols <= 0) then
      i := 0;

    if FixedRows > 0 then
    begin
      for c := i to FColumnCollection.Count - 1 do
      begin
        aField := GetDBFieldAtColumn(c);
        if (FColumnCollection.Items[c].Header <> '') then
          Cells[c, FixedRows - 1] := FColumnCollection.Items[c].Header
        else if (aField <> nil) and aField.Visible then
          Cells[c, FixedRows - 1] := aField.DisplayName;
      end;
    end;

    r := FixedRows;
    if FDataLink.DataSet.Bof and FDataLink.DataSet.Eof then
      bm := nil
    else
      bm := FDataLink.DataSet.GetBookMark;
    FDataLink.DataSet.First;
    while not FDataLink.DataSet.Eof do
    begin
      for c := i to FColumnCollection.Count - 1 do
      begin
        Value := '';
        try
          if FColumnCollection.Items[c].HTMLTemplate <> '' then
          begin
            HTMLTemplate := FColumnCollection.Items[c].HTMLTemplate;

            if Assigned(OnGetHTMLTemplate) then
              OnGetHTMLTemplate(Self, c, r, HTMLTemplate, FDataLink.DataSet.Fields);

            Value := HTMLDBReplace(HTMLTemplate, FDataLink.DataSet, c, r);
          end
          else
          begin
            aField := GetDBFieldAtColumn(c);

            if (aField <> nil) and (aField.Visible) then
            begin
              if true {not ((ShowBooleanFields and (aField.DataType = ftBoolean))
                or (ShowPictureFields and (aField.DataType = ftGraphic))
                or (FColumnCollection.Items[c].PictureField) or FColumnCollection.Items[c].CheckBoxField) }then
              begin
                if (aField.IsBlob) then
                begin
                  if (aField.DataType = ftGraphic)
                    or (FColumnCollection.Items[c].PictureField) then
                  begin
                    if ShowPictureFields then
                    begin
                      if (aField.DataType = ftBlob) and (FColumnCollection.Items[c].PictureField) and ShowPictureFields then
                      begin
                        aPicture := TPicture.Create;
                        FPicture.Assign(nil);
                        BlobFieldToStream(TBlobField(aField), sp, c,r); // Sets FPicture
                        aPicture.Assign(FPicture);
                        AddPicture(c, r, aPicture, true, FColumnCollection.Items[c].PictureStretch, 4, haCenter, vaCenter);
                        FPictureList.Add(aPicture);
                      end
                      else
                      begin
                        aPicture := TPicture.Create;
                        aPicture.Assign(TPicture(aField));
                        AddPicture(c, r, aPicture, true, FColumnCollection.Items[c].PictureStretch, 4, haCenter, vaCenter);
                        FPictureList.Add(aPicture);
                      end;
                    end
                    else
                      Value := '(GRAPHIC)';
                  end
                  else
                  begin
                    if not ShowMemoFields then
                      Value := '(MEMO)'
                    else
                      Value := AField.AsString;
                  end;
                end
                else
                begin
                  if (ShowBooleanFields and (AField.DataType = ftBoolean)) then
                  begin
                    cb := UpperCase(AField.AsString) = 'TRUE';
                    AddCheckBox(c, r, cb, true);
                    // for filtering purposes
                    if cb then
                      value := FColumnCollection.Items[c].CheckTrue
                    else
                      value := FColumnCollection.Items[c].CheckFalse;
                  end
                  else if FColumnCollection.Items[c].CheckBoxField then
                  begin
                    cb := UpperCase(AField.AsString) = UpperCase(FColumnCollection.Items[c].CheckTrue);
                    AddCheckBox(c, r, cb, true);
                    if cb then
                      value := FColumnCollection.Items[c].CheckTrue
                    else
                      value := FColumnCollection.Items[c].CheckFalse;
                  end
                  else if (FColumnCollection.Items[c].ProgressField) then
                  begin
                    Value := aField.DisplayText;
                    AddProgressEx(c,r,FColumnCollection.Items[c].ProgressColor,
                                      FColumnCollection.Items[c].ProgressTextColor,
                                      FColumnCollection.Items[c].ProgressBKColor,
                                      FColumnCollection.Items[c].ProgressTextBKColor);

                  end
                  else if (FColumnCollection.Items[c].DataImageField) then
                  begin
                    Value := aField.DisplayText;
                    AddDataImage(c,r,0,haBeforeText, vaTop)
                  end
                  else
                  begin
                    if (aField.DataType = ftWideString) and (FColumnCollection.Items[c].ShowUnicode) then
                      Value := EncodeWideStr(TWideStringField(aField).Value)
                    else
                      Value := aField.DisplayText;
                  end;
                end;
              end;
            end;
          end;
        finally
          self.Cells[c, r] := Value;
        end;
      end;
      r := r + 1;
      FDataLink.DataSet.Next;
    end;

    if Assigned(bm) then
    begin
      FDataLink.DataSet.GotoBookMark(bm);
      FDataLink.DataSet.FreeBookMark(bm);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.RemoveAllPictures;
var
  c, r, i: integer;
  aField: TField;
begin
  if not PageMode then
  begin
    i := 1;
    if not FShowDBIndicator or (FixedCols <= 0) then
      i := 0;

    for r := 1 to RowCount - 1 do
    begin
      for c := i to FColumnCollection.Count - 1 do
      begin
        if FColumnCollection.Items[c].HTMLTemplate <> '' then
        begin

        end
        else
        begin
          aField := GetDBFieldAtColumn(c);
          if aField <> nil then
          begin
            if (aField.IsBlob) then
            begin
              if (aField.DataType = ftGraphic)
                or (FColumnCollection.Items[c].PictureField) then
              begin
                self.RemovePicture(c, r);
                Cells[c, r] := '(GRAPHIC)';
              end;
            end;
          end;
        end;
      end;
    end;
    if Assigned(FPictureList) then
    begin
      for i := 0 to FPictureList.Count - 1 do
        TPicture(FPictureList.Items[i]).Free;
      FPictureList.Clear;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.RemoveAllBooleanCheckBoxes;
var
  c, r, i: integer;
  aField: TField;
begin
  if not PageMode then
  begin
    i := 1;
    if not FShowDBIndicator or (FixedCols <= 0) then
      i := 0;

    for c := i to FColumnCollection.Count - 1 do
      RemoveCheckBox(i,0);

    for r := FixedRows to RowCount - 1 do
    begin
      for c := i to FColumnCollection.Count - 1 do
      begin
        if FColumnCollection.Items[c].HTMLTemplate <> '' then
        begin

        end
        else
        begin
          aField := GetDBFieldAtColumn(c);
          if aField <> nil then
          begin
            if (aField.DataType = ftBoolean) then
            begin
              if self.CellGraphics[c, r] <> nil then
              begin
                if CellGraphics[c, r].CellBoolean then
                  Cells[c, r] := 'True'
                else
                  Cells[c, r] := 'False';
                RemoveCheckBox(c, r);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.RemoveAllGraphics;
begin
  if not PageMode then
  begin
    RemoveAllPictures;
    RemoveAllBooleanCheckBoxes;
    RemoveAllStringCheckBoxes;
  end;
end;

//------------------------------------------------------------------------------
{
procedure TDBAdvGrid.UpdateForPageModeForCheckBoxColumn;
begin
if CheckDataSet and not PageMode then
begin
  RemoveAllStringCheckBoxes;
  ActiveChange(true);
end;
end;
}
//------------------------------------------------------------------------------

procedure TDBAdvGrid.RemoveAllStringCheckBoxes;
var
  c, i: integer;
begin
  if not PageMode then
  begin
    i := 1;
    if not FShowDBIndicator or (FixedCols <= 0) then
      i := 0;

    for c := i to FColumnCollection.Count - 1 do
    begin
      if FColumnCollection.Items[c].HTMLTemplate <> '' then
      begin

      end
      else
      begin
        RemoveStringCheckBox(c);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.RemoveStringCheckBox(ACol: Integer);
var
  r: integer;
  aField: TField;
begin
  if not PageMode then
  begin
    aField := GetDBFieldAtColumn(ACol);
    if aField <> nil then
    begin
      for r := 1 to RowCount - 1 do
      begin
        if FColumnCollection.Items[ACol].CheckBoxField then
        begin
          if self.CellGraphics[ACol, r] <> nil then
          begin
            if CellGraphics[ACol, r].CellBoolean then
              Cells[ACol, r] := FColumnCollection.Items[ACol].CheckTrue
            else
              Cells[ACol, r] := FColumnCollection.Items[ACol].CheckFalse;
            RemoveCheckBox(ACol, r);
          end;
        end;
      end;
    end;
  end;
end;


//------------------------------------------------------------------------------

procedure TDBAdvGrid.AddStringCheckBox(ACol: integer);
var
  r: integer;
begin
  if not PageMode then
  begin
    for r := 1 to RowCount - 1 do
    begin
      if FColumnCollection.Items[ACol].CheckBoxField then
      begin
        if self.CellGraphics[ACol, r] = nil then
        begin
          AddCheckBox(Acol, r, uppercase(Cells[ACol, r]) = uppercase(FColumnCollection.Items[ACol].CheckTrue), false);
          Cells[ACol, r] := '';
        end;
      end;
    end;
  end;
end;

procedure TDBAdvGrid.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TDBAdvGrid) then
  begin
    DataSource := (Source as TDBAdvGrid).DataSource;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.AssignCells(Source: TPersistent);
begin
  // do nothing
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.QueryAddRow(var AllowAdd: Boolean);
begin
  inherited;
{
FInternalInsert := true;
FDataLink.DataSet.Append;// .Insert;
FInternalInsert := false;
}

  if AllowAdd and Assigned(DataSource) and Assigned(DataSource.DataSet) and (DataSource.DataSet.State in [dsEdit, dsInsert]) and (EditPostMode = epRow) then
    DataSource.DataSet.Post;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.LoadLookupList(fld: TField; list: TStrings);
var
  s, k: string;
  i: Integer;
begin
  if Assigned(fld.LookupDataSet) and ({(EditPostMode = epCell) or} (Assigned(DataSource) and Assigned(DataSource.DataSet) {and (DataSource.DataSet.State = dsBrowse)})) then
  begin
    fld.LookupDataSet.DisableControls;
    fld.LookupDataSet.First;
    i := 1;
    FLookupKeys.Clear;
    list.Clear;

    while not fld.LookupDataSet.Eof do
    begin
      s := fld.LookupDataSet.FieldByName(fld.LookupResultField).AsString;
      k := fld.LookupDataSet.FieldByName(fld.LookupKeyFields).AsString;
      FLookupKeys.AddObject(k, TObject(i));
      list.AddObject(s, TObject(i));
      inc(i);
      fld.LookupDataSet.Next;
    end;
    fld.LookupDataSet.EnableControls;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI2006_LVL}
procedure TDBAdvGrid.LoadWideLookupList(fld: TField; list: TWideStrings);
var
  s, k: widestring;
  i: Integer;
begin
  if Assigned(fld.LookupDataSet) and ({(EditPostMode = epCell) or} (Assigned(DataSource) and Assigned(DataSource.DataSet) {and (DataSource.DataSet.State = dsBrowse)})) then
  begin
    fld.LookupDataSet.DisableControls;
    fld.LookupDataSet.First;
    i := 1;
    FLookupKeys.Clear;
    list.Clear;

    while not fld.LookupDataSet.Eof do
    begin
      s := fld.LookupDataSet.FieldByName(fld.LookupResultField).AsWideString;
      k := fld.LookupDataSet.FieldByName(fld.LookupKeyFields).AsWideString;
      FLookupKeys.AddObject(k, TObject(i));
      list.AddObject(s, TObject(i));
      inc(i);
      fld.LookupDataSet.Next;
    end;
    fld.LookupDataSet.EnableControls;
  end;
end;
{$ENDIF}
//------------------------------------------------------------------------------

function TDBAdvGrid.GetLookupKey(i: Integer): string;
var
  j: Integer;
begin
  Result := '';
  for j := 1 to FLookupKeys.Count do
  begin
    if (integer(FLookupKeys.Objects[j - 1]) = i) then
    begin
      Result := FLookupKeys[j - 1];
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetCurrentCell: string;
var
  AEditor: TEditorType;
begin
  if CheckDataSet then
  begin
    if (FDataLink.DataSet.State in [dsEdit, dsInsert]) or ((FOldState = dsInsert) and not FRecordChanged)
       or (FEditEnding and (DataSetType = dtNonSequenced) and (EditPostMode = epCell)) then
    begin
      Result := inherited GetCurrentCell;

      if (Result = '') and (FDataLink.DataSet.State = dsEdit) then
      begin
        GetCellEditor(RealColIndex(Col), Row, AEditor);
        if (AEditor = edButton) then
          Result := Cells[RealColIndex(Col), Row];
      end;

      // fix for Escap issue
      {if (Result = '') and (FDataLink.DataSet.State = dsEdit) then
      begin
        GetCellEditor(RealColIndex(Col), Row, AEditor);
        if not (AEditor in [edNormal, edPositiveNumeric, edNumeric]) then
          Result := Cells[RealColIndex(Col), Row];
      end; }
    end
    else
      Result := Cells[RealColIndex(Col), Row];
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.HandleDesignChoice(X,Y: Integer);
begin
  case MouseOverDesignChoice(X, Y) of
  1: AddAllFields;
  2: RemoveAllFields;
  3: RemoveAllColumns;
  end;
end;

procedure TDBAdvGrid.PaintDesigner;
var
  r: TRect;
  fh, i: Integer;
  P: TPoint;
begin  
  if (csDesigning in ComponentState) and ShowDesignHelper then
  begin
    r := ClientRect;
    Canvas.Font.Name := 'Tahoma';
    Canvas.Font.Size := 8;
    Canvas.Brush.Color := clInfoBk;
    Canvas.Pen.Color := clGray;
    r.Left := r.Right - 150;
    r.Top := r.Bottom - 70;
    Canvas.Rectangle(r);
    Canvas.Font.Color := clNavy;
    Canvas.Font.Style := [fsUnderline];
    fh := Canvas.TextHeight('gh') + 2;
    Canvas.TextOut(r.Left + 4, r.Top + 4, s_QuickConfig+ '['+ GetVersion+']');

    GetCursorPos(P);
    P := ScreenToClient(P);

    i := MouseOverDesignChoice(P.X, P.Y);

    if i = 1 then
      Canvas.Font.Style := [fsUnderline]
    else
      Canvas.Font.Style := [];

    Canvas.TextOut(r.Left + 4, r.Top + 4 + fh, s_AddAllFields);

    if i = 2 then
      Canvas.Font.Style := [fsUnderline]
    else
      Canvas.Font.Style := [];

    Canvas.TextOut(r.Left + 4, r.Top + 4 + 2 * fh, s_RemoveAllFields);

    if i = 3 then
      Canvas.Font.Style := [fsUnderline]
    else
      Canvas.Font.Style := [];

    Canvas.TextOut(r.Left + 4, r.Top + 4 + 3 * fh, s_RemoveAllColumns);

  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  r: TRect;
  p: TPoint;
  nc: Integer;
begin
  inherited;

  if (csDesigning in ComponentState) and ShowDesignHelper then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    nc := MouseOverDesignChoice(P.X, P.Y);

    if nc <> FLastDesignChoice then
    begin
      r := ClientRect;
      r := Rect(r.Right - 150, r.Bottom - 70, r.Right, r.Bottom);
      InvalidateRect(Handle, @r, true);
    end;

    FLastDesignChoice := nc;

    if nc in [1, 2, 3] then
      Msg.Result := 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.AddAllFields;
var
  {$IFNDEF DELPHIXE3_LVL}
  {$IFNDEF DELPHI2006_LVL}
  sl: TStringList;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  sl: TWideStringList;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  sl: TStringList;
  {$ENDIF}

  i, j: Integer;
begin
  if not Assigned(DataSource) then
  begin
    ShowMessage('Cannot add fields. No datasource assigned');
    Exit;
  end;

  if not Assigned(DataSource.DataSet) then
  begin
    ShowMessage('Cannot add fields. No dataset specified for datasource');
    Exit;
  end;

  {$IFNDEF DELPHIXE3_LVL}
  {$IFNDEF DELPHI2006_LVL}
  sl := TStringList.Create;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  sl := TWideStringList.Create;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  sl := TStringList.Create;
  {$ENDIF}

  DataSource.DataSet.GetFieldNames(sl);

{  // Removing Invisible Fields
  i := 0;
  while (i < sl.Count) do
  begin
    if not DataSource.DataSet.FieldByName(sl[i]).Visible then
    begin
      sl.Delete(i);
    end
    else
      Inc(i);
  end;
}
  if ShowDBIndicator and (FixedCols > 0) then
    j := 1
  else
    j := 0;

  if (sl.Count > 0) and (sl.Count + j < Columns.Count) then
  begin
    while (sl.Count + j < Columns.Count) and (Columns.Count > j) do
      Columns[Columns.Count - 1].Free;
  end;

  for i := 1 to sl.Count do
  begin
    if Columns.Count > i - 1 + j then
    begin
      Columns[i - 1 + j].FieldName := sl[i - 1];
      if not PageMode then
        Columns[i - 1 + j].Header := sl[i - 1];
    end
    else
    begin
      with Columns.Add do
      begin
        FieldName := sl[i - 1];
        if not PageMode then
          Header := sl[i - 1];
      end;
    end;
  end;
  sl.Free;
  
  DesignerUpdate;
  
  Columns.SetOrganization;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.LoadColPositions;
begin
  LoadColumnPositions(ColumnSize.Key, ColumnSize.Section);
end;

procedure TDBAdvGrid.LoadColumnPositions(Key, Section: string);
var
  IniFile: TIniFile;
  i,j: Integer;
  il: TIntList;
begin

  IniFile := TIniFile.Create(Key);

  // skip when there are invalid entries in the INI file
  for i := 1 to Columns.Count do
  begin
    if IniFile.ReadInteger(Section,'CP'+IntToStr(i - 1),-1) = - 1 then
      Exit;
  end;

  ResetColumnOrder;

  il := TIntList.Create(-1,-1);

  for i := 1 to Columns.Count do
  begin
    j := IniFile.ReadInteger(Section,'CP'+IntToStr(i - 1),0);
    Columns[j].DefIdx := i - 1;
    il.Add(j);
  end;
  IniFile.Free;

  UnHideColumnsAll;
  Columns.ResetOrganization;

  for i := 0 to Columns.Count - 1 do
  begin
    Columns[i].DefIdx := il.Items[i];
  end;

  il.Free;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SaveColPositions;
begin
  SaveColumnPositions(ColumnSize.Key, ColumnSize.Section);
end;

procedure TDBAdvGrid.SaveColumnPositions(Key, Section: string);
var
  IniFile: TIniFile;
  i: Integer;
begin
  IniFile := TIniFile.Create(Key);

  for i := 1 to Columns.Count do
  begin
    IniFile.WriteInteger(Section,'CP'+IntToStr(i - 1),Columns[i - 1].DefIdx);
  end;
  IniFile.Free;
end;


//------------------------------------------------------------------------------

procedure TDBAdvGrid.RemoveAllColumns;
var
  i: Integer;
  cnt: integer;
begin
  for i := 1 to Columns.Count do
  begin
    Columns[i - 1].FieldName := '';
    Columns[i - 1].Header := '';
  end;

  if FixedColAlways then
    cnt := 1
  else
    cnt := 2;

  while Columns.Count > cnt do
  begin
    Columns[Columns.Count - 1].Free;
  end;


  Clear;
  RowCount := 5;
  if FixedColAlways then
    ColCount := FixedCols;

  GetParentForm(Self);
  DesignerUpdate;
end;


//------------------------------------------------------------------------------

procedure TDBAdvGrid.RemoveAllFields;
var
  i: Integer;
  flds: Boolean;
begin
  flds := false;
  for i := 1 to Columns.Count do
  begin
    if Columns[i - 1].FieldName <> '' then
      flds := true;
  end;

  if (csDesigning in ComponentState) and not flds then
    ShowMessage('No fields specified');

  for i := 1 to Columns.Count do
  begin
    if Columns[i - 1].Header = Columns[i - 1].FieldName then
      Columns[i - 1].Header := '';

    Columns[i - 1].FieldName := '';
  end;
  DesignerUpdate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.DesignerUpdate;
var
  Form: TCustomForm;
begin
  if csDesigning in ComponentState then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.Designer <> nil) then
      Form.Designer.Modified;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.MouseOverDesignChoice(X, Y: Integer): Integer;
var
  r: TRect;
  fh: Integer;
begin
  Result := -1;
  r := ClientRect;
  Canvas.Font.Name := 'Tahoma';
  Canvas.Font.Size := 8;

  fh := Canvas.TextHeight('gh') + 2;
  if (x > r.Right - 150) and (x < r.Right - 150 + Canvas.TextWidth(s_AddAllFields)) and
    (y > r.Bottom - 70 + fh + 4) and
    (y < r.Bottom - 70 + 2 * fh + 4) then
    Result := 1;

  if (x > r.Right - 150) and (x < r.Right - 150 + Canvas.TextWidth(s_RemoveAllFields)) and
    (y > r.Bottom - 70 + 2 * fh + 4) and
    (y < r.Bottom - 70 + 3 * fh + 4) then
    Result := 2;

  if (x > r.Right - 150) and (x < r.Right - 150 + Canvas.TextWidth(s_RemoveAllColumns)) and
    (y > r.Bottom - 70 + 3 * fh + 4) and
    (y < r.Bottom - 70 + 4 * fh + 4) then
    Result := 3;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.WMLButtonDown(var Msg: TWMLButtonDown);
var
  R, C: Integer;
  I: Integer;
begin
  MouseToCell(Msg.XPos, Msg.YPos, C, R);

  i := 0;

  if FloatingFooter.Visible and (FloatingFooter.FooterStyle = fsFixedLastRow) then
    Inc(i);

  if (R >= TopRow + VisibleRowCount - i) and (SearchFooter.Visible = false) then
  begin
    if Assigned(OnClickCell) then
      OnClickCell(Self, R, C);

    Exit;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R, C, RC: Integer;
  CR: TRect;
  GC: TDBGridColumnItem;
  PT: TPoint;
begin
  inherited;

  if (csDesigning in ComponentState) and ShowDesignHelper then
  begin
    HandleDesignChoice(X,Y);
  end
  else
  begin
    MouseToCell(X,Y,C,R);
    RC := RealColIndex(C);
    if (R <> -1) and (C <> -1) and (RC < FColumnCollection.Count) and (RC >= 0) then
    begin
      GC := FColumnCollection.Items[RC];
      CR := CellRect(C,R);

      PT := ClientToScreen(Point(CR.Left,CR.Bottom));

      if (Button = mbLeft) and Assigned(GC.ColumnPopup) and
         (GC.ColumnPopupType in [cpFixedCellsLClick,cpNormalCellsLClick,cpAllCellsLClick]) then
      begin
        if Assigned(FOnColumnPopup) then
          FOnColumnPopup(Self,RC,R,GC.ColumnPopup);

        if (R < FixedRows) and (GC.ColumnPopupType in [cpFixedCellsLClick,cpAllCellsLClick]) then
        begin
          GC.ColumnPopup.Popup(PT.X,PT.Y);
        end;

        if (R >= FixedRows) and (GC.ColumnPopupType in [cpNormalCellsLClick,cpAllCellsLClick]) then
        begin
          GC.ColumnPopup.Popup(PT.X,PT.Y);
        end;
      end;

      if (Button = mbRight) and Assigned(GC.ColumnPopup) and
         (GC.ColumnPopupType in [cpFixedCellsRClick,cpNormalCellsRClick,cpAllCellsRClick]) then
      begin
        if Assigned(FOnColumnPopup) then
          FOnColumnPopup(Self,RC,R,GC.ColumnPopup);

        if (R < FixedRows) and (GC.ColumnPopupType in [cpFixedCellsRClick,cpAllCellsRClick]) then
        begin
          GC.ColumnPopup.Popup(PT.X,PT.Y);
        end;

        if (R >= FixedRows) and (GC.ColumnPopupType in [cpNormalCellsRClick,cpAllCellsRClick]) then
        begin
          GC.ColumnPopup.Popup(PT.X,PT.Y);
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetDataSetType(const Value: TDataSetType);
begin
  if Value <> FDataSetType then
  begin
    FDataSetType := Value;
    if CheckDataSet then
    begin
      FDataLink.DataSet.Active := false;
      FDataLink.DataSet.Active := true;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetDBRow: Integer;
begin
  Result := inherited Row;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetDBRow(const Value: Integer);
var
  OldRow: integer;
begin
  if Value < FixedRows then
    exit;

  OldRow:= Row;

  if DataSetType = dtSequenced then
    inherited Row := Value
  else
  begin
    if (Value <= VisibleRowCount + {1}FixedRows) then
    begin
      inherited Row := Value;
    end;
  end;

  if Assigned(FOnRowChanged) and (OldRow <> Row) then
    FOnRowChanged(self, OldRow, Row);

  //PostEditRecData;
end;

procedure TDBAdvGrid.DirectWheelChange(delta: integer; var SuppressMsg: Boolean);
begin
  if PageMode and (DataSetType <> dtSequenced) then
    SuppressMsg := False;
  {
  if not ((DataSetType = dtSequenced) or not PageMode) then
  begin
  if delta > 0 then
    DBWheelDown
  else
    DBWheelUp;
  end;
  }
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.DBWheelDown: boolean;
var
  i, j: integer;
begin
  i := 0;
  j := 1;

  if FMouseWheelScrolled then
    j := 0;

  if EditMode then
  begin
    HideInplaceEdit;
    SetFocus;
  end;

  if Assigned(FDataLink) and Assigned(FDataLink.Dataset) then
    i := FDataLink.DataSet.MoveBy(j); // 0
    
  if (RowCount - 1) > VisibleRowCount then
  begin
    if i < 1 then
    begin
      TopRow := FixedRows + 1;
      UpdateScrollBar(1);
    end
    else
      TopRow := FixedRows + 1;
  end;

  if Assigned(FDataLink) and Assigned(FDataLink.Dataset) then
    Row := min(FDataLink.ActiveRecord + FixedRows, RowCount - 1);
  //if i < 1 then
  //TopRow:= 2;
  Result := true;
  FMouseWheelScrolled := False;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  Handled: boolean;
begin
  if (DataSetType = dtSequenced) or not PageMode then
    Result := inherited DoMouseWheelDown(Shift, MousePos)
  else // DataSetType = dtNonSequenced
  begin
    //Row:= Row + 1;
    //TopRow:= 1;
    Handled := false;
    Result := false;

    if Assigned(OnMouseWheelDown) then
      OnMouseWheelDown(Self, Shift, MousePos, Handled);

    if not Handled then
      Result := DBWheelDown;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.DBWheelUp: boolean;
var
  j: Integer;
begin
  j := -1;
  if FMouseWheelScrolled then
    j := 0;

  TopRow := FixedRows;

  if EditMode then
  begin
    HideInplaceEdit;
    SetFocus;
  end;

  if Assigned(FDataLink) and Assigned(FDataLink.Dataset) then
    FDataLink.DataSet.MoveBy(j);

  Result := true;
end;

function TDBAdvGrid.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  Handled: boolean;
begin
  if (DataSetType = dtSequenced) or not PageMode then
    Result := inherited DoMouseWheelUp(Shift, MousePos)
  else // DataSetType = dtNonSequenced
  begin
    Handled := false;
    Result := false;

    if Assigned(OnMouseWheelUp) then
      OnMouseWheelUp(Self, Shift, MousePos, Handled);

    if not Handled then
      Result := DBWheelUp;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.WndProc(var Message: tMessage);
var
  bu: boolean;
begin
  if ((message.msg = WM_MOUSEWHEEL) or (message.msg = WM_MBUTTONDOWN)) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not (csReading in ComponentState) and not (csDestroying in ComponentState) then
  begin
    if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active and DataSource.DataSet.ControlsDisabled and PageMode then
      Exit;
  end;

  bu := false;

  if (DataSetType = dtNonSequenced) and not FMouseWheelScrolling and EnableWheel then
  begin
    if (message.msg = WM_MOUSEWHEEL) then
    begin
      FBlockCallBack := true;
      FMouseWheelScrolling := true;
      FMouseWheelScrolled := False;
      bu := true;
      BeginUpdate;
    end;
  end;

  inherited;

  if (DataSetType = dtNonSequenced) and bu then
  begin
    FBlockCallBack := false;
    EndUpdate;
    FMouseWheelScrolling := false;
  end;
end;

procedure TDBAdvGrid.Zoom(x: Integer);
begin
  BeginUpdate;
  inherited;

  if PageMode then
    DefaultRowheight := RowHeights[FixedRows];

  EndUpdate;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetColumnByName(AValue: string): TDBGridColumnItem;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < Columns.Count do
  begin
    if Columns.Items[i].Name = AValue then
    begin
      Result := Columns.Items[i];
      Break;
    end;
    inc(i);
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetColumnCheckFalse(ACol: Integer): string;
begin
  Result := Columns[ACol].CheckFalse;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetColumnCheckTrue(ACol: Integer): string;
begin
  Result := Columns[ACol].CheckTrue;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetColumnCollection: TDBGridColumnCollection;
begin
  Result := FColumnCollection;
end;

//------------------------------------------------------------------------------

function TDBAdvGrid.GetColumnByFieldName(AValue: string): TDBGridColumnItem;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < Columns.Count do
  begin
    if (StrIComp(PChar(Columns.Items[i].FieldName), PChar(AValue)) = 0) then
    begin
      Result := Columns.Items[i];
      Break;
    end;
    inc(i);
  end;

end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.RowChangingEvent(Sender: TObject; OldRow,
  NewRow: Integer; var Allow: boolean);
//var
  //CanChange: boolean;
begin
  //CanChange:= true;
  if Assigned(FOnRowChanging) {and not FDoNotCountRow} then
    FOnRowChanging(self, OldRow, NewRow, {CanChange}Allow);

  FAllowRowChange := Allow;

  if Assigned(FDataLink.Dataset) then
  begin
    if (FDataLink.DataSet.State = dsInsert) and not FDataLink.DataSet.Modified and (NewRow >= RowCount-1)
       and not FAppending then
    begin
      Allow := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.GetEditTextEvent(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  SIOld, SINew: TScrollInfo;
  C,R,d: integer;
  LastRowClicked: Boolean;
begin
  inherited;
  if CheckDataSet and HandleAllocated and not FDoNotUpdateMe and PageMode then
    if (DataSetType = dtNonSequenced) and (RowCount <> VisibleRowcount + FixedRows) then
    begin
      SIOld.cbSize := sizeof(SIOld);
      SIOld.nMax := -1;
      SIOld.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, SB_VERT, SIOld);
      SINew := SIOld;
      SINew.nMin := 0;
      SINew.nPage := 0;
      SINew.nMax := 4;
      if FDataLink.BOF then SINew.nPos := 0
      else if FDataLink.EOF then SINew.nPos := 4
      else SINew.nPos := 2;
      if ((SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
        (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos))
        and (SIOld.nMax > 0) then
        SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
    end;

   MouseToCell(X,Y,C,R);

   if FloatingFooter.Visible or SearchFooter.Visible then
     LastRowClicked := (R = TopRow + VisibleRowCount - 1)
   else
     LastRowClicked := (R = TopRow + VisibleRowCount);

   //if ShowDBIndicator and (FixedCols > 0) and (C = 0) and (R >= 0) then
   if (R <> Row) and (R >= 0) and PageMode and (LastRowClicked or (ShowDBIndicator and (FixedCols > 0) and (C = 0))) then
   begin
     if Assigned(DataSource) then
       if Assigned(DataSource.Dataset) then
       begin
         if Assigned(FOnRowChanging) then
           SelectCell(C, R)
         else
         begin
           d := R - Row;
           DataSource.DataSet.MoveBy(d);
         end;

         if MouseActions.DisjunctRowSelect and (ShowDBIndicator and (FixedCols > 0) and (C = 0)) then
         begin
           ClearRowSelect;
           Row := R;
           SelectToRowSelect(false);
         end;
       end;
   end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.UpdateOnSelection(var GR: TGridRect);
begin
  inherited;
  if DataSetType = dtSequenced then
    Row:= GR.Bottom
  else
  begin
    Row:= GR.Top;
    GR := TGridRect(Rect(GR.Left, GR.bottom, GR.right, GR.Top));
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetDatasetTypeAuto(const Value: Boolean);
begin
  FDatasetTypeAuto := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetEditPostMode(const Value: TEditPostMode);
begin
  FEditPostMode := Value;
end;

//------------------------------------------------------------------------------
(*
procedure TDBAdvGrid.PostEditRecData;
var
  bm: TBookMark;
  i: integer;
  aField: TField;
begin
  if FEditPostMode = epRow then
  begin
    if FEditUpdating then
      exit;

    if FDataLink.DataSet.State = dsBrowse then
    begin
      if (FEditRec >=0) then
      begin
        FEditUpdating:= true;
        bm := FDataLink.DataSet.GetBookMark;
        FDataLink.DataSet.GotoBookMark(FEditRecBm);
        FDataLink.DataSet.FreeBookMark(FEditRecBm);

        FDataLink.DataSet.DisableControls;

        if (FOldEditingState = dsEdit) then
        begin
          FDataLink.DataSet.Edit;
        end
        else if FOldEditingState = dsInsert then
        begin
          FDataLink.DataSet.Insert;
        end;

        for i:= 0 to FEditRecData.Count-1 do
        begin
          aField:= GetDBFieldAtColumn(i);
          if (aField <> nil) and (integer(FEditRecData.Objects[i]) = 1 ) then
          begin
            if aField.FieldKind = fkLookup then
            begin
              FDataLink.DataSet.FieldByName(aField.KeyFields).AsString := FEditRecData[i];
            end
            else
            begin
              aField.AsString:= FEditRecData[i];
             { if (aField.IsBlob) and not ShowMemoFields then
                TempValue := '(MEMO)'
              else
                TempValue := Value;  }
            end;

          end;
        end;

        FDataLink.DataSet.Post;
        FEditRec:= -1;
        FEditRecData.Clear;
        FDataLink.DataSet.GotoBookMark(bm);
        FDataLink.DataSet.FreeBookMark(bm);
        FDataLink.DataSet.EnableControls;
        FEditUpdating:= false;
      end;

    end;
  end;
end;
*)
//------------------------------------------------------------------------------

function TDBAdvGrid.CanEditModify: Boolean;
begin
  if (Columns[col].FieldName = '') then
  begin
    Result := inherited CanEditModify;
    Exit;
  end;

  if (FEditPostMode = epCell) and PageMode and not EditMode then
  begin
    Result := inherited CanEditModify;

    // change for edit then post with out changing Cell
    if Result then
      Result := FDatalink.Editing;
    if Result then FDatalink.Modified;
  end
  else
  begin
    Result := False;
  //if not ReadOnly and FDatalink.Active and not FDatalink.Readonly then
  //with Columns[SelectedIndex] do
    //if (not ReadOnly) and Assigned(Field) and Field.CanModify
      //and (not (Field.DataType in ftNonTextTypes) or Assigned(Field.OnSetText)) then
    if CheckDataSet then
    begin
      FDatalink.Edit;
      Result := FDatalink.Editing;
      if Result then FDatalink.Modified;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetEditText(ACol, ARow: Integer; const Value: string);
begin
  inherited SetEditText(ACol, ARow, Value);
  FEditText := Value;
  FEditWideText := Value;
end;

//------------------------------------------------------------------------------
function TDBAdvGrid.CanEditShow: Boolean;
begin
  if FEditEnding then
  begin
    Result := False;
    Exit;
  end;

  if Assigned(FDataLink) then
  begin
    if FDataLink.Active and Assigned(FDataLink.Dataset) then
      if not FDataLink.DataSet.CanModify then
      begin
        Result := false;
        Exit;
      end;
  end;

  Result := inherited CanEditshow;
end;

//------------------------------------------------------------------------------
procedure TDBAdvGrid.SetAutoCreateColumns(const Value: Boolean);
begin
  FAutoCreateColumns := Value;
end;

//------------------------------------------------------------------------------
procedure TDBAdvGrid.SetAutoRemoveColumns(const Value: Boolean);
begin
  FAutoRemoveColumns := Value;
end;

//------------------------------------------------------------------------------
procedure TDBAdvGrid.UpdateSelectionRect(var GR: TGridRect);
begin
  inherited;
  if DataSetType = dtSequenced then
  begin
    if FloatingFooter.Visible then
      GR.Bottom := GR.Bottom - 1;
  end;
end;

//------------------------------------------------------------------------------
procedure TDBAdvGrid.UpdateActive;
var
  v: Boolean;
begin
  v := Assigned(FDataLink) and Assigned(FDataLink.DataSet) and FDataLink.DataSet.Active;
  ActiveChange(v);
end;
//------------------------------------------------------------------------------

procedure TDBAdvGrid.OnMouseActionsChanged(Sender: TObject);
begin
  inherited;
  if PageMode and not (csLoading in ComponentState) then
    if (MouseActions.WheelAction = waScroll) then
      MouseActions.WheelAction := waMoveSelection;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.OnNavigationChanged(Sender: TObject);
begin
  inherited;
  if {(Navigation.AdvanceDirection = adTopBottom)} (Navigation.AdvanceInsert) then
    Navigation.AdvanceDirection := adLeftRight;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.EditKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (Navigation.AlwaysEdit) and (DataSetType = dtNonSequenced) and (Row = FixedRows) and (Key = VK_UP) then
  begin
    FDataLink.DataSet.MoveBy(-1);
    ShowEditor;
  end;

  if Navigation.AppendOnArrowDown {and FKeyDownAppend} and (FDataLink.DataSet.State in [dsEdit, dsInsert]) and FRecordChanged then
  begin
    if (Key = VK_DOWN) then
    begin
      PostMessage(Self.Handle,WM_KEYDOWN, Key, 0);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.Edit_WMKeyDown(var Msg: TWMKeydown);
begin
  inherited;
  if (Msg.CharCode = VK_ESCAPE) and (EditPostMode = epCell) then
  begin
    FShouldNotPostChanges := FNewRecord and (FKeyDownAppend or FNewAppendRecord);

    if Assigned(FDatalink.DataSet) and (FDatalink.DataSet.Active) then
      FDatalink.DataSet.Cancel;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.SetInvalidPicture(const Value: TPicture);
begin
  FInvalidPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGrid.UpdateDisplay;
var
  i: Integer;
begin
  if not Assigned(FDataLink.DataSet) or not PageMode or FEditUpdating then
    Exit;

  if FDataLink.DataSet.Active then
  begin
    i := Row - TopRow;
    if (i >= 0) and (i < FDataLink.ActiveRecord) then
    begin
      TopRow := TopRow + (i - FDataLink.ActiveRecord);
    end;
  end;
end;

//------------------------------------------------------------------------------

initialization
  {$IFDEF ISDELPHI}
  try
    Classes.RegisterClass(TDBGridColumnItem);
    Classes.RegisterClass(TDBGridColumnCollection);
  except
  end;
  {$ENDIF}  

end.
