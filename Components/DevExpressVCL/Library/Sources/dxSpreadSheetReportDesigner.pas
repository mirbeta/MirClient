{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxSpreadSheetReportDesigner;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Forms, Windows, Classes, Types, SysUtils, Graphics, Controls, DB, Generics.Defaults, Generics.Collections, Variants,
  cxTreeView, ComCtrls, Math, dxCore, cxClasses, cxControls, cxDB, cxDBData, cxGeometry, cxGraphics, dxSpreadSheetTypes,
  dxSpreadSheetCore, dxSpreadSheetFunctions, dxSpreadSheetFormulas, dxGDIPlusClasses, cxLookAndFeelPainters, dxHashUtils,
  dxSpreadSheetGraphics, dxSpreadSheetSelection, dxSpreadSheetClasses, dxSpreadSheetUtils, dxSpreadSheetContainers, cxVariants,
  dxSpreadSheetFormatBinary, cxCustomData, cxDataStorage, cxFilter, cxFilterControl, cxEdit, cxEditDBRegisteredRepositoryItems,
  dxCustomTree, dxForms, dxSpreadSheetStyles, dxSpreadSheetCoreStyles, dxSpreadSheetCoreFormulas;

type
  TdxSpreadSheetReportDesigner = class;
  TdxSpreadSheetReportDesignerFieldChooser = class;
  TdxSpreadSheetReportDetails = class;
  TdxSpreadSheetReportDetail = class;
  TdxSpreadSheetReportDataController = class;
  TdxSpreadSheetReportLevelBuildInfo = class;

  TdxSpreadSheetReportSectionType = (rstHeader, rstDetail, rstFooter, rstDetailLevel, rstGroupHeader, rstGroupFooter);

  TdxSpreadSheetReportOrientation = (roHorizontal, roVertical);
  TdxSpreadSheetReportMode = (rmSingleSheet, rmMultipleSheets, rmMultipleDocuments);

  { IdxSpreadSheetDataBinding }

  IdxSpreadSheetDataBinding = interface
  ['{CD308816-6806-40B1-B7E6-4C0297234AF9}']
    function IsActive: Boolean;
    function GetFieldValue(const AFieldName: string): Variant;
  end;

  { TdxSpreadSheetReportDesignerOptions }

  TdxSpreadSheetReportDesignerOptions = class(TdxSpreadSheetPersistentObject)
  strict private
    FDesignView: Boolean;
    FOrientation: TdxSpreadSheetReportOrientation;
    FReportMode: TdxSpreadSheetReportMode;
    function GetDesigner: TdxSpreadSheetReportDesigner;
    procedure SetDesignView(AValue: Boolean);
    procedure SetOrientation(AValue: TdxSpreadSheetReportOrientation);
    procedure SetReportMode(AValue: TdxSpreadSheetReportMode);
  protected
    procedure Changed; virtual;

    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    procedure Assign(Source: TPersistent); override;
  published
    property DesignView: Boolean read FDesignView write SetDesignView default True;
    property Orientation: TdxSpreadSheetReportOrientation read FOrientation write SetOrientation default roVertical;
    property ReportMode: TdxSpreadSheetReportMode read FReportMode write SetReportMode default rmSingleSheet;
  end;

  { TdxSpreadSheetReportSection }

  TdxSpreadSheetReportSection = class
  protected
    Cell: TdxSpreadSheetCellViewInfo;
    Bounds: TRect;
    DestBounds: TRect;
    Index: Integer;
    //
    ActuallyBounds, ExtractBounds: TRect;
    CachedBounds: array[Boolean] of TRect;
    DataMap: array of array of TObject;
    Formulas: array[Boolean] of TList<TdxSpreadSheetCustomFormula>;
    MergedCells: array[Boolean] of TList<TdxSpreadSheetMergedCell>;
    DataController: TdxSpreadSheetReportDataController;
    procedure CalculateCache(ASheet: TdxSpreadSheetTableView;
      ADetails: TObjectList<TdxSpreadSheetReportLevelBuildInfo>; AIsVerticalLayout: Boolean);
    procedure ClearCache;
  public
    Name: TdxSpreadSheetDefinedName;
    SectionType: TdxSpreadSheetReportSectionType;
    constructor Create(AName: TdxSpreadSheetDefinedName; AType: TdxSpreadSheetReportSectionType; AIndex: Integer);
    destructor Destroy; override;
    function ValidateRange(var ARange: TRect): Boolean;
  end;

  { TdxSpreadSheetReportDesignerDataField }

  TdxSpreadSheetReportDesignerDataField = class(TPersistent)
  strict private
    FDataController: TdxSpreadSheetReportDataController;
    FID: Integer;
    FIndex: Integer;
    FVisible: Boolean;
    function GetField: TField;
    function GetFieldName: string;
    function GetSortOrder: TdxSortOrder;
    procedure SetFieldName(const AValue: string);
    procedure SetSortOrder(AValue: TdxSortOrder);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure Changed; virtual;
    function GetDisplayName: string; virtual;
    function GetValue: Variant;
    procedure SetIndex(AIndex: Integer);

    property ID: Integer read FID;
    property DisplayName: string read GetDisplayName;
    property Field: TField read GetField;
  public
    constructor Create(AOwner: TdxSpreadSheetReportDataController); virtual;
    destructor Destroy; override;

    property DataController: TdxSpreadSheetReportDataController read FDataController;
    property FieldName: string read GetFieldName write SetFieldName;
    property Index: Integer read FIndex;
    property SortOrder: TdxSortOrder read GetSortOrder write SetSortOrder;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TdxSpreadSheetReportSortedField }

  TdxSpreadSheetReportSortedField = class(TCollectionItem)
  private
    FFieldName: string;
    FSortOrder: TdxSortOrder;

    procedure SetFieldName(const AValue: string);
    procedure SetSortOrder(const AValue: TdxSortOrder);
  protected
    procedure ReportChanged; virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property FieldName: string read FFieldName write SetFieldName;
    property SortOrder: TdxSortOrder read FSortOrder write SetSortOrder default soAscending;
  end;

  { TdxSpreadSheetReportSortedFields }

  TdxSpreadSheetReportSortedFields = class(TOwnedCollection)
  private
    function GetDataController: TdxSpreadSheetReportDataController;
    function GetDesigner: TdxSpreadSheetReportDesigner;
    function GetItem(AIndex: Integer): TdxSpreadSheetReportSortedField;
    procedure SetItem(AIndex: Integer; AValue: TdxSpreadSheetReportSortedField);
  public
    property DataController: TdxSpreadSheetReportDataController read GetDataController;
    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
    property Items[Index: Integer]: TdxSpreadSheetReportSortedField read GetItem write SetItem; default;
  end;

  { TdxSpreadSheetReportDataGroup }

  TdxSpreadSheetReportDataGroup = class(TdxSpreadSheetReportSortedField)
  private
    FSectionID: Integer;
    function IsSectionIDStored: Boolean;
  protected
    procedure SetCollection(Value: TCollection); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property SectionID: Integer read FSectionID write FSectionID stored IsSectionIDStored;
  end;

  { TdxSpreadSheetReportDataGroups }

  TdxSpreadSheetReportDataGroups = class(TdxSpreadSheetReportSortedFields)
  private
    function GetItem(AIndex: Integer): TdxSpreadSheetReportDataGroup;
    procedure SetItem(AIndex: Integer; AValue: TdxSpreadSheetReportDataGroup);
  public
    property Items[Index: Integer]: TdxSpreadSheetReportDataGroup read GetItem write SetItem; default;
  end;

  { TdxSpreadSheetReportDataController }

  TdxSpreadSheetReportDataController = class(TcxDBDataController, IcxFilterControl)
  strict private
    FDisplayName: string;
    FFieldNames: TDictionary<string, TdxSpreadSheetReportDesignerDataField>;
    FFields: TcxObjectList;
    FFilterableItems: TList;
    FNextID: Integer;
    function GetDataControllerFromOwner: TdxSpreadSheetReportDataController;
    function GetDataGroups: TdxSpreadSheetReportDataGroups;
    function GetDesigner: TdxSpreadSheetReportDesigner;
    function GetDisplayName: string;
    function GetFieldCount: Integer;
    function GetField(AIndex: Integer): TdxSpreadSheetReportDesignerDataField;
    function GetIsActive: Boolean;
    function GetIsRoot: Boolean;
    procedure SetDisplayName(const AValue: string);
    procedure SetField(AIndex: Integer; AValue: TdxSpreadSheetReportDesignerDataField);
  protected
    procedure ActiveChanged(AActive: Boolean); override;
    procedure AddFieldObject(AField: TdxSpreadSheetReportDesignerDataField);
    procedure BeforeBuildReport; virtual;
    procedure Changed; virtual;
    function CreateItemByField(AField: TField): TdxSpreadSheetReportDesignerDataField; virtual;
    procedure DoDataSourceChanged; override;
    function GetFieldName(const AFieldName: string; AUseStorageName: Boolean = True): string;
//    function GetFieldValue(const AFieldName: string): Variant;
    procedure MasterDetailRelationsInitialize;
    procedure MasterDetailRelationsFinalize;

    procedure PopulateFields(AFields: TDictionary<string, TdxSpreadSheetReportDesignerDataField>);
    procedure RemoveFieldObject(AField: TdxSpreadSheetReportDesignerDataField);
    procedure UpdateIndexes;
    // IcxFilterControl
    function GetCaption(Index: Integer): string;
    function GetCount: Integer;
    function GetCriteria: TcxFilterCriteria;
    function GetFilterFieldName(Index: Integer): string;
    function GetInternalDisplayName: string;
    function GetItemLink(Index: Integer): TObject;
    function GetItemLinkID(Index: Integer): Integer;
    function GetItemLinkName(Index: Integer): string;
    function GetItemID(AItem: TObject): Integer; override;
    function GetItemName(AItem: TObject): string; override;
    function IcxFilterControl.GetFieldName = GetFilterFieldName;
    function GetProperties(Index: Integer): TcxCustomEditProperties;
    function GetValueType(Index: Integer): TcxValueTypeClass;
    //
    property DataGroups: TdxSpreadSheetReportDataGroups read GetDataGroups;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property IsActive: Boolean read GetIsActive;
    property IsRoot: Boolean read GetIsRoot;
    property FieldsList: TcxObjectList read FFields;
    property FieldNames: TDictionary<string, TdxSpreadSheetReportDesignerDataField> read FFieldNames;
    property FilterableItems: TList read FFilterableItems;
    property MasterDataController: TdxSpreadSheetReportDataController read GetDataControllerFromOwner;
    property NextID: Integer read FNextID;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetItem(Index: Integer): TObject; override;

    function AddField: TdxSpreadSheetReportDesignerDataField; reintroduce; overload;
    procedure CreateAllItems(AMissingItemsOnly: Boolean = False);
    procedure UpdateItems(AUpdateFields: Boolean); override;

    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
    property FieldCount: Integer read GetFieldCount;
    property Fields[AIndex: Integer]: TdxSpreadSheetReportDesignerDataField read GetField write SetField;
  end;

  { TdxSpreadSheetReportDataControllerOptions }

  TdxSpreadSheetReportDataControllerOptions = class(TPersistent)
  strict private
    FOwner: TPersistent;
    function GetAnsiSort: Boolean;
    function GetCaseInsensitive: Boolean;
    function GetDataController: TdxSpreadSheetReportDataController;
    function GetDisplayName: string;
    procedure SetAnsiSort(AValue: Boolean);
    procedure SetCaseInsensitive(AValue: Boolean);
    procedure SetDisplayName(const AValue: string);
    function IsDisplayNameStored: Boolean;
  protected
    procedure Changed; virtual;
    function GetOwner: TPersistent; override;

    property DataController: TdxSpreadSheetReportDataController read GetDataController;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property AnsiSort: Boolean read GetAnsiSort write SetAnsiSort default False;
    property CaseInsensitive: Boolean read GetCaseInsensitive write SetCaseInsensitive default False;
    property DisplayName: string read GetDisplayName write SetDisplayName stored IsDisplayNameStored;
  end;

  { TdxSpreadSheetReportDetail }

  TdxSpreadSheetReportDetail = class(TcxComponentCollectionItem, IcxFilterControl)
  strict private
    FDataController: TdxSpreadSheetReportDataController;
    FDataGroups: TdxSpreadSheetReportDataGroups;
    FDetails: TdxSpreadSheetReportDetails;
    FOptions: TdxSpreadSheetReportDataControllerOptions;
    FSectionID: Integer;
    FSortedFields: TdxSpreadSheetReportSortedFields;
    function GetDataSource: TDataSource;
    function GetDetailKeyFieldName: string;
    function GetDesigner: TdxSpreadSheetReportDesigner;
    function GetFilter: IcxFilterControl;
    function GetMasterKeyFieldName: string;
    procedure SetDataGroups(AValue: TdxSpreadSheetReportDataGroups);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDetailKeyFieldName(const AValue: string);
    procedure SetDetails(AValue: TdxSpreadSheetReportDetails);
    procedure SetMasterKeyFieldName(const AValue: string);
    procedure SetOptions(AValue: TdxSpreadSheetReportDataControllerOptions);
    procedure SetSectionID(AValue: Integer);
    procedure SetSortedFields(AValue: TdxSpreadSheetReportSortedFields);
  protected
    function CreateDataController: TdxSpreadSheetReportDataController; virtual;
    function CreateDataGroups: TdxSpreadSheetReportDataGroups; virtual;
    function CreateDetails: TdxSpreadSheetReportDetails; virtual;
    function CreateOptions: TdxSpreadSheetReportDataControllerOptions; virtual;
    function CreateSortedFields: TdxSpreadSheetReportSortedFields; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetOwner: TPersistent; override;
    procedure PopulateFields(AFields: TDictionary<string, TdxSpreadSheetReportDesignerDataField>);  virtual;
    procedure SetCollection(AValue: TcxComponentCollection); override;

    property Filter: IcxFilterControl read GetFilter implements IcxFilterControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetParentComponent(Value: TComponent); override;

    property DataController: TdxSpreadSheetReportDataController read FDataController;
    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
  published
    property DataGroups: TdxSpreadSheetReportDataGroups read FDataGroups write SetDataGroups;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Details: TdxSpreadSheetReportDetails read FDetails write SetDetails;
    property MasterKeyFieldName: string read GetMasterKeyFieldName write SetMasterKeyFieldName;
    property DetailKeyFieldName: string read GetDetailKeyFieldName write SetDetailKeyFieldName;
    property Options:  TdxSpreadSheetReportDataControllerOptions read FOptions write SetOptions;
    property SectionID: Integer read FSectionID write SetSectionID;
    property SortedFields: TdxSpreadSheetReportSortedFields read FSortedFields write SetSortedFields;
  end;

 { TdxSpreadSheetReportDetails }

  TdxSpreadSheetReportDetails = class(TcxComponentCollection)
  private
    function GetDesigner: TdxSpreadSheetReportDesigner;
    function GetItem(AIndex: Integer): TdxSpreadSheetReportDetail;
    function GetMasterDataController: TdxSpreadSheetReportDataController;
    procedure SetItem(AIndex: Integer; AValue: TdxSpreadSheetReportDetail);
  protected
    function GetOwner: TPersistent; override;
    function GetParentControl: TControl; override;
    procedure PopulateFields(AFields: TDictionary<string, TdxSpreadSheetReportDesignerDataField>);
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;

    property MasterDataController: TdxSpreadSheetReportDataController read GetMasterDataController;
  public
    function Add: TdxSpreadSheetReportDetail;

    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
    property Items[AIndex: Integer]: TdxSpreadSheetReportDetail read GetItem write SetItem; default;
  end;

  { TdxSpreadSheetReportDataBinding }

  TdxSpreadSheetReportDataBinding = class(TInterfacedPersistent)
  strict private
    FDataController: TdxSpreadSheetReportDataController;
    FDataGroups: TdxSpreadSheetReportDataGroups;
    FDesigner: TdxSpreadSheetReportDesigner;
    FDetails: TdxSpreadSheetReportDetails;
    FFields: TDictionary<string, TdxSpreadSheetReportDesignerDataField>;
    FOptions: TdxSpreadSheetReportDataControllerOptions;
    FSortedFields: TdxSpreadSheetReportSortedFields;
    function GetDataSource: TDataSource;
    procedure SetDataGroups(AValue: TdxSpreadSheetReportDataGroups);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDetails(AValue: TdxSpreadSheetReportDetails);
    procedure SetOptions(AValue: TdxSpreadSheetReportDataControllerOptions);
    procedure SetSortedFields(AValue: TdxSpreadSheetReportSortedFields);
  protected
    function CreateDataController: TdxSpreadSheetReportDataController; virtual;
    function CreateDataGroups: TdxSpreadSheetReportDataGroups; virtual;
    function CreateDetails: TdxSpreadSheetReportDetails; virtual;
    function CreateOptions: TdxSpreadSheetReportDataControllerOptions; virtual;
    function CreateSortedFields: TdxSpreadSheetReportSortedFields; virtual;
    function GetFieldValue(const AFieldName: string; var AValue: Variant): Boolean;
    function GetOwner: TPersistent; override;
    procedure RefreshFieldsList;

    property Fields: TDictionary<string, TdxSpreadSheetReportDesignerDataField> read FFields;
  public
    constructor Create(AOwner: TdxSpreadSheetReportDesigner); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;

    property DataController: TdxSpreadSheetReportDataController read FDataController;
    property Designer: TdxSpreadSheetReportDesigner read FDesigner;
  published
    property DataGroups: TdxSpreadSheetReportDataGroups read FDataGroups write SetDataGroups;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Details: TdxSpreadSheetReportDetails read FDetails write SetDetails;
    property Options: TdxSpreadSheetReportDataControllerOptions read FOptions write SetOptions;
    property SortedFields: TdxSpreadSheetReportSortedFields read FSortedFields write SetSortedFields;
  end;

  { TdxSpreadSheetReportFormulaController }

  TdxSpreadSheetReportFormulaController = class(TdxSpreadSheetFormulaController, IdxSpreadSheetDataBinding)
  strict private
    FDataBinding: TdxSpreadSheetReportDataBinding;
    function GetDesigner: TdxSpreadSheetReportDesigner; inline;
  protected
    function IsActive: Boolean; virtual;
    function GetFieldValue(const AFieldName: string): Variant; virtual;

    property DataBinding: TdxSpreadSheetReportDataBinding read FDataBinding;
    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet); override;
  end;

  { TdxSpreadSheetReportDesignerFieldChooserForm }

  TdxSpreadSheetReportDesignerFieldChooserForm = class(TdxForm)
  strict private
    FOwner: TdxSpreadSheetReportDesignerFieldChooser;
    function GetDataBinding: TdxSpreadSheetReportDataBinding;
    function GetDesigner: TdxSpreadSheetReportDesigner;
  protected
    Fields: TcxTreeView;
    function CreateFieldsList: TcxTreeView;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Init; virtual;
    procedure InitializeFieldsTreeView; virtual;
    procedure PopulateFields; virtual;
    procedure PopulateLevel(AParent: TTreeNode; ADataController: TdxSpreadSheetReportDataController; ADetails: TdxSpreadSheetReportDetails);
    procedure PopulateTableItems(AParent: TTreeNode; ADataController: TdxSpreadSheetReportDataController);
    procedure ProcessDetails(AParent: TTreeNode; ADetails: TdxSpreadSheetReportDetails);
  public
    constructor Create(AOwner: TdxSpreadSheetReportDesignerFieldChooser); reintroduce; overload;
    destructor Destroy; override;

    property DataBinding: TdxSpreadSheetReportDataBinding read GetDataBinding;
    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
    property Owner: TdxSpreadSheetReportDesignerFieldChooser read FOwner;
  end;

  { TdxSpreadSheetReportDesignerFieldChooser }

  TdxSpreadSheetReportDesignerFieldChooser = class(TdxSpreadSheetPersistentObject)
  strict private
    FDragObject: TDragObject;
    FForm: TdxSpreadSheetReportDesignerFieldChooserForm;
    FSite: TWinControl;
    function GetDesigner: TdxSpreadSheetReportDesigner;
    procedure SetDragObject(AValue: TDragObject);
    procedure SetSite(AValue: TWinControl);
  protected
    function CreateFieldChooserForm: TdxSpreadSheetReportDesignerFieldChooserForm; virtual;
    function CanAssignedSite(ASite: TWinControl): Boolean; virtual;
    procedure DoHideForm(Sender: TObject); virtual;
    procedure DoShowForm(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    procedure FormNeeded;
    procedure UpdateFieldChooser; virtual;

    property DragObject: TDragObject read FDragObject write SetDragObject;
  public
    procedure Show;
    procedure Hide;

    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
    property Form: TdxSpreadSheetReportDesignerFieldChooserForm read FForm;
  published
    property Site: TWinControl read FSite write SetSite;
  end;

  { TdxSpreadSheetReportLevelBuildInfo }

  TdxSpreadSheetReportLevelBuildInfo = class
  private
    procedure CheckIntersection(var R1, R2: TRect; AIsVerticalLayout: Boolean);
  public
    Bounds: TRect;
    DataController: TdxSpreadSheetReportDataController;
    DataGroup: TdxSpreadSheetReportDataGroup;
    DataGroupBounds: TRect;
    DataGroupInfo: TdxSpreadSheetReportLevelBuildInfo;
    Detail: TdxSpreadSheetReportSection;
    DetailBounds, ExtractBounds: TRect;
    Details: TObjectList<TdxSpreadSheetReportLevelBuildInfo>;
    GroupBounds: TRect;
    Footer: TdxSpreadSheetReportSection;
    Header: TdxSpreadSheetReportSection;
    Parent: TdxSpreadSheetReportLevelBuildInfo;
    constructor Create(AParent: TdxSpreadSheetReportLevelBuildInfo);
    destructor Destroy; override;
    procedure CheckSectionsIntersection(AIsVerticalLayout: Boolean);
    procedure ExpandDetailBounds(ASection: TdxSpreadSheetReportSection);
  end;

  { TdxSpreadSheetReportBuilder }

  TdxSpreadSheetReportBuilder = class
  strict private
    FActive: Boolean;
    FActiveDataController: TdxSpreadSheetReportDataController;
    FCurrentColumnIndex: Integer;
    FCurrentLevel: TdxSpreadSheetReportLevelBuildInfo;
    FCurrentRow: TdxSpreadSheetTableItem;
    FCurrentRowIndex: Integer;
    FCurrentSection: TdxSpreadSheetReportSection;
    FCurrentSheet: TdxSpreadSheetTableView;
    FDesigner: TdxSpreadSheetReportDesigner;
    FDestination: TdxCustomSpreadSheet;
    FOffset: TSize;
    FOrigin: TPoint;
    FPostProcessFormulasList: TList<TdxSpreadSheetCustomFormula>;
    FProgressValue: Integer;
    FRowsAutoHeight: Boolean;
    FRoot: TdxSpreadSheetReportLevelBuildInfo;
    FStyles: TDictionary<TdxSpreadSheetCellStyleHandle, TdxSpreadSheetCellStyleHandle>;
    function GetDataBinding: TdxSpreadSheetReportDataBinding;
    procedure SetProgressValue(AValue: Integer);
  protected
    function CheckAndExpandRange(AInfo: TdxSpreadSheetReportLevelBuildInfo; var R: TRect): Boolean;
    procedure CopyCell(ASource: TdxSpreadSheetCell; ADestRow: TdxSpreadSheetTableItem; ADestIndex: Integer); virtual;
    procedure CopySettingsForTableItem(ASource: TdxSpreadSheetTableItem; ADestItems: TdxSpreadSheetTableItems; ADestIndex: Integer; ACheckItemExist: Boolean); virtual;
    function CheckSection(AType: TdxSpreadSheetReportSectionType;
      var ASection: TdxSpreadSheetReportSection): Boolean;
    procedure DoBuildReport; virtual;
    procedure DoBuildDetailLevel(AInfo: TdxSpreadSheetReportLevelBuildInfo); virtual;
    procedure DoBuildDetailSection(AInfo: TdxSpreadSheetReportLevelBuildInfo;
      var ARowIndex: Integer); virtual;
    procedure DoBuildDetailSectionDataGroups(AInfo: TdxSpreadSheetReportLevelBuildInfo; var ARowIndex: Integer);
    procedure DoBuildDetailSectionDataRow(AInfo: TdxSpreadSheetReportLevelBuildInfo; var ARowIndex: Integer);
    procedure DoBuildDetailSectionDetails(AInfo: TdxSpreadSheetReportLevelBuildInfo; var ARowIndex: Integer);
    procedure DoBuildSection(ASection: TdxSpreadSheetReportSection); overload; virtual;
    procedure DoBuildSection(ASection: TdxSpreadSheetReportSection;
      ADetails: TObjectList<TdxSpreadSheetReportLevelBuildInfo>; AIsFirstStage: Boolean); overload; virtual;
    procedure DoBuildSectionFormulas(ASection: TdxSpreadSheetReportSection; AIsFirstStage: Boolean);

    function GetDesignerSection(const ARootBounds: TRect; AType: TdxSpreadSheetReportSectionType;
      AIndex: Integer; var ASectionBounds: TRect): TdxSpreadSheetReportSection;
    function IsBoundsValid(const ABounds: TRect): Boolean;
    function IsSectionValid(ASection: TdxSpreadSheetReportSection): Boolean;
    function IsVerticalLayout: Boolean;
    function MakeGroupLevelInfo(AParent: TdxSpreadSheetReportLevelBuildInfo; AIndex: Integer): TdxSpreadSheetReportLevelBuildInfo;
    function MakeLevelInfo(AParent: TdxSpreadSheetReportLevelBuildInfo; ADetail: TdxSpreadSheetReportDetail): TdxSpreadSheetReportLevelBuildInfo; overload;
    function MakeLevelInfo(AParent: TdxSpreadSheetReportLevelBuildInfo; ADataController: TdxSpreadSheetReportDataController;
      ADetails: TdxSpreadSheetReportDetails; AIndex: Integer): TdxSpreadSheetReportLevelBuildInfo; overload;
    function MakeLocalArea(const AArea, ASourceArea, ADestArea: TRect; var ALocalArea: TRect): Boolean;
    procedure NewSheet;
    procedure PostProcessFormulas; virtual;
    procedure PostProcessSection(var ASection: TdxSpreadSheetReportSection; AIndex: Integer; ANeedAutoHeight: Boolean);
    procedure PostRowsAutoHeight(ASection: TdxSpreadSheetReportSection);
    procedure PrepareBuildInfo;
    procedure ProcessEmptyReport; virtual;
    procedure SetFocusedRowIndex(ARowIndex: Integer);

    property PostProcessFormulasList: TList<TdxSpreadSheetCustomFormula> read FPostProcessFormulasList;
    property ProgressValue: Integer read FProgressValue write SetProgressValue;
    property Root: TdxSpreadSheetReportLevelBuildInfo read FRoot;
  public
    constructor Create(ADesigner: TdxSpreadSheetReportDesigner); virtual;
    destructor Destroy; override;
    procedure BuildReport;

    property Active: Boolean read FActive;
    property ActiveDataController: TdxSpreadSheetReportDataController read FActiveDataController;
    property CurrentColumnIndex: Integer read FCurrentColumnIndex;
    property CurrentLevel: TdxSpreadSheetReportLevelBuildInfo read FCurrentLevel;
    property CurrentRow: TdxSpreadSheetTableItem read FCurrentRow;
    property CurrentRowIndex: Integer read FCurrentRowIndex;
    property CurrentSection: TdxSpreadSheetReportSection read FCurrentSection;
    property CurrentSheet: TdxSpreadSheetTableView read FCurrentSheet;
    property DataBinding: TdxSpreadSheetReportDataBinding read GetDataBinding;
    property Designer: TdxSpreadSheetReportDesigner read FDesigner;
    property Destination: TdxCustomSpreadSheet read FDestination write FDestination;
    property Offset: TSize read FOffset;
    property Origin: TPoint read FOrigin;
    property RowsAutoHeight: Boolean read FRowsAutoHeight write FRowsAutoHeight;
    property Styles: TDictionary<TdxSpreadSheetCellStyleHandle, TdxSpreadSheetCellStyleHandle> read FStyles;
  end;

  { TdxSpreadSheetReport }

  TdxSpreadSheetReportDesignerPopulateFieldEvent = procedure(Sender: TdxSpreadSheetReportDesigner; AField: TField; var Allow: Boolean) of object;
  TdxSpreadSheetReportDesignerNewDocumentEvent = procedure(Sender: TdxSpreadSheetReportDesigner; var ADestination: TdxCustomSpreadSheet) of object;
  TdxSpreadSheetReportDesignerNewReportSheetEvent = procedure(Sender: TdxSpreadSheetReportDesigner; ANewSheet: TdxSpreadSheetTableView) of object;

  TdxSpreadSheetReportDesigner = class(TdxCustomSpreadSheet, IcxFilterControl)
  strict private
    FBuilder: TdxSpreadSheetReportBuilder;
    FDataBinding: TdxSpreadSheetReportDataBinding;
    FDetailID: Integer;
    FDBCells: TDictionary<TdxSpreadSheetCustomFormula, Boolean>;
    FDBFormulas: TList<TdxSpreadSheetCustomFormula>;
    FFieldChooser: TdxSpreadSheetReportDesignerFieldChooser;
    FFieldPictures: TDictionary<TdxSpreadSheetCustomFormula, TObject>;
    FFieldPictureInfos: TcxObjectList;
    FOptions: TdxSpreadSheetReportDesignerOptions;
    FRebuildNeeded: Boolean;
    FSectionList: TObjectList<TdxSpreadSheetReportSection>;
    FTextCells: TDictionary<TdxSpreadSheetCell, TdxDefaultBoolean>;
    FOnAfterBuild: TNotifyEvent;
    FOnBeforeBuild: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnHideFieldChooser: TNotifyEvent;
    FOnNewDocument: TdxSpreadSheetReportDesignerNewDocumentEvent;
    FOnNewReportSheet: TdxSpreadSheetReportDesignerNewReportSheetEvent;
    FOnShowFieldChooser: TNotifyEvent;
    FOnPopulateField: TdxSpreadSheetReportDesignerPopulateFieldEvent;
    function GetCurrentDataController: TdxSpreadSheetReportDataController;
    function GetCurrentSection: TdxSpreadSheetReportSection;
    function GetFilter: IcxFilterControl;
    procedure SetDataBinding(AValue: TdxSpreadSheetReportDataBinding);
    procedure SetFieldChooser(AValue: TdxSpreadSheetReportDesignerFieldChooser);
    procedure SetOptions(AValue: TdxSpreadSheetReportDesignerOptions);
  protected
    procedure AddFormulaInfo(AFormula: TdxSpreadSheetCustomFormula; AIsDBFormula: Boolean);
    function AddPicture(AFormula: TdxSpreadSheetCustomFormula; APictureInfo: TObject): Integer;
    procedure AfterLoad; override;
    procedure CheckChanges; override;
    procedure CheckFormula(AFormula: TdxSpreadSheetCustomFormula);
    procedure CheckFormulas; virtual;
    function CreateDataBinding: TdxSpreadSheetReportDataBinding; virtual;
    function CreateFieldChooser: TdxSpreadSheetReportDesignerFieldChooser; virtual;
    function CreateFormulaController:  TdxSpreadSheetFormulaController; override;
    function CreateReportBuilder: TdxSpreadSheetReportBuilder; virtual;
    function CreateOptions: TdxSpreadSheetReportDesignerOptions; virtual;
    procedure CreateSubClasses; override;
    procedure DataSetChange; virtual;
    procedure DefinedNamesChanged; override;
    procedure DestroySubClasses; override;
    procedure DoAfterBuild; virtual;
    procedure DoBeforeBuild; virtual;
    function DoCreateSheet(var ASheet: TdxSpreadSheetCustomView;
      const ACaption: string = ''; AViewClass: TdxSpreadSheetCustomViewClass = nil): Boolean; override;
    procedure DoDataChanged; override;
    procedure DoNewDocument(var ADocument: TdxCustomSpreadSheet); virtual;
    procedure DoNewSheet(ASheet: TdxSpreadSheetTableView); virtual;
    function DoPopulateFields(AField: TField): Boolean; virtual;
    procedure DoProgress(APercent: Integer); virtual;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure ForceRefreshFormulas; virtual;
    function GetDataControllerForSection(ASection: TdxSpreadSheetReportSection): TdxSpreadSheetReportDataController;
    function IsDBFormula(AFormula: TdxSpreadSheetCustomFormula): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopulateReportSections; virtual;
    procedure ResetCacheInformation; virtual;
    procedure SetSectionInfo(const AName: string; const ABounds: TRect; ALevel: Integer = -1);

    property Builder: TdxSpreadSheetReportBuilder read FBuilder;
    property CurrentDataController: TdxSpreadSheetReportDataController read GetCurrentDataController;
    property CurrentSection: TdxSpreadSheetReportSection read GetCurrentSection;
    property DBCells: TDictionary<TdxSpreadSheetCustomFormula, Boolean> read FDBCells;
    property DBFormulas: TList<TdxSpreadSheetCustomFormula> read FDBFormulas;
    property DetailID: Integer read FDetailID write FDetailID;
    property FieldPictures: TDictionary<TdxSpreadSheetCustomFormula, TObject> read FFieldPictures;
    property FieldPictureInfos: TcxObjectList read FFieldPictureInfos;
    property Filter: IcxFilterControl read GetFilter implements IcxFilterControl;
    property SectionList: TObjectList<TdxSpreadSheetReportSection> read FSectionList;
    property TextCells: TDictionary<TdxSpreadSheetCell, TdxDefaultBoolean> read FTextCells;
    property OnNewReportSheet: TdxSpreadSheetReportDesignerNewReportSheetEvent read FOnNewReportSheet write FOnNewReportSheet;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Build(ADestination: TdxCustomSpreadSheet);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function FindSection(const AType: TdxSpreadSheetReportSectionType; AIndex: Integer; var ASection: TdxSpreadSheetReportSection): Boolean;
    function FindSectionByCell(const ARow, AColumn: Integer; var ASection: TdxSpreadSheetReportSection): Boolean;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetSection(const AType: TdxSpreadSheetReportSectionType; AIndex: Integer = -1): TdxSpreadSheetReportSection; overload;
    function GetSection(const AName: string; AIndex: Integer = -1): TdxSpreadSheetReportSection; overload;
    function GetSectionByCell(const ARow, AColumn: Integer): TdxSpreadSheetReportSection;
    procedure RemoveSection(ASection: TdxSpreadSheetReportSection); overload;
    procedure RemoveSection(AType: TdxSpreadSheetReportSectionType; AIndex: Integer = -1); overload;
    procedure SetDetailSection(const ABounds: TRect; ALevel: Integer = -1);
    procedure SetFooterSection(const ABounds: TRect);
    procedure SetGroupHeaderSection(const ABounds: TRect; ALevel: Integer);
    procedure SetGroupFooterSection(const ABounds: TRect; ALevel: Integer);
    procedure SetHeaderSection(const ABounds: TRect);

    property RebuildNeeded: Boolean read FRebuildNeeded;
  published
    property DataBinding: TdxSpreadSheetReportDataBinding read FDataBinding write SetDataBinding;
    property FieldChooser: TdxSpreadSheetReportDesignerFieldChooser read FFieldChooser write SetFieldChooser;
    property Options: TdxSpreadSheetReportDesignerOptions read FOptions write SetOptions;
    property Align;
    property Anchors;
    property BorderStyle default cxcbsNone;
    property Enabled;
    property DialogsLookAndFeel;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsLockedStateImage;
  	property OptionsProtection;
    property OptionsView;
    property PageControl;
    property ParentDoubleBuffered;
    property ParentFont;
    property PopupMenu;
    property Styles;
    property Visible;
    //
    property OnAfterBuild: TNotifyEvent read FOnAfterBuild write FOnAfterBuild;
    property OnBeforeBuild: TNotifyEvent read FOnBeforeBuild write FOnBeforeBuild;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnHideFieldChooser: TNotifyEvent read FOnHideFieldChooser write FOnHideFieldChooser;
    property OnNewDocument: TdxSpreadSheetReportDesignerNewDocumentEvent read FOnNewDocument write FOnNewDocument;
    property OnPopulateField: TdxSpreadSheetReportDesignerPopulateFieldEvent read FOnPopulateField write FOnPopulateField;
    property OnShowFieldChooser: TNotifyEvent read FOnShowFieldChooser write FOnShowFieldChooser;
    //
    property OnActiveCellChanging;
    property OnActiveSheetChanged;
    property OnClick;
    property OnCommentHide;
    property OnCommentShow;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDrawTableViewCell;
    property OnCustomDrawTableViewCommonCell;
    property OnCustomDrawTableViewHeaderCell;
    property OnDataChanged;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEditChanged;
    property OnEdited;
    property OnEditing;
    property OnEditValueChanged;
    property OnEndDock;
    property OnEndDrag;
    property OnGetPassword;
    property OnHistoryChanged;
    property OnHyperlinkExecute;
    property OnInitEdit;
    property OnInitEditValue;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayoutChanged;
    property OnModifiedChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPrepareLockedStateImage;
    property OnProgress;
    property OnResize;
    property OnScroll;
    property OnSelectionChanged;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TdxSpreadSheetReportTableView }

  TdxSpreadSheetReportTableView = class(TdxSpreadSheetTableView)
  protected
    function CreateViewInfo: TdxSpreadSheetCustomViewViewInfo; override;
  end;

  { TdxSpreadSheetReportTableViewInfo }

  TdxSpreadSheetReportTableViewInfo = class(TdxSpreadSheetTableViewInfo)
  strict private
    function GetDesigner: TdxSpreadSheetReportDesigner;
    function GetHitTest: TdxSpreadSheetTableViewHitTest;
    function GetView: TdxSpreadSheetReportTableView;
  protected
    DrawBaseSelection: Boolean;
    DragDropAreaCell: TdxSpreadSheetCellViewInfo;

    function CreateCellViewInfo(ACell: TdxSpreadSheetCell): TdxSpreadSheetTableViewCellViewInfo; override;
    procedure CalculateHitTest(AHitTest: TdxSpreadSheetCustomHitTest); override;
    procedure DrawSelection(ACanvas: TcxCanvas); override;

    procedure RecalculateArea(AArea: TdxSpreadSheetReportSection);
    procedure SelectionChanged; override;
    procedure UpdateDragDropAreaCell(const X, Y: Integer; ADragObject: TDragObject; AAccepted: Boolean); virtual;

    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
    property HitTest: TdxSpreadSheetTableViewHitTest read GetHitTest;
    property View: TdxSpreadSheetReportTableView read GetView;
  public
    constructor Create(AView: TdxSpreadSheetCustomView); override;
    destructor Destroy; override;
    function CanDrawCellSelection: Boolean; override;
  end;

  { TdxSpreadSheetReportCellViewInfo }

  TdxSpreadSheetReportCellViewInfo = class(TdxSpreadSheetTableViewCellViewInfo)
  private
    function GetDesigner: TdxSpreadSheetReportDesigner;
  protected
    function GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass; override;
    procedure DrawValue(ACanvas: TcxCanvas); override;

    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
  end;

resourcestring
  sdxReportFieldList = 'Report Field List';

  sfnField = 'FIELD';
  sfnParameter = 'PARAMETER';
  sfnFieldPicture = 'FIELDPICTURE';
  sfnRange = 'RANGE';

  sSections = 'Sections';
  sRemoveSection = 'Remove Section';

  sHeaderCaption = 'Header';
  sFooterCaption = 'Footer';
  sDetailCaption = 'Detail';
  sDetailLevelCaption = 'DetailLevel';
  sGroupHeaderCaption = 'GroupHeader';
  sGroupFooterCaption = 'GroupFooter';

  sdxChangeSection = 'Change Report Section';

  sRange = 'Range';
  sTopLeft = 'TopLeft';

  // Reserved Area Names

  sanHeaderRange = 'HEADERRANGE';
  sanFooterRange = 'FOOTERRANGE';
  sanDetailRange = 'DETAILRANGE';
  sanDetailLevelRange = 'DETAILLEVEL';
  sanGroupHeaderRange = 'GROUPHEADER';
  sanGroupFooterRange = 'GROUPFOOTER';

const
  AreaCaptions: array[TdxSpreadSheetReportSectionType] of Pointer = (@sHeaderCaption, @sDetailCaption,
    @sFooterCaption, @sDetailLevelCaption, @sGroupHeaderCaption, @sGroupFooterCaption);

implementation

{$R *.res}

uses
  dxSpreadSheetPopupMenu, dxSpreadSheetCoreFormulasTokens, dxDPIAwareUtils, dxSpreadSheetCoreHelpers;

const
  dxSpreadSheetReportDefaultControlsIndent = 8;
  dxSpreadSheetReportDefaultFieldChooserWidth = 300;

  sscLayoutAndData = [sscData, sscLayout];

type
  TdxSpreadSheetCustomFormulaAccess = class(TdxSpreadSheetCustomFormula);
  TdxSpreadSheetCustomViewAccess = class(TdxSpreadSheetCustomView);
  TdxSpreadSheetDefinedNameAccess = class(TdxSpreadSheetDefinedName);
  TdxSpreadSheetFontAccess = class(TdxSpreadSheetCustomFont);
  TdxSpreadSheetFormulaReferenceAccess = class(TdxSpreadSheetFormulaReference);
  TdxSpreadSheetFormulaTokenAccess = class(TdxSpreadSheetFormulaToken);
  TdxSpreadSheetHitTestAccess = class(TdxSpreadSheetCustomHitTest);

  TdxSpreadSheetReportPictureInfo = class;

  TdxSpreadSheetReportSizingArea = (saNone, saLeft, saTop, saRight, saBottom, saBottomRight);

  { TdxSpreadSheetReportFieldDragObject }

  TdxSpreadSheetReportFieldDragObject = class(TDragControlObject)
  protected
    function GetDragImages: TDragImageList; override;
  public
    Designer: TdxSpreadSheetReportDesigner;
    DragImage: TDragImageList;
    Field: TdxSpreadSheetReportDesignerDataField;
    constructor Create(AControl: TControl; ADesigner: TdxSpreadSheetReportDesigner;
      AField: TdxSpreadSheetReportDesignerDataField); reintroduce; overload;
    destructor Destroy; override;
  end;

  { TdxSpreadSheetReportFieldList }

  TdxSpreadSheetReportFieldList = class(TcxTreeView)
  protected
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
  end;

  { TdxReportSectionsNameController }

  TdxReportSectionsNameController = class
  protected
    Names: TDictionary<string, TdxSpreadSheetReportSectionType>;
    procedure Initialize;
  public
    constructor Create;
    destructor Destroy; override;

    function IsSectionName(AName: TdxSpreadSheetDefinedName; var AType: TdxSpreadSheetReportSectionType; var AIndex: Integer): Boolean;
    function TryGetType(const AName: string; var AType: TdxSpreadSheetReportSectionType; var AIndex: Integer): Boolean;
  end;


  { TdxSpreadSheetTableViewResizeAreaDragAndDropObject }

  TdxSpreadSheetTableViewResizeAreaDragAndDropObject = class(TdxSpreadSheetTableViewCustomSelectionDragAndDropObject)
  private
    function GetDesigner: TdxSpreadSheetReportDesigner;
    function GetViewInfo: TdxSpreadSheetReportTableViewInfo;
  protected
    HitObjectData: TdxNativeInt;
    Section: TdxSpreadSheetReportSection;
    SourceArea: TRect;
    procedure ApplyChanges; override;
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetSelectionArea: TRect; override;
    function MakeBounds(ALeft, ATop, ARight, ABottom: Integer): TRect;
    procedure UpdateSectionBounds(ABounds: TRect); overload;
    procedure UpdateSectionBounds(ARow, AColumn: Integer); overload;

    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
    property ViewInfo: TdxSpreadSheetReportTableViewInfo read GetViewInfo;
  public
    constructor Create(AControl: TcxControl); override;
  end;

  { TdxSpreadSheetReportSectionViewInfo }

  TdxSpreadSheetReportSectionViewInfo = class(TdxSpreadSheetTableViewSelectionFrameViewInfo)
  strict private
    function GetViewInfo: TdxSpreadSheetTableViewInfo;
  protected
    Section: TdxSpreadSheetReportSection;

    function CanFillData: Boolean; override;
    function CanMoveSelection: Boolean; override;
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
  public
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); override;

    property ViewInfo: TdxSpreadSheetTableViewInfo read GetViewInfo;
  end;

  { TdxSpreadSheetReportDragDropFieldAreaCell }

  TdxSpreadSheetReportDragDropFieldAreaCell = class(TdxSpreadSheetTableViewSelectionFrameViewInfo)
  protected
    function CanFillData: Boolean; override;
    function CanMoveSelection: Boolean; override;
  public
    constructor Create(AOwner: TObject); override;
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); override;
    procedure Invalidate; override;
  end;

  { TdxSpreadSheetReportHelper }

  TdxSpreadSheetReportHelper = class
  strict private type
    TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
    TdxSpreadSheetCommentContainerAccess = class(TdxSpreadSheetCommentContainer);
    TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  strict private
    class procedure CopyContainer(AContainer: TdxSpreadSheetContainer; ATarget: TdxSpreadSheetTableView;
      const ASourceArea, ASourceAreaBounds: TRect; const ATargetCell: TPoint);
    class procedure LoadContainer(AStream: TStream; ATarget: TdxSpreadSheetTableView; const ATargetCell: TPoint);
    class procedure SaveContainer(AStream: TStream; AContainer: TdxSpreadSheetContainer; const ASourceArea, ASourceAreaBounds: TRect);
  public
    class procedure CopyContainers(ASource, ATarget: TdxSpreadSheetTableView; const ASourceCellsArea: TRect; const ATargetCell: TPoint);
    class procedure CopyFormulas(ASource, ATarget: TdxSpreadSheetCell; var AIsRangeFormula: Boolean);
    class procedure MakePicture(ATarget: TdxSpreadSheetTableView; ASection: TdxSpreadSheetReportSection; APictureInfo: TdxSpreadSheetReportPictureInfo);
  end;

  { TdxSpreadSheetReportViewBuiltInPopupMenu }

  TdxSpreadSheetReportViewBuiltInPopupMenu = class(TdxSpreadSheetBuiltInTableViewPopupMenu)
  end;

  { TdxSpreadSheetReportPictureInfo }

  TdxSpreadSheetReportPictureInfo = class
  public
    CornerAlign: Boolean;
    Height: Integer;
    Image: TdxSmartImage;
    LockAspectRatio: Boolean;
    OffsetX: Integer;
    OffsetY: Integer;
    Range: TRect;
    Value: Variant;
    Width: Integer;
    destructor Destroy; override;
    function ValidateImage: Boolean;
  end;

  { TdxSpreadSheetReportDesignerHistoryChangeSectionAction }

  TdxSpreadSheetReportDesignerHistoryChangeSectionAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetReportDesignerChangeSectionCommand }

  TdxSpreadSheetDesignerChangeSectionCommand = class(TdxSpreadSheetHistoryCustomCommand)
  private
    function GetDesigner: TdxSpreadSheetReportDesigner;
  protected
    Bounds: TRect;
    Level: Integer;
    SectionType: TdxSpreadSheetReportSectionType;
    procedure DoChange;
  public
    constructor Create(ASection: TdxSpreadSheetReportSection;
      const AName: string; const ABounds: TRect; ALevel: Integer = -1);
    procedure Redo; override;
    procedure Undo; override;

    property Designer: TdxSpreadSheetReportDesigner read GetDesigner;
  end;


var
  FieldChooserBounds: TDictionary<TdxSpreadSheetReportDesigner, TRect>;
  NamesController: TdxReportSectionsNameController;
  DatabaseGlyph: TdxSmartImage;

procedure ExcludeArea(const ABounds, AExtractBounds: TRect; var AArea1, AArea2: TRect; ACheckVertically: Boolean);
begin
  AArea1 := ABounds;
  AArea2 := cxInvalidRect;
  if cxRectIsInvalid(AExtractBounds) then
    Exit;
  AArea2 := ABounds;
  if ACheckVertically then
  begin
    AArea1.Bottom := Min(AArea1.Bottom, AExtractBounds.Top - 1);
    AArea2.Top := Max(AArea2.Top, AExtractBounds.Bottom + 1);
  end
  else
  begin
    AArea1.Right := Min(AArea1.Right, AExtractBounds.Left - 1);
    AArea2.Left := Max(AArea2.Left, AExtractBounds.Right + 1);
  end;
end;

function SectionNameController: TdxReportSectionsNameController;
begin
  if NamesController = nil then
    NamesController := TdxReportSectionsNameController.Create;
  Result := NamesController;
end;

function HasDBToken(AToken: TdxSpreadSheetFormulaToken): Boolean;
var
  ANamePtr: Pointer;
begin
  Result := False;
  if AToken = nil then
    Exit;
  if AToken is TdxSpreadSheetFormulaFunctionToken then
  begin
    ANamePtr := TdxSpreadSheetFormulaFunctionToken(AToken).Information.NamePtr;
    Result := (ANamePtr = @sfnField) or (ANamePtr = @sfnParameter) or (ANamePtr = @sfnFieldPicture);
  end;
  Result := Result or HasDBToken(AToken.FirstChild) or HasDBToken(AToken.Next);
end;

function IsRangeToken(AToken: TdxSpreadSheetFormulaToken): Boolean;
var
  ANamePtr: Pointer;
begin
  Result := AToken is TdxSpreadSheetFormulaFunctionToken;
  if Result then
  begin
    ANamePtr := TdxSpreadSheetFormulaFunctionToken(AToken).Information.NamePtr;
    Result := ANamePtr = @sfnRange;
  end;
end;

function HasRange(AToken: TdxSpreadSheetFormulaToken): Boolean;
begin
  Result := False;
  if AToken = nil then
    Exit;
  Result := IsRangeToken(AToken);
  Result := Result or HasRange(AToken.FirstChild) or HasRange(AToken.Next);
end;

function ExtractArea(AToken: TdxSpreadSheetFormulaToken): TRect;
var
  ARange: TRect;
begin
  ARange := cxInvalidRect;
  AToken.EnumReferences(
    procedure (const AArea: TRect; AView: TObject)
    begin
      ARange := AArea;
    end);
  Result := ARange;
end;

function IsPictureFormula(AToken: TdxSpreadSheetFormulaToken): Boolean;
begin
  Result := False;
  if AToken = nil then
    Exit;
  if AToken is TdxSpreadSheetFormulaFunctionToken then
    Result := TdxSpreadSheetFormulaFunctionToken(AToken).Information.NamePtr = @sfnFieldPicture;
  Result := Result or IsPictureFormula(AToken.FirstChild) or IsPictureFormula(AToken.Next);
end;

function GetDatabaseGlyph: TdxSmartImage;
begin
  if DatabaseGlyph = nil then
  begin
    DatabaseGlyph := TdxSmartImage.Create;
    DatabaseGlyph.LoadFromResource(HInstance, 'DXREPORTDATABASE', RT_RCDATA);
    DatabaseGlyph.Resize(10, 10);
  end;
  Result := DatabaseGlyph;
end;

function ActuallySectionName(const AName: string; ALevel: Integer = -1): string; overload; inline;
begin
  Result := AName;
  if ALevel >= 0 then
    Result := Result + IntToStr(ALevel);
end;

function ActuallySectionName(AType: TdxSpreadSheetReportSectionType; ALevel: Integer = -1): string; overload; inline;
const
  NameMap: array[TdxSpreadSheetReportSectionType] of string =
   (sanHeaderRange, sanDetailRange, sanFooterRange, sanDetailLevelRange, sanGroupHeaderRange, sanGroupFooterRange);
begin
  Result := ActuallySectionName(NameMap[AType], ALevel);
end;

function GetDataBinding(Sender: TdxSpreadSheetFormulaResult; out AIntf: IdxSpreadSheetDataBinding): Boolean;
begin
  Result := Supports(Sender.Owner.Controller, IdxSpreadSheetDataBinding, AIntf) and AIntf.IsActive;
end;

procedure fnField(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AIntf: IdxSpreadSheetDataBinding;
begin
  if GetDataBinding(Sender, AIntf) then
    Sender.AddValue(AIntf.GetFieldValue(Sender.ExtractStringParameter(AParams)))
  else
    Sender.SetError(ecRefErr);
end;

procedure fnFieldPicture(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  procedure AddStandardResult;
  begin
    Sender.AddValue(Format('[%s]', [cxGetResourceString(@sfnFieldPicture)]));
  end;

  function GetNextValue(var AParams: TdxSpreadSheetFormulaToken;
    AClass: TdxSpreadSheetFormulaTokenClass; var AValue: Variant): Boolean;
  var
    AErrorCode: TdxSpreadSheetFormulaErrorCode;
  begin
    Result := (AParams <> nil) and (AParams.FirstChild is AClass);
    if Result then
    begin
      AParams.FirstChild.GetValue(AValue, AErrorCode);
      AParams := AParams.Next;
      if AErrorCode <> ecNone then
        Sender.SetError(AErrorCode);
    end;
  end;

var
  ADesigner: TdxSpreadSheetReportDesigner;
  AIntf: IdxSpreadSheetDataBinding;
  APictureInfo: TdxSpreadSheetReportPictureInfo;
  ASheet: TObject;
  AToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
  S: string;
begin
  ADesigner := TdxSpreadSheetReportDesigner(TdxSpreadSheetReportFormulaController(Sender.Owner.Controller).SpreadSheet);
  if GetDataBinding(Sender, AIntf) and (ADesigner.Builder <> nil) then
  begin
    S := Sender.ExtractStringParameter(AParams);
    if (Sender.GetParamsCount(AParams) >= 3) and ADesigner.DataBinding.GetFieldValue(S, AValue) then
    begin
      APictureInfo := TdxSpreadSheetReportPictureInfo.Create;
      APictureInfo.Value := AValue;
      S := UpperCase(Sender.ExtractStringParameter(AParams, 1));
      if SameText(S, UpperCase(cxGetResourceString(@sRange))) then
        APictureInfo.CornerAlign := False
      else
        if SameText(S, UpperCase(cxGetResourceString(@sTopLeft))) then
          APictureInfo.CornerAlign := True
        else
          Sender.SetError(ecValue);

      APictureInfo.LockAspectRatio := not APictureInfo.CornerAlign;
      AToken := AParams.Next.Next.FirstChild;
      if AToken is TdxSpreadSheetFormulaReference then
        TdxSpreadSheetFormulaReferenceAccess(AToken).ExtractReference(ASheet, APictureInfo.Range);
      AToken := AToken.Parent.Next;
      if GetNextValue(AToken, TdxSpreadSheetFormulaBooleanValueToken, AValue) then
        APictureInfo.LockAspectRatio := AValue;
      if not APictureInfo.CornerAlign and not APictureInfo.LockAspectRatio then
      begin
        if GetNextValue(AToken, TdxSpreadSheetFormulaIntegerValueToken, AValue) then
          APictureInfo.OffsetX := AValue;
        if GetNextValue(AToken, TdxSpreadSheetFormulaIntegerValueToken, AValue) then
          APictureInfo.OffsetY := AValue;
      end;
      if GetNextValue(AToken, TdxSpreadSheetFormulaIntegerValueToken, AValue) then
        APictureInfo.Width := AValue;
      if GetNextValue(AToken, TdxSpreadSheetFormulaIntegerValueToken, AValue) then
        APictureInfo.Height := AValue;
      if Sender.ErrorCode = ecNone then
      begin
        Sender.AddValue(ADesigner.AddPicture(Sender.Owner, APictureInfo));
        Exit;
      end
      else
        APictureInfo.Free;
    end;
  end;
  AddStandardResult;
end;

procedure fnParameters(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  // todo: not implemented yet
  fnField(Sender, AParams);
end;

procedure fnRange(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.SetError(ecValue);
end;

procedure fpiField(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
end;

procedure fpiFieldPicture(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(8, AParamCount, AParamKind);
end;

procedure fpiRange(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
end;

{ TdxSpreadSheetReportFieldDragObject }

constructor TdxSpreadSheetReportFieldDragObject.Create(AControl: TControl;
  ADesigner: TdxSpreadSheetReportDesigner; AField: TdxSpreadSheetReportDesignerDataField);
begin
  inherited Create(AControl);
  Designer := ADesigner;
  Field := AField;
  DragImage := TDragImageList.Create(nil);
  Designer.FieldChooser.DragObject := Self;
end;

destructor TdxSpreadSheetReportFieldDragObject.Destroy;
begin
  Designer.FieldChooser.DragObject := nil;
  FreeAndNil(DragImage);
  inherited Destroy;
end;

function TdxSpreadSheetReportFieldDragObject.GetDragImages: TDragImageList;
begin
  Result := DragImage;
end;

{ TdxSpreadSheetReportFieldList }

procedure TdxSpreadSheetReportFieldList.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  TdxSpreadSheetReportDesignerFieldChooserForm(Owner).Owner.DragObject := nil;
end;

procedure TdxSpreadSheetReportFieldList.DoStartDrag(var DragObject: TDragObject);
var
  P: TPoint;
  ANode: TTreeNode;
begin
  P := GetMouseCursorClientPos;
  DragObject := nil;
  ANode := GetNodeAt(P.X, P.Y);
  if (ANode <> nil) and (TObject(ANode.Data) is TdxSpreadSheetReportDesignerDataField) then
    DragObject := TdxSpreadSheetReportFieldDragObject.Create(Self,
      TdxSpreadSheetReportDesignerFieldChooserForm(Owner).Designer, TdxSpreadSheetReportDesignerDataField(ANode.Data))
  else
    CancelDrag;
end;

{ TdxReportSectionsNameController }

constructor TdxReportSectionsNameController.Create;
begin
  Names := TDictionary<string, TdxSpreadSheetReportSectionType>.Create;
  Initialize;
end;

destructor TdxReportSectionsNameController.Destroy;
begin
  FreeAndNil(Names);
  inherited Destroy;
end;

function TdxReportSectionsNameController.IsSectionName(AName: TdxSpreadSheetDefinedName;
  var AType: TdxSpreadSheetReportSectionType; var AIndex: Integer): Boolean;
begin
  Result := TryGetType(AName.Caption, AType, AIndex);
end;

function TdxReportSectionsNameController.TryGetType(const AName: string;
  var AType: TdxSpreadSheetReportSectionType; var AIndex: Integer): Boolean;
var
  ACandidate: string;
  I: Integer;
begin
  AIndex := -1;
  ACandidate := AName;

  I := Length(ACandidate);
  while (I > 0) and dxWideIsNumeric(ACandidate[I]) do
    Dec(I);

  if (I + 1) <= Length(ACandidate) then
  begin
    TryStrToInt(Copy(ACandidate, I + 1, Length(ACandidate) - I), AIndex);
    SetLength(ACandidate, I);
  end;

  ACandidate := UpperCase(ACandidate);
  Result := Names.TryGetValue(ACandidate, AType);
end;

procedure TdxReportSectionsNameController.Initialize;
begin
  Names.Add(sanHeaderRange, rstHeader);
  Names.Add(sanFooterRange, rstFooter);
  Names.Add(sanDetailRange, rstDetail);
  Names.Add(sanDetailLevelRange, rstDetailLevel);
  Names.Add(sanGroupHeaderRange, rstGroupHeader);
  Names.Add(sanGroupFooterRange, rstGroupFooter);
end;

{ TdxSpreadSheetTableViewResizeAreaDragAndDropObject }

constructor TdxSpreadSheetTableViewResizeAreaDragAndDropObject.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  if View.HitTest.HitObject is TdxSpreadSheetReportSectionViewInfo then
  begin
    Section := TdxSpreadSheetReportSectionViewInfo(View.HitTest.HitObject).Section;
    SourceArea := Section.Bounds;
  end
  else
    raise EAbort.Create('');
  HitObjectData := View.HitTest.HitObjectData;
end;

procedure TdxSpreadSheetTableViewResizeAreaDragAndDropObject.ApplyChanges;
begin
end;

procedure TdxSpreadSheetTableViewResizeAreaDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
end;

procedure TdxSpreadSheetTableViewResizeAreaDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  CheckScrollArea(GetClientCursorPos);
  View.HitTest.Calculate(GetClientCursorPos);
  Accepted := View.HitTest.HitAtCell;
  if View.HitTest.HitAtCell then
    UpdateSectionBounds(TdxSpreadSheetTableViewCellViewInfo(View.HitTest.HitObject).Row,
      TdxSpreadSheetTableViewCellViewInfo(View.HitTest.HitObject).Column);
  inherited DragAndDrop(P, Accepted);
end;

procedure TdxSpreadSheetTableViewResizeAreaDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
var
  R: TRect;
begin
  if not Accepted then
    UpdateSectionBounds(SourceArea)
  else
  begin
    R := Section.Bounds;
    Section.Bounds := SourceArea;
    Designer.SetSectionInfo(ActuallySectionName(Section.SectionType, Section.Index), R, Section.Index);
  end;
  inherited EndDragAndDrop(Accepted);
end;

function TdxSpreadSheetTableViewResizeAreaDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  Result := crSizeNWSE;
  case HitObjectData of
    1, 3:
      Result := crSizeWE;
    2, 4:
      Result := crSizeNS;
  end;
end;

function TdxSpreadSheetTableViewResizeAreaDragAndDropObject.GetSelectionArea: TRect;
begin
  Result := Section.Bounds;
end;

function TdxSpreadSheetTableViewResizeAreaDragAndDropObject.MakeBounds(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result := cxRect(Min(ALeft, ARight), Min(ATop, ABottom), Max(ALeft, ARight), Max(ATop, ABottom));
end;

procedure TdxSpreadSheetTableViewResizeAreaDragAndDropObject.UpdateSectionBounds(ABounds: TRect);
begin
  if not cxRectIsEqual(Section.Bounds, ABounds) then
  begin
    ViewInfo.RecalculateArea(Section);
    Section.Bounds := ABounds;
    ViewInfo.RecalculateArea(Section);
    View.Invalidate;
  end;
end;

procedure TdxSpreadSheetTableViewResizeAreaDragAndDropObject.UpdateSectionBounds(ARow, AColumn: Integer);
begin
  case HitObjectData of
    1:
     UpdateSectionBounds(MakeBounds(AColumn, SourceArea.Top, SourceArea.Right, SourceArea.Bottom));
    2:
     UpdateSectionBounds(MakeBounds(SourceArea.Left, ARow, SourceArea.Right, SourceArea.Bottom));
    3:
     UpdateSectionBounds(MakeBounds(SourceArea.Left, SourceArea.Top, AColumn, SourceArea.Bottom));
    4:
     UpdateSectionBounds(MakeBounds(SourceArea.Left, SourceArea.Top, SourceArea.Right, ARow));
    5:
     UpdateSectionBounds(MakeBounds(SourceArea.Left, SourceArea.Top, AColumn, ARow));
  end;
end;

function TdxSpreadSheetTableViewResizeAreaDragAndDropObject.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := TdxSpreadSheetReportDesigner(Control);
end;

function TdxSpreadSheetTableViewResizeAreaDragAndDropObject.GetViewInfo: TdxSpreadSheetReportTableViewInfo;
begin
  Result := TdxSpreadSheetReportTableViewInfo(TdxSpreadSheetReportTableView(View).ViewInfo);
end;

{ TdxSpreadSheetReportSectionViewInfo }

procedure TdxSpreadSheetReportSectionViewInfo.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
var
  ACaption: string;
  AColor: TColor;
  APrevPPI: Integer;
  R: TRect;
begin
  if ViewInfo.SpreadSheet.ActiveSheet <> Section.Name.Scope then
    Exit;

  APrevPPI := dxSpreadSheetPrepareCanvas(ACanvas, TdxSpreadSheetFontAccess(SpreadSheet.DefaultCellStyle.Font).Handle.GraphicObject);
  try
    AColor := dxGetLighterColor(ViewInfo.SelectionParams.Color, 60);

    ACanvas.SaveClipRegion;
    try
      TdxSpreadSheetSelectionHelper.Draw(ACanvas, Bounds, AColor, ViewInfo.ContentParams.Color, [ssseFrame], [coBottomRight]);
    finally
      ACanvas.RestoreClipRegion;
    end;

    ACaption := cxGetResourceString(AreaCaptions[Section.SectionType]);
    if Section.Index >= 0 then
      ACaption := ACaption + IntToStr(Section.Index);

    R := cxRectInflate(cxRect(ACanvas.TextExtent(ACaption)), cxTextOffset * 2);
    R := cxRectSetRight(R, Bounds.Right + 1);
    if Section.SectionType in [rstFooter, rstGroupFooter] then
      R := cxRectSetBottom(R, Bounds.Bottom + 1)
    else
      R := cxRectSetTop(R, Bounds.Top - cxTextOffset);

    ACanvas.FillRect(R, AColor);
    ACanvas.Font.Color := ViewInfo.ContentParams.Color;
    ACanvas.DrawTexT(ACaption, R, taCenter, vaCenter, False, False);

    TdxSpreadSheetSelectionHelper.Draw(ACanvas, Bounds, AColor, ViewInfo.ContentParams.Color, [ssseCorners], [coBottomRight]);
  finally
    dxSpreadSheetUnprepareCanvas(ACanvas, APrevPPI);
  end;
end;

function TdxSpreadSheetReportSectionViewInfo.CanFillData: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetReportSectionViewInfo.CanMoveSelection: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetReportSectionViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
const
  ACursors: array[TdxSpreadSheetReportSizingArea] of TCursor = (crDefault, crSizeWE, crSizeNS, crSizeWE, crSizeNS, crSizeNWSE);
begin
  if (AHitTest.HitObjectData > Byte(Low(TdxSpreadSheetReportSizingArea))) and (AHitTest.HitObjectData <= Byte(High(TdxSpreadSheetReportSizingArea))) then
    Result := ACursors[TdxSpreadSheetReportSizingArea(AHitTest.HitObjectData)]
  else
    Result := inherited GetCursor(AHitTest);
end;

function TdxSpreadSheetReportSectionViewInfo.GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  if (AHitTest.HitObjectData <> 0) and CanMoveSelection then
    Result := TdxSpreadSheetTableViewResizeAreaDragAndDropObject
  else
    Result := nil;
end;

function TdxSpreadSheetReportSectionViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
const
  HitTestZoneSize = 6;
  Areas: array[TcxBorder] of TdxSpreadSheetReportSizingArea = (saLeft, saTop, saRight, saBottom);
var
  ABorder: TcxBorder;
  ACornerBounds: TRect;

  procedure SetupHitTest(AData: TdxSpreadSheetReportSizingArea);
  begin
    TdxSpreadSheetHitTestAccess(AHitTest).HitObject := Self;
    TdxSpreadSheetHitTestAccess(AHitTest).HitObjectData := Integer(AData);
    TdxSpreadSheetHitTestAccess(AHitTest).SetHitCode(hcResizeArea, True);
  end;

  function GetBorderBounds: TRect;
  begin
    Result := cxRectInflate(Bounds, HitTestZoneSize  div 2);
    case ABorder of
      bLeft:
        Result.Right := Result.Left + HitTestZoneSize;
      bTop:
        Result.Bottom := Result.Top + HitTestZoneSize;
      bRight:
        Result.Left := Result.Right - HitTestZoneSize;
      bBottom:
        Result.Top := Result.Bottom - HitTestZoneSize;
    end;
  end;

begin
  Result := TdxSpreadSheetSelectionHelper.IsInFrame(Bounds, AHitTest.ActualHitPoint);
  if Result then
  begin
    ACornerBounds := TdxSpreadSheetSelectionHelper.GetCornerBounds(Bounds, coBottomRight);
    ACornerBounds := cxRectCenter(ACornerBounds, dxSpreadSheetCornetHitTestZoneSize, dxSpreadSheetCornetHitTestZoneSize);
    if PtInRect(ACornerBounds, AHitTest.ActualHitPoint) then
      SetupHitTest(saBottomRight)
    else
      for ABorder := bLeft to bBottom do
        if PtInRect(GetBorderBounds, AHitTest.ActualHitPoint) then
          SetupHitTest(Areas[ABorder]);
  end;
end;

function TdxSpreadSheetReportSectionViewInfo.GetViewInfo: TdxSpreadSheetTableViewInfo;
begin
  Result := TdxSpreadSheetReportTableView(View).ViewInfo;
end;

{ TdxSpreadSheetReportDragDropFieldAreaCell }

constructor TdxSpreadSheetReportDragDropFieldAreaCell.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FVisible := False;
end;

procedure TdxSpreadSheetReportDragDropFieldAreaCell.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
begin
  TdxSpreadSheetSelectionHelper.Draw(ACanvas, Bounds, clRed, clWhite, [ssseFrame], []);
end;

procedure TdxSpreadSheetReportDragDropFieldAreaCell.Invalidate;
var
  R: TRect;
begin
  if Visible then
  begin
    R := cxRectInflate(Bounds, 5);
    InvalidateRect(R);
    R := cxRectScale(cxRectInflate(Bounds, -5), TdxSpreadSheetCustomViewAccess(View).ZoomFactor, 100);
    ValidateRect(SpreadSheet.Handle, R);
  end;
end;

function TdxSpreadSheetReportDragDropFieldAreaCell.CanFillData: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetReportDragDropFieldAreaCell.CanMoveSelection: Boolean;
begin
  Result := False;
end;

{ TdxSpreadSheetReportHelper }

class procedure TdxSpreadSheetReportHelper.CopyContainers(
  ASource, ATarget: TdxSpreadSheetTableView; const ASourceCellsArea: TRect; const ATargetCell: TPoint);
var
  AContainer: TdxSpreadSheetContainerAccess;
  ASourceAreaBounds: TRect;
  I: Integer;
begin
  ASourceAreaBounds := cxRect(
    ASource.Columns.GetPosition(ASourceCellsArea.Left),
    ASource.Rows.GetPosition(ASourceCellsArea.Top),
    ASource.Columns.GetPosition(ASourceCellsArea.Right + 1),
    ASource.Rows.GetPosition(ASourceCellsArea.Bottom + 1));

  ATarget.BeginUpdate;
  try
    for I := 0 to ASource.Containers.Count - 1 do
    begin
      AContainer := TdxSpreadSheetContainerAccess(ASource.Containers[I]);
      if cxRectIntersect(ASourceAreaBounds, AContainer.Calculator.CalculateBounds) then
        CopyContainer(AContainer, ATarget, ASourceCellsArea, ASourceAreaBounds, ATargetCell);
    end;
  finally
    ATarget.EndUpdate;
  end;
end;

class procedure TdxSpreadSheetReportHelper.CopyFormulas(
  ASource, ATarget: TdxSpreadSheetCell; var AIsRangeFormula: Boolean);
var
  AWriter: TcxWriter;
  AReader: TcxReader;
  AStream: TMemoryStream;
  AFormulaRefs: TdxSpreadSheetFormulaAsTextInfoList;
begin
  AIsRangeFormula := False;
  AStream := TMemoryStream.Create;
  try
    AWriter := TcxWriter.Create(AStream, dxSpreadSheetBinaryFormatVersion);
    try
      TdxSpreadSheetCellAccess(ASource).SaveToStream(AWriter);
    finally
      AWriter.Free;
    end;
    AStream.Position := 0;
    AFormulaRefs := TdxSpreadSheetFormulaAsTextInfoList.Create(ASource.SpreadSheet);
    try
      AReader := TcxReader.Create(AStream, dxSpreadSheetBinaryFormatVersion);
      try
        TdxSpreadSheetCellAccess(ATarget).LoadFromStream(AReader, AFormulaRefs);
      finally
        AReader.Free;
      end;
      AFormulaRefs.ResolveReferences;
    finally
      AFormulaRefs.Free;
    end;
  finally
    AStream.Free;
  end;
  AIsRangeFormula := (ATarget.AsFormula <> nil) and HasRange(ATarget.AsFormula.Tokens);
end;

class procedure TdxSpreadSheetReportHelper.MakePicture(ATarget: TdxSpreadSheetTableView;
  ASection: TdxSpreadSheetReportSection; APictureInfo: TdxSpreadSheetReportPictureInfo);
var
  ADestBounds, AImageBounds, ADestArea: TRect;
  APicture: TdxSpreadSheetPictureContainer;
const
  AFitMode: array[Boolean] of TcxImageFitMode = (ifmFit, ifmProportionalStretch);
begin
  if not APictureInfo.ValidateImage then
    Exit;
  ADestArea := cxRectOffset(cxRectOffset(APictureInfo.Range,
    ASection.ActuallyBounds.TopLeft, False), ASection.DestBounds.TopLeft);
  ADestBounds := IdxSpreadSheetTableView(ATarget).GetAbsoluteCellBounds(ADestArea.Top, ADestArea.Left);
  AImageBounds := cxRectSetSize(ADestBounds, APictureInfo.Image.Size);
  if not APictureInfo.CornerAlign then
  begin
    ADestBounds.BottomRight := IdxSpreadSheetTableView(ATarget).GetAbsoluteCellBounds(
      ADestArea.Bottom, ADestArea.Right).BottomRight;

    Inc(ADestBounds.Left, APictureInfo.OffsetX);
    Inc(ADestBounds.Top, APictureInfo.OffsetY);
    AImageBounds := cxGetImageRect(ADestBounds, APictureInfo.Image.Size,
      AFitMode[APictureInfo.LockAspectRatio], False, 100);
    APictureInfo.Width := cxRectWidth(AImageBounds);
    APictureInfo.Height := cxRectHeight(AImageBounds);
    AImageBounds.Right := AImageBounds.Right - ADestBounds.Right;
    AImageBounds.Bottom := AImageBounds.Bottom - ADestBounds.Bottom;
  end
  else
    ADestBounds := cxRectSetSize(ADestBounds, APictureInfo.Height, APictureInfo.Image.Width);

  ATarget.BeginUpdate;
  try
    ATarget.Containers.Add(TdxSpreadSheetPictureContainer, APicture);
    APicture.Picture.Image := APictureInfo.Image;
    APicture.RelativeResize := APictureInfo.LockAspectRatio;
    APicture.AnchorPoint1.Cell := ATarget.CreateCell(ADestArea.Top, ADestArea.Left);
    APicture.AnchorPoint1.Offset := Point(APictureInfo.OffsetX, APictureInfo.OffsetY);
    if APictureInfo.CornerAlign then
    begin
      APicture.AnchorType := catOneCell;
      APicture.AnchorPoint2.Cell := APicture.AnchorPoint1.Cell;
      APicture.AnchorPoint2.Offset := cxPointOffset(APicture.AnchorPoint1.Offset,
        APictureInfo.Height, APictureInfo.Image.Width);
    end
    else
    begin
      APicture.AnchorType := catTwoCell;
      APicture.AnchorPoint2.Cell := ATarget.CreateCell(ADestArea.Bottom + 1, ADestArea.Right + 1);
      APicture.AnchorPoint2.Offset := AImageBounds.BottomRight;
    end;
  finally
    ATarget.EndUpdate;
  end;
end;

class procedure TdxSpreadSheetReportHelper.CopyContainer(AContainer: TdxSpreadSheetContainer;
  ATarget: TdxSpreadSheetTableView; const ASourceArea, ASourceAreaBounds: TRect; const ATargetCell: TPoint);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveContainer(AStream, AContainer, ASourceArea, ASourceAreaBounds);
    AStream.Position := 0;
    LoadContainer(AStream, ATarget, ATargetCell);
  finally
    AStream.Free;
  end;
end;

class procedure TdxSpreadSheetReportHelper.LoadContainer(
  AStream: TStream; ATarget: TdxSpreadSheetTableView; const ATargetCell: TPoint);
var
  AClass: TClass;
  AContainer: TdxSpreadSheetContainerAccess;
  AContainerBounds: TRect;
  AReader: TcxReader;
  ATargetPoint: TPoint;
begin
  ATargetPoint := cxPoint(ATarget.Columns.GetPosition(ATargetCell.X), ATarget.Rows.GetPosition(ATargetCell.Y));

  AReader := TcxReader.Create(AStream, dxSpreadSheetBinaryFormatVersion);
  try
    AContainerBounds := AReader.ReadRect;
    AClass := GetClass(dxAnsiStringToString(AReader.ReadAnsiString));
    if (AClass <> nil) and AClass.InheritsFrom(TdxSpreadSheetContainer) then
    try
      if AClass.InheritsFrom(TdxSpreadSheetCommentContainer) then
      begin
        ATarget.Containers.Add(TdxSpreadSheetCommentContainer, AContainer);
        AContainer.BeginChanging;
        try
          AContainer.AnchorType := TdxSpreadSheetContainerAnchorType(AReader.ReadWord);
          TdxSpreadSheetCommentContainerAccess(AContainer).LoadFromStream(
            AReader, AReader.ReadWord, ATargetCell.Y, ATargetCell.X);
          AContainer.Calculator.UpdateAnchors(cxRectOffset(AContainerBounds, ATargetPoint));
        finally
          AContainer.EndChanging;
        end;
      end
      else
      begin
        AContainer := TdxSpreadSheetContainerAccess(ATarget.Containers.Add(TdxSpreadSheetContainerClass(AClass)));
        AContainer.BeginChanging;
        try
          AContainer.LoadFromStream(AReader);
          AContainer.Calculator.UpdateAnchors(cxRectOffset(AContainerBounds, ATargetPoint));
        finally
          AContainer.EndChanging;
        end;
      end;
    except
      AContainer.Free;
      raise;
    end;
  finally
    AReader.Free;
  end;
end;

class procedure TdxSpreadSheetReportHelper.SaveContainer(
  AStream: TStream; AContainer: TdxSpreadSheetContainer; const ASourceArea, ASourceAreaBounds: TRect);
var
  AWriter: TcxWriter;
begin
  AWriter := TcxWriter.Create(AStream, dxSpreadSheetBinaryFormatVersion);
  try
    AWriter.WriteRect(cxRectOffset(TdxSpreadSheetContainerAccess(AContainer).Calculator.CalculateBounds, ASourceAreaBounds.TopLeft, False));
    AWriter.WriteAnsiString(dxStringToAnsiString(AContainer.ClassName));
    if AContainer is TdxSpreadSheetCommentContainer then
      TdxSpreadSheetCommentContainerAccess(AContainer).SaveToStream(AWriter, ASourceArea.Top, ASourceArea.Left)
    else
      TdxSpreadSheetContainerAccess(AContainer).SaveToStream(AWriter);
  finally
    AWriter.Free;
  end;
end;

{ TdxSpreadSheetReportPictureInfo }

destructor TdxSpreadSheetReportPictureInfo.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

function TdxSpreadSheetReportPictureInfo.ValidateImage: Boolean;
begin
  Result := True;
  if Image <> nil then
    Exit;
  Image := TdxSmartImage.Create;
  try
    Image.LoadFromFieldValue(Value);
    if Width = 0 then
      Width := Image.Width;
    if Height = 0 then
      Height := Image.Height;
  except
    FreeAndNil(Image);
  end;
  Result := Image <> nil;
end;

{ TdxSpreadSheetReportDesignerHistoryChangeSectionAction }

class function TdxSpreadSheetReportDesignerHistoryChangeSectionAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxChangeSection);
end;

{ TdxSpreadSheetDesignerChangeSectionCommand }

constructor TdxSpreadSheetDesignerChangeSectionCommand.Create(ASection: TdxSpreadSheetReportSection;
  const AName: string; const ABounds: TRect; ALevel: Integer = -1);
begin
  Level := ALevel;
  Bounds := cxInvalidRect;
  if ASection <> nil then
  begin
    SectionType := ASection.SectionType;
    Bounds := ASection.Bounds;
    Level := ASection.Index;
  end
  else
    SectionNameController.TryGetType(AName, SectionType, ALevel);
end;

procedure TdxSpreadSheetDesignerChangeSectionCommand.DoChange;
var
  R: TRect;
  ASection: TdxSpreadSheetReportSection;
begin
  R := cxInvalidRect;
  ASection := Designer.GetSection(SectionType, Level);
  if ASection <> nil then
    R := ASection.Bounds;
  try
    if cxRectIsInvalid(Bounds) then
      Designer.RemoveSection(SectionType, Level)
    else
      Designer.SetSectionInfo(ActuallySectionName(SectionType, Level), Bounds, Level);
  finally
    Bounds := R;
  end;
end;

procedure TdxSpreadSheetDesignerChangeSectionCommand.Redo;
begin
  DoChange;
end;

procedure TdxSpreadSheetDesignerChangeSectionCommand.Undo;
begin
  DoChange;
end;

function TdxSpreadSheetDesignerChangeSectionCommand.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := TdxSpreadSheetReportDesigner(View.SpreadSheet);
end;

{ TdxSpreadSheetReportDesignerDataField }

constructor TdxSpreadSheetReportDesignerDataField.Create(AOwner: TdxSpreadSheetReportDataController);
begin
  FDataController := AOwner;
  FIndex := -1;
  FVisible := True;
  FID := DataController.NextID;
  DataController.AddFieldObject(Self);
end;

destructor TdxSpreadSheetReportDesignerDataField.Destroy;
begin
  DataController.RemoveFieldObject(Self);
  inherited Destroy;
end;

procedure TdxSpreadSheetReportDesignerDataField.Changed;
begin
  DataController.Changed;
end;

function TdxSpreadSheetReportDesignerDataField.GetDisplayName: string;
begin
  Result := FieldName;
  if Field <> nil then
    Result := Field.DisplayName;
end;

function TdxSpreadSheetReportDesignerDataField.GetValue: Variant;
begin
  if DataController.FocusedRecordIndex <> -1 then
    Result := DataController.Values[DataController.FocusedRecordIndex, Index]
  else
    Result := Null;
end;

procedure TdxSpreadSheetReportDesignerDataField.SetIndex(AIndex: Integer);
begin
  FIndex := AIndex;
end;

function TdxSpreadSheetReportDesignerDataField.GetField: TField;
begin
  Result := DataController.GetItemField(FIndex);
end;

function TdxSpreadSheetReportDesignerDataField.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(FIndex);
end;

function TdxSpreadSheetReportDesignerDataField.GetSortOrder: TdxSortOrder;
begin
  Result := DataController.GetItemSortOrder(Index);
end;

procedure TdxSpreadSheetReportDesignerDataField.SetFieldName(const AValue: string);
begin
  DataController.ChangeFieldName(Index, AValue);
  Changed;
end;

procedure TdxSpreadSheetReportDesignerDataField.SetSortOrder(AValue: TdxSortOrder);
begin
  DataController.ChangeSorting(Index, AValue);
  Changed;
end;

procedure TdxSpreadSheetReportDesignerDataField.SetVisible(AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

{ TdxSpreadSheetReportSortedField }

constructor TdxSpreadSheetReportSortedField.Create(Collection: TCollection);
begin
  FSortOrder := soAscending;
  inherited Create(Collection);
end;

procedure TdxSpreadSheetReportSortedField.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetReportSortedField then
  begin
    FieldName := TdxSpreadSheetReportSortedField(Source).FieldName;
    SortOrder := TdxSpreadSheetReportSortedField(Source).SortOrder;
  end;
end;

procedure TdxSpreadSheetReportSortedField.ReportChanged;
begin
  if Collection <> nil then
    TdxSpreadSheetReportSortedFields(Collection).Designer.AddChanges(sscLayoutAndData);
end;

procedure TdxSpreadSheetReportSortedField.SetFieldName(const AValue: string);
begin
  if FieldName <> AValue then
  begin
    FFieldName := AValue;
    ReportChanged;
  end;
end;

procedure TdxSpreadSheetReportSortedField.SetSortOrder(const AValue: TdxSortOrder);
begin
  if SortOrder <> AValue then
  begin
    FSortOrder := AValue;
    ReportChanged;
  end;
end;

{ TdxSpreadSheetReportSortedFields }

function TdxSpreadSheetReportSortedFields.GetDataController: TdxSpreadSheetReportDataController;
begin
  Result := nil;
  if Owner is TdxSpreadSheetReportDataBinding then
    Result := TdxSpreadSheetReportDataBinding(Owner).DataController
  else
    if Owner is TdxSpreadSheetReportDetail then
      Result := TdxSpreadSheetReportDetail(Owner).DataController;
end;

function TdxSpreadSheetReportSortedFields.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := nil;
  if DataController <> nil then
    Result := DataController.Designer;
 end;

function TdxSpreadSheetReportSortedFields.GetItem(AIndex: Integer): TdxSpreadSheetReportSortedField;
begin
  Result := inherited Items[AIndex] as TdxSpreadSheetReportSortedField;
end;

procedure TdxSpreadSheetReportSortedFields.SetItem(AIndex: Integer; AValue: TdxSpreadSheetReportSortedField);
begin
  inherited Items[AIndex].Assign(AValue);
end;

{ TdxSpreadSheetReportDataGroup }

procedure TdxSpreadSheetReportDataGroup.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetReportDataGroup then
    FSectionID := TdxSpreadSheetReportDataGroup(Source).SectionID;
  inherited Assign(Source);
end;

procedure TdxSpreadSheetReportDataGroup.SetCollection(Value: TCollection);
begin
  inherited SetCollection(Value);
  if Collection <> nil then
    FSectionID := Index;
end;

function TdxSpreadSheetReportDataGroup.IsSectionIDStored: Boolean;
begin
  Result := (Collection = nil) or (SectionID <> Index)
end;

{ TdxSpreadSheetReportDataGroups }

function TdxSpreadSheetReportDataGroups.GetItem(AIndex: Integer): TdxSpreadSheetReportDataGroup;
begin
  Result := inherited Items[AIndex] as TdxSpreadSheetReportDataGroup;
end;

procedure TdxSpreadSheetReportDataGroups.SetItem(AIndex: Integer; AValue: TdxSpreadSheetReportDataGroup);
begin
  inherited Items[AIndex].Assign(AValue);
end;

{ TdxSpreadSheetReportDataController }

constructor TdxSpreadSheetReportDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetMasterMode(nil, True);
  FFields := TcxObjectList.Create;
  FFilterableItems := TList.Create;
  FFieldNames := TDictionary<string, TdxSpreadSheetReportDesignerDataField>.Create;
end;

destructor TdxSpreadSheetReportDataController.Destroy;
begin
  FreeAndNil(FFieldNames);
  FreeAndNil(FFilterableItems);
  FreeAndNil(FFields);
  inherited Destroy;
end;

function TdxSpreadSheetReportDataController.GetItem(Index: Integer): TObject;
begin
  Result := FFields[Index];
end;

function TdxSpreadSheetReportDataController.AddField: TdxSpreadSheetReportDesignerDataField;
begin
  Result := TdxSpreadSheetReportDesignerDataField.Create(Self);
end;

procedure TdxSpreadSheetReportDataController.CreateAllItems(AMissingItemsOnly: Boolean = False);
var
  I: Integer;
begin
  if DataSet <> nil then
  begin
    BeginUpdate;
    try
      if not AMissingItemsOnly then
      begin
        FNextID := 0;
        for I := FFields.Count - 1 downto 0 do
          FFields[I].Free;
      end;
      for I := 0 to DataSet.FieldCount - 1 do
        if not AMissingItemsOnly or (GetItemByFieldName(DataSet.Fields[I].FieldName) = nil) then
          CreateItemByField(DataSet.Fields[I]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetReportDataController.UpdateItems(AUpdateFields: Boolean);
begin
  inherited UpdateItems(AUpdateFields);
  if  (DataControllerInfo.LockCount = 0) and (Designer <> nil) and (FieldsList <> nil) then
    Designer.DataSetChange;
end;

procedure TdxSpreadSheetReportDataController.ActiveChanged(AActive: Boolean);
begin
  inherited ActiveChanged(AActive);
  DoDataSourceChanged;
end;

procedure TdxSpreadSheetReportDataController.AddFieldObject(AField: TdxSpreadSheetReportDesignerDataField);
begin
  FieldsList.Add(AField);
  AddItem(AField);
  UpdateIndexes;
  Inc(FNextID);
end;

procedure TdxSpreadSheetReportDataController.BeforeBuildReport;

  procedure ApplySortingAndGrouping(ASortedFields, ADataGroups: TdxSpreadSheetReportSortedFields);
  var
    AIndex: Integer;
    ADataField: TdxSpreadSheetReportDesignerDataField;
  begin
    BeginUpdate;
    try
      for AIndex := 0 to ADataGroups.Count - 1 do
        if FieldNames.TryGetValue(UpperCase(ADataGroups[AIndex].FieldName), ADataField) then
        begin
          ChangeSorting(ADataField.Index, ADataGroups[AIndex].SortOrder);
          DataControllerInfo.ChangeGrouping(inherited Fields.FieldByItem(ADataField), AIndex);
        end;
      for AIndex := 0 to ASortedFields.Count - 1 do
        if FieldNames.TryGetValue(UpperCase(ASortedFields[AIndex].FieldName), ADataField) then
          ChangeSorting(ADataField.Index, ASortedFields[AIndex].SortOrder);
    finally
      EndUpdate;
    end;
  end;

var
  I: Integer;
  AField: TdxSpreadSheetReportDesignerDataField;
begin
  FieldNames.Clear;
  ClearSorting(False);
  DataControllerInfo.ClearGrouping;
  for I := 0 to FieldCount - 1 do
  begin
    AField := Fields[I];
    FieldNames.AddOrSetValue(UpperCase(AField.GetDisplayName), AField);
    FieldNames.AddOrSetValue(UpperCase(GetFieldName(AField.GetDisplayName, True)), AField);
  end;
  if Owner is TdxSpreadSheetReportDetail then
    ApplySortingAndGrouping(TdxSpreadSheetReportDetail(Owner).SortedFields, TdxSpreadSheetReportDetail(Owner).DataGroups)
  else
    ApplySortingAndGrouping(Designer.DataBinding.SortedFields, Designer.DataBinding.DataGroups);
end;

procedure TdxSpreadSheetReportDataController.Changed;
begin
  Change([dccLayout]);
end;

function TdxSpreadSheetReportDataController.CreateItemByField(AField: TField): TdxSpreadSheetReportDesignerDataField;
begin
  Result := AddField;
  Result.FieldName := AField.FieldName;
  Result.Visible := AField.Visible;
  if Result.Visible then
    FilterableItems.Add(Result);
end;

procedure TdxSpreadSheetReportDataController.DoDataSourceChanged;
begin
  CreateAllItems(False);
end;

function TdxSpreadSheetReportDataController.GetFieldName(
  const AFieldName: string; AUseStorageName: Boolean = True): string;
begin
  Result := AFieldName;
  if AUseStorageName then
    Result := DisplayName + '.' + Result;
end;

{function TdxSpreadSheetReportDataController.GetFieldValue(const AFieldName: string): Variant;
begin

end;}

procedure TdxSpreadSheetReportDataController.MasterDetailRelationsInitialize;
var
  I: Integer;
  ARelation: TcxCustomDataRelation;
begin
  BeginUpdate;
  try
    Relations.Clear;
    if MasterDataController = nil then
      SetMasterMode(nil, False)
    else
    begin
      MasterDataController.Relations.Clear;
      ARelation := MasterDataController.Relations.Add(Self);
      SetMasterRelation(ARelation, -1);
      SetMasterRelation(ARelation, MasterDataController.FocusedRecordIndex);
    end;
  finally
    EndUpdate;
  end;
  if DataGroups.Count > 0 then
  begin
    BeginUpdate;
    try
      for I := RowCount - 1 downto 0 do
        DataControllerInfo.ChangeExpanding(I, True, True);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetReportDataController.MasterDetailRelationsFinalize;
begin
  BeginUpdate;
  try
    Relations.Clear;
    SetMasterMode(nil, True);
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetReportDataController.PopulateFields(
  AFields: TDictionary<string, TdxSpreadSheetReportDesignerDataField>);
var
  I: Integer;
begin
  for I := 0 to FieldCount - 1 do
    AFields.AddOrSetValue(UpperCase(Fields[I].DisplayName), Fields[I]);
end;

procedure TdxSpreadSheetReportDataController.RemoveFieldObject(AField: TdxSpreadSheetReportDesignerDataField);
begin
  if FieldsList = nil then
    Exit;
  FieldsList.Remove(AField);
  RemoveItem(AField);
  FilterableItems.Remove(AField);
  UpdateIndexes;
end;

procedure TdxSpreadSheetReportDataController.UpdateIndexes;
var
  I: Integer;
begin
  for I := 0 to FieldsList.Count - 1 do
    TdxSpreadSheetReportDesignerDataField(FieldsList[I]).SetIndex(I);
  UpdateItemIndexes;
end;

function TdxSpreadSheetReportDataController.GetCaption(Index: Integer): string;
begin
  Result := GetItemLinkName(Index);
end;

function TdxSpreadSheetReportDataController.GetCount: Integer;
begin
  Result := FilterableItems.Count;
end;

function TdxSpreadSheetReportDataController.GetCriteria: TcxFilterCriteria;
begin
  Result := Filter;
end;

function TdxSpreadSheetReportDataController.GetFilterFieldName(Index: Integer): string;
begin
  Result := TdxSpreadSheetReportDesignerDataField(FilterableItems[Index]).FieldName;
end;

function TdxSpreadSheetReportDataController.GetInternalDisplayName: string;
begin
  Result := '';
  if DataSet <> nil then
    Result := DataSet.Name;
end;

function TdxSpreadSheetReportDataController.GetItemLink(Index: Integer): TObject;
begin
  Result := TdxSpreadSheetReportDesignerDataField(FilterableItems[Index]);
end;

function TdxSpreadSheetReportDataController.GetItemLinkID(Index: Integer): Integer;
begin
  Result := TdxSpreadSheetReportDesignerDataField(FilterableItems[Index]).ID;
end;

function TdxSpreadSheetReportDataController.GetItemLinkName(Index: Integer): string;
begin
  Result := TdxSpreadSheetReportDesignerDataField(FilterableItems[Index]).FieldName;
end;

function TdxSpreadSheetReportDataController.GetItemID(AItem: TObject): Integer;
begin
  if AItem is TdxSpreadSheetReportDesignerDataField then
    Result := TdxSpreadSheetReportDesignerDataField(AItem).ID
  else
    Result := inherited GetItemID(AItem);
end;

function TdxSpreadSheetReportDataController.GetItemName(AItem: TObject): string;
begin
  if AItem is TdxSpreadSheetReportDesignerDataField then
    Result := TdxSpreadSheetReportDesignerDataField(AItem).FieldName
  else
    Result := inherited GetItemName(AItem);
end;

function TdxSpreadSheetReportDataController.GetProperties(Index: Integer): TcxCustomEditProperties;
var
  ARepository: TcxEditRepositoryItem;
begin
  Result := nil;
  ARepository := GetDefaultEditDBRepositoryItems.GetItemByField(
    TdxSpreadSheetReportDesignerDataField(FilterableItems[Index]).Field);
  if ARepository <> nil then
    Result := ARepository.Properties
end;

function TdxSpreadSheetReportDataController.GetValueType(Index: Integer): TcxValueTypeClass;
begin
  Result := GetItemValueTypeClass(TdxSpreadSheetReportDesignerDataField(FilterableItems[Index]).Index);
end;

function TdxSpreadSheetReportDataController.GetDataControllerFromOwner: TdxSpreadSheetReportDataController;
begin
  Result := nil;
  if Owner is TdxSpreadSheetReportDetail then
    Result := TdxSpreadSheetReportDetails(TdxSpreadSheetReportDetail(Owner).Collection).MasterDataController;
end;

function TdxSpreadSheetReportDataController.GetDataGroups: TdxSpreadSheetReportDataGroups;
begin
  if Owner is TdxSpreadSheetReportDetail then
    Result := TdxSpreadSheetReportDetail(Owner).DataGroups
  else
    Result := Designer.DataBinding.DataGroups;
end;

function TdxSpreadSheetReportDataController.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := nil;
  if Owner is TdxSpreadSheetReportDetail then
    Result := TdxSpreadSheetReportDetail(Owner).Designer
  else
    if Owner is TdxSpreadSheetReportDesigner then
      Result := TdxSpreadSheetReportDesigner(Owner);
end;

function TdxSpreadSheetReportDataController.GetDisplayName: string;
begin
  Result := FDisplayName;
  if Result = '' then
    Result := GetInternalDisplayName;
end;

function TdxSpreadSheetReportDataController.GetFieldCount: Integer;
begin
  Result := ItemCount;
end;

function TdxSpreadSheetReportDataController.GetField(AIndex: Integer): TdxSpreadSheetReportDesignerDataField;
begin
  Result := FFields[AIndex] as TdxSpreadSheetReportDesignerDataField;
end;

function TdxSpreadSheetReportDataController.GetIsActive: Boolean;
begin
  Result := Active;
end;

function TdxSpreadSheetReportDataController.GetIsRoot: Boolean;
begin
  Result := Owner = Designer;
end;

procedure TdxSpreadSheetReportDataController.SetDisplayName(const AValue: string);
begin
  if DisplayName <> AValue then
  begin
    FDisplayName := AValue;
    DoDataSourceChanged;
  end;
end;

procedure TdxSpreadSheetReportDataController.SetField(AIndex: Integer; AValue: TdxSpreadSheetReportDesignerDataField);
begin
  Fields[AIndex].Assign(AValue);
end;

{ TdxSpreadSheetReportDataControllerOptions }

constructor TdxSpreadSheetReportDataControllerOptions.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
end;

procedure TdxSpreadSheetReportDataControllerOptions.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetReportDataControllerOptions then
  begin
    AnsiSort := TdxSpreadSheetReportDataControllerOptions(Source).AnsiSort;
    CaseInsensitive := TdxSpreadSheetReportDataControllerOptions(Source).CaseInsensitive;
    DisplayName := TdxSpreadSheetReportDataControllerOptions(Source).DisplayName;
  end;
end;

procedure TdxSpreadSheetReportDataControllerOptions.Changed;
begin
  DataController.Designer.AddChanges(sscLayoutAndData);
end;

function TdxSpreadSheetReportDataControllerOptions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TdxSpreadSheetReportDataControllerOptions.GetAnsiSort: Boolean;
begin
  Result := dcoAnsiSort in DataController.Options;
end;

function TdxSpreadSheetReportDataControllerOptions.GetCaseInsensitive: Boolean;
begin
  Result := dcoCaseInsensitive in DataController.Options;
end;

function TdxSpreadSheetReportDataControllerOptions.GetDataController: TdxSpreadSheetReportDataController;
begin
  if GetOwner is TdxSpreadSheetReportDataBinding then
    Result := TdxSpreadSheetReportDataBinding(GetOwner).DataController
  else
    if GetOwner is TdxSpreadSheetReportDetail then
      Result := TdxSpreadSheetReportDetail(GetOwner).DataController
    else
      Result := nil;
end;

function TdxSpreadSheetReportDataControllerOptions.GetDisplayName: string;
begin
  Result := DataController.DisplayName;
end;

procedure TdxSpreadSheetReportDataControllerOptions.SetAnsiSort(AValue: Boolean);
begin
  if AnsiSort <> AValue then
  begin
    DataController.Options := DataController.Options + [dcoAnsiSort];
    Changed;
  end
end;

procedure TdxSpreadSheetReportDataControllerOptions.SetCaseInsensitive(AValue: Boolean);
begin
  if CaseInsensitive <> AValue then
  begin
    DataController.Options := DataController.Options + [dcoCaseInsensitive];
    Changed;
  end
end;

procedure TdxSpreadSheetReportDataControllerOptions.SetDisplayName(const AValue: string);
begin
  DataController.DisplayName := AValue;
end;

function TdxSpreadSheetReportDataControllerOptions.IsDisplayNameStored: Boolean;
begin
  Result := (DataController.DisplayName <> '') and
    (DataController.DisplayName <> DataController.GetInternalDisplayName);
end;

{ TdxSpreadSheetReportDetail }

constructor TdxSpreadSheetReportDetail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSectionID := -1;
end;

destructor TdxSpreadSheetReportDetail.Destroy;
begin
  FreeAndNil(FDataGroups);
  FreeAndNil(FSortedFields);
  FreeAndNil(FDetails);
  FreeAndNil(FOptions);
  FreeAndNil(FDataController);
  inherited Destroy;
end;

procedure TdxSpreadSheetReportDetail.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetReportDetail then
  begin
    DataSource := TdxSpreadSheetReportDetail(Source).DataSource;
    Details := TdxSpreadSheetReportDetail(Source).Details;
    MasterKeyFieldName := TdxSpreadSheetReportDetail(Source).MasterKeyFieldName;
    Options := TdxSpreadSheetReportDetail(Source).Options;
    SectionID := TdxSpreadSheetReportDetail(Source).SectionID;
    DataGroups := TdxSpreadSheetReportDetail(Source).DataGroups;
    SortedFields := TdxSpreadSheetReportDetail(Source).SortedFields;
  end
  else
    inherited
end;

procedure TdxSpreadSheetReportDetail.SetParentComponent(Value: TComponent);
begin
  FDataController := CreateDataController;
  FDetails := CreateDetails;
  FOptions := CreateOptions;
  FDataGroups := CreateDataGroups;
  FSortedFields := CreateSortedFields;
  inherited SetParentComponent(Value);
end;

function TdxSpreadSheetReportDetail.CreateDataController: TdxSpreadSheetReportDataController;
begin
  Result := TdxSpreadSheetReportDataController.Create(Self);
end;

function TdxSpreadSheetReportDetail.CreateDataGroups: TdxSpreadSheetReportDataGroups;
begin
  Result := TdxSpreadSheetReportDataGroups.Create(Self, TdxSpreadSheetReportDataGroup);
end;

function TdxSpreadSheetReportDetail.CreateDetails: TdxSpreadSheetReportDetails;
begin
  Result := TdxSpreadSheetReportDetails.Create(Self, TdxSpreadSheetReportDetail);
end;

function TdxSpreadSheetReportDetail.CreateOptions: TdxSpreadSheetReportDataControllerOptions;
begin
  Result := TdxSpreadSheetReportDataControllerOptions.Create(Self);
end;

function TdxSpreadSheetReportDetail.CreateSortedFields: TdxSpreadSheetReportSortedFields;
begin
  Result := TdxSpreadSheetReportSortedFields.Create(Self, TdxSpreadSheetReportSortedField);
end;

procedure TdxSpreadSheetReportDetail.GetChildren(Proc: TGetChildProc; Root: TComponent);

  procedure DoStore(ADetail: TdxSpreadSheetReportDetail);
  begin
    Proc(ADetail);
  end;

var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to Details.Count - 1 do
    Proc(Details[I]);
end;

function TdxSpreadSheetReportDetail.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := nil;
  if AParent is TdxSpreadSheetReportDetail then
    Result := TdxSpreadSheetReportDetail(AParent).Details
  else
    if AParent is TdxSpreadSheetReportDesigner then
      Result := TdxSpreadSheetReportDesigner(AParent).DataBinding.Details;
end;

function TdxSpreadSheetReportDetail.GetOwner: TPersistent;
begin
  Result := Collection;
end;

procedure TdxSpreadSheetReportDetail.PopulateFields(
  AFields: TDictionary<string, TdxSpreadSheetReportDesignerDataField>);
begin
  DataController.PopulateFields(AFields);
  Details.PopulateFields(AFields);
end;

procedure TdxSpreadSheetReportDetail.SetCollection(AValue: TcxComponentCollection);
begin
  inherited SetCollection(AValue);
  if Designer <> nil then
  begin
    if FSectionID = -1 then
      FSectionID := Designer.DetailID;
    Designer.DetailID := Designer.DetailID + 1;
    if not (Designer.IsDesigning or Designer.IsLoading) then
      Designer.PopulateReportSections;
  end;
end;

function TdxSpreadSheetReportDetail.GetDataSource: TDataSource;
begin
  Result := DataController.DataSource;
end;

function TdxSpreadSheetReportDetail.GetDetailKeyFieldName: string;
begin
  Result := DataController.DetailKeyFieldNames;
end;

function TdxSpreadSheetReportDetail.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TdxSpreadSheetReportDetails(Collection).Designer;
end;

function TdxSpreadSheetReportDetail.GetFilter: IcxFilterControl;
begin
  Supports(DataController, IcxFilterControl, Result);
end;

function TdxSpreadSheetReportDetail.GetMasterKeyFieldName: string;
begin
  Result := DataController.MasterKeyFieldNames;
end;

procedure TdxSpreadSheetReportDetail.SetDataGroups(AValue: TdxSpreadSheetReportDataGroups);
begin
  DataGroups.Assign(AValue);
end;

procedure TdxSpreadSheetReportDetail.SetDataSource(AValue: TDataSource);
begin
  if DataSource <> AValue then
  begin
    DataController.BeginUpdate;
    try
      DataController.DataSource := AValue;
    finally
      DataController.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetReportDetail.SetDetailKeyFieldName(const AValue: string);
begin
  if AValue <> DetailKeyFieldName then
    DataController.DetailKeyFieldNames := AValue;
end;

procedure TdxSpreadSheetReportDetail.SetDetails(AValue: TdxSpreadSheetReportDetails);
begin
  Details.Assign(AValue);
end;

procedure TdxSpreadSheetReportDetail.SetMasterKeyFieldName(const AValue: string);
begin
  if AValue <> MasterKeyFieldName then
    DataController.MasterKeyFieldNames := AValue;
end;

procedure TdxSpreadSheetReportDetail.SetOptions(AValue: TdxSpreadSheetReportDataControllerOptions);
begin
  FOptions.Assign(AValue);
end;

procedure TdxSpreadSheetReportDetail.SetSectionID(AValue: Integer);
begin
  FSectionID := Max(0, AValue);
end;

procedure TdxSpreadSheetReportDetail.SetSortedFields(AValue: TdxSpreadSheetReportSortedFields);
begin
  SortedFields.Assign(AValue);
end;

{ TdxSpreadSheetReportDetails }

function TdxSpreadSheetReportDetails.Add: TdxSpreadSheetReportDetail;
begin
  Result := inherited Add as TdxSpreadSheetReportDetail;
end;

function TdxSpreadSheetReportDetails.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := GetParentControl as TdxSpreadSheetReportDesigner;
end;

function TdxSpreadSheetReportDetails.GetItem(AIndex: Integer): TdxSpreadSheetReportDetail;
begin
  Result := inherited Items[AIndex] as TdxSpreadSheetReportDetail;
end;

function TdxSpreadSheetReportDetails.GetMasterDataController: TdxSpreadSheetReportDataController;
begin
  Result := nil;
  if ParentComponent is TdxSpreadSheetReportDesigner then
    Result := TdxSpreadSheetReportDesigner(ParentComponent).DataBinding.DataController
  else
    if ParentComponent is TdxSpreadSheetReportDetail then
      Result := TdxSpreadSheetReportDetail(ParentComponent).DataController;
end;

procedure TdxSpreadSheetReportDetails.SetItem(AIndex: Integer; AValue: TdxSpreadSheetReportDetail);
begin
  Items[AIndex].Assign(AValue);
end;

function TdxSpreadSheetReportDetails.GetOwner: TPersistent;
begin
  if ParentComponent is TdxSpreadSheetReportDesigner then
    Result := TdxSpreadSheetReportDesigner(ParentComponent).DataBinding
  else
    Result := ParentComponent;
end;

function TdxSpreadSheetReportDetails.GetParentControl: TControl;
begin
  if ParentComponent is TdxSpreadSheetReportDesigner then
    Result := TdxSpreadSheetReportDesigner(ParentComponent)
  else
    if ParentComponent is TdxSpreadSheetReportDetail then
      Result := TdxSpreadSheetReportDetail(ParentComponent).Designer
    else
      Result := inherited GetParentControl;
end;

procedure TdxSpreadSheetReportDetails.PopulateFields(
  AFields: TDictionary<string, TdxSpreadSheetReportDesignerDataField>);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].PopulateFields(AFields);
end;

procedure TdxSpreadSheetReportDetails.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  if GetParentForm(Designer) = nil then
    Exit;
  AItem.Name := CreateUniqueName(GetParentForm(Designer), Designer, AItem, 'TdxSpreadSheetReport', '', Designer.DetailID);
end;

{ TdxSpreadSheetReportSection }

constructor TdxSpreadSheetReportSection.Create(AName: TdxSpreadSheetDefinedName;
  AType: TdxSpreadSheetReportSectionType; AIndex: Integer);
begin
  SectionType := AType;
  Index := AIndex;
  Name := AName;
  ExtractBounds := cxInvalidRect;
  Bounds := TdxSpreadSheetDefinedNameHelper.GetBoundingArea(Name);
  Cell := TdxSpreadSheetReportSectionViewInfo.Create(AName.SpreadSheet.ActiveSheetAsTable);
  TdxSpreadSheetReportSectionViewInfo(Cell).Section := Self;
end;

destructor TdxSpreadSheetReportSection.Destroy;
begin
  ClearCache;
  FreeAndNil(Cell);
  inherited Destroy;
end;

function TdxSpreadSheetReportSection.ValidateRange(var ARange: TRect): Boolean;
begin
  Result := (ARange.Top >= Bounds.Top) and (ARange.Bottom <= Bounds.Bottom);
  if Result then
  begin
    ARange.Top := DestBounds.Top + (ARange.Top - Bounds.Top);
    ARange.Bottom := DestBounds.Bottom - (Bounds.Bottom - ARange.Bottom);
  end;
end;

procedure TdxSpreadSheetReportSection.CalculateCache(ASheet: TdxSpreadSheetTableView;
  ADetails: TObjectList<TdxSpreadSheetReportLevelBuildInfo>; AIsVerticalLayout: Boolean);
var
  AStage: Boolean;
  ACell: TdxSpreadSheetCell;
  ARow: TdxSpreadSheetTableItem;
  ARowIndex, ADataRowIndex, AColumnIndex, AIndex, ACount: Integer;
begin
  if MergedCells[True] <> nil then
    Exit;
  ExcludeArea(ActuallyBounds, ExtractBounds, CachedBounds[True], CachedBounds[False], AIsVerticalLayout);
  for AStage := False to True do
  begin
    MergedCells[AStage] := TList<TdxSpreadSheetMergedCell>.Create;
    Formulas[AStage] := TList<TdxSpreadSheetCustomFormula>.Create;
    if not cxRectIsInvalid(CachedBounds[AStage]) then
    begin
      ASheet.MergedCells.Extract(CachedBounds[AStage], MergedCells[AStage]);
      for AIndex := MergedCells[AStage].Count - 1 downto 0 do
        if dxSpreadSheetIntersects(MergedCells[AStage][AIndex].Area, ExtractBounds) then
          MergedCells[AStage].Delete(AIndex);
    end;
  end;
  //
  ACount := cxRectWidth(ActuallyBounds) + 1;
  SetLength(DataMap, cxRectHeight(ActuallyBounds) + 1, ACount + 1);
  ARowIndex := ActuallyBounds.Top;
  while ARowIndex <= ActuallyBounds.Bottom do
  begin
    ARow := ASheet.Rows[ARowIndex];
    ADataRowIndex := ARowIndex - ActuallyBounds.Top;
    Inc(ARowIndex);
    DataMap[ADataRowIndex, 0] := ARow;
    if ARow = nil then
      Continue;
    for AColumnIndex := ActuallyBounds.Left to ActuallyBounds.Right do
    begin
      ACell := ARow.Cells[AColumnIndex];
      DataMap[ADataRowIndex, 1 + AColumnIndex - ActuallyBounds.Left] := ACell;
      if (ACell <> nil) and ACell.IsFormula and
        TdxSpreadSheetReportDesigner(ACell.SpreadSheet).IsDBFormula(ACell.AsFormula) then
      begin
        if dxSpreadSheetContains(CachedBounds[True], ARowIndex - 1, AColumnIndex) then
          Formulas[True].Add(ACell.AsFormula)
        else
          if dxSpreadSheetContains(CachedBounds[False], ARowIndex - 1, AColumnIndex) then
            Formulas[False].Add(ACell.AsFormula);
      end;
    end;
  end;
  //
  if not cxRectIsInvalid(ExtractBounds) then
    for ARowIndex := ExtractBounds.Top to ExtractBounds.Bottom do
      DataMap[ARowIndex - ActuallyBounds.Top, 0] := nil;
end;

procedure TdxSpreadSheetReportSection.ClearCache;
var
  AStage: Boolean;
begin
  if Self = nil then
    Exit;
  for AStage := False to True do
  begin
    FreeAndNil(MergedCells[AStage]);
    FreeAndNil(Formulas[AStage]);
    CachedBounds[AStage] := cxInvalidRect;
  end;
  SetLength(DataMap, 0, 0);
  DestBounds := cxNullRect;
end;

{ TdxSpreadSheetReportDataBinding }

constructor TdxSpreadSheetReportDataBinding.Create(AOwner: TdxSpreadSheetReportDesigner);
begin
  FDesigner := AOwner;
  FDetails := CreateDetails;
  FDataController := CreateDataController;
  FDataGroups := CreateDataGroups;
  FSortedFields := CreateSortedFields;
  FOptions := CreateOptions;
  FFields := TDictionary<string, TdxSpreadSheetReportDesignerDataField>.Create();
end;

destructor TdxSpreadSheetReportDataBinding.Destroy;
begin
  FreeAndNil(FDataGroups);
  FreeAndNil(FSortedFields);
  FreeAndNil(FFields);
  FreeAndNil(FOptions);
  FreeAndNil(FDetails);
  FreeAndNil(FDataController);
  inherited Destroy;
end;

procedure TdxSpreadSheetReportDataBinding.Assign(ASource: TPersistent);
begin
  if ASource is TdxSpreadSheetReportDataBinding then
  begin
    DataSource := TdxSpreadSheetReportDataBinding(ASource).DataSource;
    Details := TdxSpreadSheetReportDataBinding(ASource).Details;
    Options := TdxSpreadSheetReportDataBinding(ASource).Options;
    DataGroups := TdxSpreadSheetReportDataBinding(ASource).DataGroups;
    SortedFields := TdxSpreadSheetReportDataBinding(ASource).SortedFields;
  end
  else
    inherited;
end;

function TdxSpreadSheetReportDataBinding.CreateDataController: TdxSpreadSheetReportDataController;
begin
  Result := TdxSpreadSheetReportDataController.Create(Designer);
end;

function TdxSpreadSheetReportDataBinding.CreateDataGroups: TdxSpreadSheetReportDataGroups;
begin
  Result := TdxSpreadSheetReportDataGroups.Create(Self, TdxSpreadSheetReportDataGroup);
end;

function TdxSpreadSheetReportDataBinding.CreateDetails: TdxSpreadSheetReportDetails;
begin
  Result := TdxSpreadSheetReportDetails.Create(Designer, TdxSpreadSheetReportDetail);
end;

function TdxSpreadSheetReportDataBinding.CreateOptions: TdxSpreadSheetReportDataControllerOptions;
begin
  Result := TdxSpreadSheetReportDataControllerOptions.Create(Self);
end;

function TdxSpreadSheetReportDataBinding.CreateSortedFields: TdxSpreadSheetReportSortedFields;
begin
  Result := TdxSpreadSheetReportSortedFields.Create(Self, TdxSpreadSheetReportSortedField);
end;

function TdxSpreadSheetReportDataBinding.GetFieldValue(const AFieldName: string; var AValue: Variant): Boolean;
var
  AName: string;
  AField: TdxSpreadSheetReportDesignerDataField;
  ADataController: TdxSpreadSheetReportDataController;
begin
  Result := DataController.IsActive;
  if not Result or (Designer.Builder = nil) then
  begin
    AValue := Format('[%s]', [AFieldName]);
    Result := True;
    Exit;
  end;
  AName := UpperCase(AFieldName);
  ADataController := Designer.Builder.ActiveDataController;
  Result := ADataController.FieldNames.TryGetValue(AName, AField);
  if not Result then
  begin
    Result := Fields.TryGetValue(AName, AField);
    if not Result then
      Result := Fields.TryGetValue(UpperCase(DataController.DisplayName + '.' + AName), AField);
  end;
  if Result then
    AValue := AField.GetValue
  else
    AValue := Null;
end;

function TdxSpreadSheetReportDataBinding.GetOwner: TPersistent;
begin
  Result := Designer;
end;

procedure TdxSpreadSheetReportDataBinding.RefreshFieldsList;
begin
  DataController.PopulateFields(Fields);
  Details.PopulateFields(Fields);
end;

{function TdxSpreadSheetReportDataBinding.IsActive: Boolean;
begin
  Result := DataController.Active;
end;}

{
procedure TdxSpreadSheetReportDataBinding.DataSetChange;
begin
  inherited DataSetChange;
  if FIsActive <> IsActive then
  begin
    FIsActive := IsActive;
    PopulateFields;
    Designer.DataSetChange;
  end;
end;



function TdxSpreadSheetReportDataBinding.GetField(const AFieldName: string): TField;
var
  I: Integer;
begin
  Result := nil;
  if not DataLink.Active or not FieldNames.TryGetValue(UpperCase(AFieldName), Result) then
    for I := 0 to FieldList.Count - 1 do
      if dxSpreadSheetTextIsEqual(AFieldName, TField(FieldList[I]).DisplayName) then
      begin
        Result := FieldList[I];
        Break;
      end;
end;


function TdxSpreadSheetReportDataBinding.GetFieldName(const AField: TField):  string;
begin
  Result := AField.DisplayName;
  if AField.DataSet.Name <> '' then
    Result := AField.DataSet.Name + '.' + Result;
end;

procedure TdxSpreadSheetReportDataBinding.PopulateFields;
var
  I: Integer;
  AField: TField;
begin
  if DataSet = nil then
    Exit;
  FieldList.Clear;
  FieldNames.Clear;
  if IsActive then
  begin
    for I := 0 to DataSet.FieldCount - 1 do
    begin
      AField := DataSet.Fields[I];
      if Designer.DoPopulateFields(AField) then
      begin
        FieldList.Add(AField);
        FieldNames.Add(UpperCase(GetFieldName(AField)), AField);
      end;
    end;
  end;
end;
}

function TdxSpreadSheetReportDataBinding.GetDataSource: TDataSource;
begin
  Result := DataController.DataSource;
end;

procedure TdxSpreadSheetReportDataBinding.SetDataGroups(AValue: TdxSpreadSheetReportDataGroups);
begin
  DataGroups.Assign(AValue);
end;

procedure TdxSpreadSheetReportDataBinding.SetDataSource(AValue: TDataSource);
begin
  if DataController.DataSource <> AValue then
  begin
    DataController.BeginUpdate;
    try
      DataController.DataSource := AValue;
    finally
      DataController.EndUpdate;
    end;
  end
end;

procedure TdxSpreadSheetReportDataBinding.SetDetails(AValue: TdxSpreadSheetReportDetails);
begin
  FDetails.Assign(AValue);
end;

procedure TdxSpreadSheetReportDataBinding.SetOptions(AValue: TdxSpreadSheetReportDataControllerOptions);
begin
  Options.Assign(AValue);
end;

procedure TdxSpreadSheetReportDataBinding.SetSortedFields(AValue: TdxSpreadSheetReportSortedFields);
begin
  SortedFields.Assign(AValue);
end;

{ TdxSpreadSheetReportFormulaController }

constructor TdxSpreadSheetReportFormulaController.Create(AOwner: TdxCustomSpreadSheet);
begin
  inherited Create(AOwner);
  FDataBinding := Designer.DataBinding;
end;

function TdxSpreadSheetReportFormulaController.IsActive: Boolean;
begin
  Result := DataBinding.DataController.IsActive;
end;

function TdxSpreadSheetReportFormulaController.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := TdxSpreadSheetReportDesigner(SpreadSheet);
end;

function TdxSpreadSheetReportFormulaController.GetFieldValue(const AFieldName: string): Variant;
begin
  DataBinding.GetFieldValue(AFieldName, Result);
end;

{ TdxSpreadSheetReportDesignerFieldChooserForm }

constructor TdxSpreadSheetReportDesignerFieldChooserForm.Create(AOwner: TdxSpreadSheetReportDesignerFieldChooser);
var
  R: TRect;
begin
  FOwner := AOwner;
  inherited CreateNew(Designer);
  Position := poDesigned;
  Fields := CreateFieldsList;
  if Owner.Site <> nil then
    Align := alClient
  else
  begin
    if FieldChooserBounds.TryGetValue(Designer, R) then
      BoundsRect := R
    else
    begin
      R := Designer.ClientRect;
      R.TopLeft := Designer.ClientToScreen(Point(R.Right, R.Top));
      R := cxRectSetSize(R, ScaleFactor.Apply(dxSpreadSheetReportDefaultFieldChooserWidth),
        Max(dxSpreadSheetReportDefaultFieldChooserWidth, Designer.Height));
      BoundsRect := R;
    end;
  end;
  InitializeFieldsTreeView;
end;

destructor TdxSpreadSheetReportDesignerFieldChooserForm.Destroy;
begin
  FieldChooserBounds.AddOrSetValue(Designer, BoundsRect);
  inherited Destroy;
end;

function TdxSpreadSheetReportDesignerFieldChooserForm.CreateFieldsList;
begin
  Result := TdxSpreadSheetReportFieldList.Create(Self);
end;

procedure TdxSpreadSheetReportDesignerFieldChooserForm.CreateParams(
  var Params: TCreateParams);
const
  AStyle: array[Boolean] of DWORD = (WS_POPUP, WS_CHILD);
begin
  inherited;
  with Params do
  begin
    Style := Style or AStyle[Parent <> nil];
    if Parent <> nil then
      WndParent := Parent.Handle
    else
      if (Designer <> nil) and not Designer.IsDestroying then
        WndParent := Designer.Handle
      else
        WndParent := 0;
  end;
end;

procedure TdxSpreadSheetReportDesignerFieldChooserForm.Init;
var
  I: Integer;
const
  UnusedItems: array[0..4, 0..1] of Integer =
    ((7, MF_BYPOSITION), (5, MF_BYPOSITION), (SC_MAXIMIZE, MF_BYCOMMAND),
    (SC_MINIMIZE, MF_BYCOMMAND), (SC_RESTORE, MF_BYCOMMAND));
  BorderStyleMap: array[Boolean] of TFormBorderStyle = (bsNone, bsSizeToolWin);
begin
  Caption := cxGetResourceString(@sdxReportFieldList);
  BorderStyle := BorderStyleMap[Parent = nil];
  BorderIcons := [biSystemMenu];
  Color := clBtnFace;
  for I := 0 to High(UnusedItems) do
    DeleteMenu(GetSystemMenu(Handle, False), UnusedItems[I, 0], UnusedItems[I, 1]);
  ScaleForPPI(Designer.ScaleFactor.TargetDPI);
  Font.Assign(Designer.Font);
  PopulateFields;
end;

procedure TdxSpreadSheetReportDesignerFieldChooserForm.InitializeFieldsTreeView;
begin
  Fields.BoundsRect := cxRectInflate(ClientRect, -ScaleFactor.Apply(dxSpreadSheetReportDefaultControlsIndent));
  Fields.Parent := Self;
  Fields.Anchors := [akLeft..akBottom];
  Fields.DragMode := dmAutomatic;
end;

procedure TdxSpreadSheetReportDesignerFieldChooserForm.PopulateFields;
begin
  if (DataBinding.Fields.Count = 0) and (Fields.Items.Count = 0) then
    Exit;
  Fields.Items.BeginUpdate;
  try
    Fields.Items.Clear;
    PopulateLevel(nil, DataBinding.DataController, DataBinding.Details);
    Fields.FullExpand;
  finally
    Fields.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetReportDesignerFieldChooserForm.PopulateLevel(
  AParent: TTreeNode; ADataController: TdxSpreadSheetReportDataController; ADetails: TdxSpreadSheetReportDetails);
begin
  AParent := Fields.Items.AddChildObject(AParent, ADataController.DisplayName, ADataController);
  PopulateTableItems(AParent, ADataController);
  ProcessDetails(AParent, ADetails);
end;

procedure TdxSpreadSheetReportDesignerFieldChooserForm.PopulateTableItems(
  AParent: TTreeNode; ADataController: TdxSpreadSheetReportDataController);
var
  I: Integer;
  AField: TdxSpreadSheetReportDesignerDataField;
begin
  if ADataController.FieldCount = 0 then
    Exit;
  for I := 0 to ADataController.FieldsList.Count - 1 do
  begin
    AField := ADataController.Fields[I];
    if AField.Visible then
      Fields.Items.AddChildObject(AParent, AField.DisplayName, AField);
  end;
end;

procedure TdxSpreadSheetReportDesignerFieldChooserForm.ProcessDetails(
  AParent: TTreeNode; ADetails: TdxSpreadSheetReportDetails);
var
  I: Integer;
begin
  for I := 0 to ADetails.Count - 1 do
    PopulateLevel(AParent, ADetails[I].DataController, ADetails[I].Details);
end;

function TdxSpreadSheetReportDesignerFieldChooserForm.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := Owner.Designer;
end;

function TdxSpreadSheetReportDesignerFieldChooserForm.GetDataBinding: TdxSpreadSheetReportDataBinding;
begin
  Result := Designer.DataBinding;
end;

{ TdxSpreadSheetReportDesignerFieldChooser }

procedure TdxSpreadSheetReportDesignerFieldChooser.Show;
begin
  FormNeeded;
  Form.Show;
end;

procedure TdxSpreadSheetReportDesignerFieldChooser.Hide;
begin
  if Form <> nil then
    Form.Hide;
end;

function TdxSpreadSheetReportDesignerFieldChooser.CreateFieldChooserForm: TdxSpreadSheetReportDesignerFieldChooserForm;
begin
  Result := TdxSpreadSheetReportDesignerFieldChooserForm.Create(Self);
end;

function TdxSpreadSheetReportDesignerFieldChooser.CanAssignedSite(ASite: TWinControl): Boolean;
begin
   Result := (ASite = nil) or (ASite <> Designer);
end;


procedure TdxSpreadSheetReportDesignerFieldChooser.DoHideForm(Sender: TObject);
begin
  FieldChooserBounds.AddOrSetValue(Designer, Form.BoundsRect);
  dxCallNotify(Designer.OnHideFieldChooser, Sender);
end;

procedure TdxSpreadSheetReportDesignerFieldChooser.DoShowForm(Sender: TObject);
var
  R: TRect;
begin
  if (FForm <> nil) and FieldChooserBounds.TryGetValue(Designer, R) then
    Form.BoundsRect := R;
  dxCallNotify(Designer.OnShowFieldChooser, Sender);
end;

procedure TdxSpreadSheetReportDesignerFieldChooser.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FForm then
      FForm := nil;
    if AComponent = FSite then
      FSite := nil;
  end;
end;

procedure TdxSpreadSheetReportDesignerFieldChooser.FormNeeded;
begin
  if Form <> nil then
    Exit;
  FForm := CreateFieldChooserForm;
  FForm.FreeNotification(Designer);
  FForm.Parent := Site;
  if Site <> nil then
    FForm.Align := alClient;
  FForm.OnShow := DoShowForm;
  FForm.OnHide := DoHideForm;
  Form.Init;
end;

procedure TdxSpreadSheetReportDesignerFieldChooser.UpdateFieldChooser;
begin
  if Form <> nil then
    Form.PopulateFields;
end;

function TdxSpreadSheetReportDesignerFieldChooser.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := TdxSpreadSheetReportDesigner(SpreadSheet);
end;

procedure TdxSpreadSheetReportDesignerFieldChooser.SetDragObject(AValue: TDragObject);
var
  P: TPoint;
begin
  FreeAndNil(FDragObject);
  FDragObject := AValue;
  P := cxPointScale(Designer.GetMouseCursorClientPos, 100, TdxSpreadSheetReportTableView(Designer.ActiveSheetAsTable).ZoomFactor);
  Designer.ActiveSheetAsTable.HitTest.Calculate(P);
  TdxSpreadSheetReportTableViewInfo(TdxSpreadSheetReportTableView(Designer.ActiveSheetAsTable).ViewInfo).UpdateDragDropAreaCell(
    P.X, P.Y, FDragObject, (FDragObject <> nil) and Designer.ActiveSheetAsTable.HitTest.HitAtCell);
  if AValue = nil then
    Designer.Invalidate;
end;

procedure TdxSpreadSheetReportDesignerFieldChooser.SetSite(AValue: TWinControl);
begin
  if (FSite = AValue) or not CanAssignedSite(AValue) then Exit;
  if FSite <> nil then
    FSite.RemoveFreeNotification(Designer);
  FSite := AValue;
  if FSite <> nil then
    FSite.FreeNotification(Designer);
  if Form <> nil then
  begin
    Form.Parent := Site;
    UpdateFieldChooser;
  end;
end;

{ TdxSpreadSheetReportLevelBuildInfo }

constructor TdxSpreadSheetReportLevelBuildInfo.Create(AParent: TdxSpreadSheetReportLevelBuildInfo);
begin
  Details := TObjectList<TdxSpreadSheetReportLevelBuildInfo>.Create;
  ExtractBounds := cxInvalidRect;
  Parent := AParent;
  if AParent <> nil then
  begin
    Bounds := Parent.DetailBounds;
    DataController := Parent.DataController;
  end;
end;

destructor TdxSpreadSheetReportLevelBuildInfo.Destroy;
begin
  Header.ClearCache;
  Footer.ClearCache;
  Detail.ClearCache;
  DataController.MasterDetailRelationsFinalize;
  DataGroupInfo.Free;
  Details.Free;
  inherited Destroy;
end;

procedure TdxSpreadSheetReportLevelBuildInfo.CheckSectionsIntersection(AIsVerticalLayout: Boolean);
begin
  if (Detail <> nil) and (Footer <> nil) then
    CheckIntersection(Detail.ActuallyBounds, Footer.ActuallyBounds, AIsVerticalLayout);
  if Header <> nil then
  begin
    if Detail <> nil then
      CheckIntersection(Header.ActuallyBounds, Detail.ActuallyBounds, AIsVerticalLayout)
    else
      if Footer <> nil then
        CheckIntersection(Header.ActuallyBounds, Footer.ActuallyBounds, AIsVerticalLayout);
  end;
  if Detail <> nil then
    DetailBounds := Detail.ActuallyBounds;
  //
  Bounds := DetailBounds;
  if Header <> nil then
    Bounds := dxSpreadSheetCellsUnion(Bounds, Header.ActuallyBounds);
  if Footer <> nil then
    Bounds := dxSpreadSheetCellsUnion(Bounds, Footer.ActuallyBounds);
end;

procedure TdxSpreadSheetReportLevelBuildInfo.ExpandDetailBounds(ASection: TdxSpreadSheetReportSection);
begin
  if dxSpreadSheetIntersects(Detail.Bounds, ASection.Bounds) then
  begin
    Detail.DestBounds := dxSpreadSheetCellsUnion(Detail.DestBounds, ASection.DestBounds);
    if Parent <> nil then
      Parent.ExpandDetailBounds(Detail);
  end;
end;

procedure TdxSpreadSheetReportLevelBuildInfo.CheckIntersection(var R1, R2: TRect; AIsVerticalLayout: Boolean);
begin
  if AIsVerticalLayout then
  begin
    if R1.Bottom >= R2.Top then
      R1.Bottom := R2.Top - 1;
  end
  else
    if R1.Right >= R2.Right then
      R1.Right := R2.Right - 1;
end;

{ TdxSpreadSheetReportBuilder }

constructor TdxSpreadSheetReportBuilder.Create(ADesigner: TdxSpreadSheetReportDesigner);
begin
  FDesigner := ADesigner;
  FStyles := TDictionary<TdxSpreadSheetCellStyleHandle, TdxSpreadSheetCellStyleHandle>.Create();
  FPostProcessFormulasList := TList<TdxSpreadSheetCustomFormula>.Create;
end;

destructor TdxSpreadSheetReportBuilder.Destroy;
begin
  FreeAndNil(FPostProcessFormulasList);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

procedure TdxSpreadSheetReportBuilder.BuildReport;
begin
  FreeAndNil(FRoot);
  FCurrentRowIndex := 0;
  FCurrentColumnIndex := 0;
  FOrigin := cxInvalidPoint;
  FOffset := cxNullSize;
  FProgressValue := -1;
  PostProcessFormulasList.Clear;
  Destination.BeginUpdate;
  ShowHourglassCursor;
  try
    FActive := True;
    PrepareBuildInfo;
    try
      if (Root <> nil) and (Root.Detail <> nil) then
      begin
        try
          DoBuildReport;
          PostProcessFormulas;
        finally
          FActive := False;
        end;
      end;
    finally
      FreeAndNil(FRoot);
    end;
    if Destination.SheetCount = 0 then
      ProcessEmptyReport;
  finally
    HideHourglassCursor;
    Destination.EndUpdate;
    ProgressValue := 100;
    Designer.ForceRefreshFormulas;
  end;
end;

function TdxSpreadSheetReportBuilder.CheckAndExpandRange(
  AInfo: TdxSpreadSheetReportLevelBuildInfo; var R: TRect): Boolean;

  function CheckSection(ASection: TdxSpreadSheetReportSection): Boolean;
  begin
    Result := (ASection <> nil) and dxSpreadSheetIntersects(ASection.Bounds, R);
    if Result then
      Result := ASection.ValidateRange(R);
  end;

var
  ADetail: TdxSpreadSheetReportLevelBuildInfo;
begin
  if AInfo = nil then
  begin
    Result := False;
    Exit;
  end;

  Result := CheckSection(AInfo.Header) or CheckSection(AInfo.Detail) or CheckSection(AInfo.Footer);
  if not Result then
    for ADetail in AInfo.Details do
    begin
      Result := CheckAndExpandRange(ADetail, R);
      if Result then
        Break;
    end;
end;

procedure TdxSpreadSheetReportBuilder.CopyCell(ASource: TdxSpreadSheetCell;
  ADestRow: TdxSpreadSheetTableItem; ADestIndex: Integer);
var
  APictureInfo: TObject;
  AIsRangeFormula: Boolean;
  ADest: TdxSpreadSheetCell;
begin
  if ASource = nil then
    Exit;
  ADest := ADestRow.CreateCell(ADestIndex);
  AIsRangeFormula := False;
  ADest.StyleHandle := ADest.SpreadSheet.CellStyles.AddClone(ASource.StyleHandle);
  case ASource.DataType of
    cdtBoolean:
      ADest.AsBoolean := ASource.AsBoolean;
    cdtError:
      ADest.AsError := ASource.AsError;
    cdtCurrency:
      ADest.AsCurrency := ASource.AsCurrency;
    cdtFloat:
      ADest.AsFloat := ASource.AsFloat;
    cdtDateTime:
      ADest.AsDateTime := ASource.AsDateTime;
    cdtInteger:
      ADest.AsInteger := ASource.AsInteger;
    cdtString:
      ADest.AsString := ASource.AsString;
    cdtFormula:
    begin
      if Designer.IsDBFormula(ASource.AsFormula) then
      begin
        if IsPictureFormula(ASource.AsFormula.Tokens) then
        begin
           if Designer.FieldPictures.TryGetValue(ASource.AsFormula, APictureInfo) then
             TdxSpreadSheetReportHelper.MakePicture(CurrentSheet, CurrentSection, TdxSpreadSheetReportPictureInfo(APictureInfo));
        end
        else
          if (ASource.AsFormula <> nil) and not VarIsNull(ASource.AsFormula.Value) then
            ADest.AsVariant := ASource.AsVariant
      end
      else
        TdxSpreadSheetReportHelper.CopyFormulas(ASource, ADest, AIsRangeFormula);
      if AIsRangeFormula then
        PostProcessFormulasList.Add(ADest.AsFormula);
    end;
  end;
end;

procedure TdxSpreadSheetReportBuilder.CopySettingsForTableItem(ASource: TdxSpreadSheetTableItem;
  ADestItems: TdxSpreadSheetTableItems; ADestIndex: Integer; ACheckItemExist: Boolean);
begin
  if ASource = nil then
    Exit;
  if not ACheckItemExist or (ADestItems[ADestIndex] = nil) then
    ADestItems.CreateItem(ADestIndex).Assign(ASource);
end;

function TdxSpreadSheetReportBuilder.CheckSection(AType: TdxSpreadSheetReportSectionType;
  var ASection: TdxSpreadSheetReportSection): Boolean;
begin
  ASection := Designer.GetSection(AType);
  Result := IsSectionValid(ASection);
end;

procedure TdxSpreadSheetReportBuilder.DoBuildReport;
var
  ARowIndex, AIndex: Integer;
  ANewSheetOnEachRecord: Boolean;
begin
  AIndex := 0;
  ANewSheetOnEachRecord := (Designer.Options.ReportMode <> rmSingleSheet);
  FActiveDataController := DataBinding.DataController;
  FCurrentLevel := Root;
  if ActiveDataController.IsActive and (Root.Detail <> nil) then
  begin
    ActiveDataController.MasterDetailRelationsInitialize;
    ARowIndex := 0;
    while ARowIndex < ActiveDataController.RowCount do
    try
      SetFocusedRowIndex(ARowIndex);
      if ANewSheetOnEachRecord or (ARowIndex = 0) then
      begin
        NewSheet;
        DoBuildSection(Root.Header);
        AIndex := IfThen(IsVerticalLayout, CurrentRowIndex, CurrentColumnIndex);
        RowsAutoHeight := True;
      end;
      DoBuildDetailSection(Root, ARowIndex);
    finally
      if ANewSheetOnEachRecord or (ARowIndex = DataBinding.DataController.RowCount) then
      try
        PostProcessSection(Root.Detail, AIndex, True);
        RowsAutoHeight := False;
      finally
        DoBuildSection(Root.Footer);
      end;
    end;
  end
  else
  begin
    DoBuildSection(Root.Header);
    DoBuildSection(Root.Detail);
    DoBuildSection(Root.Footer);
  end;
end;

procedure TdxSpreadSheetReportBuilder.DoBuildSectionFormulas(
  ASection: TdxSpreadSheetReportSection; AIsFirstStage: Boolean);
var
  I: Integer;
begin
  if ASection.Formulas[AIsFirstStage].Count = 0 then
    Exit;
  for I := 0 to ASection.Formulas[AIsFirstStage].Count - 1 do
    TdxSpreadSheetCustomFormulaAccess(ASection.Formulas[AIsFirstStage][I]).ForceRefresh;
end;

procedure TdxSpreadSheetReportBuilder.DoBuildSection(ASection: TdxSpreadSheetReportSection);
begin
  DoBuildSection(ASection, nil, True);
end;

procedure TdxSpreadSheetReportBuilder.DoBuildSection(ASection: TdxSpreadSheetReportSection;
  ADetails: TObjectList<TdxSpreadSheetReportLevelBuildInfo>; AIsFirstStage: Boolean);
var
  ASaveBounds, ASaveDestBounds, R: TRect;
  ASource: TdxSpreadSheetTableView;
  AStartRowIndex, ARowIndex, AMapRowIndex, I, AddIndex: Integer;
  AStartColumnIndex, AColumnIndex: Integer;
begin
  if ASection = nil then
    Exit;
  ASource := Designer.ActiveSheetAsTable;
  FCurrentSection := ASection;
  ASection.CalculateCache(ASource, ADetails, IsVerticalLayout);
  if cxRectIsInvalid(ASection.CachedBounds[AIsFirstStage])  then
    Exit;
  ASaveBounds := ASection.ActuallyBounds;
  ASaveDestBounds := ASection.DestBounds;
  ASection.ActuallyBounds := ASection.CachedBounds[AIsFirstStage];
  DoBuildSectionFormulas(ASection, AIsFirstStage);
// # need check
  for AColumnIndex := 0 to cxRectWidth(ASection.ActuallyBounds) do
    CopySettingsForTableItem(ASource.Columns[ASection.ActuallyBounds.Left + AColumnIndex],
      CurrentSheet.Columns, CurrentColumnIndex + AColumnIndex, IsVerticalLayout);
  for ARowIndex := 0 to cxRectHeight(ASection.ActuallyBounds) do
    CopySettingsForTableItem(ASource.Rows[ASection.ActuallyBounds.Top +  ARowIndex],
      CurrentSheet.Rows, CurrentRowIndex + ARowIndex, not IsVerticalLayout);
  //
  ARowIndex := ASection.ActuallyBounds.Top;
  AStartRowIndex := CurrentRowIndex;
  AStartColumnIndex := CurrentColumnIndex;
  if IsVerticalLayout then
    AStartColumnIndex := ASection.ActuallyBounds.Left - Origin.X;
  ASection.DestBounds := cxRectSetOrigin(cxRectSetLeft(ASection.ActuallyBounds, ASection.ActuallyBounds.Left - Origin.X),
    cxPoint(AStartColumnIndex, AStartRowIndex));
  try
    while ARowIndex <= ASection.ActuallyBounds.Bottom do
    begin
      AMapRowIndex := ARowIndex - ASection.Bounds.Top;
      if ASection.DataMap[AMapRowIndex] <> nil then
      begin
        FCurrentRow := CurrentSheet.Rows.CreateItem(CurrentRowIndex + ARowIndex - ASection.ActuallyBounds.Top);
        AddIndex := ASection.ActuallyBounds.Left - ASection.Bounds.Left + 1;
        for AColumnIndex := 0 to ASection.ActuallyBounds.Right - ASection.ActuallyBounds.Left do
        begin
          if ASection.DataMap[AMapRowIndex, AColumnIndex + AddIndex] = nil then
            Continue;
          if IsVerticalLayout then
            CopyCell(TdxSpreadSheetCell(ASection.DataMap[AMapRowIndex, AColumnIndex + AddIndex]), FCurrentRow,
              CurrentColumnIndex + AColumnIndex + (ASection.ActuallyBounds.Left - Origin.X))
          else
            CopyCell(TdxSpreadSheetCell(ASection.DataMap[AMapRowIndex, AColumnIndex + AddIndex]),
              FCurrentRow, CurrentColumnIndex + AColumnIndex);
        end;
{        if IsVerticalLayout and RowsAutoHeight then
          FCurrentRow.ApplyBestFit;}
      end;
      Inc(ARowIndex);
    end;
    //
    if IsVerticalLayout then
      Inc(FCurrentRowIndex, cxRectHeight(ASection.ActuallyBounds) + 1)
    else
      Inc(FCurrentColumnIndex, cxRectWidth(ASection.ActuallyBounds) + 1);
    //
    for I := 0 to ASection.MergedCells[AIsFirstStage].Count - 1 do
      if MakeLocalArea(ASection.MergedCells[AIsFirstStage][I].Area, ASection.ActuallyBounds, ASection.DestBounds, R) then
        CurrentSheet.MergedCells.Add(R);
    //
    TdxSpreadSheetReportHelper.CopyContainers(ASource, CurrentSheet, ASection.ActuallyBounds, ASection.DestBounds.TopLeft);
    //
  finally
    Designer.FieldPictures.Clear;
    Designer.FieldPictureInfos.Clear;
    ASection.ActuallyBounds := ASaveBounds;
    if not AIsFirstStage then
      ASection.DestBounds := dxSpreadSheetCellsUnion(ASection.DestBounds, ASaveDestBounds);
    if ASection.SectionType in [rstGroupHeader, rstGroupFooter] then
      CurrentLevel.ExpandDetailBounds(ASection);
    if CurrentLevel.Parent <> nil then
      CurrentLevel.Parent.ExpandDetailBounds(ASection);

    PostProcessFormulas;
  end;
end;

procedure TdxSpreadSheetReportBuilder.DoBuildDetailLevel(AInfo: TdxSpreadSheetReportLevelBuildInfo);
var
  AAutoHeight: Boolean;
  ARowIndex, AIndex: Integer;
  AParent: TdxSpreadSheetReportLevelBuildInfo;
  ADataController: TdxSpreadSheetReportDataController;
begin
  if AInfo.Detail = nil then
    Exit;
  AAutoHeight := RowsAutoHeight;
  ADataController := FActiveDataController;
  AParent := FCurrentLevel;
  try
    FCurrentLevel := AInfo;
    FActiveDataController := AInfo.DataController;
    ActiveDataController.MasterDetailRelationsInitialize;
    RowsAutoHeight := False;
    DoBuildSection(AInfo.Header);
    AIndex := IfThen(IsVerticalLayout, CurrentRowIndex, CurrentColumnIndex);
    try
      ARowIndex := 0;
      while ARowIndex < ActiveDataController.RowCount do
      begin
        SetFocusedRowIndex(ARowIndex);
        DoBuildDetailSection(AInfo, ARowIndex);
      end;
    finally
      PostProcessSection(AInfo.Detail, AIndex, False);
    end;
    RowsAutoHeight := False;
    DoBuildSection(AInfo.Footer);
  finally
    FCurrentLevel := AParent;
    FActiveDataController := ADataController;
    RowsAutoHeight := AAutoHeight;
  end;
end;

procedure TdxSpreadSheetReportBuilder.DoBuildDetailSection(
  AInfo: TdxSpreadSheetReportLevelBuildInfo; var ARowIndex: Integer);
var
  AIndex: Integer;
begin
  if AInfo.Detail = nil then
    Exit;
  AIndex := IfThen(IsVerticalLayout, CurrentRowIndex, CurrentColumnIndex);
  try
    if AInfo.DataGroupInfo <> nil then
    begin
      AInfo.Detail.DestBounds := cxRectSetOrigin(cxRectSetLeft(AInfo.Detail.ActuallyBounds,
        AInfo.Detail.ActuallyBounds.Left - Origin.X), cxPoint(CurrentColumnIndex, CurrentRowIndex));
      DoBuildDetailSectionDataGroups(AInfo, ARowIndex)
    end
    else
      DoBuildDetailSectionDetails(AInfo, ARowIndex);
  finally
    PostProcessSection(AInfo.Detail, AIndex, True);
  end
end;

procedure TdxSpreadSheetReportBuilder.DoBuildDetailSectionDataGroups(
  AInfo: TdxSpreadSheetReportLevelBuildInfo; var ARowIndex: Integer);
var
  ARowInfo: TcxRowInfo;
  ADataGroupInfo: TdxSpreadSheetReportLevelBuildInfo;
begin
  SetFocusedRowIndex(ARowIndex);
  ARowInfo := ActiveDataController.GetRowInfo(ARowIndex);
  Inc(ARowIndex);
  ADataGroupInfo := AInfo.DataGroupInfo;
  while (AInfo.Parent <> nil) and (AInfo.Parent.DataController = ADataGroupInfo.DataController) do
    AInfo.Parent := AInfo.Parent;
  DoBuildSection(ADataGroupInfo.Header);
  if ADataGroupInfo.DataGroupInfo <> nil then
    DoBuildDetailSectionDataGroups(ADataGroupInfo, ARowIndex)
  else
  begin
    AInfo.DataGroupBounds := ADataGroupInfo.DataGroupBounds;
    while (ARowIndex < ActiveDataController.RowCount) and
      (ActiveDataController.GetRowInfo(ARowIndex).Level > ARowInfo.Level) do
      DoBuildDetailSectionDataRow(AInfo, ARowIndex);
  end;
  DoBuildSection(ADataGroupInfo.Footer);
end;

procedure TdxSpreadSheetReportBuilder.DoBuildDetailSectionDataRow(
  AInfo: TdxSpreadSheetReportLevelBuildInfo; var ARowIndex: Integer);
var
  ADestBounds: TRect;
begin
  SetFocusedRowIndex(ARowIndex);
  if AInfo.Detail <> nil then
  begin
    ADestBounds := AInfo.Detail.DestBounds;
    AInfo.Detail.CalculateCache(Designer.ActiveSheetAsTable, AInfo.Details, IsVerticalLayout);
    AInfo.Detail.CachedBounds[True] := AInfo.DataGroupBounds;
    try
      DoBuildSection(AInfo.Detail);
    finally
      AInfo.Detail.DestBounds := dxSpreadSheetCellsUnion(ADestBounds, AInfo.Detail.DestBounds);
    end;
  end;
  DoBuildDetailSectionDetails(AInfo, ARowIndex);
end;

procedure TdxSpreadSheetReportBuilder.DoBuildDetailSectionDetails(
  AInfo: TdxSpreadSheetReportLevelBuildInfo; var ARowIndex: Integer);
var
  ADetail: TdxSpreadSheetReportLevelBuildInfo;
begin
  RowsAutoHeight := True;
  if AInfo.DataGroupInfo = nil then
  DoBuildSection(AInfo.Detail, AInfo.Details, True);
  if (AInfo.Details <> nil) and (AInfo.Details.Count > 0) then
  begin
    for ADetail in AInfo.Details do
      DoBuildDetailLevel(ADetail);
  end;
  if AInfo.DataGroupInfo = nil then
    DoBuildSection(AInfo.Detail, AInfo.Details, False);
  Inc(ARowIndex);
end;

function TdxSpreadSheetReportBuilder.GetDesignerSection(const ARootBounds: TRect;
  AType: TdxSpreadSheetReportSectionType; AIndex: Integer; var ASectionBounds: TRect): TdxSpreadSheetReportSection;
begin
  ASectionBounds := cxInvalidRect;
  Result := Designer.GetSection(AType, AIndex);
  if not IsSectionValid(Result) then
    Result := nil
  else
  begin
    ASectionBounds := Result.Bounds;
    if not cxRectIsInvalid(ARootBounds) and not dxSpreadSheetIntersects(ASectionBounds, ARootBounds, ASectionBounds) then
      Result := nil;
  end;
  if (Result = nil) or not IsBoundsValid(ASectionBounds) then
  begin
    Result := nil;
    ASectionBounds := cxInvalidRect;
  end;
  if Result <> nil then
    Result.ActuallyBounds := ASectionBounds;
end;

function TdxSpreadSheetReportBuilder.IsBoundsValid(const ABounds: TRect): Boolean;
begin
  Result := (cxRectWidth(ABounds) >= 0) and (cxRectHeight(ABounds) >= 0) and
    not cxRectIsInvalid(ABounds);
end;

function TdxSpreadSheetReportBuilder.IsSectionValid(ASection: TdxSpreadSheetReportSection): Boolean;
begin
  Result := (ASection <> nil) and IsBoundsValid(ASection.ActuallyBounds)
end;

function TdxSpreadSheetReportBuilder.IsVerticalLayout: Boolean;
begin
  Result := Designer.Options.Orientation = roVertical;
end;

function TdxSpreadSheetReportBuilder.MakeGroupLevelInfo(AParent: TdxSpreadSheetReportLevelBuildInfo;
  AIndex: Integer): TdxSpreadSheetReportLevelBuildInfo;
var
  R: TRect;
begin
  Result := nil;
  if AIndex >= AParent.DataController.DataGroups.Count then
    Exit;
  Result := TdxSpreadSheetReportLevelBuildInfo.Create(AParent);
  Result.DataGroup := AParent.DataController.DataGroups[AIndex];
  R := AParent.DetailBounds;
  Result.DataGroupBounds := R;
  Result.Header := GetDesignerSection(AParent.DetailBounds, rstGroupHeader, Result.DataGroup.SectionID, R);
  if Result.Header <> nil then
    Result.DataGroupBounds.Top := Result.Header.Bounds.Bottom + 1;
  Result.Footer := GetDesignerSection(AParent.DetailBounds, rstGroupFooter, Result.DataGroup.SectionID, R);
  if Result.Footer <> nil then
    Result.DataGroupBounds.Bottom := Result.Footer.Bounds.Top - 1;
  if (Result.Header = nil) and (Result.Footer = nil) then
    FreeAndNil(Result)
  else
    if IsBoundsValid(Result.DataGroupBounds) then
      Result.DataGroupInfo := MakeGroupLevelInfo(Result, AIndex + 1);
end;

function TdxSpreadSheetReportBuilder.MakeLevelInfo(AParent: TdxSpreadSheetReportLevelBuildInfo;
  ADetail: TdxSpreadSheetReportDetail): TdxSpreadSheetReportLevelBuildInfo;
begin
  Result := MakeLevelInfo(AParent, ADetail.DataController, ADetail.Details, ADetail.SectionID);
end;

function TdxSpreadSheetReportBuilder.MakeLevelInfo(AParent: TdxSpreadSheetReportLevelBuildInfo;
  ADataController: TdxSpreadSheetReportDataController;
  ADetails: TdxSpreadSheetReportDetails; AIndex: Integer): TdxSpreadSheetReportLevelBuildInfo;
var
  I: Integer;
  ABounds, R: TRect;
  AInfo: TdxSpreadSheetReportLevelBuildInfo;
begin
  Result := TdxSpreadSheetReportLevelBuildInfo.Create(AParent);
  ABounds := cxInvalidRect;
  if AParent <> nil then
    ABounds := AParent.DetailBounds;
  Result.DataController := ADataController;
  Result.Header := GetDesignerSection(ABounds, rstHeader, AIndex, R);
  Result.Footer := GetDesignerSection(ABounds, rstFooter, AIndex, R);
  if AParent = nil then
    Result.Detail := GetDesignerSection(ABounds, rstDetail, AIndex, Result.DetailBounds)
  else
    Result.Detail := GetDesignerSection(ABounds, rstDetailLevel, AIndex, Result.DetailBounds);
  if Result.Detail = nil then
    FreeAndNil(Result)
  else
  begin
    for I := 0 to ADetails.Count - 1 do
    begin
      AInfo := MakeLevelInfo(Result, ADetails[I]);
      if AInfo <> nil then
      begin
        Result.Details.Add(AInfo);
        if I = 0 then
          Result.ExtractBounds := AInfo.Bounds
        else
          Result.ExtractBounds := dxSpreadSheetCellsUnion(Result.ExtractBounds, AInfo.Bounds);
      end
    end;
    Result.CheckSectionsIntersection(IsVerticalLayout);
    Result.Detail.ExtractBounds := Result.ExtractBounds;
    Result.DataController.BeforeBuildReport;
  end;
  if ADataController.DataGroups.Count > 0 then
    Result.DataGroupInfo := MakeGroupLevelInfo(Result, 0);
end;

function TdxSpreadSheetReportBuilder.MakeLocalArea(const AArea, ASourceArea, ADestArea: TRect;
  var ALocalArea: TRect): Boolean;
begin
  Result := dxSpreadSheetIntersects(AArea, ASourceArea, ALocalArea);
  if not Result then
    Exit;
  ALocalArea := cxRectOffset(cxRectOffset(ALocalArea, ASourceArea.TopLeft, False), ADestArea.TopLeft);
  ALocalArea.Top := Max(0, ALocalArea.Top);
  ALocalArea.Left := Max(0, ALocalArea.Left);
  Result := (ALocalArea.Left <= ALocalArea.Right) and (ALocalArea.Top <= ALocalArea.Bottom);
end;

procedure TdxSpreadSheetReportBuilder.NewSheet;
var
  ADoc: TdxCustomSpreadSheet;
begin
  if (Designer.Options.ReportMode = rmMultipleDocuments) and (DataBinding.DataController.FocusedRowIndex > 0) then
  begin
    ADoc := FDestination;
    Designer.DoNewDocument(FDestination);
    if ADoc <> FDestination then
      ADoc.EndUpdate;
    FDestination.BeginUpdate;
  end;
  FCurrentSheet := Destination.AddSheet() as TdxSpreadSheetTableView;
  Designer.DoNewSheet(FCurrentSheet);
  FCurrentSheet.Options.GridLines := bFalse;
  FCurrentRowIndex := 0;
  FCurrentColumnIndex := 0;
end;

procedure TdxSpreadSheetReportBuilder.PostProcessFormulas;

  function AreaToReference(const Area: TRect): TdxSpreadSheetFormulaToken;
  begin
    if (Area.Left = Area.Right) and (Area.Top = Area.Bottom) then
      Result := TdxSpreadSheetFormulaReference.Create(Area.Top, Area.Left, False, False)
    else
      Result := TdxSpreadSheetFormulaAreaReference.Create(Area.Top, Area.Left, Area.Bottom, Area.Right,
        False, False, False, False);
  end;

  procedure ModifyRangeToken(var AToken: TdxSpreadSheetFormulaToken);
  var
    R: TRect;
  begin
    R := ExtractArea(AToken);
    if CheckAndExpandRange(CurrentLevel, R) or CheckAndExpandRange(CurrentLevel.Parent, R) or CheckAndExpandRange(Root, R)  then
    begin
      OffsetRect(R, -(AToken.Owner.AnchorColumn + Origin.X), -AToken.Owner.AnchorRow);
      R.Top := Min(R.Top, R.Bottom);
      R.Left := Min(R.Left, R.Right);
      TdxSpreadSheetFormulaTokenAccess(AToken).Replace(AToken, AreaToReference(R));
    end
  end;

  procedure ProcessTokens(AToken: TdxSpreadSheetFormulaToken);
  begin
    if AToken = nil then
      Exit;
    if IsRangeToken(AToken) then
      ModifyRangeToken(AToken);
    ProcessTokens(AToken.FirstChild);
    ProcessTokens(AToken.Next);
  end;

var
  AFormula: TdxSpreadSheetCustomFormula;
begin
  for AFormula in PostProcessFormulasList do
    ProcessTokens(AFormula.Tokens);
  PostProcessFormulasList.Clear;
end;

procedure TdxSpreadSheetReportBuilder.PostProcessSection(
  var ASection: TdxSpreadSheetReportSection; AIndex: Integer; ANeedAutoHeight: Boolean);
begin
  if IsVerticalLayout then
    ASection.DestBounds.Top := AIndex
  else
  begin
    ASection.DestBounds.Right := AIndex;
    if ANeedAutoHeight then
      PostRowsAutoHeight(ASection);
  end;
end;

procedure TdxSpreadSheetReportBuilder.PostRowsAutoHeight(ASection: TdxSpreadSheetReportSection);
var
  AIsPictureRow: Boolean;
  ACell: TdxSpreadSheetCell;
  ARow: TdxSpreadSheetTableItem;
  ARowIndex, AIndex: Integer;
begin
  for ARowIndex := ASection.ActuallyBounds.Top to ASection.ActuallyBounds.Bottom do
  begin
    ARow := Designer.ActiveSheetAsTable.Rows[ARowIndex];
    if ARow <> nil then
    begin
      AIsPictureRow := False;
      for AIndex := ASection.ActuallyBounds.Left to ASection.ActuallyBounds.Right do
      begin
        ACell := ARow.Cells[AIndex];
        AIsPictureRow := AIsPictureRow or (ACell <> nil) and ACell.IsFormula and
          IsPictureFormula(ACell.AsFormula.Tokens);
      end;
      if not AIsPictureRow and RowsAutoHeight then
        if CurrentSheet.Rows[ARowIndex - ASection.ActuallyBounds.Top + ASection.DestBounds.Top] <> nil then
          CurrentSheet.Rows[ARowIndex - ASection.ActuallyBounds.Top + ASection.DestBounds.Top].ApplyBestFit;
    end;
  end;
end;

procedure TdxSpreadSheetReportBuilder.PrepareBuildInfo;

  procedure CheckOrigin(ASection: TdxSpreadSheetReportSection);
  begin
    if ASection <> nil then
    begin
      if cxPointIsInvalid(FOrigin) then
        FOrigin := ASection.Bounds.TopLeft
      else
      begin
        FOrigin.X := Min(FOrigin.X, ASection.Bounds.Left);
        FOrigin.Y := Min(FOrigin.Y, ASection.Bounds.Top);
      end;
    end;
  end;

begin
  FRoot := MakeLevelInfo(nil, DataBinding.DataController, DataBinding.Details, -1);
  if Root <> nil then
  begin
    CheckOrigin(Root.Header);
    CheckOrigin(Root.Detail);
    CheckOrigin(Root.Footer);
    //
    FOffset.cy := Designer.ActiveSheetAsTable.Rows.GetPosition(FOrigin.Y);
    FOffset.cx := Designer.ActiveSheetAsTable.Columns.GetPosition(FOrigin.X);
  end;
end;

procedure TdxSpreadSheetReportBuilder.ProcessEmptyReport;
begin
  Destination.AddSheet('Empty Sheet');
  Destination.ActiveSheetAsTable.CreateCell(0, 0).SetText('< NO DATA TO DISPLAY >');
  Destination.ActiveSheetAsTable.Cells[0, 0].Style.Font.Size := 20;
end;

procedure TdxSpreadSheetReportBuilder.SetFocusedRowIndex(ARowIndex: Integer);
begin
  ActiveDataController.FocusedRowIndex := ARowIndex;
  if ActiveDataController = DataBinding.DataController then
    ProgressValue := MulDiv(Max(0, ActiveDataController.FocusedRowIndex), 100, ActiveDataController.RowCount);
end;

function TdxSpreadSheetReportBuilder.GetDataBinding: TdxSpreadSheetReportDataBinding;
begin
  Result := Designer.DataBinding;
end;

procedure TdxSpreadSheetReportBuilder.SetProgressValue(AValue: Integer);
begin
  if FProgressValue <> AValue then
  begin
    FProgressValue := AValue;
    Designer.DoProgress(ProgressValue);
  end;
end;

{ TdxSpreadSheetReportDesignerOptions }

constructor TdxSpreadSheetReportDesignerOptions.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(ASpreadSheet);
  FOrientation := roVertical;
  FDesignView := True;
end;

procedure TdxSpreadSheetReportDesignerOptions.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetReportDesignerOptions then
  begin
    DesignView := TdxSpreadSheetReportDesignerOptions(Source).DesignView;
    Orientation := TdxSpreadSheetReportDesignerOptions(Source).Orientation;
    ReportMode := TdxSpreadSheetReportDesignerOptions(Source).ReportMode;
  end;
end;

procedure TdxSpreadSheetReportDesignerOptions.Changed;
begin
  Designer.AddChanges(sscLayoutAndData);
end;

function TdxSpreadSheetReportDesignerOptions.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := TdxSpreadSheetReportDesigner(SpreadSheet);
end;

procedure TdxSpreadSheetReportDesignerOptions.SetDesignView(AValue: Boolean);
begin
  if AValue <> DesignView then
  begin
    FDesignView :=  AValue;
    Designer.CheckFormulas;
    Designer.AddChanges([sscLayout]);
  end;
end;

procedure TdxSpreadSheetReportDesignerOptions.SetOrientation(AValue: TdxSpreadSheetReportOrientation);
begin
  if Orientation <> AValue then
  begin
    FOrientation := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetReportDesignerOptions.SetReportMode(AValue: TdxSpreadSheetReportMode);
begin
  if ReportMode <> AValue then
  begin
    FReportMode := AValue;
    Changed;
  end;
end;

{ TdxSpreadSheetReport }

procedure TdxSpreadSheetReportDesigner.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetReportDesigner then
  begin
    DataBinding := TdxSpreadSheetReportDesigner(Source).DataBinding;
    FieldChooser := TdxSpreadSheetReportDesigner(Source).FieldChooser;
    Options := TdxSpreadSheetReportDesigner(Source).Options;
  end;
  inherited;
end;

procedure TdxSpreadSheetReportDesigner.Build(ADestination: TdxCustomSpreadSheet);
begin
  if ADestination = nil then
    Exit;
  FBuilder := CreateReportBuilder;
  try
    FBuilder.Destination := ADestination;
    DoBeforeBuild;
    try
      FBuilder.BuildReport;
    finally
      DoAfterBuild;
    end;
  finally
    FreeAndNil(FBuilder);
    AddChanges(sscLayoutAndData);
    FRebuildNeeded := False;
  end;
end;

procedure TdxSpreadSheetReportDesigner.DragDrop(Source: TObject; X, Y: Integer);
var
  P: TPoint;
  AValue: string;
  ARow, AColumn: Integer;
begin
  if Source is TdxSpreadSheetReportFieldDragObject then
  begin
    P := cxPointScale(GetMouseCursorClientPos, 100, TdxSpreadSheetReportTableView(ActiveSheetAsTable).ZoomFactor);
    TdxSpreadSheetReportTableView(ActiveSheetAsTable).CellAtPoint(P, ARow, AColumn);
    if (ARow >= 0) and (AColumn >= 0) then
    begin
      P := TdxSpreadSheetReportTableView(ActiveSheetAsTable).MergedCells.CheckCell(ARow, AColumn).TopLeft;
      AValue := Format('=' + sfnField + '("%s")',
        [TdxSpreadSheetReportFieldDragObject(Source).Field.GetDisplayName]);
      ActiveSheetAsTable.CreateCell(P.Y, P.X).SetText(AValue, True);
      SetFocus;
    end;
  end;
end;

function TdxSpreadSheetReportDesigner.FindSection(const AType: TdxSpreadSheetReportSectionType;
  AIndex: Integer; var ASection: TdxSpreadSheetReportSection): Boolean;
var
  I: Integer;
  AName: string;
begin
  Result := False;
  ASection := nil;
  AName := ActuallySectionName(AType, AIndex);
  for I := 0 to SectionList.Count - 1 do
  begin
    Result := SameText(SectionList[I].Name.Caption, AName);
    if Result then
    begin
      ASection := SectionList[I];
      Break;
    end;
  end;
end;

function TdxSpreadSheetReportDesigner.FindSectionByCell(const ARow, AColumn: Integer;
  var ASection: TdxSpreadSheetReportSection): Boolean;
var
  I: Integer;
begin
  Result := False;
  ASection := nil;
  for I := 0 to SectionList.Count - 1 do
  begin
    Result := dxSpreadSheetContains(SectionList[I].Bounds, ARow, AColumn);
    if Result and ((ASection = nil) or (SectionList[I].Index > ASection.Index)) then
      ASection := SectionList[I];
  end;
end;

procedure TdxSpreadSheetReportDesigner.GetChildren(Proc: TGetChildProc; Root: TComponent);

  procedure DoStore(ADetail: TdxSpreadSheetReportDetail);
  begin
    Proc(ADetail);
  end;

var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to DataBinding.Details.Count - 1 do
    DoStore(DataBinding.Details[I]);
end;

function TdxSpreadSheetReportDesigner.GetSection(const AName: string; AIndex: Integer = -1): TdxSpreadSheetReportSection;
var
  ID: Integer;
  AType: TdxSpreadSheetReportSectionType;
begin
  ID := AIndex;
  if SectionNameController.TryGetType(AName, AType, AIndex) then
  begin
    if (AIndex = -1) and (ID <> -1) then
      AIndex := ID;
    Result := GetSection(AType, AIndex)
  end
  else
    Result := nil;
end;

function TdxSpreadSheetReportDesigner.GetSection(const AType: TdxSpreadSheetReportSectionType; AIndex: Integer = -1): TdxSpreadSheetReportSection;
begin
  FindSection(AType, AIndex, Result);
end;

function TdxSpreadSheetReportDesigner.GetSectionByCell(const ARow, AColumn: Integer): TdxSpreadSheetReportSection;
begin
  FindSectionByCell(ARow, AColumn, Result);
end;

procedure TdxSpreadSheetReportDesigner.RemoveSection(ASection: TdxSpreadSheetReportSection);
begin
  if ASection = nil then
    Exit;
  History.BeginAction(TdxSpreadSheetReportDesignerHistoryChangeSectionAction);
  try
    if History.CanAddCommand then
      History.AddCommand(TdxSpreadSheetDesignerChangeSectionCommand.Create(ASection, '', cxInvalidRect, ASection.Index));
    ASection.Name.Free;
    PopulateReportSections;
  finally
    History.EndAction;
  end;
  LayoutChanged;
end;

procedure TdxSpreadSheetReportDesigner.RemoveSection(AType: TdxSpreadSheetReportSectionType; AIndex: Integer = -1);
var
  ASection: TdxSpreadSheetReportSection;
begin
  if FindSection(AType, AIndex, ASection) then
    RemoveSection(ASection);
end;

procedure TdxSpreadSheetReportDesigner.SetDetailSection(const ABounds: TRect; ALevel: Integer);
begin
  if ALevel < 0 then
    SetSectionInfo(sanDetailRange, ABounds, ALevel)
  else
    SetSectionInfo(sanDetailLevelRange, ABounds, ALevel)
end;

procedure TdxSpreadSheetReportDesigner.SetFooterSection(const ABounds: TRect);
begin
  SetSectionInfo(sanFooterRange, ABounds);
end;

procedure TdxSpreadSheetReportDesigner.SetGroupHeaderSection(const ABounds: TRect; ALevel: Integer);
begin
  SetSectionInfo(sanGroupHeaderRange, ABounds, ALevel);
end;

procedure TdxSpreadSheetReportDesigner.SetGroupFooterSection(const ABounds: TRect; ALevel: Integer);
begin
  SetSectionInfo(sanGroupFooterRange, ABounds, ALevel);
end;

procedure TdxSpreadSheetReportDesigner.SetHeaderSection(const ABounds: TRect);
begin
  SetSectionInfo(sanHeaderRange, ABounds);
end;

procedure TdxSpreadSheetReportDesigner.AddFormulaInfo(AFormula: TdxSpreadSheetCustomFormula; AIsDBFormula: Boolean);
begin
  DBCells.Add(AFormula, AIsDBFormula);
  if AIsDBFormula then
    DBFormulas.Add(AFormula);
end;

function TdxSpreadSheetReportDesigner.AddPicture(AFormula: TdxSpreadSheetCustomFormula; APictureInfo: TObject): Integer;
begin
  FieldPictures.AddOrSetValue(AFormula, APictureInfo);
  Result := FFieldPictureInfos.Add(APictureInfo);
end;

procedure TdxSpreadSheetReportDesigner.AfterLoad;
begin
  inherited AfterLoad;
  PopulateReportSections;
end;

procedure TdxSpreadSheetReportDesigner.CheckChanges;
begin
  FRebuildNeeded := FRebuildNeeded and (Changes <> []) and not IsLoading;
  if not (IsLocked or ProcessingChanges) and (sscLayoutAndData * Changes = sscLayoutAndData) then
  begin
    DataBinding.RefreshFieldsList;
    if FieldChooser <> nil then
      FieldChooser.UpdateFieldChooser;
  end;
  inherited CheckChanges;
end;

procedure TdxSpreadSheetReportDesigner.CheckFormula(AFormula: TdxSpreadSheetCustomFormula);
var
  ACell: TdxSpreadSheetCell;
  AValue: TdxDefaultBoolean;
begin
  if AFormula is TdxSpreadSheetFormula then
  begin
    ACell := TdxSpreadSheetFormula(AFormula).Cell;
    if ACell <> nil then
    begin
      if (AFormula.ResultValue.ErrorCode <> ecNone) and (ACell.ShowFormula <> bTrue) then
        TextCells.AddOrSetValue(ACell, ACell.ShowFormula);
      AddFormulaInfo(AFormula, HasDBToken(AFormula.Tokens));
      if not Options.DesignView and TextCells.TryGetValue(ACell, AValue) then
        ACell.ShowFormula := AValue;
    end;
  end;
end;

procedure TdxSpreadSheetReportDesigner.CheckFormulas;
var
  I: Integer;
  AChanges: TdxSpreadSheetChanges;
begin
  ResetCacheInformation;
  AChanges := Changes;
  BeginUpdate;
  try
    for I := 0 to FormulaController.Count - 1 do
      CheckFormula(FormulaController.Items[I]);
  finally
    Changes := AChanges;
    EndUpdate;
  end;
end;

function TdxSpreadSheetReportDesigner.CreateDataBinding: TdxSpreadSheetReportDataBinding;
begin
  Result := TdxSpreadSheetReportDataBinding.Create(Self);
end;

function TdxSpreadSheetReportDesigner.CreateFieldChooser: TdxSpreadSheetReportDesignerFieldChooser;
begin
  Result := TdxSpreadSheetReportDesignerFieldChooser.Create(Self);
end;

function TdxSpreadSheetReportDesigner.CreateFormulaController: TdxSpreadSheetFormulaController;
begin
  Result := TdxSpreadSheetReportFormulaController.Create(Self);
end;

function TdxSpreadSheetReportDesigner.CreateReportBuilder: TdxSpreadSheetReportBuilder;
begin
  Result := TdxSpreadSheetReportBuilder.Create(Self);
end;

function TdxSpreadSheetReportDesigner.CreateOptions: TdxSpreadSheetReportDesignerOptions;
begin
  Result := TdxSpreadSheetReportDesignerOptions.Create(Self);
end;

procedure TdxSpreadSheetReportDesigner.CreateSubClasses;
begin
  FDataBinding := CreateDataBinding;
  FDBCells := TDictionary<TdxSpreadSheetCustomFormula, Boolean>.Create;
  FDBFormulas := TList<TdxSpreadSheetCustomFormula>.Create;
  FSectionList := TObjectList<TdxSpreadSheetReportSection>.Create;
  FFieldChooser := CreateFieldChooser;
  FTextCells := TDictionary<TdxSpreadSheetCell, TdxDefaultBoolean>.Create;
  FOptions := CreateOptions;
  FFieldPictures := TDictionary<TdxSpreadSheetCustomFormula, TObject>.Create;
  FFieldPictureInfos := TcxObjectList.Create;
  inherited CreateSubClasses;
end;

procedure TdxSpreadSheetReportDesigner.DataSetChange;
begin
  if Builder = nil then
    AddChanges(sscLayoutAndData);
end;

procedure TdxSpreadSheetReportDesigner.DefinedNamesChanged;
begin
  if IsDestroying then
    Exit;
  inherited DefinedNamesChanged;
  if State <> [sssReading] then
    PopulateReportSections;
end;

procedure TdxSpreadSheetReportDesigner.DestroySubClasses;
begin
  inherited DestroySubClasses;
  FreeAndNil(FSectionList);
  FreeAndNil(FDataBinding);
  FreeAndNil(FFieldChooser);
  FreeAndNil(FTextCells);
  FreeAndNil(FDBCells);
  FreeAndNil(FDBFormulas);
  FreeAndNil(FOptions);
  FreeAndNil(FFieldPictures);
  FreeAndNil(FFieldPictureInfos);
end;

procedure TdxSpreadSheetReportDesigner.DoAfterBuild;
begin
  if Assigned(FOnAfterBuild) then
    FOnAfterBuild(Self);
end;

procedure TdxSpreadSheetReportDesigner.DoBeforeBuild;
begin
  if Assigned(FOnBeforeBuild) then
    FOnBeforeBuild(Self);
end;

function TdxSpreadSheetReportDesigner.DoCreateSheet(var ASheet: TdxSpreadSheetCustomView;
  const ACaption: string = ''; AViewClass: TdxSpreadSheetCustomViewClass = nil): Boolean;
begin
  if (AViewClass = nil) or (AViewClass = TdxSpreadSheetTableView) then
    AViewClass := TdxSpreadSheetReportTableView;
  Result := inherited DoCreateSheet(ASheet, ACaption, AViewClass);
end;

procedure TdxSpreadSheetReportDesigner.DoDataChanged;
begin
  CheckFormulas;
  inherited DoDataChanged;
end;

procedure TdxSpreadSheetReportDesigner.DoNewDocument(var ADocument: TdxCustomSpreadSheet);
begin
  if Assigned(FOnNewDocument) then
    FOnNewDocument(Self, ADocument);
end;

procedure TdxSpreadSheetReportDesigner.DoNewSheet(ASheet: TdxSpreadSheetTableView);
begin
  if Assigned(FOnNewReportSheet) then
    FOnNewReportSheet(Self, ASheet);
end;

function TdxSpreadSheetReportDesigner.DoPopulateFields(AField: TField): Boolean;
begin
  Result := AField.Visible;
  if Assigned(FOnPopulateField) then
    OnPopulateField(Self, AField, Result);
end;

procedure TdxSpreadSheetReportDesigner.DoProgress(APercent: Integer);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, APercent);
end;

procedure TdxSpreadSheetReportDesigner.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  AController: TdxSpreadSheetCustomController;
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if Source is TdxSpreadSheetReportFieldDragObject then
  begin
    Accept := ControllerFromPoint(X, Y, AController) and (AController is TdxSpreadSheetTableViewController);
    if Accept then
    begin
      AController.HitTest.Calculate(Point(X, Y));
      Accept := TdxSpreadSheetTableViewController(AController).HitTest.HitAtCell;
      TdxSpreadSheetReportTableViewInfo(TdxSpreadSheetReportTableView(ActiveSheetAsTable).ViewInfo).UpdateDragDropAreaCell(X, Y, TdxSpreadSheetReportFieldDragObject(Source), Accept);
    end;
  end;
end;

function TdxSpreadSheetReportDesigner.GetDataControllerForSection(
  ASection: TdxSpreadSheetReportSection): TdxSpreadSheetReportDataController;
begin
  if ASection <> nil then
    Result := ASection.DataController
  else
    Result := nil;
end;

function TdxSpreadSheetReportDesigner.IsDBFormula(AFormula: TdxSpreadSheetCustomFormula): Boolean;
begin
  if not DBCells.TryGetValue(AFormula, Result) then
    Result := False;
end;

procedure TdxSpreadSheetReportDesigner.ForceRefreshFormulas;
var
  I: Integer;
begin
  for I := 0 to DBFormulas.Count - 1 do
    TdxSpreadSheetCustomFormulaAccess(DBFormulas[I]).ForceRefresh;
end;

procedure TdxSpreadSheetReportDesigner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if FieldChooser <> nil then
    FieldChooser.Notification(AComponent, Operation);
end;

procedure TdxSpreadSheetReportDesigner.PopulateReportSections;
var
  I, AIndex: Integer;
  ASection: TdxSpreadSheetReportSection;
  AType: TdxSpreadSheetReportSectionType;
  ADataControllers: TDictionary<Integer, TdxSpreadSheetReportDataController>;

  procedure PopulateDataControllers(ADataController: TdxSpreadSheetReportDataController;
    ID: Integer; ADetails: TdxSpreadSheetReportDetails);
  var
    I: Integer;
    AValue: TdxSpreadSheetReportDataController;
  begin
    if not ADataControllers.TryGetValue(ID, AValue) then
      ADataControllers.AddOrSetValue(ID, ADataController);
    for I  := 0 to ADetails.Count - 1 do
      PopulateDataControllers(ADetails[I].DataController, ADetails[I].SectionID, ADetails[I].Details);
  end;

begin
  SectionList.Clear;
  ADataControllers := TDictionary<Integer, TdxSpreadSheetReportDataController>.Create;
  try
    PopulateDataControllers(DataBinding.DataController, -1, DataBinding.Details);
    for I := 0 to DefinedNames.Count - 1 do
      if SectionNameController.IsSectionName(DefinedNames[I], AType, AIndex) then
      begin
        ASection := TdxSpreadSheetReportSection.Create(DefinedNames[I], AType, AIndex);
        if not ADataControllers.TryGetValue(AIndex, ASection.DataController) then
          ASection.DataController := nil;
        SectionList.Add(ASection);
      end;
  finally
    ADataControllers.Free;
    LayoutChanged;
  end;
end;

procedure TdxSpreadSheetReportDesigner.ResetCacheInformation;
begin
  DBCells.Clear;
  DBFormulas.Clear;
  TextCells.Clear;
end;

procedure TdxSpreadSheetReportDesigner.SetSectionInfo(
  const AName: string; const ABounds: TRect; ALevel: Integer = -1);
var
  ADefinedName: TdxSpreadSheetDefinedName;
  ASection: TdxSpreadSheetReportSection;
begin
  ASection := GetSection(AName, ALevel);
  History.BeginAction(TdxSpreadSheetReportDesignerHistoryChangeSectionAction);
  try
    if History.CanAddCommand then
      History.AddCommand(TdxSpreadSheetDesignerChangeSectionCommand.Create(ASection, AName, ABounds, ALevel));
    if (ASection <> nil) and cxRectIsInvalid(ABounds) then
      ASection.Name.Free  // todo: check rebuild sections after remove name also need check undo/redo operation
    else
    begin
      if ASection = nil then
        ADefinedName := DefinedNames.Add(ActuallySectionName(AName, ALevel),
          dxReferenceToString(ABounds, OptionsView.R1C1Reference, 0, 0))
      else
        ADefinedName := ASection.Name;
      if ADefinedName <> nil then // todo: need check undo/redo operation when section area was changed
      begin
        ADefinedName.Reference := dxReferenceToString(ABounds, OptionsView.R1C1Reference, 0, 0);
        if ADefinedName.Scope = nil then
          ADefinedName.Scope := ActiveSheetAsTable;
      end;
    end;
  finally
    History.EndAction();
  end;
  LayoutChanged;
end;

function TdxSpreadSheetReportDesigner.GetCurrentDataController: TdxSpreadSheetReportDataController;
begin
  Result := GetDataControllerForSection(CurrentSection);
end;

function TdxSpreadSheetReportDesigner.GetCurrentSection: TdxSpreadSheetReportSection;
begin
  Result := GetSectionByCell(ActiveSheetAsTable.Selection.FocusedRow,
    ActiveSheetAsTable.Selection.FocusedColumn);
end;

function TdxSpreadSheetReportDesigner.GetFilter: IcxFilterControl;
begin
  Supports(DataBinding.DataController, IcxFilterControl, Result);
end;

procedure TdxSpreadSheetReportDesigner.SetDataBinding(AValue: TdxSpreadSheetReportDataBinding);
begin
  DataBinding.Assign(AValue);
end;

procedure TdxSpreadSheetReportDesigner.SetFieldChooser(AValue: TdxSpreadSheetReportDesignerFieldChooser);
begin
  FFieldChooser.Assign(AValue);
end;

procedure TdxSpreadSheetReportDesigner.SetOptions(AValue: TdxSpreadSheetReportDesignerOptions);
begin
  FOptions.Assign(AValue);
end;

{ TdxSpreadSheetReportTableView }

function TdxSpreadSheetReportTableView.CreateViewInfo: TdxSpreadSheetCustomViewViewInfo;
begin
  Result := TdxSpreadSheetReportTableViewInfo.Create(Self);
end;

{ TdxSpreadSheetReportTableViewInfo }

constructor TdxSpreadSheetReportTableViewInfo.Create(AView: TdxSpreadSheetCustomView);
begin
  inherited Create(AView);
  DragDropAreaCell := TdxSpreadSheetReportDragDropFieldAreaCell.Create(AView);
  TdxSpreadSheetReportDragDropFieldAreaCell(DragDropAreaCell).FVisible := False;
end;

destructor TdxSpreadSheetReportTableViewInfo.Destroy;
begin
  FreeAndNil(DragDropAreaCell);
  inherited Destroy;
end;

function TdxSpreadSheetReportTableViewInfo.CanDrawCellSelection: Boolean;
begin
  DrawBaseSelection := inherited CanDrawCellSelection;
  Result := True;
end;

function TdxSpreadSheetReportTableViewInfo.CreateCellViewInfo(ACell: TdxSpreadSheetCell): TdxSpreadSheetTableViewCellViewInfo;
begin
  Result := TdxSpreadSheetReportCellViewInfo.Create(Self);
end;

procedure TdxSpreadSheetReportTableViewInfo.CalculateHitTest(AHitTest: TdxSpreadSheetCustomHitTest);
var
  I: Integer;
begin
  for I := 0 to Designer.SectionList.Count - 1 do
  begin
    if TdxSpreadSheetReportSectionViewInfo(Designer.SectionList[I].Cell).InitHitTest(AHitTest) then
      Exit;
  end;
  inherited CalculateHitTest(AHitTest);
end;

procedure TdxSpreadSheetReportTableViewInfo.DrawSelection(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  if DrawBaseSelection then
    inherited DrawSelection(ACanvas);
  ACanvas.SaveClipRegion;
  try
    if DragDropAreaCell.Visible then
      DragDropAreaCell.Draw(ACanvas, dsSecond);
    if Designer.Options.DesignView then
    begin
      for I := 0 to Designer.SectionList.Count - 1 do
        if Designer.SectionList[I].Cell <> nil then
          Designer.SectionList[I].Cell.Draw(ACanvas, dsSecond);
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxSpreadSheetReportTableViewInfo.RecalculateArea(AArea: TdxSpreadSheetReportSection);
begin
  AArea.Cell.SetBounds(GetAreaBounds(AArea.Bounds), Bounds);
  AArea.Cell.Recalculate;
end;

procedure TdxSpreadSheetReportTableViewInfo.SelectionChanged;
var
  I: Integer;
begin
  inherited SelectionChanged;
  for I := 0 to Designer.SectionList.Count - 1 do
    RecalculateArea(Designer.SectionList[I]);
end;

procedure TdxSpreadSheetReportTableViewInfo.UpdateDragDropAreaCell(
  const X, Y: Integer; ADragObject: TDragObject; AAccepted: Boolean);
var
  R: TRect;
  ARow, AColumn: Integer;
  AVisibleChanged: Boolean;
begin
  AAccepted := AAccepted and (Designer.FieldChooser.DragObject <> nil);
  if AAccepted then
  begin
    View.CellAtPoint(cxPointScale(Point(X, Y), 100, View.ZoomFactor), ARow, AColumn);
    AAccepted := AAccepted and (ARow >= 0) and (AColumn >= 0);
  end;
  AVisibleChanged := DragDropAreaCell.Visible <> AAccepted;
  TdxSpreadSheetReportDragDropFieldAreaCell(DragDropAreaCell).FVisible := AAccepted;
  if AAccepted then
  begin
    R := View.MergedCells.CheckCell(ARow, AColumn);
    if DragDropAreaCell.Visible and not AVisibleChanged then
      DragDropAreaCell.Invalidate;
    DragDropAreaCell.SetBounds(GetAreaBounds(R), Bounds);
    if DragDropAreaCell.Visible then
      DragDropAreaCell.Invalidate;
  end
  else
    if AVisibleChanged then
      View.Invalidate;
end;

function TdxSpreadSheetReportTableViewInfo.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := TdxSpreadSheetReportDesigner(SpreadSheet);
end;

function TdxSpreadSheetReportTableViewInfo.GetHitTest: TdxSpreadSheetTableViewHitTest;
begin
  Result := View.HitTest;
end;

function TdxSpreadSheetReportTableViewInfo.GetView: TdxSpreadSheetReportTableView;
begin
  Result := TdxSpreadSheetReportTableView(inherited View);
end;

{ TdxSpreadSheetReportCellViewInfo }

function TdxSpreadSheetReportCellViewInfo.GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass;
begin
  Result := TdxSpreadSheetReportViewBuiltInPopupMenu;
end;

procedure  TdxSpreadSheetReportCellViewInfo.DrawValue(ACanvas: TcxCanvas);
var
  R: TRect;
  ARgn1, ARgn2: TcxRegion;
begin
  if (DisplayText <> '') and Designer.Options.DesignView and Designer.IsDBFormula(ActualCell.AsFormula) then
  begin
    ARgn1 := ACanvas.GetClipRegion;
    inherited DrawValue(ACanvas);
    ARgn2 := ACanvas.GetClipRegion;
    ACanvas.SetClipRegion(ARgn1, roSet);
    R := ContentBounds;
    R := cxRectSetTop(cxRectSetRight(GetDatabaseGlyph.ClientRect,
      R.Right - cxTextOffset), R.Top + cxTextOffset);
    GetDatabaseGlyph.StretchDraw(ACanvas.Handle, R);
    ACanvas.SetClipRegion(ARgn2, roSet);
  end
  else
    inherited DrawValue(ACanvas);
end;

function TdxSpreadSheetReportCellViewInfo.GetDesigner: TdxSpreadSheetReportDesigner;
begin
  Result := TdxSpreadSheetReportDesigner(SpreadSheet);
end;

{ Other }

procedure AddSpreadSheetReportResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxReportFieldList', @sdxReportFieldList);
  AProduct.Add('sfnField', @sfnField);
  AProduct.Add('sfnParameter', @sfnParameter);
  AProduct.Add('sfnFieldPicture', @sfnFieldPicture);
  AProduct.Add('sfnRange', @sfnRange);
{
  sSections = 'Sections';
  sRemoveSection = 'Remove Section';
}

  AProduct.Add('sHeaderCaption', @sHeaderCaption);
  AProduct.Add('sFooterCaption', @sFooterCaption);
  AProduct.Add('sDetailCaption', @sDetailCaption);
  AProduct.Add('sDetailLevelCaption', @sDetailLevelCaption);
  AProduct.Add('sGroupHeaderCaption', @sGroupHeaderCaption);
  AProduct.Add('sGroupFooterCaption', @sGroupFooterCaption);

  AProduct.Add('sRange', @sRange);
  AProduct.Add('sTopLeft', @sTopLeft);

  AProduct.Add('sdxChangeSection', @sdxChangeSection);
end;

procedure RegisterAssistants;
begin
  FieldChooserBounds := TDictionary<TdxSpreadSheetReportDesigner, TRect>.Create;
  dxSpreadSheetFunctionsRepository.Add(@sfnField, fnField, fpiField, frkValue, 255, ftDatabase);
  dxSpreadSheetFunctionsRepository.Add(@sfnParameter, fnParameters, fpiField, frkValue, 255, ftDatabase);
  dxSpreadSheetFunctionsRepository.Add(@sfnFieldPicture, fnFieldPicture, fpiFieldPicture, frkValue, 255, ftDatabase);
  dxSpreadSheetFunctionsRepository.Add(@sfnRange, fnRange, fpiRange, frkValue, 255, ftDatabase);

  RegisterClasses([TdxSpreadSheetReportDesigner, TdxSpreadSheetReportTableView, TdxSpreadSheetReportDetail]);
  dxResourceStringsRepository.RegisterProduct('ExpressSpreadSheet Report Designer', @AddSpreadSheetReportResourceStringNames);
end;

procedure UnregisterAssistants;
begin
  FreeAndNil(NamesController);
  FreeAndNil(DatabaseGlyph);
  dxSpreadSheetFunctionsRepository.Remove(@sfnField);
  dxSpreadSheetFunctionsRepository.Remove(@sfnParameter);
  dxSpreadSheetFunctionsRepository.Remove(@sfnFieldPicture);
  dxSpreadSheetFunctionsRepository.Remove(@sfnRange);
  FreeAndNil(FieldChooserBounds);
  dxResourceStringsRepository.UnRegisterProduct('ExpressSpreadSheet Report Designer', @AddSpreadSheetReportResourceStringNames);
end;

initialization
  RegisterAssistants;

finalization
  UnregisterAssistants;

end.

