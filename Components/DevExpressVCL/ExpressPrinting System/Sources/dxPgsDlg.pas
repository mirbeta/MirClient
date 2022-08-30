{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPgsDlg;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Buttons, Menus, ToolWin, Registry, ImgList, dxMessages, cxClasses, dxCustomPreview, dxPSESys, dxPSForm, dxBkgnd,
  dxPreVw, dxPSGlbl, dxfmClr, dxPrnPg, dxPrnDev, dxExtCtrls, cxLookAndFeelPainters, cxButtons, cxRadioGroup, cxPC,
  cxControls, cxContainer, cxEdit, cxCheckBox, cxLabel, cxTextEdit, cxMemo, cxListBox, cxGraphics, cxMaskEdit,
  cxDropDownEdit, cxGroupBox, cxSpinEdit, cxDrawTextUtils, dxCore, cxGeometry, cxLookAndFeels, IniFiles,
  dxPSHFToolBarBld, dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, cxImageList, cxCustomListBox;

type
  TdxPageSetupDlgButtonKind = (psbHelp, psbStyleOptions, psbPreview, psbPrint);
  TdxPageSetupDlgButtons = set of TdxPageSetupDlgButtonKind;
  TdxPageSetupDlgOption =
    (psoCenterOnPage, psoMargins, psoPageOrder, psoShading, psoStyleCaption,
     psoHFAutoText, psoHFBackground, psoHFFont, psoHFText, psoHFFunctions,
     psoHFMargins, psoHFReverse, psoHFVertAlignment);
  TdxPageSetupDlgOptions = set of TdxPageSetupDlgOption;
  TdxHFMode = (hfmThreeSections, hfmOneSection);

const
  psbAll = [Low(TdxPageSetupDlgButtonKind)..High(TdxPageSetupDlgButtonKind)];
  psbDefault = [psbStyleOptions, psbPreview, psbPrint];
  psoAll = [Low(TdxPageSetupDlgOption)..High(TdxPageSetupDlgOption)];
  psoDefaultOptionsEnabled = psoAll;
  psoDefaultOptionsVisible = psoAll;

type
  TdxPrintStyleManager = class;
  TBasedxPrintStyle = class;
  TdxPrintStyleClass = class of TBasedxPrintStyle;
  TAbstractdxStyleManagerDesigner = class;
  TdxPageSetupDialog = class;

  { TdxPrintStylePrinterPage }

  TdxPrintStylePrinterPageClass = class of TdxPrintStylePrinterPage;
  TdxPrintStylePrinterPage = class(TdxPrinterPage)
  private
    FPrintStyle: TBasedxPrintStyle;
  protected
    function GetOwner: TPersistent; override;

    function IsPageFooterTitleStored(Index: Integer): Boolean; override;
    function IsPageHeaderTitleStored(Index: Integer): Boolean; override;
    procedure PageParamsChanged(AUpdateCodes: TdxPrinterPageUpdateCodes); override;
  public
    property PrintStyle: TBasedxPrintStyle read FPrintStyle;
  end;

  TdxPrintStyleState = (pssCopy, pssOptionsDialog);
  TdxPrintStyleStates = set of TdxPrintStyleState;

  TdxPageParamsChangedEvent = procedure(Sender: TdxPrinterPage;
    APrintStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes) of object;

  TdxFilterPaperEvent = procedure(Sender: TBasedxPrintStyle;
    const APaper: TdxPaperInfo; var AIsSupported: Boolean) of object;

  { TBasedxPrintStyle }

  TBasedxPrintStyle = class(TComponent)
  private
    FAllowChangeHFText: Boolean;
    FAllowChangeMargins: Boolean;
    FAllowChangeOrientation: Boolean;
    FAllowChangePaper: Boolean;
    FAllowChangeScale: Boolean;
    FAllowCustomPaperSizes: Boolean;
    FBuiltIn: Boolean;
    FData: Pointer;
    FDefaultStyleGlyph: TBitmap;
    FDescription: string;
    FImageIndex: Integer;
    FIsDescriptionAssigned: Boolean;
    FIsStyleCaptionAssigned: Boolean;
    FIsStyleGlyphAssigned: Boolean;
    FPrinterPage: TdxPrinterPage;
    FShowPageSetupDlg: Boolean;
    FState: TdxPrintStyleStates;
    FStyleCaption: string;
    FStyleGlyph: TBitmap;
    FStyleManager: TdxPrintStyleManager;

    FOnDestroy: TNotifyEvent;
    FOnFilterPaper: TdxFilterPaperEvent;

    function GetDescription: string;
    function GetIndex: Integer;
    function GetIsCurrentStyle: Boolean;
    function GetStyleCaption: string;
    function GetStyleGlyph: TBitmap;
    function IsDescriptionStored: Boolean;
    function IsStyleCaptionStored: Boolean;
    function IsStyleGlyphStored: Boolean;
    procedure SetBuiltIn(Value: Boolean);
    procedure SetDescription(const Value: string);
    procedure SetImageIndex(Value: Integer);
    procedure SetIndex(Value: Integer);
    procedure SetIsCurrentStyle(Value: Boolean);
    procedure SetPrinterPage(Value: TdxPrinterPage);
    procedure SetStyleCaption(const Value: string);
    procedure SetStyleGlyph(Value: TBitmap);
    procedure SetStyleManager(Value: TdxPrintStyleManager);

    procedure DesignerModified;
    procedure DesignerUpdate(TheAll: Boolean);
    function IsDesigning: Boolean;
    function IsLoading: Boolean;

    procedure ReadData(Reader: TReader);
    procedure ReadIsDescriptionAssigned(Reader: TReader);
    procedure ReadIsStyleCaptionAssigned(Reader: TReader);
    procedure ReadIsStyleGlyphAssigned(Reader: TReader);
    procedure SynchronizePixelsPerInch;
    procedure WriteData(Writer: TWriter);
    procedure WriteIsDescriptionAssigned(Writer: TWriter);
    procedure WriteIsStyleCaptionAssigned(Writer: TWriter);
    procedure WriteIsStyleGlyphAssigned(Writer: TWriter);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadState(Reader: TReader); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(AParent: TComponent); override;

    function GetAllowChangeHFText: Boolean; virtual;
    function GetAllowChangeMargins: Boolean; virtual;
    function GetAllowChangeOrientation: Boolean; virtual;
    function GetAllowChangePaper: Boolean; virtual;
    function GetAllowChangeScale: Boolean; virtual;
    function GetAllowCustomPaperSizes: Boolean; virtual;
    procedure SetAllowChangeHFText(Value: Boolean); virtual;
    procedure SetAllowChangeMargins(Value: Boolean); virtual;
    procedure SetAllowChangeOrientation(Value: Boolean); virtual;
    procedure SetAllowChangePaper(Value: Boolean); virtual;
    procedure SetAllowChangeScale(Value: Boolean); virtual;
    procedure SetAllowCustomPaperSizes(Value: Boolean); virtual;

    function CreatePrinterPage: TdxPrinterPage; virtual;
    function GetPrinterPageClass: TdxPrinterPageClass; virtual;
    procedure InitializePrinterPage(APrinterPage: TdxPrinterPage); virtual;

    procedure DoAfterPrinting; dynamic;
    procedure DoBeforePrinting; dynamic;
    procedure DoDestroy; dynamic;
    function IsSupportedPaper(const APaper: TdxPaperInfo): Boolean; dynamic;
    procedure PageParamsChanged(AUpdateCodes: TdxPrinterPageUpdateCodes); dynamic;

    procedure InitializeDefaultStyleGlyph(ABitmap: TBitmap); virtual;
    procedure StyleGlyphChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    class function StyleClass: TdxPrintStyleClass;

    function DefaultDescription: string; virtual;
    function DefaultPageFooterText(APart: TdxPageTitlePart): string; virtual;
    function DefaultPageHeaderText(APart: TdxPageTitlePart): string; virtual;
    function DefaultStyleCaption: string; virtual;
    function DefaultStyleGlyph: TBitmap; virtual;

    procedure AfterPrinting;
    procedure BeforePrinting;

    procedure GetFilteredPapers(AStrings: TStrings);

    function PageSetup: Boolean; overload;
    function PageSetup(AnActivePageIndex: Integer; AShowPreviewBtn, AShowPrintBtn: Boolean;
      out APreviewBtnClicked, APrintBtnClicked: Boolean): Boolean; overload;

    function ShowStyleOptionsDlg: Boolean; virtual;
    function HasStyleOptionsDlg: Boolean; virtual;

    procedure RestoreDefaultGlyph; virtual;
    procedure RestoreDefaults; virtual;

    property BuiltIn: Boolean read FBuiltIn write SetBuiltIn;
    property Data: Pointer read FData write FData;
    property IsStyleGlyphAssigned: Boolean read FIsStyleGlyphAssigned;
    property State: TdxPrintStyleStates read FState;
    property StyleManager: TdxPrintStyleManager read FStyleManager write SetStyleManager;
  published
    property AllowChangeHFText: Boolean read GetAllowChangeHFText write SetAllowChangeHFText default True;
    property AllowChangeMargins: Boolean read GetAllowChangeMargins write SetAllowChangeMargins default True;
    property AllowChangeOrientation: Boolean read GetAllowChangeOrientation write SetAllowChangeOrientation default True;
    property AllowChangePaper: Boolean read GetAllowChangePaper write SetAllowChangePaper default True;
    property AllowChangeScale: Boolean read GetAllowChangeScale write SetAllowChangeScale default True;
    property AllowCustomPaperSizes: Boolean read GetAllowCustomPaperSizes write SetAllowCustomPaperSizes default True;
    property Description: string read GetDescription write SetDescription stored IsDescriptionStored;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Index: Integer read GetIndex write SetIndex stored False;
    property IsCurrentStyle: Boolean read GetIsCurrentStyle write SetIsCurrentStyle stored False;
    property PrinterPage: TdxPrinterPage read FPrinterPage write SetPrinterPage;
    property ShowPageSetupDlg: Boolean read FShowPageSetupDlg write FShowPageSetupDlg stored False;
    property StyleCaption: string read GetStyleCaption write SetStyleCaption stored IsStyleCaptionStored;
    property StyleGlyph: TBitmap read GetStyleGlyph write SetStyleGlyph stored IsStyleGlyphStored; {32 x 32}

    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnFilterPaper: TdxFilterPaperEvent read FOnFilterPaper write FOnFilterPaper;
  end;

  { TdxPrintStyleManager }

  TdxPrintStyleManager = class(TcxScalableComponent)
  private
    FAlreadySaved: Boolean;
    FAutoHFTextEntries: TStrings;
    FAutoSave: Boolean;
    FCloneStyleCaptionPrefix: string;
    FCurrentStyle: TBasedxPrintStyle;
    FDesigner: TAbstractdxStyleManagerDesigner;
    FHelpContext: THelpContext;
    FImages: TImageList;
    FInternalStreaming: Boolean;
    FIsCloneStyleCaptionPrefixAssigned: Boolean;
    FIsTitleAssigned: Boolean;
    FLoadedExistingStyles: TStringList;
    FPageSetupDialog: TdxPageSetupDialog;
    FPreviewBtnClicked: Boolean;
    FPrintBtnClicked: Boolean;
    FStorageName: string;
    FStyles: TList;
    FTitle: string;
    FUpdateCount: Integer;
    FVersion: Integer;
    FWindowHandle: hWnd;

    FOnChangeCurrentStyle: TNotifyEvent;
    FOnStyleListChanged: TNotifyEvent;

    function GetCloneStyleCaptionPrefix: string;
    function GetCount: Integer;
    function GetCurrentStyleIndex: Integer;
    function GetRegistryPath: string;
    function GetStyle(Index: Integer): TBasedxPrintStyle;
    function GetTitle: string;
    function IsAutoHFTextEntriesStored: Boolean;
    function IsCloneStyleCaptionPrefixStored: Boolean;
    function IsTitleStored: Boolean;
    procedure SetAutoHFTextEntries(Value: TStrings);
    procedure SetCloneStyleCaptionPrefix(const Value: string);
    procedure SetCurrentStyle(Value: TBasedxPrintStyle);
    procedure SetCurrentStyleIndex(Value: Integer);
    procedure SetImages(Value: TImageList);
    procedure SetNewStyleCaption(AStyle: TBasedxPrintStyle; AIndex: Integer);
    procedure SetPageSetupDialog(Value: TdxPageSetupDialog);
    procedure SetStyle(Index: Integer; Value: TBasedxPrintStyle);
    procedure SetTitle(const Value: string);

    function AllowAutoSave: Boolean;
    procedure DesignerModified;
    procedure DesignerUpdate(AStyle: TBasedxPrintStyle);
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;

    procedure FreeAndNilStyles;
    procedure InsertStyle(Value: TBasedxPrintStyle);
    procedure MoveStyle(ACurIndex, ANewIndex: Integer);
    procedure RemoveStyle(Value: TBasedxPrintStyle);
    procedure ResyncCurrentStyle(AIndex: Integer);

    // AutoHFTextEntries - v2.2
    procedure AutoHFTextEntriesChanged(Sender: TObject);
    procedure OnAutoHFTextEntryClick(Sender: TObject);
    procedure OnEditAutoHFTextEntriesClick(Sender: TObject);

    procedure WndProc(var Message: TMessage);

    procedure SetNameHandler(Reader: TReader; Component: TComponent; var Name: string);

    procedure ReadIsCloneStyleCaptionPrefixAssigned(Reader: TReader);
    procedure ReadIsTitleAssigned(Reader: TReader);
    procedure WriteIsCloneStyleCaptionPrefixAssigned(Writer: TWriter);
    procedure WriteIsTitleAssigned(Writer: TWriter);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetName(const NewName: TComponentName); override;

    function CreateAutoHFTextEntries: TStrings; virtual;
    procedure DoRestoreDefaults; virtual;

    procedure ChangeCurrentStyle; dynamic;
    procedure PageParamsChanged(APrintStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes); dynamic;
    procedure StyleListChanged; dynamic;

    property RegistryPath: string read GetRegistryPath;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function AddStyle(AStyleClass: TdxPrintStyleClass): TBasedxPrintStyle;
    function AddStyleEx(AStyleClass: TdxPrintStyleClass; AOwner: TComponent): TBasedxPrintStyle;
    procedure AssignStyles(Source: TdxPrintStyleManager);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure DeleteNonBuiltIns;
    function IndexOfStyle(Value: TBasedxPrintStyle): Integer;
    function NonBuiltInsExists: Boolean;
    function StyleByCaption(const ACaption: string): TBasedxPrintStyle;
    function StyleByName(const AName: string): TBasedxPrintStyle;

    function BeginClone(AIndex: Integer): TBasedxPrintStyle;
    procedure EndClone(AStyle: TBasedxPrintStyle);

    procedure BeginUpdate;
    procedure EndUpdate;

    function DefaultCloneStyleCaptionPrefix: string; virtual;
    function DefaultTitle: string; virtual;

    // AutoHFTextEntries - v2.2
    procedure BuildAutoHFTextEntriesMenu(ARootItem: TComponent; AData: Pointer;
      AIncludeSetupAutoHFTextEntriesItem: Boolean = True);
    procedure RefreshAutoHFTextEntries;
    function ShowAutoHFTextEntriesDlg: Boolean;

    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure LoadFromRegistry(const APath: string);
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure SaveToRegistry(const APath: string);

    procedure DefinePrintStylesDlg(out APreviewBtnClicked, APrintBtnClicked: Boolean); overload;

    procedure RestoreDefaultAutoHFTextEntries;
    procedure RestoreDefaults;
    procedure RestoreDefaultStyles;

    procedure LoadFromFile(const AName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AName: string);
    procedure SaveToStream(AStream: TStream);

    property Count: Integer read GetCount;
    property CurrentStyleIndex: Integer read GetCurrentStyleIndex write SetCurrentStyleIndex;
    property Designer: TAbstractdxStyleManagerDesigner read FDesigner; {accessible only in DesignTime}
    property PreviewBtnClicked: Boolean read FPreviewBtnClicked;
    property PrintBtnClicked: Boolean read FPrintBtnClicked;
    property Styles[Index: Integer]: TBasedxPrintStyle read GetStyle write SetStyle; default;
    property UpdateCount: Integer read FUpdateCount;
  published
    property AutoHFTextEntries: TStrings read FAutoHFTextEntries write SetAutoHFTextEntries stored IsAutoHFTextEntriesStored;
    property AutoSave: Boolean read FAutoSave write FAutoSave default False;
    property CloneStyleCaptionPrefix: string read GetCloneStyleCaptionPrefix write SetCloneStyleCaptionPrefix stored IsCloneStyleCaptionPrefixStored;
    property CurrentStyle: TBasedxPrintStyle read FCurrentStyle write SetCurrentStyle;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property Images: TImageList read FImages write SetImages;
    property PageSetupDialog: TdxPageSetupDialog read FPageSetupDialog write SetPageSetupDialog;
    property StorageName: string read FStorageName write FStorageName;
    property Title: string read GetTitle write SetTitle stored IsTitleStored;
    property Version: Integer read FVersion write FVersion;

    property OnChangeCurrentStyle: TNotifyEvent read FOnChangeCurrentStyle write FOnChangeCurrentStyle;
    property OnStyleListChanged: TNotifyEvent read FOnStyleListChanged write FOnStyleListChanged;
  end;

  TAbstractdxStyleManagerDesigner = class
  private
    FStyleManager: TdxPrintStyleManager;
  protected
    procedure Modified; virtual; abstract;
    procedure Update(AItem: TBasedxPrintStyle); virtual; abstract;
  public
    constructor Create(AStyleManager: TdxPrintStyleManager);
    destructor Destroy; override;

    procedure BeginUpdate; virtual; abstract;
    procedure CancelUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;

    property StyleManager: TdxPrintStyleManager read FStyleManager;
  end;

  { TdxPageSetupDialog }

  TdxCustomDrawPreviewEvent = procedure(APrintStyle: TBasedxPrintStyle;
    ACanvas: TCanvas; APageRect, AContentRect, AHeaderRect, AFooterRect: TRect) of object;

  PdxPageSetupDlgEvents = ^TdxPageSetupDlgEvents;
  TdxPageSetupDlgEvents = record
    OnClose: TNotifyEvent;
    OnCustomDrawPreview: TdxCustomDrawPreviewEvent;
    OnShow: TNotifyEvent;
  end;

  PdxPageSetupDlgData = ^TdxPageSetupDlgData;
  TdxPageSetupDlgData = record
    PrintStyle: TBasedxPrintStyle;
    ActivePageIndex: Integer;
    Title: string;
    HelpContext: THelpContext;
    HFMode: TdxHFMode;
    ButtonsEnabled: TdxPageSetupDlgButtons;
    ButtonsVisible: TdxPageSetupDlgButtons;
    OptionsEnabled: TdxPageSetupDlgOptions;
    OptionsVisible: TdxPageSetupDlgOptions;
    Events: TdxPageSetupDlgEvents;
    PreviewBtnClicked: Boolean;
    PrintBtnClicked: Boolean;
    iReserved: Integer;
  end;

  TdxPageSetupDialog = class(TcxCustomComponent)
  private
    FActivePageIndex: Integer;
    FButtonsEnabled: TdxPageSetupDlgButtons;
    FButtonsVisible: TdxPageSetupDlgButtons;
    FHFMode: TdxHFMode;
    FHelpContext: THelpContext;
    FIsTitleAssigned: Boolean;
    FOptionsEnabled: TdxPageSetupDlgOptions;
    FOptionsVisible: TdxPageSetupDlgOptions;
    FPreviewBtnClicked: Boolean;
    FPrintBtnClicked: Boolean;
    FPrintStyle: TBasedxPrintStyle;
    FTitle: string;
    FOnClose: TNotifyEvent;
    FOnCustomDrawPreview: TdxCustomDrawPreviewEvent;
    FOnShow: TNotifyEvent;
    function GetTitle: string;
    function IsTitleStored: Boolean;
    procedure SetPrintStyle(Value: TBasedxPrintStyle);
    procedure SetTitle(const Value: string);

    procedure ReadIsTitleAssigned(Reader: TReader);
    procedure WriteIsTitleAssigned(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    function DefaultTitle: string; virtual;
    function Execute: Boolean;
    function RealTitle: string;
    procedure RestoreDefaults; virtual;

    property PreviewBtnClicked: Boolean read FPreviewBtnClicked;
    property PrintBtnClicked: Boolean read FPrintBtnClicked;
  published
    property ActivePageIndex: Integer read FActivePageIndex write FActivePageIndex default 0;
    property ButtonsEnabled: TdxPageSetupDlgButtons read FButtonsEnabled write FButtonsEnabled
      default [psbStyleOptions, psbPreview, psbPrint];
    property ButtonsVisible: TdxPageSetupDlgButtons read FButtonsVisible write FButtonsVisible
      default [psbStyleOptions, psbPreview, psbPrint];
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property HFMode: TdxHFMode read FHFMode write FHFMode default hfmThreeSections;
    property OptionsEnabled: TdxPageSetupDlgOptions read FOptionsEnabled write FOptionsEnabled
      default [Low(TdxPageSetupDlgOption)..High(TdxPageSetupDlgOption)]; {psoDefaultOptionsEnabled}
    property OptionsVisible: TdxPageSetupDlgOptions read FOptionsVisible write FOptionsVisible
      default psoDefaultOptionsVisible;
    property PrintStyle: TBasedxPrintStyle read FPrintStyle write SetPrintStyle;
    property Title: string read GetTitle write SetTitle stored IsTitleStored;

    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCustomDrawPreview: TdxCustomDrawPreviewEvent read FOnCustomDrawPreview write FOnCustomDrawPreview;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TdxfmPageSetupDialog = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnFix: TcxButton;
    btnFooterBackGround: TcxButton;
    btnFooterFont: TcxButton;
    btnHeaderBackground: TcxButton;
    btnHeaderFont: TcxButton;
    btnHelp: TcxButton;
    btnOK: TcxButton;
    btnOptions: TcxButton;
    btnPrint: TcxButton;
    btnPrintPreview: TcxButton;
    btnRestoreOriginalMargins: TcxButton;
    btnVertAlignBottom: TcxButton;
    btnVertAlignCenter: TcxButton;
    btnVertAlignTop: TcxButton;
    bvlMarginsWarningHolder: TdxLayoutItem;
    bvlOrientationHolder: TdxLayoutItem;
    bvlPreviewHolder: TdxLayoutItem;
    cbxPaperSource: TcxComboBox;
    chbxCenterHorz: TcxCheckBox;
    chbxCenterVert: TcxCheckBox;
    chbxReverseOnEvenPages: TcxCheckBox;
    chbxShading: TcxCheckBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem30: TdxLayoutItem;
    dxLayoutItem31: TdxLayoutItem;
    dxLayoutItem32: TdxLayoutItem;
    litbPredefined: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edFooterFontInfo: TcxTextEdit;
    edHeaderFontInfo: TcxTextEdit;
    edStyleName: TcxTextEdit;
    gbxFunctions: TdxLayoutGroup;
    gbxMargins: TdxLayoutGroup;
    gbxOrientation: TdxLayoutGroup;
    gbxPaper: TdxLayoutGroup;
    gbxPrintOrder: TdxLayoutGroup;
    gbxShading: TdxLayoutGroup;
    gbxVertAlignment: TdxLayoutGroup;
    ilBins: TcxImageList;
    ilPapers: TcxImageList;
    ilPaperTypes: TcxImageList;
    ilPrintOrders: TcxImageList;
    lblCenterOnPage: TcxLabel;
    lblMarginBottom: TdxLayoutItem;
    lblMarginFooter: TdxLayoutItem;
    lblMarginHeader: TdxLayoutItem;
    lblMarginLeft: TdxLayoutItem;
    lblMarginRight: TdxLayoutItem;
    lblMarginTop: TdxLayoutItem;
    lblPaperHeight: TdxLayoutItem;
    lblPaperSource: TdxLayoutItem;
    lblPaperWidth: TdxLayoutItem;
    lblPercentOfNormalSize: TdxLayoutItem;
    lblStyleName: TdxLayoutItem;
    lbxPaperType: TcxListBox;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnCancel: TdxLayoutItem;
    libtnFooterBackground: TdxLayoutItem;
    libtnFooterFont: TdxLayoutItem;
    libtnHeaderBackground: TdxLayoutItem;
    libtnHeaderFont: TdxLayoutItem;
    libtnHelp: TdxLayoutItem;
    libtnOK: TdxLayoutItem;
    libtnOptions: TdxLayoutItem;
    libtnPrint: TdxLayoutItem;
    libtnPrintPreview: TdxLayoutItem;
    lichbxCenterHorz: TdxLayoutItem;
    lichbxCenterVert: TdxLayoutItem;
    liedFooterFontInfo: TdxLayoutItem;
    liedHeaderFontInfo: TdxLayoutItem;
    lilblCenterOnPage: TdxLayoutItem;
    limemFooterCenter: TdxLayoutItem;
    limemFooterLeft: TdxLayoutItem;
    limemFooterRight: TdxLayoutItem;
    limemHeaderCenter: TdxLayoutItem;
    limemHeaderRight: TdxLayoutItem;
    memFooterCenter: TcxMemo;
    memFooterLeft: TcxMemo;
    memFooterRight: TcxMemo;
    memHeaderCenter: TcxMemo;
    memHeaderLeft: TcxMemo;
    memHeaderRight: TcxMemo;
    pbxPageOrder: TPaintBox;
    pgctrlMain: TdxLayoutGroup;
    pnlBottom: TdxLayoutGroup;
    pnlButtons: TdxLayoutGroup;
    pnlFooter: TdxLayoutGroup;
    pnlFooterFont: TdxLayoutGroup;
    pnlFooterMemos: TdxLayoutGroup;
    pnlHeader: TdxLayoutGroup;
    pnlHeaderFont: TdxLayoutGroup;
    pnlHeaderMemos: TdxLayoutGroup;
    pnlHFMargins: TdxLayoutGroup;
    pnlMargins: TdxLayoutGroup;
    pnlReverse: TdxLayoutItem;
    pnlStyleName: TdxLayoutGroup;
    rbtnAdjustTo: TcxRadioButton;
    rBtnAutoOrientation: TcxRadioButton;
    rbtnDownThenOver: TcxRadioButton;
    rbtnFitTo: TcxRadioButton;
    rBtnLandscape: TcxRadioButton;
    rbtnOverThenDown: TcxRadioButton;
    rBtnPortrait: TcxRadioButton;
    seAdjustTo: TcxSpinEdit;
    seMarginBottom: TcxSpinEdit;
    seMarginFooter: TcxSpinEdit;
    seMarginHeader: TcxSpinEdit;
    seMarginLeft: TcxSpinEdit;
    seMarginRight: TcxSpinEdit;
    seMarginTop: TcxSpinEdit;
    sePaperHeight: TcxSpinEdit;
    sePaperWidth: TcxSpinEdit;
    tbPredefined: TPanel;
    tshHeaderFooter: TdxLayoutGroup;
    tshMargins: TdxLayoutGroup;
    tshPage: TdxLayoutGroup;
    tshScaling: TdxLayoutGroup;
    liPagesWide: TdxLayoutItem;
    sePagesWide: TcxSpinEdit;
    liPagesTall: TdxLayoutItem;
    sePagesTall: TcxSpinEdit;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;

    procedure AdjustToExit(Sender: TObject);
    procedure BackgroundClick(Sender: TObject);
    procedure btnFixClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnHFFontClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnPrintPreviewClick(Sender: TObject);
    procedure btnRestoreOriginalMarginsClick(Sender: TObject);
    procedure cbxPaperSourceChange(Sender: TObject);
    procedure cbxPaperSourcePropertiesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure CenterOnPageClick(Sender: TObject);
    procedure chbxReverseOnEvenPagesClick(Sender: TObject);
    procedure chbxShadingClick(Sender: TObject);
    procedure edStyleNameChange(Sender: TObject);
    procedure edStyleNameExit(Sender: TObject);
    procedure FitToPageChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lblMarginTopClick(Sender: TObject);
    procedure lblPaperSourceClick(Sender: TObject);
    procedure lbxPaperTypeClick(Sender: TObject);
    procedure lbxPaperTypeDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect;   AState: TOwnerDrawState);
    procedure MarginChange(Sender: TObject);
    procedure MarginExit(Sender: TObject);
    procedure MarginValueChanged(Sender: TObject);
    procedure memHeaderCenterPropertiesChange(Sender: TObject);
    procedure memHeaderLeftChange(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure MemoEnter(Sender: TObject);
    procedure MemoExit(Sender: TObject);
    procedure OrientationClick(Sender: TObject);
    procedure OrientationDblClick(Sender: TObject);
    procedure PageOrderClick(Sender: TObject);
    procedure PaperWidthHeightChange(Sender: TObject);
    procedure PaperWidthHeightUpdateInfos(Sender: TObject);
    procedure pbxPageOrderDblClick(Sender: TObject);
    procedure pbxPageOrderPaint(Sender: TObject);
    procedure pgctrlMainChange(Sender: TObject);
    procedure pgctrlMainPageChanging(Sender: TObject; NewPage: TcxTabSheet; var AllowChange: Boolean);
    procedure ScaleChanged(Sender: TObject);
    procedure ScalingClick(Sender: TObject);
    procedure VertTextAlignClick(Sender: TObject);
    procedure sePagesWidePropertiesChange(Sender: TObject);
    procedure sePagesWideExit(Sender: TObject);
  private
    FBmpArrow: TBitmap;
    FCanShowStyleOptionsDlg: Boolean;
    FControlsUpdating: Boolean;
    FFooterBkGndGlyph: TBitmap;
    FHeaderBkGndGlyph: TBitmap;
    FHFFunctionList: TStringList;
    FHFToolBarHelper: TdxPSHFToolBarHelper;
    FPredefinedImages: TcxImageList;
    FMarginsChanged: Boolean;
    FMarginsChanging: Boolean;
    FMarginsInvalid: Boolean;
    FMarginsOutside: Boolean;
    FModified: Boolean;
    FOrientationChanging: Boolean;
    FOrientationPreview: TdxPreview;
    FPaperSizeLocked: Boolean;
    FPreview: TdxPreview;
    FPreviewBtnClicked: Boolean;
    FPrintBtnClicked: Boolean;
    FPrintStyle: TBasedxPrintStyle;
    FSavePrintStyle: TBasedxPrintStyle;
    FStyleManager: TdxPrintStyleManager;
    FwpMargins: TdxPSWarningPane;

    FOnClose: TNotifyEvent;
    FOnCustomDrawPreview: TdxCustomDrawPreviewEvent;
    FOnShow: TNotifyEvent;
    procedure AutoHFTextEntriesClick(Sender: TObject);
    procedure ChangeBkgndGlyph(AButton: TcxButton);
    procedure DoInsertMacrosValue(Sender: TObject; const AMacrosValue: string);
    procedure DrawBackgroundBitmapContent(ACanvas: TcxCanvas; R: TRect; ABackground: TdxBackground);
    procedure OrientationPreviewCalcPageCount(Sender: TObject);
    procedure PreviewCalcPageCount(Sender: TObject);
    procedure PreviewDrawPageContent(Sender: TObject;
      ACanvas: TCanvas; ABounds: TRect; APageIndex: Integer);
    procedure PreviewAfterDragMargin(Sender: TObject; AMargin: TdxPreviewPageMargin);

    procedure CheckModified;
    procedure CreateControls;
    procedure EnabledMemoAttr(AEnabled: Boolean);
    procedure FixupMargins;
    procedure FixupMarginsOutside;
    function GetCurrentPaperInfo: TdxPaperInfo;
    function GetPage: TdxPrinterPage;
    function GetPaperInfo(Index: Integer): TdxPaperInfo;
    function GetPaperInfoCount: Integer;
    function GetSelectedTitle: TCustomdxPageObject;
    function GetSelectedTitlePart: TdxPageTitlePart;
    function GetSelectedTitlePartEditor: TcxCustomMemo;
    function GetSelectedTitlePartTextAlignY: TcxTextAlignY;
    function GetTitlePart(AComponent: TComponent): TdxPageTitlePart;
    function GetUnitsDenominator: Integer;
    procedure LoadStrings;
    procedure RestoreOriginalMargins;
    procedure SaveMargins;
    procedure SaveStyleCaption;
    procedure SaveUserInput;
    procedure SetMarginsInvalid(Value: Boolean);
    procedure SetMarginsOutside(Value: Boolean);
    procedure SetSelectedTitlePartTextAlignY(AAlign: TcxTextAlignY);
    procedure SetupDialog(const APageSetupDlgData: TdxPageSetupDlgData);
    procedure StartSetting;
    procedure TrySetActiveControl(AControl: TWinControl);
    procedure UpdateControlsState;
    procedure UpdateMarginsBounds;
    procedure UpdateMarginsEdits;
    procedure UpdatePageInfos;
    procedure UpdatePreviewMargin(const AValue: Extended; AMargin: TdxPreviewPageMargin);
    procedure UpdatePreviewMargins;
    procedure UpdateVerticalAlignmentButtonsState;
    procedure UpdateWarningPane(AValue, APairValue: Boolean; const AHint, APairHint: string);
    function ValidateMargins(out AInvalidMarginControl: TWinControl): Boolean;
    function ValidateMarginsOutside(out AInvalidMarginControl: TWinControl): Boolean;
    function ValidateStyleCaption: Boolean;
    function ValidateUserInput(out AControl: TWinControl): Boolean;

    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;

    property MarginsInvalid: Boolean read FMarginsInvalid write SetMarginsInvalid;
    property MarginsOutside: Boolean read FMarginsOutside write SetMarginsOutside;
    property SelectedTitle: TCustomdxPageObject read GetSelectedTitle;
    property SelectedTitlePart: TdxPageTitlePart read GetSelectedTitlePart;
    property SelectedTitlePartEditor: TcxCustomMemo read GetSelectedTitlePartEditor;
    property SelectedTitlePartTextAlignY: TcxTextAlignY read GetSelectedTitlePartTextAlignY write SetSelectedTitlePartTextAlignY;
    property UnitsDenominator: Integer read GetUnitsDenominator;
  protected
    procedure CreateWnd; override;
    procedure DoHide; override;
    procedure DoShow; override;
    procedure ScaleFactorChanged(M: Integer; D: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;

    procedure SetPrintStyle(Value: TBasedxPrintStyle);

    property CurrentPaperInfo: TdxPaperInfo read GetCurrentPaperInfo;
    property Modified: Boolean read FModified;
    property Page: TdxPrinterPage read GetPage;
    property PaperInfoCount: Integer read GetPaperInfoCount;
    property PaperInfos[Index: Integer]: TdxPaperInfo read GetPaperInfo;
    property PreviewBtnClicked: Boolean read FPreviewBtnClicked;
    property PrintBtnClicked: Boolean read FPrintBtnClicked;
    property PrintStyle: TBasedxPrintStyle read FPrintStyle;
    property OnCustomDrawPreview: TdxCustomDrawPreviewEvent read FOnCustomDrawPreview write FOnCustomDrawPreview;
  end;

type
  TdxGetDateTimeFormatsProc = procedure(AStrings: TStrings);
  TdxGetAutoHFTextEntriesProc = procedure(AStrings: TStrings);

const
  dxMaxStyleCaption = 31;
  dxDefaultPrintStyleClass: TdxPrintStyleClass = nil;

var
  dxGetDateFormatsProc: TdxGetDateTimeFormatsProc = nil;
  dxGetTimeFormatsProc: TdxGetDateTimeFormatsProc = nil;
  dxGetAutoHFTextEntriesProc: TdxGetAutoHFTextEntriesProc = nil;

function DateFormats: TStrings;
function DefaultAutoHFTextEntries: TStrings;
function GetFormatedDate(const ADateTime: TDateTime; const AFormat: string): string;
function GetFormatedTime(const ADateTime: TDateTime; const AFormat: string): string;
function PageNumberFormats: TStrings;
function TimeFormats: TStrings;
procedure GetFormatedDateStrings(const ADateTime: TDateTime; ADateFormats, AFormatedStrings: TStrings);
procedure GetFormatedTimeStrings(const ADateTime: TDateTime; ATimeFormats, AFormatedStrings: TStrings);
procedure RefreshDateFormats;
procedure RefreshTimeFormats;

{ Registration routines }
procedure dxPSGetRegisteredPrintStylesList(AStrings: TStrings);
procedure dxPSRegisterPrintStyle(AStyleClass: TdxPrintStyleClass; AMakeAsDefault: Boolean = False);
procedure dxPSUnregisterPrintStyle(AStyleClass: TdxPrintStyleClass);

{ Utility routines }
procedure dxPSDrawStyleItem(AStyle: TBasedxPrintStyle; AListBox: TListBox; Index: Integer;
  State: TOwnerDrawState; ABounds: TRect; AMultiline, ABoldedCurrent: Boolean; AScaleFactor: TdxScaleFactor);
procedure dxPSDefaultDrawPagePreview(APrintStyle: TBasedxPrintStyle; ACanvas: TCanvas;
  APageBounds, AContentBounds, AHeaderBounds, AFooterBounds: TRect; AScaleFactor: TdxScaleFactor);
function dxPSPrintStyleUniqueName(AStyleController: TdxPrintStyleManager; AComponent: TComponent): string;

function dxPageSetupDialog(var APageSetupDlgData: TdxPageSetupDlgData): Boolean;
implementation

{$R *.DFM}

uses
  TypInfo, CommCtrl, Consts, dxPSImgs, dxPSEngn, dxPSEvnt, dxfmMnPg, dxfmDfnStl,
  dxPSUtl, dxPSRes, dxPSAutoHFTextMnuBld, dxPSfmAutoHFTextFmt, dxBase,
  dxPSHFLibrary, Math, cxFormats;

const
  FDefaultAutoHFTextEntries: TStrings = nil;
  FStyleClassList: TdxPersistentClassList = nil;
  FDateFormats: TStrings = nil;
  FPageNumberFormats: TStrings = nil;
  FTimeFormats: TStrings = nil;

  sdxAutoHFTextEntries = '\AutoHFTextEntries';                    // Don't localize
  sdxCantCreateUniqueName = 'Can''t create unique name for %s.';  // Don't localize
  sdxStyleNameTemplate = 'Style%d';                               // Don't localize

type
  TdxPreviewAccess = class(TdxPreview);
  TdxPrinterPageAccess = class(TdxPrinterPage);

function dxPageSetupDialog(var APageSetupDlgData: TdxPageSetupDlgData): Boolean;
var
  Dialog: TdxfmPageSetupDialog;
begin
  Result := False;
  if APageSetupDlgData.PrintStyle = nil then Exit;

  Dialog := TdxfmPageSetupDialog.Create(nil);
  try
    Dialog.SetupDialog(APageSetupDlgData);
    Result := Dialog.Execute;
    if Result then
    begin
      if Dialog.Modified then
        APageSetupDlgData.PrintStyle.Assign(Dialog.FPrintStyle);
      if Dialog.PreviewBtnClicked or Dialog.PrintBtnClicked then
        APageSetupDlgData.PrintStyle.IsCurrentStyle := True;
    end;
    APageSetupDlgData.ActivePageIndex := Dialog.pgctrlMain.ItemIndex;
    APageSetupDlgData.PreviewBtnClicked := Dialog.PreviewBtnClicked;
    APageSetupDlgData.PrintBtnClicked := Dialog.PrintBtnClicked;
  finally
    Dialog.Free;
  end;
end;

function dxGetPreviewPaperOrientation(APrinterOrientation: TdxPrinterOrientation): TdxPreviewPaperOrientation;
begin
  Result := TdxPreviewPaperOrientation(Byte(APrinterOrientation) mod 2);
end;

{ PrintStyle Registration Routines }

procedure dxPSRegisterPrintStyle(AStyleClass: TdxPrintStyleClass;
  AMakeAsDefault: Boolean = False);
begin
  if FStyleClassList = nil then
    FStyleClassList := TdxPersistentClassList.Create;
  FStyleClassList.Register(AStyleClass);
  if AMakeAsDefault then
    dxDefaultPrintStyleClass := AStyleClass;
end;

procedure dxPSUnregisterPrintStyle(AStyleClass: TdxPrintStyleClass);
begin
  if FStyleClassList <> nil then
    FStyleClassList.Unregister(AStyleClass);
end;

procedure dxPSUnregisterAllPrintStyles;
begin
  FreeAndNil(FStyleClassList);
end;

procedure dxPSGetRegisteredPrintStylesList(AStrings: TStrings);
var
  I: Integer;
  StyleClass: TPersistentClass;
begin
  if FStyleClassList <> nil then
  begin
    AStrings.BeginUpdate;
    try
      for I := 0 to FStyleClassList.Count - 1 do
      begin
        StyleClass := FStyleClassList[I];
        AStrings.AddObject(StyleClass.ClassName, TObject(StyleClass));
      end;
    finally
      AStrings.EndUpdate;
    end;
  end;
end;

{ utility routines }

function MarginsMessageDlg(const Message: string): TModalResult;
var
  Form: TForm;
  B: TComponent;
begin
  Form := CreateMessageDialog(Message, mtWarning, mbYesNoCancel);
  try
    B := Form.FindComponent('Yes');
    if B is TButton then
      with TButton(B) do
      begin
        Form.Width := Form.Width + 3 * Width div 2;
        Width := 3 * Width div 2;
        Caption := cxGetResourceString(@sdxBtnFix);
      end;

    B := Form.FindComponent('No');
    if B is TButton then
      with TButton(B) do
      begin
        Left := Left + Width div 2;
        Width := 3 * Width div 2;
        Caption := cxGetResourceString(@sdxBtnRestoreOriginal);
      end;

    B := Form.FindComponent('Cancel');
    if B is TButton then
      with TButton(B) do
      begin
        Left := Left + Width;
        Width := 3 * Width div 2;
        Caption := cxGetResourceString(@sdxBtnClose);
      end;
    Result := Form.ShowModal;
  finally
    Form.Free;
  end;
end;

function MarginsOutsideMessageDlg(const Message: string): TModalResult;
var
  Form: TForm;
  B: TComponent;
begin
  Form := CreateMessageDialog(Message, mtWarning, [mbYes, mbIgnore]);
  try
    B := Form.FindComponent('Yes');
    if B is TButton then
      TButton(B).Caption := cxGetResourceString(@sdxBtnFix);
    Result := Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure dxPSDrawStyleItem(AStyle: TBasedxPrintStyle; AListBox: TListBox; Index: Integer;
  State: TOwnerDrawState; ABounds: TRect; AMultiline, ABoldedCurrent: Boolean; AScaleFactor: TdxScaleFactor);

  function GetColor: TColor;
  begin
    if odSelected in State then
      Result := clHighlight
    else
      Result := clWindow;
  end;

const
  uFormat: array[Boolean] of UINT = (DT_LEFT or DT_WORDBREAK, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
var
  AImageRect: TRect;
  AImages: TCustomImageList;
  R: TRect;
begin
  if AStyle.StyleManager <> nil then
    AImages := AStyle.StyleManager.Images
  else
    AImages := nil;

  AListBox.Canvas.Brush.Color := GetColor;
  AListBox.Canvas.FillRect(ABounds);

  AImageRect := cxRectSetWidth(ABounds, cxRectHeight(ABounds));
  AImageRect := cxRectCenter(AImageRect, dxGetImageSize(AStyle.StyleGlyph, AImages, AStyle.ImageIndex, AScaleFactor));

  cxDrawImage(AListBox.Canvas.Handle, AImageRect, AImageRect, AStyle.StyleGlyph, AImages, AStyle.ImageIndex, idmNormal, True);

  ABounds.Left := AImageRect.Right + AScaleFactor.Apply(4);

  AListBox.Canvas.Brush.Style := bsClear;
  if ABoldedCurrent and AStyle.IsCurrentStyle then
    AListBox.Canvas.Font.Style := AListBox.Canvas.Font.Style + [fsBold];

  if AMultiline then
  begin
    R := ABounds;
    cxDrawText(AListBox.Canvas.Handle, AListBox.Items[Index], R, DT_LEFT or DT_SINGLELINE or DT_CALCRECT);
    cxDrawText(AListBox.Canvas, AListBox.Items[Index], ABounds, uFormat[cxRectWidth(R) <= cxRectWidth(ABounds)]);
  end
  else
    cxDrawText(AListBox.Canvas, AListBox.Items[Index], ABounds, uFormat[True]);

  if ABoldedCurrent and AStyle.IsCurrentStyle then
    AListBox.Canvas.Font.Style := AListBox.Canvas.Font.Style - [fsBold];
  AListBox.Canvas.Brush.Style := bsSolid;
end;

procedure dxPSDefaultDrawPagePreview(APrintStyle: TBasedxPrintStyle; ACanvas: TCanvas;
  APageBounds, AContentBounds, AHeaderBounds, AFooterBounds: TRect; AScaleFactor: TdxScaleFactor);
begin
  dxDrawDefaultPagePreview(ACanvas, AContentBounds,
    APrintStyle.PrinterPage.CenterOnPageH, APrintStyle.PrinterPage.CenterOnPageV, AScaleFactor);
end;

function dxPSPrintStyleUniqueName(AStyleController: TdxPrintStyleManager; AComponent: TComponent): string;
var
  S: string;
  I, J: Integer;
  NameExists: Boolean;
  Item: TBasedxPrintStyle;
begin
  S := AStyleController.Name + sdxStyleNameTemplate;
  for I := 1 to High(Integer) do
  begin
    Result := Format(S, [I]);
    NameExists := False;
    Item := AStyleController.StyleByName(Result);
    if Item = nil then
    begin
      with AStyleController.Owner do
        for J := 0 to ComponentCount - 1 do
          if dxSameText(Components[J].Name, Result) then
          begin
            NameExists := True;
            Break;
          end;
      if not NameExists then Exit;
    end;
  end;
  if AStyleController.IsDesigning then
    raise EdxException.CreateFmt(sdxCantCreateUniqueName, [AComponent.ClassName])
  else
    Result := '';
end;

{ TdxPageSetupDialog }

constructor TdxPageSetupDialog.Create(AOwner: TComponent);
begin
  inherited;
  FButtonsEnabled := psbDefault;
  FOptionsEnabled := psoDefaultOptionsEnabled;
  FHFMode := hfmThreeSections;
  FButtonsVisible := psbDefault;
  FOptionsVisible := psoDefaultOptionsVisible;
end;

procedure TdxPageSetupDialog.Assign(Source: TPersistent);
begin
  if Source is TdxPageSetupDialog then
    with TdxPageSetupDialog(Source) do
    begin
      Self.ActivePageIndex := ActivePageIndex;
      Self.ButtonsEnabled := ButtonsEnabled;
      Self.OptionsEnabled := OptionsEnabled;
      Self.HFMode := HFMode;
      Self.Title := Title;
      Self.ButtonsVisible := ButtonsVisible;
      Self.OptionsVisible := OptionsVisible;

      Self.FIsTitleAssigned := FIsTitleAssigned;
    end
  else
    inherited;
end;

function TdxPageSetupDialog.DefaultTitle: string;
begin
  Result := cxGetResourceString(@sdxPageSetupCaption);
end;

procedure TdxPageSetupDialog.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('IsTitleAssigned', ReadIsTitleAssigned, WriteIsTitleAssigned,
    FIsTitleAssigned and (Title = ''));
end;

procedure TdxPageSetupDialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = PrintStyle) and (Operation = opRemove) then
    PrintStyle := nil;
end;

function TdxPageSetupDialog.Execute: Boolean;
var
  APageSetupDlgData: TdxPageSetupDlgData;
  APageSetupDlgEvents: TdxPageSetupDlgEvents;
begin
  if PrintStyle = nil then
  begin
    Result := False;
    Exit;
  end;
  FillChar(APageSetupDlgData, SizeOf(TdxPageSetupDlgData), 0);
  FillChar(APageSetupDlgEvents, SizeOf(TdxPageSetupDlgEvents), 0);
  with APageSetupDlgEvents do
  begin
    OnClose := Self.OnClose;
    OnCustomDrawPreview := Self.OnCustomDrawPreview;
    OnShow := Self.OnShow;
  end;
  APageSetupDlgData.Events := APageSetupDlgEvents;
  APageSetupDlgData.PrintStyle := Self.PrintStyle;
  APageSetupDlgData.HelpContext := Self.HelpContext;
  APageSetupDlgData.Title := Self.RealTitle;
  APageSetupDlgData.ActivePageIndex := ActivePageIndex;

  APageSetupDlgData.ButtonsEnabled := ButtonsEnabled;
  APageSetupDlgData.ButtonsVisible := ButtonsVisible;
  APageSetupDlgData.OptionsEnabled := OptionsEnabled;
  APageSetupDlgData.OptionsVisible := OptionsVisible;
  APageSetupDlgData.HFMode := HFMode;

  Result := dxPageSetupDialog(APageSetupDlgData);
  FPreviewBtnClicked := APageSetupDlgData.PreviewBtnClicked;
  FPrintBtnClicked := APageSetupDlgData.PrintBtnClicked;
end;

function TdxPageSetupDialog.RealTitle: string;
begin
  Result := Title;
  if PrintStyle.StyleManager <> nil then
  begin
    if PrintStyle <> nil then
      Result := Result + ': ' + PrintStyle.StyleCaption;
    if Result[Length(Result) - 1] = ':' then
      Delete(Result, Length(Result) - 1, 1);
  end;
end;

procedure TdxPageSetupDialog.RestoreDefaults;
begin
  FIsTitleAssigned := False;
  ActivePageIndex := 0;
  FButtonsEnabled := psbDefault;
  FOptionsEnabled := psoDefaultOptionsEnabled;
  FHFMode := hfmThreeSections;
  FButtonsVisible := psbDefault;
  FOptionsVisible := psoDefaultOptionsVisible;
end;

function TdxPageSetupDialog.GetTitle: string;
begin
  if FIsTitleAssigned then
    Result := FTitle
  else
    Result := DefaultTitle;
end;

function TdxPageSetupDialog.IsTitleStored: Boolean;
begin
  Result := FisTitleAssigned and (Title <> DefaultTitle);
end;

procedure TdxPageSetupDialog.SetPrintStyle(Value: TBasedxPrintStyle);
begin
  if FPrintStyle <> Value then
  begin
    FPrintStyle := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TdxPageSetupDialog.SetTitle(const Value: string);
begin
  if Title <> Value then
  begin
    FTitle := Value;
    FIsTitleAssigned := True;
  end;
end;

procedure TdxPageSetupDialog.ReadIsTitleAssigned(Reader: TReader);
begin
  FIsTitleAssigned := Reader.ReadBoolean;
end;

procedure TdxPageSetupDialog.WriteIsTitleAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsTitleAssigned);
end;

{  TdxfmPageSetupDialog }

constructor TdxfmPageSetupDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBmpArrow := dxPSUtl.CreateArrowBitmap(udgDown);
  FFooterBkGndGlyph := TBitmap.Create;
  FHeaderBkGndGlyph := TBitmap.Create;

  FHFFunctionList := TStringList.Create;
  dxGetHFFunctionsList(FHFFunctionList);

  FPredefinedImages := TcxImageList.Create(Self);
  FPredefinedImages.AllocBy := FHFFunctionList.Count;

  FHFToolBarHelper := TdxPSHFToolBarHelper.Create(tbPredefined);
  FHFToolBarHelper.OnAutoTextClick := AutoHFTextEntriesClick;
  FHFToolBarHelper.OnInsertMacros := DoInsertMacrosValue;
  FHFToolBarHelper.ButtonsImageList := FPredefinedImages;

  HelpContext := dxPSGlbl.dxhcPageSetupDlg;

  FPreviewBtnClicked := False;
  FPrintBtnClicked := False;

  CreateControls;
  LoadStrings;
end;

destructor TdxfmPageSetupDialog.Destroy;
begin
  FreeAndNil(FHFToolBarHelper);
  FreeAndNil(FHFFunctionList);
  FreeAndNil(FFooterBkGndGlyph);
  FreeAndNil(FHeaderBkGndGlyph);
  FreeAndNil(FBmpArrow);
  FreeAndNil(FPrintStyle);
  inherited;
end;

function TdxfmPageSetupDialog.Execute: Boolean;
begin
  Result := False;
  if PrintStyle = nil then Exit;
  StartSetting;
  Result := ShowModal = mrOk;// and FModified;
end;

procedure TdxfmPageSetupDialog.SetPrintStyle(Value: TBasedxPrintStyle);
begin
  if FPrintStyle <> nil then
  begin
    FPrintStyle.Free;
    FPrintStyle := nil;
  end;
  if Value <> nil then
  begin
    FSavePrintStyle := Value;
    FPrintStyle := Value.StyleClass.Create(nil);
    FPrintStyle.Assign(Value);
//    MarginsOutside := not ValidateMarginsOutside(nil);
  end;
end;

procedure TdxfmPageSetupDialog.CreateWnd;
begin
  inherited;
  if Icon.Handle = 0 then
    dxLoadIconFromResource(Icon, IDB_DXPSPAGESETUP);
  SendMessage(Handle, WM_SETICON, 1, Icon.Handle);
end;

procedure TdxfmPageSetupDialog.DoHide;
begin
  if Assigned(FOnClose) then FOnClose(Self);
  inherited;
end;

procedure TdxfmPageSetupDialog.DoShow;
begin
  inherited;
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TdxfmPageSetupDialog.ScaleFactorChanged(M: Integer; D: Integer);
begin
  inherited ScaleFactorChanged(M, D);

  if Page <> nil then
  begin
    ChangeBkgndGlyph(btnHeaderBackground);
    ChangeBkgndGlyph(btnFooterBackGround);
  end;
end;

function TdxfmPageSetupDialog.GetUnitsDenominator: Integer;
begin
  case Page.GetInnerMeasurementUnits of
    muInches:
      Result := 254;
    muMillimeters:
      Result := 10;
    else
      Result := 10;
  end;
end;

function TdxfmPageSetupDialog.GetSelectedTitlePartTextAlignY: TcxTextAlignY;
begin
  Result := SelectedTitle.TextAlignY[SelectedTitlePart];
end;

procedure TdxfmPageSetupDialog.SetSelectedTitlePartTextAlignY(AAlign: TcxTextAlignY);
begin
  SelectedTitle.TextAlignY[SelectedTitlePart] := AAlign;
end;

procedure TdxfmPageSetupDialog.SetupDialog(const APageSetupDlgData: TdxPageSetupDlgData);

  function GetLegendText: string;
  begin
    case Page.GetInnerMeasurementUnits of
      muInches:
        Result := cxGetResourceString(@sdxUnitsInches);
      muMillimeters:
        Result := cxGetResourceString(@sdxUnitsMillimeters);
      else
        Result := '';
    end;
  end;

  procedure SetLegendText(AEdit: TcxSpinEdit; const ALegendText: string);
  const
    DisplayFormatMap: array[TcxSpinEditValueType] of string = ('0 ', '0.00 ');
  begin
    AEdit.Properties.DisplayFormat :=
      DisplayFormatMap[AEdit.Properties.ValueType] + ALegendText;
  end;

  procedure SetupPrintPageSizes(Denominator: Integer);
  begin
    if Page.Orientation = poLandscape then
    begin
      sePaperWidth.Properties.MinValue := dxPrintDevice.MinExtentY / Denominator;
      sePaperWidth.Properties.MaxValue := dxPrintDevice.MaxExtentY / Denominator;
      sePaperHeight.Properties.MinValue := dxPrintDevice.MinExtentX / Denominator;
      sePaperHeight.Properties.MaxValue := dxPrintDevice.MaxExtentX / Denominator;
    end
    else
    begin
      sePaperWidth.Properties.MinValue := dxPrintDevice.MinExtentX / Denominator;
      sePaperWidth.Properties.MaxValue := dxPrintDevice.MaxExtentX / Denominator;
      sePaperHeight.Properties.MinValue := dxPrintDevice.MinExtentY / Denominator;
      sePaperHeight.Properties.MaxValue := dxPrintDevice.MaxExtentY / Denominator;
    end;

    sePaperWidth.Properties.ReadOnly := not PrintStyle.AllowCustomPaperSizes;
    sePaperHeight.Properties.ReadOnly := not PrintStyle.AllowCustomPaperSizes;
    sePaperWidth.TabStop := not sePaperWidth.Properties.ReadOnly;
    sePaperHeight.TabStop := not sePaperHeight.Properties.ReadOnly;
  end;

  procedure SetupScalingPage;
  begin
    if tshScaling.Visible then
    begin
      rbtnAdjustTo.Enabled := tshScaling.Enabled;
      rbtnFitTo.Enabled := tshScaling.Enabled;
      rbtnFitTo.Enabled := tshScaling.Enabled;
      lblPercentOfNormalSize.Enabled := tshScaling.Enabled;
      seAdjustTo.Enabled := tshScaling.Enabled;
      sePagesWide.Enabled := tshScaling.Enabled;
      sePagesTall.Enabled := tshScaling.Enabled;

      rbtnAdjustTo.Checked := PrintStyle.PrinterPage.ScaleMode = smAdjust;
      rbtnFitTo.Checked := PrintStyle.PrinterPage.ScaleMode = smFit;
      seAdjustTo.Value := PrintStyle.PrinterPage.ScaleFactor;
      sePagesWide.Value := PrintStyle.PrinterPage.FitToPagesHorizontally;
      sePagesTall.Value := PrintStyle.PrinterPage.FitToPagesVertically;
    end;
  end;

  procedure SetupHeaderFooterPage;
  begin
    if libtnHeaderFont.Visible then
      btnHeaderFont.Enabled := (psoHFFont in APageSetupDlgData.OptionsEnabled);
    if libtnHeaderBackground.Visible then
      btnHeaderBackground.Enabled := (psoHFBackground in APageSetupDlgData.OptionsEnabled);

    if pnlHeaderMemos.Visible then
    begin
      pnlHeaderMemos.Enabled := PrintStyle.AllowChangeHFText and (psoHFText in APageSetupDlgData.OptionsEnabled);
      memHeaderLeft.Properties.ReadOnly := not pnlHeaderMemos.Enabled;
      memHeaderLeft.TabStop := not memHeaderLeft.Properties.ReadOnly;
      memHeaderCenter.Properties.ReadOnly := not pnlHeaderMemos.Enabled;
      memHeaderCenter.TabStop := not memHeaderCenter.Properties.ReadOnly;
      memHeaderRight.Properties.ReadOnly := not pnlHeaderMemos.Enabled;
      memHeaderRight.TabStop := not memHeaderRight.Properties.ReadOnly;
    end;

    if libtnFooterFont.Visible then
      btnFooterFont.Enabled := (psoHFFont in APageSetupDlgData.OptionsEnabled);
    if libtnFooterBackground.Visible then
      btnFooterBackground.Enabled := (psoHFBackground in APageSetupDlgData.OptionsEnabled);
    if pnlFooterMemos.Visible then
    begin
      pnlFooterMemos.Enabled := PrintStyle.AllowChangeHFText and (psoHFText in APageSetupDlgData.OptionsEnabled);
      memFooterLeft.Properties.ReadOnly := not pnlFooterMemos.Enabled;
      memFooterLeft.TabStop := not memFooterLeft.Properties.ReadOnly;
      memFooterCenter.Properties.ReadOnly := not pnlFooterMemos.Enabled;
      memFooterCenter.TabStop := not memFooterCenter.Properties.ReadOnly;
      memFooterRight.Properties.ReadOnly := not pnlFooterMemos.Enabled;
      memFooterRight.TabStop := not memFooterRight.Properties.ReadOnly;
    end;
  end;

  procedure SetupHeaderFooterPageToolbars;
  var
    AutoHFTextEntries: TStrings;
  begin
    if litbPredefined.Visible then
    begin
      tbPredefined.Enabled := psoHFFunctions in APageSetupDlgData.OptionsEnabled;
      if FStyleManager = nil then
        AutoHFTextEntries := nil
      else
        AutoHFTextEntries := FStyleManager.AutoHFTextEntries;

      FHFToolBarHelper.Build(FHFFunctionList, AutoHFTextEntries, PrintStyle.PrinterPage.ImageCollection,
        Assigned(FStyleManager) and (psoHFAutoText in APageSetupDlgData.OptionsVisible));
      SetControlLookAndFeel(tbPredefined, dxPSEngine.DialogsLookAndFeel);
    end;

    if gbxVertAlignment.Visible then
      gbxVertAlignment.Enabled := psoHFVertAlignment in APageSetupDlgData.OptionsEnabled;

    if pnlReverse.Visible then
      chbxReverseOnEvenPages.Enabled := psoHFReverse in APageSetupDlgData.OptionsEnabled;
  end;

  procedure UpdateActiveTabSheet;
  var
    AIndex: Integer;
  begin
    AIndex := Min(Max(APageSetupDlgData.ActivePageIndex, 0), pgctrlMain.Count - 1);
    if not pgctrlMain.Items[AIndex].Visible then
      AIndex := 0;
    pgctrlMain.ItemIndex := AIndex;
  end;

  procedure UpdateControlEnableState(AControl: TdxLayoutGroup; AEnabled: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to AControl.Count - 1 do
      AControl.Items[I].Enabled := AEnabled;
  end;

  procedure UpdateControlVisibility;
  begin
    lichbxCenterHorz.Visible := psoCenterOnPage in APageSetupDlgData.OptionsVisible;
    lichbxCenterVert.Visible := psoCenterOnPage in APageSetupDlgData.OptionsVisible;
    lilblCenterOnPage.Visible := psoCenterOnPage in APageSetupDlgData.OptionsVisible;
    tshMargins.Visible := gbxMargins.Visible or lblCenterOnPage.Visible;

    libtnHeaderFont.Visible := psoHFFont in APageSetupDlgData.OptionsVisible;
    liedHeaderFontInfo.Visible := psoHFFont in APageSetupDlgData.OptionsVisible;
    libtnHeaderBackground.Visible := psoHFBackground in APageSetupDlgData.OptionsVisible;
    pnlHeaderFont.Visible := libtnHeaderFont.Visible or libtnHeaderBackGround.Visible;
    pnlHeaderMemos.Visible := (psoHFText in APageSetupDlgData.OptionsVisible);
    if pnlHeaderMemos.Visible and (APageSetupDlgData.HFMode = hfmOneSection) then
    begin
      limemHeaderCenter.Visible := False;
      limemHeaderRight.Visible := False;
    end;
    pnlHeader.Visible := pnlHeaderFont.Visible or pnlHeaderMemos.Visible;

    libtnFooterFont.Visible := (psoHFFont in APageSetupDlgData.OptionsVisible);
    liedFooterFontInfo.Visible := (psoHFFont in APageSetupDlgData.OptionsVisible);
    libtnFooterBackground.Visible := (psoHFBackground in APageSetupDlgData.OptionsVisible);

    pnlFooterFont.Visible := libtnHeaderFont.Visible or libtnHeaderBackGround.Visible;
    pnlFooterMemos.Visible := (psoHFText in APageSetupDlgData.OptionsVisible);

    if pnlFooterMemos.Visible and (APageSetupDlgData.HFMode = hfmOneSection) then
    begin
      limemFooterCenter.Visible := False;
      limemFooterRight.Visible := False;
    end;
    pnlFooter.Visible := pnlFooterFont.Visible or pnlFooterMemos.Visible;

    litbPredefined.Visible := (psoHFFunctions in APageSetupDlgData.OptionsVisible);
    gbxFunctions.Visible := litbPredefined.Visible;
    gbxVertAlignment.Visible := (psoHFVertAlignment in APageSetupDlgData.OptionsVisible);
    pnlBottom.Visible := gbxFunctions.Visible or gbxVertAlignment.Visible;
    pnlReverse.Visible := (psoHFReverse in APageSetupDlgData.OptionsVisible);

    tshHeaderFooter.Visible := pnlHeader.Visible or pnlFooter.Visible or pnlBottom.Visible or pnlReverse.Visible;
    tshScaling.Visible := PrintStyle.AllowChangeScale;

    if pnlStyleName.Visible then
    begin
      pnlStyleName.Enabled := not FSavePrintStyle.BuiltIn or IsDesignTime;
      edStyleName.Properties.ReadOnly := not pnlStyleName.Enabled;
      edStyleName.TabStop := not edStyleName.Properties.ReadOnly;
      edStyleName.Properties.MaxLength := dxMaxStyleCaption;
    end;
    FCanShowStyleOptionsDlg := pnlStyleName.Enabled and (psbStyleOptions in APageSetupDlgData.ButtonsEnabled);

    lbxPaperType.Enabled := PrintStyle.AllowChangePaper;
    rBtnPortrait.Enabled := PrintStyle.AllowChangeOrientation;
    rBtnLandscape.Enabled := PrintStyle.AllowChangeOrientation;
    rBtnAutoOrientation.Enabled := PrintStyle.AllowChangeOrientation;

    if gbxPrintOrder.Visible then
    begin
      gbxPrintOrder.Enabled := (psoPageOrder in APageSetupDlgData.OptionsEnabled);
      rbtnDownThenOver.Enabled := gbxPrintOrder.Enabled;
      rbtnOverThenDown.Enabled := gbxPrintOrder.Enabled;
    end;

    if gbxShading.Visible then
    begin
      gbxShading.Enabled := (psoShading in APageSetupDlgData.OptionsEnabled);
      chbxShading.Enabled := gbxShading.Enabled;
    end;

    if lblCenterOnPage.Visible then
    begin
      chbxCenterHorz.Enabled := psoCenterOnPage in APageSetupDlgData.OptionsEnabled;
      chbxCenterVert.Enabled := psoCenterOnPage in APageSetupDlgData.OptionsEnabled;
      lblCenterOnPage.Enabled := psoCenterOnPage in APageSetupDlgData.OptionsEnabled;
    end;
  end;

  procedure SetupMargins;
  begin
    gbxMargins.Visible := psoMargins in APageSetupDlgData.OptionsVisible;
    if psoMargins in APageSetupDlgData.OptionsVisible then
      FPreview.OptionsView := FPreview.OptionsView + [povMargins]
    else
      FPreview.OptionsView := FPreview.OptionsView - [povMargins];

    pnlHFMargins.Visible := gbxMargins.Visible and (psoHFMargins in APageSetupDlgData.OptionsVisible);
    if povMargins in FPreview.OptionsView then
    begin
      FPreview.Margins.Header.Visible := pnlHFMargins.Visible;
      FPreview.Margins.Footer.Visible := pnlHFMargins.Visible;
    end;

    if gbxMargins.Visible then
      UpdateControlEnableState(gbxMargins, PrintStyle.AllowChangeMargins and (psoMargins in APageSetupDlgData.OptionsEnabled));

    if gbxMargins.Enabled then
      FPreview.OptionsBehavior := FPreview.OptionsBehavior + [pobAllowDragMargins]
    else
      FPreview.OptionsBehavior := FPreview.OptionsBehavior - [pobAllowDragMargins];

    if pnlHFMargins.Visible then
      UpdateControlEnableState(pnlHFMargins, gbxMargins.Enabled and (psoHFMargins in APageSetupDlgData.OptionsEnabled));
    FPreview.Margins.Header.Enabled := pnlHFMargins.Enabled;
    FPreview.Margins.Footer.Enabled := pnlHFMargins.Enabled;
  end;

  procedure UpdateLegendText(const ALegendText: string);
  begin
    SetLegendText(sePaperWidth, ALegendText);
    SetLegendText(sePaperHeight, ALegendText);
    SetLegendText(seMarginTop, ALegendText);
    SetLegendText(seMarginLeft, ALegendText);
    SetLegendText(seMarginRight, ALegendText);
    SetLegendText(seMarginBottom, ALegendText);
    SetLegendText(seMarginHeader, ALegendText);
    SetLegendText(seMarginFooter, ALegendText);
  end;

begin
  FControlsUpdating := True;
  try
    SetPrintStyle(APageSetupDlgData.PrintStyle);
    FStyleManager := FSavePrintStyle.StyleManager;

    Caption := APageSetupDlgData.Title;
    if APageSetupDlgData.HelpContext <> 0 then
      HelpContext := APageSetupDlgData.HelpContext;

    FOnShow := APageSetupDlgData.Events.OnShow;
    FOnClose := APageSetupDlgData.Events.OnClose;
    OnCustomDrawPreview := APageSetupDlgData.Events.OnCustomDrawPreview;

    btnPrintPreview.Enabled := psbPreview in APageSetupDlgData.ButtonsEnabled;
    btnPrint.Enabled := psbPrint in APageSetupDlgData.ButtonsEnabled;
    libtnOptions.Visible := psbStyleOptions in APageSetupDlgData.ButtonsVisible;
    libtnPrintPreview.Visible := psbPreview in APageSetupDlgData.ButtonsVisible;
    libtnPrint.Visible := psbPrint in APageSetupDlgData.ButtonsVisible;
    pnlStyleName.Visible := psoStyleCaption in APageSetupDlgData.OptionsVisible;

    rBtnPortrait.Checked := PrintStyle.PrinterPage.Orientation = poPortrait;
    rBtnLandscape.Checked := PrintStyle.PrinterPage.Orientation = poLandscape;
    rBtnAutoOrientation.Checked := PrintStyle.PrinterPage.Orientation = poAuto;

    gbxPrintOrder.Visible := psoPageOrder in APageSetupDlgData.OptionsVisible;
    gbxShading.Visible := psoShading in APageSetupDlgData.OptionsVisible;

    SetupMargins;
    UpdateControlVisibility;
    SetupHeaderFooterPage;
    SetupHeaderFooterPageToolbars;
    SetupScalingPage;
    SetupPrintPageSizes(UnitsDenominator);
    UpdateLegendText(GetLegendText);
    UpdateActiveTabSheet;
    UpdateControlsState;
  finally
    FControlsUpdating := False;
  end;
  if pssCopy in FSavePrintStyle.State then
    CheckModified;
end;

procedure TdxfmPageSetupDialog.DoInsertMacrosValue(
  Sender: TObject; const AMacrosValue: string);
begin
  if ActiveControl is TcxCustomInnerMemo then
  begin
    TcxCustomInnerMemo(ActiveControl).SelText := AMacrosValue;
    CheckModified;
  end;
end;

procedure TdxfmPageSetupDialog.DrawBackgroundBitmapContent(ACanvas: TcxCanvas; R: TRect; ABackground: TdxBackground);

  procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect);
  var
    Brush: HBRUSH;
    PrevColor: TColor;
    R1: TRect;
    S: string;
  begin
    R1 := R;
    ACanvas.FrameRect(R1, clBtnFace);
    InflateRect(R1, -1, -1);

    case ABackground.Mode of
      bmNone:
        begin
          S := '[' + DropAmpersand(cxGetResourceString(@sdxBtnNoFill)) + ']';
          ACanvas.FillRect(R1, clBtnFace);
          ACanvas.Brush.Style := bsClear;
          ACanvas.Font.Color := clHighlight;
          ACanvas.DrawTexT(S, R1, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;

      bmBrush:
        begin
          ACanvas.FrameRect(R1, clBtnShadow);
          InflateRect(R1, -1, -1);
          PrevColor := SetBkColor(ACanvas.Handle, ColorToRGB(ABackground.BkColor));
          Brush := CreateSolidBrush(ColorToRGB(ABackground.Brush.Color));
          FillRect(ACanvas.Handle, R1, Brush);
          DeleteObject(Brush);
          SetBkColor(ACanvas.Handle, PrevColor);
        end;

      bmBrushBitmap:
        begin
          ACanvas.FrameRect(R1, clBtnShadow);
          InflateRect(R1, -1, -1);
          Brush := CreatePatternBrush(TBitmap(ABackground.Picture).Handle);
          FillRect(ACanvas.Handle, R1, Brush);
          DeleteObject(Brush);
        end;

      bmPicture:
        begin
          S := '[' + DropAmpersand(cxGetResourceString(@sdxPicture)) + ']';
          ACanvas.FillRect(R1, clBtnFace);
          ACanvas.Brush.Style := bsClear;
          ACanvas.Font.Color := clHighlight;
          ACanvas.DrawTexT(S, R1, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
    end;
  end;

var
  ARect: TRect;
begin
  ACanvas.SaveState;
  try
    ACanvas.Font := Font;
    ACanvas.FillRect(R, clBtnFace);

    ARect := cxRectSetWidth(R, cxRectWidth(R) - ScaleFactor.Apply(FBmpArrow.Width));
    InflateRect(ARect, -ScaleFactor.Apply(4), 0);
    DrawBackground(ACanvas, ARect);

    ARect := cxRectSetRight(R, R.Right, ScaleFactor.Apply(FBmpArrow.Width));
    ARect := cxRectCenterVertically(ARect, ScaleFactor.Apply(FBmpArrow.Height));
    dxPSEngine.DialogsLookAndFeel.Painter.DrawScaledArrow(ACanvas, ARect, cxbsNormal, adDown, ScaleFactor, False);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxfmPageSetupDialog.ChangeBkgndGlyph(AButton: TcxButton);
var
  ABackground: TdxBackground;
  ACanvas: TcxCanvas;
  AGlyph: TBitmap;
begin
  if AButton = btnHeaderBackGround then
  begin
    ABackground := Page.PageHeader.Background;
    AGlyph := FHeaderBkGndGlyph;
  end
  else
  begin
    ABackground := Page.PageFooter.Background;
    AGlyph := FFooterBkGndGlyph;
  end;

  AGlyph.SetSize(ScaleFactor.Apply(75), AButton.Height - ScaleFactor.Apply(6)); { 75 = TextArea width + DownArrow width }

  ACanvas := TcxCanvas.Create(AGlyph.Canvas);
  try
    DrawBackgroundBitmapContent(ACanvas, Rect(0, 0, AGlyph.Width, AGlyph.Height), ABackground);
  finally
    ACanvas.Free;
  end;

  AButton.Glyph.Assign(AGlyph);
  AButton.Glyph.SourceDPI := ScaleFactor.Apply(96);
end;

procedure TdxfmPageSetupDialog.CreateControls;

  function CreatePreview(AHolder: TdxLayoutItem): TdxPreview;
  begin
    Result := TdxPreview.Create(Self);
    TdxPreviewAccess(Result).BorderStyle := cxcbsNone;
    Result.OptionsView := Result.OptionsView - [povAutoHideScrollBars];
    Result.Color := clBtnFace;
    Result.ScrollBars := ssNone;
    Result.ZoomMode := pzmPages;
    Result.Transparent := True;
    AHolder.Control := Result;
    AHolder.ControlOptions.OriginalHeight := 10;
  end;

begin
  FOrientationPreview := CreatePreview(bvlOrientationHolder);
  FOrientationPreview.PageXCount := 1;
  FOrientationPreview.OptionsHint := FOrientationPreview.OptionsHint - [pohShowForMargins, pohShowOnDrag];
  FOrientationPreview.OptionsView := FOrientationPreview.OptionsView - [povPageSelection, povMargins];
  FOrientationPreview.OptionsZoom := FOrientationPreview.OptionsZoom - [pozZoomOnClick];
  TdxPreviewAccess(FOrientationPreview).PageSize.MinUsefulSize := cxNullPoint;
  TdxPreviewAccess(FOrientationPreview).OnCalcPageCount := OrientationPreviewCalcPageCount;

  FPreview := CreatePreview(bvlPreviewHolder);
  FPreview.OptionsZoom := FPreview.OptionsZoom - [pozZoomOnClick];
  FPreview.OptionsView := FPreview.OptionsView - [povPageSelection];
  FPreview.OnCalcPageCount := PreviewCalcPageCount;
  FPreview.OnDrawPageContent := PreviewDrawPageContent;
  FPreview.OnAfterDragMargin := PreviewAfterDragMargin;

  FwpMargins := TdxPSWarningPane.Create(Self);
  bvlMarginsWarningHolder.Control := FwpMargins;
end;

procedure TdxfmPageSetupDialog.LoadStrings;
begin
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  btnPrint.Caption := cxGetResourceString(@sdxBtnPrint);
  btnPrintPreview.Caption := cxGetResourceString(@sdxBtnPrintPreview);
  btnOptions.Caption := cxGetResourceString(@sdxBtnOptions);

  lblStyleName.Caption := cxGetResourceString(@sdxStyleName);

  tshPage.Caption := cxGetResourceString(@sdxPage);
  tshMargins.Caption := cxGetResourceString(@sdxMargins);
  tshHeaderFooter.Caption := cxGetResourceString(@sdxHeaderFooter);
  tshScaling.Caption := cxGetResourceString(@sdxScaling);

  gbxPaper.Caption := cxGetResourceString(@sdxPaper);
  lblPaperWidth.Caption := cxGetResourceString(@sdxPaperWidth);
  lblPaperHeight.Caption := cxGetResourceString(@sdxPaperHeight);
  lblPaperSource.Caption := cxGetResourceString(@sdxPaperSource);

  gbxOrientation.Caption := cxGetResourceString(@sdxOrientation);
  rBtnPortrait.Caption := cxGetResourceString(@sdxPortrait);
  rBtnLandscape.Caption := cxGetResourceString(@sdxLandscape);
  rBtnAutoOrientation.Caption := cxGetResourceString(@sdxAutoOrientation);
  gbxPrintOrder.Caption := cxGetResourceString(@sdxPrintOrder);
  rbtnDownThenOver.Caption := cxGetResourceString(@sdxDownThenOver);
  rbtnOverThenDown.Caption := cxGetResourceString(@sdxOverThenDown);
  gbxShading.Caption := cxGetResourceString(@sdxShading);
  chbxShading.Caption := cxGetResourceString(@sdxPrintUsingGrayShading);

  lblMarginTop.Caption := cxGetResourceString(@sdxTop);
  lblMarginLeft.Caption := cxGetResourceString(@sdxLeft);
  lblMarginRight.Caption := cxGetResourceString(@sdxRight);
  lblMarginBottom.Caption := cxGetResourceString(@sdxBottom);
  lblMarginHeader.Caption := cxGetResourceString(@sdxHeader2);
  lblMarginFooter.Caption := cxGetResourceString(@sdxFooter2);
  btnFix.Caption := cxGetResourceString(@sdxBtnFix);
  btnRestoreOriginalMargins.Caption := cxGetResourceString(@sdxBtnRestoreOriginal);

  lblCenterOnPage.Caption := cxGetResourceString(@sdxCenterOnPage);
  chbxCenterHorz.Caption := cxGetResourceString(@sdxHorizontally);
  chbxCenterVert.Caption := cxGetResourceString(@sdxVertically);

  pnlHeader.Caption := cxGetResourceString(@sdxHeader);
  btnHeaderFont.Caption := cxGetResourceString(@sdxBtnHeaderFont);
  btnHeaderBackground.Caption := cxGetResourceString(@sdxBtnHeaderBackground);
  pnlFooter.Caption := cxGetResourceString(@sdxFooter);
  btnFooterFont.Caption := cxGetResourceString(@sdxBtnFooterFont);
  btnFooterBackground.Caption := cxGetResourceString(@sdxBtnFooterBackground);

  gbxVertAlignment.Caption := cxGetResourceString(@sdxVertAlignment);
  gbxFunctions.Caption := cxGetResourceString(@sdxPredefinedFunctions);
  chbxReverseOnEvenPages.Caption := cxGetResourceString(@sdxReverseOnEvenPages);

  rbtnAdjustTo.Caption := cxGetResourceString(@sdxAdjustTo);
  rbtnFitTo.Caption := cxGetResourceString(@sdxFitTo);
  lblPercentOfNormalSize.Caption := cxGetResourceString(@sdxPercentOfNormalSize);
  liPagesWide.Caption := cxGetResourceString(@sdxPagesWideBy);
  liPagesTall.Caption := cxGetResourceString(@sdxTall);
end;

procedure TdxfmPageSetupDialog.PaperWidthHeightUpdateInfos(Sender: TObject);
begin
  if not FOrientationChanging then
    UpdatePageInfos;
end;

procedure TdxfmPageSetupDialog.PaperWidthHeightChange(Sender: TObject);
begin
  if not FControlsUpdating then
    CheckModified;
end;

procedure TdxfmPageSetupDialog.UpdatePageInfos;
var
  I: Integer;
begin
  if not FControlsUpdating then
  begin
    Page.RealPageSize := Point(Round(1000 * sePaperWidth.Value), Round(1000 * sePaperHeight.Value));
    TdxPreviewAccess(FPreview).PageSize.Size := Page.PageSizeLoMetric;
    TdxPreviewAccess(FOrientationPreview).PageSize.Size := Page.PageSizeLoMetric;
    UpdateMarginsBounds;
    FPaperSizeLocked := True;
    try
      for I := 0 to PaperInfoCount - 1 do
        if PaperInfos[I].DMPaper = Page.DMPaper then
        begin
          lbxPaperType.ItemIndex := I;
          Break;
        end;
    finally
      FPaperSizeLocked := False;
    end;
  end;
end;

procedure TdxfmPageSetupDialog.AdjustToExit(Sender: TObject);
begin
  if not FControlsUpdating then
  begin
    if rbtnAdjustTo.Checked then
      PrintStyle.PrinterPage.ScaleFactor := seAdjustTo.Value;
  end;
end;

procedure TdxfmPageSetupDialog.ScaleChanged(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  rbtnAdjustTo.Checked := True;
  ActiveControl := TWinControl(Sender);
  CheckModified;
end;

procedure TdxfmPageSetupDialog.FitToPageChange(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  rbtnFitTo.Checked := True;
  ActiveControl := TWinControl(Sender);
  CheckModified;
end;

procedure TdxfmPageSetupDialog.UpdateControlsState;
begin
  FControlsUpdating := True;
  try
    btnPrintPreview.Enabled := True;
    btnPrint.Enabled := True;
    btnOptions.Enabled := FCanShowStyleOptionsDlg and PrintStyle.HasStyleOptionsDlg;
    btnFix.Enabled := MarginsOutside or MarginsInvalid;
    btnRestoreOriginalMargins.Enabled := gbxMargins.Enabled and FMarginsChanged;
  finally
    FControlsUpdating := False;
  end
end;

procedure TdxfmPageSetupDialog.CheckModified;
begin
  FModified := True;
  UpdateControlsState;
end;

function TdxfmPageSetupDialog.GetCurrentPaperInfo: TdxPaperInfo;
begin
  with lbxPaperType do
    if ItemIndex <> -1 then
      Result := PaperInfos[ItemIndex]
    else
      Result := nil;
end;

function TdxfmPageSetupDialog.GetPage: TdxPrinterPage;
begin
  if PrintStyle <> nil then
    Result := PrintStyle.PrinterPage
  else
    Result := nil
end;

function TdxfmPageSetupDialog.GetPaperInfo(Index: Integer): TdxPaperInfo;
begin
  Result := TdxPaperInfo(lbxPaperType.Items.Objects[Index]);
end;

function TdxfmPageSetupDialog.GetPaperInfoCount: Integer;
begin
  Result := lbxPaperType.Items.Count;
end;

function TdxfmPageSetupDialog.GetSelectedTitle: TCustomdxPageObject;
var
  ASelectedMemo: TcxCustomMemo;
begin
  ASelectedMemo := SelectedTitlePartEditor;
  if (ASelectedMemo <> nil) and (ASelectedMemo.Tag >= 3) and (ASelectedMemo.Tag < 6) then
    Result := Page.PageFooter
  else
    Result := Page.PageHeader;
end;

function TdxfmPageSetupDialog.GetSelectedTitlePart: TdxPageTitlePart;
begin
  Result := GetTitlePart(SelectedTitlePartEditor);
end;

function TdxfmPageSetupDialog.GetSelectedTitlePartEditor: TcxCustomMemo;
begin
  if ActiveControl is TcxCustomMemo then
    Result := TcxCustomMemo(ActiveControl)
  else
    if ActiveControl is TcxCustomInnerMemo then
      Result := GetInnerControlContainer(ActiveControl) as TcxCustomMemo
    else
      Result := nil;
end;

function TdxfmPageSetupDialog.GetTitlePart(AComponent: TComponent): TdxPageTitlePart;
var
  ATag: Integer;
begin
  if AComponent = nil then
    raise EdxException.Create('Invalid TitlePart');
  ATag := TTagToInt(AComponent.Tag);
  if ATag >= 3 then
    Dec(ATag, 3);
  Result := dxPSPageTitlePartMap[ATag];
end;

procedure TdxfmPageSetupDialog.StartSetting;

  procedure DeleteCustomPapers;
  var
    I: Integer;
  begin
    // TODO: Check
    for I := lbxPaperType.Items.Count - 1 downto 0 do
      if PaperInfos[I].DMPaper >= DMPAPER_USER then
        lbxPaperType.Items.Delete(I);
  end;

  procedure SetupPapers;
  var
    I: Integer;
  begin
    lbxPaperType.Items.BeginUpdate;
    try
      lbxPaperType.Clear;
      PrintStyle.GetFilteredPapers(lbxPaperType.Items);
      if lbxPaperType.Items.Count > 0 then
      begin
        if not PrintStyle.AllowCustomPaperSizes then
          DeleteCustomPapers;

        for I := 0 to PaperInfoCount - 1 do
          if PrintStyle.PrinterPage.DMPaper = PaperInfos[I].DMPaper then // TODO: FindByDMPaper
          begin
            lbxPaperType.ItemIndex := I;
            Break;
          end;

        if lbxPaperType.ItemIndex = -1 then
          if not PrintStyle.AllowCustomPaperSizes then
            lbxPaperType.ItemIndex := 0
          else
          begin
            I := 0;
            //TdxPaperInfo(lbxPaperType.Items.Objects[I]).DMPaper < DMPAPER_USER
            while (I < lbxPaperType.Items.Count) and (Pos('Custom', lbxPaperType.Items[I]) = 0) do
              Inc(I);
            if (I < lbxPaperType.Items.Count) then
              lbxPaperType.ItemIndex := I
            else
              lbxPaperType.ItemIndex := 0;
          end;
      end;
    finally
      lbxPaperType.Items.EndUpdate;
    end;
    if lbxPaperType.Enabled then
      lbxPaperType.Enabled := lbxPaperType.Items.Count > 0;
  end;

  procedure SetupBins;
  begin
    cbxPaperSource.Properties.Items.BeginUpdate;
    try
      cbxPaperSource.Properties.Items.Clear;
      if dxPrintDevice.Bins <> nil then
        cbxPaperSource.Properties.Items := dxPrintDevice.Bins;
      cbxPaperSource.Enabled := cbxPaperSource.Properties.Items.Count > 0;
      if cbxPaperSource.Enabled then
        cbxPaperSource.ItemIndex := Max(0, cbxPaperSource.Properties.Items.IndexOfObject(TObject(Page.PaperSource)));
    finally
      cbxPaperSource.Properties.Items.EndUpdate;
    end;
  end;

begin
  FControlsUpdating := True;
  try
    SetupPapers;
    if lbxPaperType.Items.Count > 0 then
      lbxPaperTypeClick(lbxPaperType);
    SetupBins;

    ChangeBkgndGlyph(btnHeaderBackground);
    ChangeBkgndGlyph(btnFooterBackGround);
    edStyleName.Text := Copy(FSavePrintStyle.StyleCaption, 1, edStyleName.Properties.MaxLength);

    chbxShading.Checked := PrintStyle.PrinterPage.GrayShading;

    EnabledMemoAttr(False);
    UpdateMarginsEdits;
    FPreview.MeasurementUnits := TdxPreviewMeasurementUnits(Page.MeasurementUnits);
    TdxPreviewAccess(FPreview).PageSize.MinUsefulSize := Point(Page.MinPrintableAreaLoMetric, Page.MinPrintableAreaLoMetric);
    TdxPreviewAccess(FPreview).PageSize.Orientation := dxGetPreviewPaperOrientation(Page.Orientation);
    TdxPreviewAccess(FPreview).PageSize.Header := Page.HeaderLoMetric;
    TdxPreviewAccess(FPreview).PageSize.Footer := Page.FooterLoMetric;
    TdxPreviewAccess(FPreview).PageSize.Margins := Page.MarginsLoMetric;

    FOrientationPreview.MeasurementUnits := TdxPreviewMeasurementUnits(Page.MeasurementUnits);
    TdxPreviewAccess(FOrientationPreview).PageSize.Orientation := dxGetPreviewPaperOrientation(Page.Orientation);

    chbxCenterHorz.Checked := PrintStyle.PrinterPage.CenterOnPageH;
    chbxCenterVert.Checked := PrintStyle.PrinterPage.CenterOnPageV;
    rbtnDownThenOver.Checked := (PrintStyle.PrinterPage.PageOrder = poDownThenOver);
    rbtnOverThenDown.Checked := (PrintStyle.PrinterPage.PageOrder = poOverThenDown);

    memHeaderLeft.Lines := Page.PageHeader.LeftTitle;
    memHeaderCenter.Lines := Page.PageHeader.CenterTitle;
    memHeaderRight.Lines := Page.PageHeader.RightTitle;
    memFooterLeft.Lines := Page.PageFooter.LeftTitle;
    memFooterCenter.Lines := Page.PageFooter.CenterTitle;
    memFooterRight.Lines := Page.PageFooter.RightTitle;
    FontInfoToText(Page.PageHeader.Font, edHeaderFontInfo);
    FontInfoToText(Page.PageFooter.Font, edFooterFontInfo);

    rbtnAdjustTo.Checked := Page.ScaleMode = smAdjust;
    rbtnFitTo.Checked := Page.ScaleMode = smFit;
    seAdjustTo.Value := Page.ScaleFactor;
    chbxReverseOnEvenPages.Checked := Page.ReverseTitlesOnEvenPages;

    CheckDialogFormHelpContext(Self, libtnHelp);
  finally
    FControlsUpdating := False;
    UpdateControlsState;
  end;
  TdxPreviewAccess(FOrientationPreview).FullRefresh;
  TdxPreviewAccess(FPreview).FullRefresh;

  if (FStyleManager <> nil) and edStyleName.CanFocus then
    ActiveControl := edStyleName
  else
    if pgctrlMain.Index = 0 then
    begin
      if lbxPaperType.CanFocus then
        ActiveControl := lbxPaperType
    end
    else
      if pgctrlMain.Index = 1 then
      begin
        if seMarginTop.CanFocus and not seMarginTop.Properties.ReadOnly then
          ActiveControl := seMarginTop;
      end;
end;

procedure TdxfmPageSetupDialog.UpdateMarginsBounds;
var
  APrevValue: Boolean;
begin
  APrevValue := FControlsUpdating;
  if not APrevValue then
    FControlsUpdating := True;
  try
    with Page do
    begin
      seMarginHeader.Properties.MinValue := MinMargins.Top / 1000;
      seMarginHeader.Properties.MaxValue := (RealPageSize.Y - MinPrintableArea - MinMargins.Bottom) / 1000;
      seMarginFooter.Properties.MinValue := MinMargins.Bottom / 1000;
      seMarginFooter.Properties.MaxValue := (RealPageSize.Y - MinPrintableArea - MinMargins.Top) / 1000;
      seMarginTop.Properties.MinValue := MinMargins.Top / 1000;
      seMarginTop.Properties.MaxValue := (RealPageSize.Y - MinPrintableArea - MinMargins.Bottom) / 1000;
      seMarginBottom.Properties.MinValue := MinMargins.Bottom / 1000;
      seMarginBottom.Properties.MaxValue := (RealPageSize.Y - MinPrintableArea - MinMargins.Top) / 1000;
      seMarginLeft.Properties.MinValue := MinMargins.Left / 1000;
      seMarginLeft.Properties.MaxValue := (RealPageSize.X - MinPrintableArea - MinMargins.Right) / 1000;
      seMarginRight.Properties.MinValue := MinMargins.Right / 1000;
      seMarginRight.Properties.MaxValue := (RealPageSize.X - MinPrintableArea - MinMargins.Left) / 1000;
    end;
  finally
    if not APrevValue then
      FControlsUpdating := False;
  end;
end;

procedure TdxfmPageSetupDialog.SetMarginsInvalid(Value: Boolean);
begin
  if FMarginsInvalid <> Value then
  begin
    FMarginsInvalid := Value;
    UpdateWarningPane(MarginsInvalid, MarginsOutside,
      cxGetResourceString(@sdxInvalidMargins), cxGetResourceString(@sdxOutsideMargins));
    TdxPreviewAccess(FPreview).InvalidatePages;
  end;
end;

procedure TdxfmPageSetupDialog.SetMarginsOutside(Value: Boolean);
begin
  if FMarginsOutside <> Value then
  begin
    FMarginsOutside := Value;
    UpdateWarningPane(MarginsOutside, MarginsInvalid,
      cxGetResourceString(@sdxOutsideMargins), cxGetResourceString(@sdxInvalidMargins));
  end;
end;

procedure TdxfmPageSetupDialog.MarginExit(Sender: TObject);
var
  InvalidMarginControl: TWinControl;
begin
  if FMarginsChanging then Exit;
  FMarginsChanging := True;
  try
    MarginsInvalid := not ValidateMargins(InvalidMarginControl);
    MarginsOutside := not ValidateMarginsOutside(InvalidMarginControl);
    if not MarginsInvalid then UpdatePreviewMargins;
  finally
    FMarginsChanging := False;
  end;
  UpdateControlsState;
end;

procedure TdxfmPageSetupDialog.MarginChange(Sender: TObject);
begin
  if FControlsUpdating or FMarginsChanging then Exit;
  FMarginsChanged := True;
  CheckModified;
end;

procedure TdxfmPageSetupDialog.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TdxfmPageSetupDialog.btnHFFontClick(Sender: TObject);
var
  PageObject: TCustomdxPageObject;
  Editor: TcxTextEdit;
begin
  if TTagToInt(TComponent(Sender).Tag) = 0 then
  begin
    PageObject := Page.PageHeader;
    Editor := edHeaderFontInfo;
  end
  else
  begin
    PageObject := Page.PageFooter;
    Editor := edFooterFontInfo;
  end;

  dxPSGlbl.FontDialog.Font := PageObject.Font;
  if dxPSGlbl.FontDialog.Execute then
  begin
    PageObject.Font := dxPSGlbl.FontDialog.Font;
    FontInfoToText(PageObject.Font, Editor);
    CheckModified;
  end;
end;

procedure TdxfmPageSetupDialog.btnOptionsClick(Sender: TObject);
begin
  if PrintStyle.HasStyleOptionsDlg and PrintStyle.ShowStyleOptionsDlg then
    CheckModified;
end;

procedure TdxfmPageSetupDialog.AutoHFTextEntriesClick(Sender: TObject);
var
  Part1, Part2, Part3: string;
begin
  if ActiveControl is TcxCustomInnerMemo then
  begin
    dxPSSplitAutoHFTextEntry(FStyleManager.AutoHFTextEntries[TTagToInt(TComponent(Sender).Tag)], Part1, Part2, Part3);

    if (Part2 = '') and (Part3 = '') then
      TcxCustomInnerMemo(ActiveControl).SelText := Part1
    else
      if TTagToInt(GetInnerControlContainer(ActiveControl).Tag) < 4 then
      begin
        if Part1 <> '' then memHeaderLeft.SelText := Part1;
        if Part2 <> '' then memHeaderCenter.SelText := Part2;
        if Part3 <> '' then memHeaderRight.SelText := Part3;
      end
      else
      begin
        if Part1 <> '' then memFooterLeft.SelText := Part1;
        if Part2 <> '' then memFooterCenter.SelText := Part2;
        if Part3 <> '' then memFooterRight.SelText := Part3;
      end;
    CheckModified;
  end;
end;

procedure TdxfmPageSetupDialog.MemoExit(Sender: TObject);
begin
  if not (ActiveControl is TcxCustomInnerMemo) then
    EnabledMemoAttr(False);
end;

procedure TdxfmPageSetupDialog.MemoEnter(Sender: TObject);
begin
  EnabledMemoAttr(True);
  UpdateVerticalAlignmentButtonsState;
end;

procedure TdxfmPageSetupDialog.pgctrlMainChange(Sender: TObject);
begin
  EnabledMemoAttr(False);
end;

procedure TdxfmPageSetupDialog.EnabledMemoAttr(AEnabled: Boolean);
var
  I: Integer;
begin
  tbPredefined.Enabled := AEnabled;
  for I := 0 to tbPredefined.ControlCount - 1 do
    tbPredefined.Controls[I].Enabled := AEnabled;

  btnVertAlignTop.Enabled := AEnabled;
  btnVertAlignCenter.Enabled := AEnabled;
  btnVertAlignBottom.Enabled := AEnabled;
end;

procedure TdxfmPageSetupDialog.BackgroundClick(Sender: TObject);
var
  Pt: TPoint;
  ABackground: TdxBackground;
  AParams: TdxBackgroundDlgData;
begin
  Pt := TWinControl(Sender).ClientOrigin;
  Inc(Pt.Y, TWinControl(Sender).Height);
  FillChar(AParams, SizeOf(TdxBackgroundDlgData), 0);
  with AParams do
  begin
    BorderStyle := bsNone;
    NoBtnCaption := cxGetResourceString(@sdxBtnNoFill);
    ShowFillEffects := True;
    ShowMoreColors := True;
  end;
  if TTagToInt(TComponent(Sender).Tag) = 0 then
    ABackground := Page.PageHeader.Background
  else
    ABackground := Page.PageFooter.Background;
  if dxChooseBackgroundDlg(ABackground, Pt, AParams) then
  begin
    ChangeBkgndGlyph(TcxButton(Sender));
    CheckModified;
  end;
end;

procedure TdxfmPageSetupDialog.VertTextAlignClick(Sender: TObject);
begin
  if ActiveControl is TcxCustomInnerMemo then
  begin
    SelectedTitlePartTextAlignY := TcxTextAlignY(TTagToInt(TComponent(Sender).Tag));
    CheckModified;
  end;
end;

procedure TdxfmPageSetupDialog.memHeaderLeftChange(Sender: TObject);
begin
  CheckModified;
  TWinControl(Sender).Invalidate;
end;

procedure TdxfmPageSetupDialog.chbxReverseOnEvenPagesClick(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  PrintStyle.PrinterPage.ReverseTitlesOnEvenPages := TcxCheckBox(Sender).Checked;
  CheckModified;
end;

procedure TdxfmPageSetupDialog.btnPrintPreviewClick(Sender: TObject);
begin
  FModified := True;
  FPreviewBtnClicked := True;
  ModalResult := mrOK
end;

procedure TdxfmPageSetupDialog.btnPrintClick(Sender: TObject);
begin
  FModified := True;
  FPrintBtnClicked := True;
  ModalResult := mrOK;
end;

procedure TdxfmPageSetupDialog.OrientationPreviewCalcPageCount(Sender: TObject);
begin
  TdxPreview(Sender).PageCount := 1;
end;

procedure TdxfmPageSetupDialog.PreviewCalcPageCount(Sender: TObject);
begin
  TdxPreview(Sender).PageCount := 1;
end;

procedure TdxfmPageSetupDialog.PreviewDrawPageContent(Sender: TObject;
  ACanvas: TCanvas; ABounds: TRect; APageIndex: Integer);
var
  AContentBounds, AFooterBounds, AHeaderBounds: TRect;
  AInvalidMarginControl: TWinControl;
begin
  with TdxPreviewAccess(Sender) do
  begin
    AContentBounds := PageGetContentBounds(ABounds);
    AFooterBounds := PageGetFooterBounds(ABounds);
    AHeaderBounds := PageGetHeaderBounds(ABounds);

    if ValidateMargins(AInvalidMarginControl) then
      OptionsView := OptionsView + [povMargins]
    else
      OptionsView := OptionsView - [povMargins];

    if povMargins in OptionsView then
      if Assigned(FOnCustomDrawPreview) then
        FOnCustomDrawPreview(PrintStyle, ACanvas, ABounds, AContentBounds, AHeaderBounds, AFooterBounds)
      else
        dxPSDefaultDrawPagePreview(PrintStyle, ACanvas, ABounds, AContentBounds, AHeaderBounds, AFooterBounds, Self.ScaleFactor);
  end;
end;

procedure TdxfmPageSetupDialog.PreviewAfterDragMargin(Sender: TObject;
  AMargin: TdxPreviewPageMargin);
var
  V: Extended;
begin
  V := AMargin.Value / UnitsDenominator;
  if (AMargin is TdxPreviewPageMarginFooter) or (AMargin is TdxPreviewPageMarginHeader) then
    FMarginsChanging := True;

  try
    if AMargin is TdxPreviewPageMarginLeft then
    begin
      seMarginLeft.Value := V;
      MarginExit(seMarginLeft);
    end;
    if AMargin is TdxPreviewPageMarginTop then
    begin
      seMarginTop.Value := V;
      MarginExit(seMarginTop);
    end;
    if AMargin is TdxPreviewPageMarginRight then
    begin
      seMarginRight.Value := V;
      MarginExit(seMarginRight);
    end;
    if AMargin is TdxPreviewPageMarginBottom then
    begin
      seMarginBottom.Value := V;
      MarginExit(seMarginBottom);
    end;
    if AMargin is TdxPreviewPageMarginHeader then
    begin
      seMarginHeader.Value := V;
      if seMarginTop.Value < seMarginHeader.Value then
        seMarginTop.Value := seMarginHeader.Value;
      FMarginsChanged := True;
    end;
    if AMargin is TdxPreviewPageMarginFooter then
    begin
      seMarginFooter.Value := V;
      if seMarginBottom.Value < seMarginFooter.Value then
        seMarginBottom.Value := seMarginFooter.Value;
      FMarginsChanged := True;
    end;
  finally
    if (AMargin is TdxPreviewPageMarginFooter) or (AMargin is TdxPreviewPageMarginHeader) then
    begin
      FMarginsChanging := False;
      if AMargin is TdxPreviewPageMarginHeader then
        MarginExit(seMarginHeader)
      else
        MarginExit(seMarginBottom);
    end;
  end;
end;

procedure TdxfmPageSetupDialog.lblMarginTopClick(Sender: TObject);
begin
  ActiveControl := TcxLabel(Sender).FocusControl;
end;

procedure TdxfmPageSetupDialog.lblPaperSourceClick(Sender: TObject);
var
  ALabel: TcxLabel;
begin
  ALabel := Sender as TcxLabel;
  ActiveControl := ALabel.FocusControl;
  if ALabel.FocusControl is TcxComboBox then
    (ALabel.FocusControl as TcxComboBox).DroppedDown := True;
end;

procedure TdxfmPageSetupDialog.pbxPageOrderPaint(Sender: TObject);
const
  ImageIndexes: array[Boolean] of Integer = (1, 0);
begin
  cxPaintCanvas.BeginPaint(TPaintBox(Sender).Canvas);
  try
    cxDrawImage(cxPaintCanvas, TPaintBox(Sender).ClientRect, nil,
      ilPrintOrders, ImageIndexes[rbtnDownThenOver.Checked],
      gbxPrintOrder.Enabled, nil, ScaleFactor);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxfmPageSetupDialog.PageOrderClick(Sender: TObject);
begin
  if not FControlsUpdating then
  begin
    PrintStyle.PrinterPage.PageOrder := TdxPageOrder(TTagToInt(TcxRadioButton(Sender).Tag));
    pbxPageOrder.Invalidate;
    CheckModified;
  end;
end;

procedure TdxfmPageSetupDialog.pbxPageOrderDblClick(Sender: TObject);
begin
  rbtnDownThenOver.Checked := not rbtnDownThenOver.Checked;
  rbtnOverThenDown.Checked := not rbtnDownThenOver.Checked;
end;

procedure TdxfmPageSetupDialog.chbxShadingClick(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  PrintStyle.PrinterPage.GrayShading := TcxCheckBox(Sender).Checked;
  CheckModified;
end;

procedure TdxfmPageSetupDialog.CenterOnPageClick(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  with TcxCheckBox(Sender) do
    if TTagToInt(Tag) = 0 then
      PrintStyle.PrinterPage.CenterOnPageH := Checked
    else
      PrintStyle.PrinterPage.CenterOnPageV := Checked;
  CheckModified;
  TdxPreviewAccess(FPreview).InvalidatePages;
end;

procedure TdxfmPageSetupDialog.OrientationClick(Sender: TObject);
var
  V, W, H: Extended;
  APrinterOrientation: TdxPrinterOrientation;
  APrevPreviewPaperOrientation, APreviewPaperOrientation: TdxPreviewPaperOrientation;
begin
  if FControlsUpdating or FOrientationChanging then Exit;

  FOrientationChanging := True;
  try
    APrinterOrientation := TdxPrinterOrientation(TTagToInt(TComponent(Sender).Tag));
    APreviewPaperOrientation := dxGetPreviewPaperOrientation(APrinterOrientation);
    if not Page.AutoSwapMargins then
    begin
      FPreview.OptionsBehavior := FPreview.OptionsBehavior - [pobAutoSwapMargins];
      FOrientationPreview.OptionsBehavior := FOrientationPreview.OptionsBehavior - [pobAutoSwapMargins];
    end;
    APrevPreviewPaperOrientation := TdxPreviewAccess(FPreview).PageSize.Orientation;
    TdxPreviewAccess(FPreview).PageSize.Orientation := APreviewPaperOrientation;
    TdxPreviewAccess(FOrientationPreview).PageSize.Orientation := APreviewPaperOrientation;
    SaveMargins;
    Page.Orientation := APrinterOrientation;
    if APrevPreviewPaperOrientation <> APreviewPaperOrientation then
    begin
      W := sePaperWidth.Value;
      H := sePaperHeight.Value;
      V := sePaperWidth.Properties.MaxValue;
      sePaperWidth.Properties.MaxValue := sePaperHeight.Properties.MaxValue;
      sePaperHeight.Properties.MaxValue := V;
      V := sePaperWidth.Properties.MinValue;
      sePaperWidth.Properties.MinValue := sePaperHeight.Properties.MinValue;
      sePaperHeight.Properties.MinValue := V;
      sePaperWidth.Value := H;
      sePaperHeight.Value := W;
      UpdateMarginsEdits;
    end;
    CheckModified;
  finally
    FOrientationChanging := False;
  end;
end;

procedure TdxfmPageSetupDialog.OrientationDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TdxfmPageSetupDialog.UpdateMarginsEdits;
begin
  UpdateMarginsBounds;
  with Page do
  begin
    seMarginHeader.Value := Header / 1000;
    seMarginFooter.Value := Footer / 1000;
    seMarginTop.Value := Margins.Top / 1000;
    seMarginBottom.Value := Margins.Bottom / 1000;
    seMarginLeft.Value := Margins.Left / 1000;
    seMarginRight.Value := Margins.Right / 1000;
  end;
  if not FControlsUpdating then
  begin
    MarginExit(seMarginHeader);
    MarginExit(seMarginFooter);
    MarginExit(seMarginTop);
    MarginExit(seMarginBottom);
    MarginExit(seMarginLeft);
    MarginExit(seMarginRight);
  end;
end;

procedure TdxfmPageSetupDialog.cbxPaperSourceChange(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  with TcxComboBox(Sender) do
    Page.PaperSource := Integer(Properties.Items.Objects[ItemIndex]);
  CheckModified;
end;

procedure TdxfmPageSetupDialog.lbxPaperTypeClick(Sender: TObject);
var
  ASavedValue: Boolean;
  PaperInfo: TdxPaperInfo;
begin
  if FPaperSizeLocked then Exit;

  PaperInfo := CurrentPaperInfo;
  TdxPreviewAccess(FPreview).PageSize.Size := Point(PaperInfo.Width, PaperInfo.Height);
  TdxPreviewAccess(FOrientationPreview).PageSize.Size := Point(PaperInfo.Width, PaperInfo.Height);

  ASavedValue := FControlsUpdating;
  try
    FControlsUpdating := True;
    Page.DMPaper := PaperInfo.DMPaper;
    sePaperWidth.Value := Page.RealPageSize.X / 1000;
    sePaperHeight.Value := Page.RealPageSize.Y / 1000;
  finally
    FControlsUpdating := ASavedValue;
  end;

  UpdateMarginsBounds;
  if not FControlsUpdating then
    CheckModified;
end;

procedure TdxfmPageSetupDialog.ScalingClick(Sender: TObject);
begin
  if FControlsUpdating then
    Exit;

  case TTagToInt(TComponent(Sender).Tag) of
    0:
      begin
        PrintStyle.PrinterPage.ScaleMode := smAdjust;
        ActiveControl := seAdjustTo;
      end;
    1:
      begin
        PrintStyle.PrinterPage.ScaleMode := smFit;
        ActiveControl := sePagesWide;
      end;
  end;
  CheckModified;
end;

procedure TdxfmPageSetupDialog.sePagesWideExit(Sender: TObject);
begin
  if not FControlsUpdating then
  begin
    if rbtnFitTo.Checked then
    begin
      PrintStyle.PrinterPage.FitToPagesHorizontally := sePagesWide.Value;
      PrintStyle.PrinterPage.FitToPagesVertically := sePagesTall.Value;
    end;
  end;
end;

procedure TdxfmPageSetupDialog.sePagesWidePropertiesChange(Sender: TObject);
begin
  if FControlsUpdating then Exit;

  rbtnFitTo.Checked := True;
  ActiveControl := TWinControl(Sender);
  CheckModified;
end;

procedure TdxfmPageSetupDialog.MemoChange(Sender: TObject);
begin
  if not FControlsUpdating then
    CheckModified;
end;

procedure TdxfmPageSetupDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and cxShiftStateMoveOnly(Shift) and (ActiveControl is TcxCustomInnerMemo) then
    ModalResult := mrCancel;
end;

procedure TdxfmPageSetupDialog.edStyleNameChange(Sender: TObject);
begin
  if not FControlsUpdating then
    CheckModified;
end;

procedure TdxfmPageSetupDialog.edStyleNameExit(Sender: TObject);
var
  StyleManager: TdxPrintStyleManager;
begin
  if edStyleName.Enabled and (PrintStyle.StyleCaption <> edStyleName.Text) then
  begin
    PrintStyle.StyleCaption := edStyleName.Text;
    StyleManager := FSavePrintStyle.StyleManager;
    if (StyleManager <> nil) and (StyleManager.PageSetupDialog <> nil) then
      Caption := StyleManager.PageSetupDialog.RealTitle
    else
      Caption := cxGetResourceString(@sdxPageSetupCaption) + ': ' + PrintStyle.StyleCaption;

    FModified := True;
  end;
end;

function TdxfmPageSetupDialog.ValidateStyleCaption: Boolean;
var
  S: string;
  I: Integer;
  AStyle: TBasedxPrintStyle;
begin
  Result := True;
  if (FStyleManager <> nil) and not edStyleName.Properties.ReadOnly then
  begin
    S := edStyleName.Text;
    for I := 0 to FStyleManager.Count - 1 do
    begin
      AStyle := FStyleManager[I];
      if (AStyle <> FSavePrintStyle) and dxSameText(AStyle.StyleCaption, S) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TdxfmPageSetupDialog.ValidateUserInput(out AControl: TWinControl): Boolean;
var
  IsFixupMarginsOutside: Boolean;
  Stub: TWinControl;
begin
  AControl := nil;
  Result := ValidateStyleCaption;
  if not Result then
  begin
    MessageWarning(Format(cxGetResourceString(@sdxInvalideStyleCaption), [edStyleName.Text]));
    AControl := edStyleName;
  end
  else
  begin
    IsFixupMarginsOutside := False;
    if not ValidateMarginsOutside(Stub) then
    begin
      Result := True;
      Beep;
      IsFixupMarginsOutside := MarginsOutsideMessageDlg(cxGetResourceString(@sdxOutsideMarginsMessage2)) = mrYes;
      if IsFixupMarginsOutside then FixupMarginsOutside;
    end;
    if not IsFixupMarginsOutside then
    begin
      Result := ValidateMargins(AControl);
      if not Result then
      begin
        Beep;
        Result := True;
        case MarginsMessageDlg(cxGetResourceString(@sdxInvalidMarginsMessage)) of
          mrYes:
            FixupMargins;
          mrNo:
            RestoreOriginalMargins;
        end;
      end;
    end;
  end;
end;

procedure TdxfmPageSetupDialog.SaveMargins;
begin
  Page.BeginUpdate;
  try
    Page.Header := Round(seMarginHeader.Value * 1000);
    Page.Footer := Round(seMarginFooter.Value * 1000);
    Page.Margins.Left := Round(seMarginLeft.Value * 1000);
    Page.Margins.Top := Round(seMarginTop.Value * 1000);
    Page.Margins.Right := Round(seMarginRight.Value * 1000);
    Page.Margins.Bottom := Round(seMarginBottom.Value * 1000);
  finally
    Page.EndUpdate;
  end;
end;

procedure TdxfmPageSetupDialog.SaveStyleCaption;
begin
  if not edStyleName.Properties.ReadOnly then
    PrintStyle.StyleCaption := edStyleName.Text;
end;

procedure TdxfmPageSetupDialog.SaveUserInput;
begin
  SaveStyleCaption;
  SaveMargins;
end;

function TdxfmPageSetupDialog.ValidateMarginsOutside(out AInvalidMarginControl: TWinControl): Boolean;
var
  AMin: TRect;
begin
  Page.GetRealMinMargins(AMin.Left, AMin.Right, AMin.Top, AMin.Bottom);
  Result := seMarginHeader.Value >= AMin.Top / 1000;
  if Result then
    Result := seMarginFooter.Value >= AMin.Bottom / 1000
  else
  begin
    AInvalidMarginControl := seMarginHeader;
    Exit;
  end;
  if Result then
    Result := seMarginLeft.Value >= AMin.Left / 1000
  else
  begin
    AInvalidMarginControl := seMarginFooter;
    Exit;
  end;
  if Result then
    Result := seMarginRight.Value >= AMin.Right / 1000
  else
  begin
    AInvalidMarginControl := seMarginLeft;
    Exit;
  end;

  if Result then
    AInvalidMarginControl := nil
  else
    AInvalidMarginControl := seMarginRight;
end;

function TdxfmPageSetupDialog.ValidateMargins(out AInvalidMarginControl: TWinControl): Boolean;

  function EqualOrGreaterThen(const AValue1, AValue2: Extended): Boolean;
  const
    Eps = 0.00001;
  begin
    Result := (AValue1 > AValue2) or (Abs(AValue1 - AValue2) < Eps);
  end;

  function EqualOrLessThen(const AValue1, AValue2: Extended): Boolean;
  const
    Eps = 0.00001;
  begin
    Result := (AValue1 < AValue2) or (Abs(AValue1 - AValue2) < Eps);
  end;

var
  Min, Max, APageSizeX, APageSizeY, AMinPrintableArea: Extended;
begin
  with Page do
  begin
    APageSizeX := RealPageSize.X / 1000;
    APageSizeY := RealPageSize.Y / 1000;
    AMinPrintableArea := MinPrintableArea / 1000;
   {header}
    Min := 0;
    Max := APageSizeY - AMinPrintableArea;
    Result := EqualOrGreaterThen(seMarginHeader.Value, Min) and
      EqualOrLessThen(seMarginHeader.Value, Max);
    if Result then
    begin
   {footer}
      Min := 0;
      Max := APageSizeY - AMinPrintableArea - seMarginHeader.Value;
      Result := EqualOrGreaterThen(seMarginFooter.Value, Min) and
        EqualOrLessThen(seMarginFooter.Value, Max);
    end
    else
    begin
      AInvalidMarginControl := seMarginHeader;
      Exit;
    end;
   {top}
    if Result then
    begin
      Min := seMarginHeader.Value;
      Max := APageSizeY - AMinPrintableArea - seMarginBottom.Value;
      Result := EqualOrGreaterThen(seMarginTop.Value, Min) and
        EqualOrLessThen(seMarginTop.Value, Max);
    end
    else
    begin
      AInvalidMarginControl := seMarginFooter;
      Exit;
    end;
    if Result then
    begin {bottom}
      Min := seMarginFooter.Value;
      Max := APageSizeY - AMinPrintableArea - seMarginTop.Value;
      Result := EqualOrGreaterThen(seMarginBottom.Value, Min) and
        EqualOrLessThen(seMarginBottom.Value, Max);
    end
    else
    begin
      AInvalidMarginControl := seMarginTop;
      Exit;
    end;
    if Result then
    begin {left}
      Min := 0;
      Max := APageSizeX - AMinPrintableArea;
      Result := EqualOrGreaterThen(seMarginLeft.Value, Min) and
        EqualOrLessThen(seMarginLeft.Value, Max);
    end
    else
    begin
      AInvalidMarginControl := seMarginBottom;
      Exit;
    end;
    if Result then
    begin {right}
      Min := 0;
      Max := APageSizeX - AMinPrintableArea - seMarginLeft.Value;
      Result := EqualOrGreaterThen(seMarginRight.Value, Min) and
        EqualOrLessThen(seMarginRight.Value, Max);
    end
    else
    begin
      AInvalidMarginControl := seMarginLeft;
      Exit;
    end;
  end;
  if Result then
    AInvalidMarginControl := nil
  else
    AInvalidMarginControl := seMarginRight;
end;

procedure TdxfmPageSetupDialog.UpdatePreviewMargin(
  const AValue: Extended; AMargin: TdxPreviewPageMargin);
begin
  AMargin.Value := MulDiv(Round(AValue * 1000), UnitsDenominator, 1000);
end;

procedure TdxfmPageSetupDialog.UpdatePreviewMargins;
begin
  UpdatePreviewMargin(seMarginHeader.Value, FPreview.Margins.Header);
  UpdatePreviewMargin(seMarginTop.Value, FPreview.Margins.Top);
  UpdatePreviewMargin(seMarginFooter.Value, FPreview.Margins.Footer);
  UpdatePreviewMargin(seMarginBottom.Value, FPreview.Margins.Bottom);
  UpdatePreviewMargin(seMarginLeft.Value, FPreview.Margins.Left);
  UpdatePreviewMargin(seMarginRight.Value, FPreview.Margins.Right);
end;

procedure TdxfmPageSetupDialog.UpdateVerticalAlignmentButtonsState;
begin
  case SelectedTitlePartTextAlignY of
    taCenterY:
      btnVertAlignCenter.Down := True;
    taBottom:
      btnVertAlignBottom.Down := True;
    else
      btnVertAlignTop.Down := True;
  end;
end;

procedure TdxfmPageSetupDialog.UpdateWarningPane(AValue, APairValue: Boolean;
  const AHint, APairHint: string);
begin
  if AValue then
    FwpMargins.SetStateAndHint(AValue, AHint)
  else
    if APairValue then
      FwpMargins.SetStateAndHint(APairValue, APairHint)
    else
      FwpMargins.State := False;
end;

procedure TdxfmPageSetupDialog.FixupMarginsOutside;

  function SetSpinEditValidValue(AEdit: TcxSpinEdit;
    var AValue: Integer; const ADenominator: Integer): Boolean;
  begin
    Result := False;
    while AEdit.Value < AValue / ADenominator do
    begin
      Inc(AValue);
      AEdit.Value := AValue / ADenominator;
      Result := True;
    end;
  end;

var
  AMinLeft, AMinRight, AMinTop, AMinBottom: Integer;
begin
  Page.GetRealMinMargins(AMinLeft, AMinRight, AMinTop, AMinBottom);
  FMarginsChanging := True;
  try
    if SetSpinEditValidValue(seMarginTop, AMinTop, 1000) then
      UpdatePreviewMargin(AMinTop / 1000, FPreview.Margins.Top);
    if SetSpinEditValidValue(seMarginHeader, AMinTop, 1000) then
      UpdatePreviewMargin(AMinTop / 1000, FPreview.Margins.Header);
    if SetSpinEditValidValue(seMarginBottom, AMinBottom, 1000) then
      UpdatePreviewMargin(AMinBottom / 1000, FPreview.Margins.Bottom);
    if SetSpinEditValidValue(seMarginFooter, AMinBottom, 1000) then
      UpdatePreviewMargin(AMinBottom / 1000, FPreview.Margins.Footer);
    if SetSpinEditValidValue(seMarginLeft, AMinLeft, 1000) then
      UpdatePreviewMargin(AMinLeft / 1000, FPreview.Margins.Left);
    if SetSpinEditValidValue(seMarginRight, AMinRight, 1000) then
      UpdatePreviewMargin(AMinRight / 1000, FPreview.Margins.Right);
  finally
    FMarginsChanging := False;
  end;
  MarginsOutside := False;
  MarginsInvalid := False;
  CheckModified;
end;

procedure TdxfmPageSetupDialog.FixupMargins;

  function MinMax(const Value, Min, Max: Double): Double;
  begin
    if Value < Min then
      Result := Min
    else if Value > Max then
      Result := Max
    else
      Result := Value;
  end;

var
  V: Double;
  AMinLeft, AMinRight, AMinTop, AMinBottom: Integer;
begin
  Page.GetRealMinMargins(AMinLeft, AMinRight, AMinTop, AMinBottom);
  FMarginsChanging := True;
  try
    V := (Page.RealPageSize.Y - Page.MinPrintableArea - AMinBottom) / 1000;
    seMarginHeader.Value := MinMax(seMarginHeader.Value, AMinTop / 1000, V);
    V := (Page.RealPageSize.Y - Page.MinPrintableArea - 1000 * seMarginHeader.Value) / 1000;
    seMarginFooter.Value := MinMax(seMarginFooter.Value, AMinBottom / 1000, V);
    V := (Page.RealPageSize.Y - Page.MinPrintableArea - 1000 * seMarginFooter.Value) / 1000;
    seMarginTop.Value := MinMax(seMarginTop.Value, seMarginHeader.Value, V);
    V := (Page.RealPageSize.Y - Page.MinPrintableArea - 1000 * seMarginTop.Value) / 1000;
    seMarginBottom.Value := MinMax(seMarginBottom.Value, seMarginFooter.Value, V);

    V := (Page.RealPageSize.X - Page.MinPrintableArea - AMinRight) / 1000;
    seMarginLeft.Value := MinMax(seMarginLeft.Value, AMinLeft / 1000, V);
    V := (Page.RealPageSize.X - Page.MinPrintableArea - 1000 * seMarginLeft.Value) / 1000;
    seMarginRight.Value := MinMax(seMarginRight.Value, AMinRight / 1000, V);

    UpdatePreviewMargin(0, FPreview.Margins.Footer);
    UpdatePreviewMargin(0, FPreview.Margins.Bottom);
    UpdatePreviewMargin(0, FPreview.Margins.Right);
    UpdatePreviewMargin(seMarginTop.Value, FPreview.Margins.Top);
    UpdatePreviewMargin(seMarginHeader.Value, FPreview.Margins.Header);
    UpdatePreviewMargin(seMarginBottom.Value, FPreview.Margins.Bottom);
    UpdatePreviewMargin(seMarginFooter.Value, FPreview.Margins.Footer);
    UpdatePreviewMargin(seMarginLeft.Value, FPreview.Margins.Left);
    UpdatePreviewMargin(seMarginRight.Value, FPreview.Margins.Right);
  finally
    FMarginsChanging := False;
  end;

  MarginsInvalid := False;
  CheckModified;
end;

procedure TdxfmPageSetupDialog.RestoreOriginalMargins;
begin
  FMarginsChanging := True;
  try
    with FSavePrintStyle.PrinterPage do
    begin
      seMarginHeader.Value := Footer / 1000;
      FPreview.Margins.Header.Value := HeaderLoMetric;
      seMarginFooter.Value := Footer / 1000;
      FPreview.Margins.Footer.Value := FooterLoMetric;
      seMarginLeft.Value := Margins.Left / 1000;
      FPreview.Margins.Left.Value := MarginsLoMetric.Left;
      seMarginTop.Value := Margins.Top / 1000;
      FPreview.Margins.Top.Value := MarginsLoMetric.Top;
      seMarginRight.Value := Margins.Right / 1000;
      FPreview.Margins.Right.Value := MarginsLoMetric.Right;
      seMarginBottom.Value := Margins.Bottom / 1000;
      FPreview.Margins.Bottom.Value := MarginsLoMetric.Bottom;
    end;
  finally
    FMarginsChanging := False;
  end;
  MarginsOutside := False;
  MarginsInvalid := False;
  CheckModified;
end;

procedure TdxfmPageSetupDialog.TrySetActiveControl(AControl: TWinControl);
begin
  if AControl.CanFocus then
    ActiveControl := AControl;
end;

procedure TdxfmPageSetupDialog.btnRestoreOriginalMarginsClick(Sender: TObject);
begin
  RestoreOriginalMargins;
end;

procedure TdxfmPageSetupDialog.btnFixClick(Sender: TObject);
begin
  if MarginsOutside then
    FixupMarginsOutside
  else
    if MarginsInvalid then
      FixupMargins;
end;

procedure TdxfmPageSetupDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Control: TWinControl;
begin
  if TdxPreviewAccess(FPreview).DraggingMargin <> nil then
    CanClose := False
  else
    if ModalResult = mrOK then
    begin
      CanClose := ValidateUserInput(Control);
      if not CanClose then
      begin
        FPrintBtnClicked := False;
        FPreviewBtnClicked := False;
        TrySetActiveControl(Control);
      end
      else
        SaveUserInput;
    end;
end;

procedure TdxfmPageSetupDialog.FormClose(
  Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    if (ActiveControl is TcxCustomInnerTextEdit) or
       (ActiveControl is TcxCustomEdit)
    then
      ActiveControl.Perform(CM_EXIT, 0, 0);
  end;
end;

procedure TdxfmPageSetupDialog.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if pgctrlMain.ItemIndex = tshMargins.Index then
    FPreview.Invalidate;
end;

{ TdxPrintStylePrinterPage }

function TdxPrintStylePrinterPage.GetOwner: TPersistent;
begin
  Result := PrintStyle;
end;

function TdxPrintStylePrinterPage.IsPageFooterTitleStored(Index: Integer): Boolean;
var
  Part: TdxPageTitlePart;
begin
  Part := dxPSPageTitlePartMap[Index];
  Result := PageFooter.Titles[Part].Text <> PrintStyle.DefaultPageFooterText(Part);
end;

function TdxPrintStylePrinterPage.IsPageHeaderTitleStored(Index: Integer): Boolean;
var
  Part: TdxPageTitlePart;
begin
  Part := dxPSPageTitlePartMap[Index];
  Result := PageHeader.Titles[Part].Text <> PrintStyle.DefaultPageHeaderText(Part);
end;

procedure TdxPrintStylePrinterPage.PageParamsChanged(AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  inherited;
  if (UpdateCount = 0) and PrintStyle.IsCurrentStyle then
    PrintStyle.PageParamsChanged(AUpdateCodes);
end;

{ TBasedxPrintStyle }

constructor TBasedxPrintStyle.Create(AOwner: TComponent);
begin
  inherited;
  FAllowChangeHFText := True;
  FAllowChangeMargins := True;
  FAllowChangeOrientation := True;
  FAllowChangePaper := True;
  FAllowChangeScale := True;
  FAllowCustomPaperSizes := True;
  FImageIndex := -1;
  FBuiltIn := IsDesigning;

  FPrinterPage := CreatePrinterPage;

  FStyleGlyph := TBitmap.Create;
  FStyleGlyph.OnChange := StyleGlyphChanged;
end;

destructor TBasedxPrintStyle.Destroy;
begin
  try
    if (StyleManager <> nil) and StyleManager.AllowAutoSave then
    try
      StyleManager.SaveToFile(StyleManager.StorageName);
    finally
      StyleManager.FAlreadySaved := True;
    end;
  finally
    StyleManager := nil;
    FreeAndNil(FPrinterPage);
    FreeAndNil(FStyleGlyph);
    FreeAndNil(FDefaultStyleGlyph);
    inherited;
  end;
end;

procedure TBasedxPrintStyle.BeforeDestruction;
begin
  DoDestroy;
  inherited;
end;

procedure TBasedxPrintStyle.Assign(Source: TPersistent);
begin
  if Source is TBasedxPrintStyle then
    with TBasedxPrintStyle(Source) do
    begin
      Self.AllowChangeHFText := AllowChangeHFText;
      Self.AllowChangeMargins := AllowChangeMargins;
      Self.AllowChangeOrientation := AllowChangeOrientation;
      Self.AllowChangePaper := AllowChangePaper;
      Self.AllowChangeScale := AllowChangeScale;
      Self.AllowCustomPaperSizes := AllowCustomPaperSizes;
      Self.Description := Description;
      Self.ImageIndex := ImageIndex;
      Self.PrinterPage := PrinterPage;
      Self.StyleGlyph := StyleGlyph;
      Self.StyleCaption := StyleCaption;

      Self.FIsDescriptionAssigned := FIsDescriptionAssigned;
      Self.FIsStyleCaptionAssigned := FIsStyleCaptionAssigned;
      Self.FIsStyleGlyphAssigned := FIsStyleGlyphAssigned;
    end
  else
    inherited;
end;

function TBasedxPrintStyle.GetParentComponent: TComponent;
begin
  Result := StyleManager;
end;

function TBasedxPrintStyle.HasParent: Boolean;
begin
  Result := StyleManager <> nil;
end;

procedure TBasedxPrintStyle.ChangeScale(M, D: Integer);
begin
  TdxPrinterPageAccess(PrinterPage).ChangeScale(M, D);
  SynchronizePixelsPerInch;
end;

procedure TBasedxPrintStyle.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('BuiltInStyle', ReadData, WriteData, True);
  Filer.DefineProperty('IsDescriptonAssigned', ReadIsDescriptionAssigned, WriteIsDescriptionAssigned,
    FIsDescriptionAssigned and (Description = ''));
  Filer.DefineProperty('IsStyleCaptionAssigned', ReadIsStyleCaptionAssigned, WriteIsStyleCaptionAssigned,
    FIsStyleCaptionAssigned and (StyleCaption = ''));
  Filer.DefineProperty('IsStyleGlyphAssigned', ReadIsStyleGlyphAssigned, WriteIsStyleGlyphAssigned,
    FIsStyleGlyphAssigned and StyleGlyph.Empty);
end;

procedure TBasedxPrintStyle.ReadState(Reader: TReader);
begin
  inherited;
  if Reader.Parent is TdxPrintStyleManager then
    StyleManager := TdxPrintStyleManager(Reader.Parent);
end;

procedure TBasedxPrintStyle.SetName(const NewName: TComponentName);
begin
  inherited;
  DesignerUpdate(False);
end;

procedure TBasedxPrintStyle.SetParentComponent(AParent: TComponent);
begin
  inherited;
  if not IsLoading then
    StyleManager := AParent as TdxPrintStyleManager;
end;

class function TBasedxPrintStyle.StyleClass: TdxPrintStyleClass;
begin
  Result := TdxPrintStyleClass(GetTypeData(ClassInfo)^.ClassType);
end;

function TBasedxPrintStyle.DefaultDescription: string;
begin
  Result := '';
end;

function TBasedxPrintStyle.DefaultPageFooterText(APart: TdxPageTitlePart): string;
begin
  Result := '';
end;

function TBasedxPrintStyle.DefaultPageHeaderText(APart: TdxPageTitlePart): string;
begin
  Result := '';
end;

function TBasedxPrintStyle.DefaultStyleCaption: string;
begin
  Result := cxGetResourceString(@sdxBaseStyle);
end;

function TBasedxPrintStyle.DefaultStyleGlyph: TBitmap;
begin
  if FDefaultStyleGlyph = nil then
  begin
    FDefaultStyleGlyph := TBitmap.Create;
    InitializeDefaultStyleGlyph(FDefaultStyleGlyph);
  end;
  Result := FDefaultStyleGlyph;
end;

procedure TBasedxPrintStyle.AfterPrinting;
begin
  DoAfterPrinting;
end;

procedure TBasedxPrintStyle.BeforePrinting;
begin
  DoBeforePrinting;
end;

procedure TBasedxPrintStyle.GetFilteredPapers(AStrings: TStrings);
var
  Papers: TStrings;
  I: Integer;
  Paper: TdxPaperInfo;
begin
  if AStrings = nil then Exit;

  AStrings.BeginUpdate;
  try
    Papers := dxPrintDevice.Papers;
    if Papers = nil then Exit;

    for I := 0 to Papers.Count - 1 do
    begin
      Paper := TdxPaperInfo(Papers.Objects[I]);
      if IsSupportedPaper(Paper) then
        AStrings.AddObject(Paper.Name, Paper);
      if Paper.DMPaper = DMPAPER_USER then
      begin
        Paper.Width := PrinterPage.PageSizeLoMetric.X;
        Paper.Height := PrinterPage.PageSizeLoMetric.Y;
      end;
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

function TBasedxPrintStyle.PageSetup: Boolean;
var
  PreviewBtnClicked, PrintBtnClicked: Boolean;
begin
  Result := PageSetup(0, False, False, PreviewBtnClicked, PrintBtnClicked);
end;

function TBasedxPrintStyle.PageSetup(
  AnActivePageIndex: Integer; AShowPreviewBtn, AShowPrintBtn: Boolean;
  out APreviewBtnClicked, APrintBtnClicked: Boolean): Boolean;

  function GetDialog(out ANewCreated: Boolean): TdxPageSetupDialog;
  begin
    if StyleManager <> nil then
      Result := StyleManager.PageSetupDialog
    else
      Result := nil;
    ANewCreated := Result = nil;
    if ANewCreated then
      Result := TdxPageSetupDialog.Create(nil);
  end;

var
  Dialog: TdxPageSetupDialog;
  NewCreated: Boolean;
  PrevStyle: TBasedxPrintStyle;
begin
  Dialog := GetDialog(NewCreated);
  try
    PrevStyle := Dialog.PrintStyle;
    Dialog.PrintStyle := Self;
    Dialog.ActivePageIndex := AnActivePageIndex;

    if not AShowPreviewBtn then
      Dialog.ButtonsVisible := Dialog.ButtonsVisible - [psbPreview];

    if not AShowPrintBtn then
      Dialog.ButtonsVisible := Dialog.ButtonsVisible - [psbPrint];

    if StyleManager = nil then
      Dialog.OptionsVisible := Dialog.OptionsVisible - [psoStyleCaption];

    Result := Dialog.Execute;
    Dialog.PrintStyle := PrevStyle;

    APreviewBtnClicked := AShowPreviewBtn and Dialog.PreviewBtnClicked;
    APrintBtnClicked := AShowPrintBtn and Dialog.PrintBtnClicked;
  finally
    if NewCreated then Dialog.Free;
  end;
end;

function TBasedxPrintStyle.ShowStyleOptionsDlg: Boolean;
begin
  Result := False;
end;

function TBasedxPrintStyle.HasStyleOptionsDlg: Boolean;
begin
  Result := False;
end;

procedure TBasedxPrintStyle.RestoreDefaultGlyph;
begin
  FIsStyleGlyphAssigned := False;
  DesignerModified;
  DesignerUpdate(False);
end;

procedure TBasedxPrintStyle.RestoreDefaults;
begin
  AllowChangeHFText := True;
  AllowChangeMargins := True;
  AllowChangeOrientation := True;
  AllowChangePaper := True;
  AllowChangeScale := True;
  AllowCustomPaperSizes := True;
  PrinterPage.RestoreDefaults;

  FIsDescriptionAssigned := False;
  FIsStyleCaptionAssigned := False;
  FIsStyleGlyphAssigned := False;
end;

function TBasedxPrintStyle.GetAllowChangeHFText: Boolean;
begin
  Result := FAllowChangeHFText;
end;

function TBasedxPrintStyle.GetAllowChangeMargins: Boolean;
begin
  Result := FAllowChangeMargins;
end;

function TBasedxPrintStyle.GetAllowChangePaper: Boolean;
begin
  Result := FAllowChangePaper;
end;

function TBasedxPrintStyle.GetAllowChangeScale: Boolean;
begin
  Result := FAllowChangeScale;
end;

procedure TBasedxPrintStyle.SetAllowChangeHFText(Value: Boolean);
begin
  FAllowChangeHFText := Value;
end;

function TBasedxPrintStyle.GetAllowChangeOrientation: Boolean;
begin
  Result := FAllowChangeOrientation;
end;

function TBasedxPrintStyle.GetAllowCustomPaperSizes: Boolean;
begin
  Result := FAllowCustomPaperSizes;
end;

procedure TBasedxPrintStyle.SetAllowChangeMargins(Value: Boolean);
begin
  FAllowChangeMargins := Value;
end;

procedure TBasedxPrintStyle.SetAllowChangeOrientation(Value: Boolean);
begin
  FAllowChangeOrientation := Value;
end;

procedure TBasedxPrintStyle.SetAllowChangePaper(Value: Boolean);
begin
  FAllowChangePaper := Value;
end;

procedure TBasedxPrintStyle.SetAllowChangeScale(Value: Boolean);
begin
  FAllowChangeScale := Value;
end;

procedure TBasedxPrintStyle.SetAllowCustomPaperSizes(Value: Boolean);
begin
  FAllowCustomPaperSizes := Value;
end;

function TBasedxPrintStyle.CreatePrinterPage: TdxPrinterPage;
begin
  Result := GetPrinterPageClass.Create;
  InitializePrinterPage(Result);
end;

function TBasedxPrintStyle.GetPrinterPageClass: TdxPrinterPageClass;
begin
  Result := TdxPrintStylePrinterPage;
end;

procedure TBasedxPrintStyle.InitializePrinterPage(APrinterPage: TdxPrinterPage);
begin
  TdxPrintStylePrinterPage(APrinterPage).FPrintStyle := Self;
end;

procedure TBasedxPrintStyle.DoAfterPrinting;
begin
end;

procedure TBasedxPrintStyle.DoBeforePrinting;
begin
end;

procedure TBasedxPrintStyle.DoDestroy;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
end;

function TBasedxPrintStyle.IsSupportedPaper(const APaper: TdxPaperInfo): Boolean;
begin
  Result := True;
  if Assigned(FOnFilterPaper) then
  begin
    FOnFilterPaper(Self, APaper, Result);
    if not Result and (APaper.DMPaper = PrinterPage.DMPaper) then
      Result := True;
  end;
end;

procedure TBasedxPrintStyle.PageParamsChanged(AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  if StyleManager <> nil then
    StyleManager.PageParamsChanged(Self, AUpdateCodes);
end;

procedure TBasedxPrintStyle.InitializeDefaultStyleGlyph(ABitmap: TBitmap);
begin
end;

procedure TBasedxPrintStyle.StyleGlyphChanged(Sender: TObject);
begin
  FIsStyleGlyphAssigned := True;
  DesignerUpdate(False);
end;

function TBasedxPrintStyle.GetDescription: string;
begin
  if FIsDescriptionAssigned then
    Result := FDescription
  else
    Result := DefaultDescription;
end;

function TBasedxPrintStyle.GetIndex: Integer;
begin
  if StyleManager <> nil then
    Result := StyleManager.IndexOfStyle(Self)
  else
    Result := -1;
end;

function TBasedxPrintStyle.GetIsCurrentStyle: Boolean;
begin
  Result := (StyleManager <> nil) and (StyleManager.CurrentStyle = Self);
end;

function TBasedxPrintStyle.GetStyleCaption: string;
begin
  if FIsStyleCaptionAssigned then
    Result := FStyleCaption
  else
    Result := DefaultStyleCaption;
end;

function TBasedxPrintStyle.GetStyleGlyph: TBitmap;
begin
  if FIsStyleGlyphAssigned or (csLoading in ComponentState) then
    Result := FStyleGlyph
  else
    Result := DefaultStyleGlyph;
end;

function TBasedxPrintStyle.IsDescriptionStored: Boolean;
begin
  Result := FIsDescriptionAssigned and (FDescription <> DefaultDescription);
end;

function TBasedxPrintStyle.IsStyleCaptionStored: Boolean;
begin
  Result := FIsStyleCaptionAssigned and (FStyleCaption <> DefaultStyleCaption);
end;

function TBasedxPrintStyle.IsStyleGlyphStored: Boolean;
begin
  Result := FIsStyleGlyphAssigned and not dxPSUtl.dxAreGraphicsEqual(FStyleGlyph, DefaultStyleGlyph);
end;

procedure TBasedxPrintStyle.SetBuiltIn(Value: Boolean);
begin
  FBuiltIn := Value;
end;

procedure TBasedxPrintStyle.SetDescription(const Value: string);
begin
  if Description <> Value then
  begin
    FDescription := Value;
    FIsDescriptionAssigned := True;
  end;
end;

procedure TBasedxPrintStyle.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if (StyleManager <> nil) and (StyleManager.Images <> nil) then
      DesignerUpdate(False);
  end;
end;

procedure TBasedxPrintStyle.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  if FStyleManager = nil then Exit;
  if Value < 0 then Value := 0;
  if Value > StyleManager.Count - 1 then
    Value := StyleManager.Count - 1;
  CurIndex := GetIndex;
  if CurIndex <> Value then
    StyleManager.MoveStyle(CurIndex, Value);
end;

procedure TBasedxPrintStyle.SetIsCurrentStyle(Value: Boolean);
begin
  if Value then
    if not (csReading in ComponentState) and (StyleManager <> nil) then
      StyleManager.CurrentStyle := Self;
end;

procedure TBasedxPrintStyle.SetPrinterPage(Value: TdxPrinterPage);
begin
  PrinterPage.Assign(Value);
end;

procedure TBasedxPrintStyle.SetStyleCaption(const Value: string);
begin
  if StyleCaption <> Value then
  begin
    FStyleCaption := Value;
    FIsStyleCaptionAssigned := True;
  end;
end;

procedure TBasedxPrintStyle.SetStyleGlyph(Value: TBitmap);
begin
  FStyleGlyph.Assign(Value);
end;

procedure TBasedxPrintStyle.SetStyleManager(Value: TdxPrintStyleManager);
begin
  if FStyleManager <> Value then
  begin
    if FStyleManager <> nil then
      FStyleManager.RemoveStyle(Self);
    if Value <> nil then
      Value.InsertStyle(Self);
    SynchronizePixelsPerInch;
  end;
end;

procedure TBasedxPrintStyle.DesignerModified;
begin
  if StyleManager <> nil then StyleManager.DesignerModified;
end;

procedure TBasedxPrintStyle.DesignerUpdate(TheAll: Boolean);
begin
  if StyleManager <> nil then
    if TheAll then
      StyleManager.DesignerUpdate(nil)
    else
      StyleManager.DesignerUpdate(Self);
end;

function TBasedxPrintStyle.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TBasedxPrintStyle.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TBasedxPrintStyle.ReadData(Reader: TReader);
begin
  FBuiltIn := Reader.ReadBoolean;
end;

procedure TBasedxPrintStyle.ReadIsDescriptionAssigned(Reader: TReader);
begin
  FIsDescriptionAssigned := Reader.ReadBoolean;
end;

procedure TBasedxPrintStyle.ReadIsStyleCaptionAssigned(Reader: TReader);
begin
  FIsStyleCaptionAssigned := Reader.ReadBoolean;
end;

procedure TBasedxPrintStyle.ReadIsStyleGlyphAssigned(Reader: TReader);
begin
  FIsStyleGlyphAssigned := Reader.ReadBoolean;
end;

procedure TBasedxPrintStyle.SynchronizePixelsPerInch;
begin
  if StyleManager <> nil then
    TdxPrinterPageAccess(PrinterPage).PixelsPerInch := StyleManager.PixelsPerInch;
end;

procedure TBasedxPrintStyle.WriteData(Writer: TWriter);
begin
  Writer.WriteBoolean(FBuiltIn);
end;

procedure TBasedxPrintStyle.WriteIsDescriptionAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsDescriptionAssigned);
end;

procedure TBasedxPrintStyle.WriteIsStyleCaptionAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsStyleCaptionAssigned);
end;

procedure TBasedxPrintStyle.WriteIsStyleGlyphAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsStyleGlyphAssigned);
end;

{ TdxPrintStyleManager }

constructor TdxPrintStyleManager.Create(AOwner: TComponent);
begin
  inherited;
  FAutoHFTextEntries := CreateAutoHFTextEntries;
  FStyles := TList.Create;
  FWindowHandle := dxPSUtl.dxAllocatehWnd(WndProc);
end;

destructor TdxPrintStyleManager.Destroy;
begin
  try
    if AllowAutoSave then
    try
      SaveToFile(StorageName);
    finally
      FAlreadySaved := True;
    end;
  finally
    dxPSUtl.dxDeallocateHWnd(FWindowHandle);

    FreeAndNil(FDesigner);
    FreeAndNilStyles;
    if not IsDesigning and (RegistryPath <> '') then
      SaveToRegistry(RegistryPath);
    FreeAndNil(FAutoHFTextEntries);
    inherited;
  end;
end;

procedure TdxPrintStyleManager.Assign(Source: TPersistent);
begin
  if Source is TdxPrintStyleManager then
    with TdxPrintStyleManager(Source) do
    begin
      Self.AutoHFTextEntries := AutoHFTextEntries;
      Self.AssignStyles(TdxPrintStyleManager(Source));

      Self.FIsCloneStyleCaptionPrefixAssigned := FIsCloneStyleCaptionPrefixAssigned;
      Self.FIsTitleAssigned := FIsTitleAssigned;
    end
  else
    inherited;
end;

procedure TdxPrintStyleManager.Loaded;
begin
  inherited;
  if not IsDesigning and (RegistryPath <> '') then
    LoadFromRegistry(RegistryPath);

  if not FInternalStreaming and not IsDesigning and AutoSave then
    LoadFromFile(StorageName);
end;

procedure TdxPrintStyleManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Images then Images := nil;
    if AComponent = PageSetupDialog then PageSetupDialog := nil;
  end;
end;

procedure TdxPrintStyleManager.ReadState(Reader: TReader);
begin
  if FInternalStreaming then
    Reader.OnSetName := SetNameHandler;
  try
    inherited;
  finally
    if FInternalStreaming then
      Reader.OnSetName := nil;
  end;
end;

procedure TdxPrintStyleManager.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Styles[I].ChangeScale(M, D);
end;

procedure TdxPrintStyleManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('IsTitleAssigned', ReadIsTitleAssigned, WriteIsTitleAssigned,
    FIsTitleAssigned and (Title = ''));
  Filer.DefineProperty('IsCloneStyleCaptionPrefixAssigned',
    ReadIsCloneStyleCaptionPrefixAssigned, WriteIsCloneStyleCaptionPrefixAssigned,
    FIsCloneStyleCaptionPrefixAssigned and (CloneStyleCaptionPrefix = ''));
end;

procedure TdxPrintStyleManager.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Style: TBasedxPrintStyle;
begin
  for I := 0 to Count - 1 do
  begin
    Style := Styles[I];
    if (Root = Style.Owner) or (FInternalStreaming and (Root = Style.StyleManager)) then
      Proc(Style);
  end;
end;

procedure TdxPrintStyleManager.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited;
  if FStyles.IndexOf(Child) > -1 then
    (Child as TBasedxPrintStyle).Index := Order;
end;

procedure TdxPrintStyleManager.AutoHFTextEntriesChanged(Sender: TObject);
var
  Event: TdxEvent;
begin
  Event := TdxHFTextEntriesChangedEvent.Create(Self);
  dxPSProcessEvent(Event);
end;

procedure TdxPrintStyleManager.OnAutoHFTextEntryClick(Sender: TObject);
var
  S: string;
  Event: TdxEvent;
begin
  try
    S := AutoHFTextEntries[dxPSAutoHFTextMenuBuilderFactory.ActiveBuilder.ExtractAutoHFTextEntryIndexFromObj(Sender)];
    Event := TdxHFTextEntryChooseEvent.Create(Self, S);
    dxPSProcessEvent(Event);
  except
    Application.HandleException(Self);
  end;
end;

procedure TdxPrintStyleManager.OnEditAutoHFTextEntriesClick(Sender: TObject);
begin
  ShowAutoHFTextEntriesDlg;
end;

procedure TdxPrintStyleManager.WndProc(var Message: TMessage);
var
  I: Integer;
begin
  with Message do
  begin
    case Msg of
      WM_SETTINGCHANGE:
  //        if (PChar(message.lParam) = 'devices') then
        begin
          RereadDefaultPrinterPage;
          for I := 0 to Count - 1 do
            TdxPrintStylePrinterPage(Styles[I].PrinterPage).SynchronizeMeasurementUnits;
          DesignerModified;
        end;

      DXM_PS_PRINTSTYLELISTCHANGED:
        begin
          StyleListChanged;
          ChangeCurrentStyle;
        end;
    end;
    Result := DefWindowProc(FWindowHandle, Msg, WParam, LParam);
  end;
end;

procedure TdxPrintStyleManager.SetNameHandler(Reader: TReader; Component: TComponent;
  var Name: string);
begin
  if (Component is TBasedxPrintStyle) and (StyleByName(Name) <> nil) then
  begin
    if FLoadedExistingStyles = nil then
      FLoadedExistingStyles := TStringList.Create;
    FLoadedExistingStyles.AddObject(Name, Component);
    Name := '';
  end;
end;

procedure TdxPrintStyleManager.ReadIsCloneStyleCaptionPrefixAssigned(Reader: TReader);
begin
  FIsCloneStyleCaptionPrefixAssigned := Reader.ReadBoolean;
end;

procedure TdxPrintStyleManager.ReadIsTitleAssigned(Reader: TReader);
begin
  FIsTitleAssigned := Reader.ReadBoolean;
end;

procedure TdxPrintStyleManager.WriteIsCloneStyleCaptionPrefixAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsCloneStyleCaptionPrefixAssigned);
end;

procedure TdxPrintStyleManager.WriteIsTitleAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsTitleAssigned);
end;

function TdxPrintStyleManager.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TdxPrintStyleManager.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxPrintStyleManager.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TdxPrintStyleManager.AllowAutoSave: Boolean;
begin
  Result := AutoSave and not IsDesigning and IsDestroying and not FAlreadySaved;
end;

procedure TdxPrintStyleManager.DesignerUpdate(AStyle: TBasedxPrintStyle);
begin
  if IsDesigning and (Designer <> nil) then
    Designer.Update(AStyle);
end;

procedure TdxPrintStyleManager.DesignerModified;
begin
  if IsDesigning and (Designer <> nil) then
    Designer.Modified;
end;

procedure TdxPrintStyleManager.SetName(const NewName: TComponentName);
var
  OldName, ItemName, AName: string;
  P, I: Integer;
begin
  OldName := Name;
  inherited SetName(NewName);
  if IsDesigning and (Count > 0) then
  try
    if Designer <> nil then Designer.BeginUpdate;
    try
      for I := 0 to Count - 1 do
      begin
        ItemName := Styles[I].Name;
        P := Pos(OldName, ItemName);
        if P = 0 then
          AName := Name + ItemName
        else
          AName := Copy(ItemName, 1, P - 1) + Name +
            Copy(ItemName, P + Length(OldName), Length(ItemName) - P - Length(OldName) + 1);
        Styles[I].Name := AName;
      end;
    finally
      if Designer <> nil then Designer.EndUpdate;
    end;
  except
    on EComponentError do ; {Ignore rename errors }
  end;
end;

procedure TdxPrintStyleManager.SetPageSetupDialog(Value: TdxPageSetupDialog);
begin
  if FPageSetupDialog <> Value then
  begin
    FPageSetupDialog := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

function TdxPrintStyleManager.GetStyle(Index: Integer): TBasedxPrintStyle;
begin
  Result := TBasedxPrintStyle(FStyles[Index]);
end;

function TdxPrintStyleManager.GetTitle: string;
begin
  if FIsTitleAssigned then
    Result := FTitle
  else
    Result := DefaultTitle;
end;

function TdxPrintStyleManager.IsAutoHFTextEntriesStored: Boolean;
begin
  Result := not AutoHFTextEntries.Equals(DefaultAutoHFTextEntries);
end;

function TdxPrintStyleManager.IsCloneStyleCaptionPrefixStored: Boolean;
begin
  Result := FIsCloneStyleCaptionPrefixAssigned and (FCloneStyleCaptionPrefix <> DefaultCloneStyleCaptionPrefix);
end;

function TdxPrintStyleManager.IsTitleStored: Boolean;
begin
  Result := FIsTitleAssigned and (FTitle <> DefaultTitle);
end;

procedure TdxPrintStyleManager.SetStyle(Index: Integer; Value: TBasedxPrintStyle);
begin
  Styles[Index].Assign(Value);
end;

procedure TdxPrintStyleManager.SetTitle(const Value: string);
begin
  if Title <> Value then
  begin
    FTitle := Value;
    FIsTitleAssigned := True;
  end;
end;

procedure TdxPrintStyleManager.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    if FImages <> nil then
      FImages.FreeNotification(Self);
    DesignerUpdate(nil);
  end;
end;

procedure TdxPrintStyleManager.AssignStyles(Source: TdxPrintStyleManager);
var
  I: Integer;
  Style: TBasedxPrintStyle;
begin
  BeginUpdate;
  try
    Clear;
    Images := Source.Images;
    for I := 0 to Source.Count - 1 do
    begin
      Style := Source[I];
      AddStyle(Style.StyleClass).Assign(Style);
    end;
  finally
    EndUpdate;
  end;
end;

function TdxPrintStyleManager.IndexOfStyle(Value: TBasedxPrintStyle): Integer;
begin
  Result := FStyles.IndexOf(Value);
end;

function TdxPrintStyleManager.StyleByCaption(const ACaption: string): TBasedxPrintStyle;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if dxSameText(ACaption, Styles[I].StyleCaption) then
    begin
      Result := Styles[I];
      Exit;
    end;
  Result := nil;
end;

function TdxPrintStyleManager.StyleByName(const AName: string): TBasedxPrintStyle;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Styles[I];
    if CompareText(AName, Result.Name) = 0 then Exit;
  end;
  Result := nil;
end;

procedure TdxPrintStyleManager.Delete(Index: Integer);
var
  Style: TBasedxPrintStyle;
begin
  if (Index > -1) and (Index < Count) then
  begin
    Style := Styles[Index];
    Style.Free;
  end;
end;

procedure TdxPrintStyleManager.Clear;
begin
  BeginUpdate;
  try
    while Count > 0 do Delete(Count - 1);
  finally
    EndUpdate;
  end;
end;

function TdxPrintStyleManager.NonBuiltInsExists: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if not Styles[I].BuiltIn then Exit;
  Result := False;
end;

procedure TdxPrintStyleManager.DeleteNonBuiltIns;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Count - 1 downto 0 do
      if not Styles[I].BuiltIn then Delete(I);
  finally
    EndUpdate;
  end;
end;

function TdxPrintStyleManager.BeginClone(AIndex: Integer): TBasedxPrintStyle;
var
  StyleClass: TdxPrintStyleClass;
begin
  Result := nil;
  if (AIndex < -1) or (AIndex > Count - 1) then Exit;
  if AIndex = -1 then
    StyleClass := dxDefaultPrintStyleClass
  else
    StyleClass := Styles[AIndex].StyleClass;
  if StyleClass = nil then Exit;
  BeginUpdate;
  Result := AddStyle(StyleClass);
  Result.Index := AIndex + 1;
  Include(Result.FState, pssCopy);
  if AIndex > -1 then
  begin
    Result.Assign(Styles[AIndex]);
    SetNewStyleCaption(Result, AIndex);
  end;
end;

procedure TdxPrintStyleManager.EndClone(AStyle: TBasedxPrintStyle);
begin
//  CurrentStyle := AStyle;
  if IsDesigning then
    AStyle.Name := dxPSPrintStyleUniqueName(Self, AStyle);
  EndUpdate;
  Exclude(AStyle.FState, pssCopy);
end;

procedure TdxPrintStyleManager.SetNewStyleCaption(AStyle: TBasedxPrintStyle;
  AIndex: Integer);

  function CheckName(const Source: string): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Count - 1 do
      if Styles[I] <> AStyle then
      begin
        Result := not dxSameStr(Source, Styles[I].StyleCaption);
        if not Result then Exit;
      end;
    Result := True;
  end;

const
  MaskCount = 4;
  Mask: array[0..MaskCount - 1] of string = ('(%d) ', '(%d)', '%d ', '%d');
var
  S, S2: string;
  OkName: Boolean;
  I, K: Integer;
begin
  OkName := False;
  if not OkName then
  begin
    S := CloneStyleCaptionPrefix + Styles[AIndex].StyleCaption;
    for K := 0 to MaskCount - 1 do
    begin
      I := Pos(Mask[K], S);
      if I > 0 then
      begin
        System.Delete(S, I, Length(Mask[K]));
        Break;
      end;
    end;
    if Length(S) > dxMaxStyleCaption then SetLength(S, dxMaxStyleCaption);
    OkName := CheckName(S);
    if not OkName then
    begin
      S2 := CloneStyleCaptionPrefix + Styles[AIndex].StyleCaption;
      if (Length(S2) > dxMaxStyleCaption) then SetLength(S2, dxMaxStyleCaption);
      I := 2;
      while not OkName and (I < MaxInt) do
      begin
        try
          S := Format(S2, [I]);
        except
          S := S2;
        end;
        if Length(S) > dxMaxStyleCaption then
          SetLength(S, dxMaxStyleCaption);
        if dxSameStr(S, S2) then
        begin
          AStyle.StyleCaption := S;
          Exit;
        end;
        OkName := CheckName(S);
        Inc(I);
      end;
    end;
  end;
  if OkName then AStyle.StyleCaption := S;
end;

function TdxPrintStyleManager.AddStyle(AStyleClass: TdxPrintStyleClass): TBasedxPrintStyle;
begin
  Result := AddStyleEx(AStyleClass, Self.Owner);
end;

function TdxPrintStyleManager.AddStyleEx(AStyleClass: TdxPrintStyleClass;
  AOwner: TComponent): TBasedxPrintStyle;
begin
  Result := nil;
  if AStyleClass = nil then
    Exit;
  Result := AStyleClass.Create(AOwner);
  Result.StyleManager := Self;
end;

procedure TdxPrintStyleManager.ResyncCurrentStyle(AIndex: Integer);
begin
  if AIndex > Count - 1 then
    AIndex := Count - 1;
  if AIndex < 0 then
  begin
    FCurrentStyle := nil;
    ChangeCurrentStyle;
  end
  else
    CurrentStyle := Styles[AIndex];
end;
procedure TdxPrintStyleManager.FreeAndNilStyles;
begin
  Clear;
  FreeAndNil(FStyles);
end;

procedure TdxPrintStyleManager.InsertStyle(Value: TBasedxPrintStyle);
begin
  FStyles.Add(Value);
  Value.FStyleManager := Self;
  if Count = 1 then
    CurrentStyle := Value;
  StyleListChanged;
end;

procedure TdxPrintStyleManager.MoveStyle(ACurIndex, ANewIndex: Integer);
begin
  FStyles.Move(ACurIndex, ANewIndex);
  DesignerUpdate(nil);
end;

procedure TdxPrintStyleManager.RemoveStyle(Value: TBasedxPrintStyle);
var
  Index: Integer;
begin
  if FCurrentStyle = Value then
    Index := Value.Index
  else
    Index := -1;
  FStyles.Remove(Value);
  Value.FStyleManager := nil;
  if Index <> -1 then
    ResyncCurrentStyle(Index);
  StyleListChanged;
end;

function TdxPrintStyleManager.GetCloneStyleCaptionPrefix: string;
begin
  if FIsCloneStyleCaptionPrefixAssigned then
    Result := FCloneStyleCaptionPrefix
  else
    Result := DefaultCloneStyleCaptionPrefix;
end;

function TdxPrintStyleManager.GetCount: Integer;
begin
  Result := FStyles.Count;
end;

procedure TdxPrintStyleManager.RestoreDefaultAutoHFTextEntries;
begin
  AutoHFTextEntries.Assign(DefaultAutoHFTextEntries);
end;

procedure TdxPrintStyleManager.RestoreDefaults;
begin
  BeginUpdate;
  try
    DoRestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TdxPrintStyleManager.RestoreDefaultStyles;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Styles[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TdxPrintStyleManager.LoadFromFile(const AName: string);
var
  AStream: TFileStream;
begin
  if (AName <> '') and FileExists(AName) then
  begin
    AStream := TFileStream.Create(AName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxPrintStyleManager.LoadFromStream(AStream: TStream);

  procedure CheckExistingStyles(AExistingStyleCount: Integer);
  var
    I, ALoadedExistingStyleIndex: Integer;
    AStyle: TBasedxPrintStyle;
  begin
    for I := AExistingStyleCount - 1 downto 0 do
    begin
      if FLoadedExistingStyles = nil then
        ALoadedExistingStyleIndex := -1
      else
        ALoadedExistingStyleIndex := FLoadedExistingStyles.IndexOf(Styles[I].Name);
      if ALoadedExistingStyleIndex = -1 then
        Styles[I].Free
      else
      begin
        AStyle := TBasedxPrintStyle(FLoadedExistingStyles.Objects[ALoadedExistingStyleIndex]);
        Styles[I].Assign(AStyle);
        AStyle.Free;
      end;
    end;
    FreeAndNil(FLoadedExistingStyles);
  end;

var
  Version: Integer;
  M: TMemoryStream;
  AExistingStyleCount, I: Integer;
  Style: TBasedxPrintStyle;
  CurrentStyleIndex: Integer;
begin
  AStream.ReadBuffer(Version , SizeOf(Integer));
  if Version <> Self.Version then Exit;
  BeginUpdate;
  try
    M := TMemoryStream.Create;
    try
      AExistingStyleCount := Count;
      FInternalStreaming := True;
      try
        M.WriteComponent(Self);
        try
          try
            AStream.ReadBuffer(CurrentStyleIndex , SizeOf(Integer));
            AStream.ReadComponent(Self);
            Self.CurrentStyleIndex := CurrentStyleIndex;
          finally
            CheckExistingStyles(AExistingStyleCount);
          end;
        except
          Clear;  // links to styles in other components will be lost
          M.Position := 0;
          M.ReadComponent(Self);
          Application.HandleException(Self);
        end;
      finally
        Loaded;
        for I := 0 to Count - 1 do
        begin
          Style := Styles[I];
          if Style.Owner = Self then
          begin
            RemoveComponent(Style);
            Owner.InsertComponent(Style);
          end;
          Style.Loaded;
        end;
        FInternalStreaming := False;
      end;
    finally
      M.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxPrintStyleManager.SaveToFile(const AName: string);
var
  AStream: TFileStream;
begin
  if ValidateFileName(AName) then
  begin
    AStream := TFileStream.Create(AName, fmCreate or fmShareDenyWrite);
    try
      SaveToStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxPrintStyleManager.SaveToStream(AStream: TStream);
var
  ACurrentStyleIndex: Integer;
begin
  FInternalStreaming := True;
  try
    AStream.WriteBuffer(Version , SizeOf(Integer));
    ACurrentStyleIndex := CurrentStyleIndex;
    AStream.WriteBuffer(ACurrentStyleIndex , SizeOf(Integer));
    AStream.WriteComponent(Self);
  finally
    FInternalStreaming := False;
  end;
end;

function TdxPrintStyleManager.CreateAutoHFTextEntries: TStrings;
begin
  FAutoHFTextEntries := TStringList.Create;
  with TStringList(FAutoHFTextEntries) do
  begin
    Duplicates := dupIgnore;
    Assign(DefaultAutoHFTextEntries);
    OnChange := AutoHFTextEntriesChanged;
  end;
  Result := FAutoHFTextEntries;
end;

procedure TdxPrintStyleManager.DoRestoreDefaults;
begin
  RestoreDefaultAutoHFTextEntries;
  RestoreDefaultStyles;
  AutoSave := False;
  FIsCloneStyleCaptionPrefixAssigned := False;
  FIsTitleAssigned := False;
end;

procedure TdxPrintStyleManager.ChangeCurrentStyle;
begin
  if (FUpdateCount = 0) and not IsLoading and not IsDestroying then
    if Assigned(FOnChangeCurrentStyle) then FOnChangeCurrentStyle(Self);
end;

procedure TdxPrintStyleManager.StyleListChanged;
var
  Event: TdxEvent;
begin
  if (FUpdateCount = 0) and not IsLoading and not IsDestroying then
  begin
    if Assigned(FOnStyleListChanged) then FOnStyleListChanged(Self);
    Event := TdxSMStyleListChangedEvent.Create(Self);
    dxPSProcessEvent(Event);
  end;
end;

procedure TdxPrintStyleManager.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxPrintStyleManager.EndUpdate;
begin
  if FUpdateCount <> 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      StyleListChanged;
      ChangeCurrentStyle;
    end;
  end;
end;

function TdxPrintStyleManager.DefaultCloneStyleCaptionPrefix: string;
begin
  Result := cxGetResourceString(@sdxCloneStyleCaptionPrefix);
end;

function TdxPrintStyleManager.DefaultTitle: string;
begin
  Result := cxGetResourceString(@sdxDefinePrintStylesCaption);
end;

procedure TdxPrintStyleManager.PageParamsChanged(APrintStyle: TBasedxPrintStyle;
  AUpdateCodes: TdxPrinterPageUpdateCodes);
var
  Event: TdxEvent;
begin
  Event := TdxSMPageParamsChangedEvent.Create(Self, APrintStyle, AUpdateCodes);
  dxPSProcessEvent(Event);
end;

procedure TdxPrintStyleManager.SetAutoHFTextEntries(Value: TStrings);
begin
  AutoHFTextEntries.Assign(Value);
end;

procedure TdxPrintStyleManager.SetCloneStyleCaptionPrefix(const Value: string);
begin
  if CloneStyleCaptionPrefix <> Value then
  begin
    FCloneStyleCaptionPrefix := Value;
    FIsCloneStyleCaptionPrefixAssigned := True;
  end;
end;

procedure TdxPrintStyleManager.SetCurrentStyle(Value: TBasedxPrintStyle);
begin
  if (FCurrentStyle <> Value) and (IndexOfStyle(Value) <> -1) then
  begin
    FCurrentStyle := Value;
    PageParamsChanged(Value, ucAll);
    ChangeCurrentStyle;
    DesignerUpdate(Value);//nil);
  end;
end;

function TdxPrintStyleManager.GetCurrentStyleIndex: Integer;
begin
  if CurrentStyle <> nil then
    Result := CurrentStyle.Index
  else
    Result := -1;
end;

function TdxPrintStyleManager.GetRegistryPath: string;
begin
  if dxPSEngine.RealRegistryPath <> '' then
    Result := dxPSEngine.RealRegistryPath + sdxAutoHFTextEntries + '\' + Name
  else
    Result := '';
end;

procedure TdxPrintStyleManager.SetCurrentStyleIndex(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > Count - 1 then
    Value := Count - 1;
  if Value <> CurrentStyleIndex then
    CurrentStyle := Styles[Value];
end;

procedure TdxPrintStyleManager.BuildAutoHFTextEntriesMenu(ARootItem: TComponent;
  AData: Pointer; AIncludeSetupAutoHFTextEntriesItem: Boolean = True);
var
  MenuBuilder: TAbstractdxPSAutoHFTextMenuBuilder;
begin
  MenuBuilder := dxPSAutoHFTextMenuBuilderFactory.ActiveBuilder.Create;
  try
    try
      MenuBuilder.BuildAutoHFTextEntriesMenu(ARootItem, AData, AIncludeSetupAutoHFTextEntriesItem,
        AutoHFTextEntries, OnAutoHFTextEntryClick, OnEditAutoHFTextEntriesClick);
    except
      Application.HandleException(Self);
    end;
  finally
    MenuBuilder.Free;
  end;
end;

procedure TdxPrintStyleManager.RefreshAutoHFTextEntries;
begin
  FreeAndNil(FAutoHFTextEntries);
end;

function TdxPrintStyleManager.ShowAutoHFTextEntriesDlg: Boolean;
begin
  Result := dxShowAutoTextDlg(AutoHFTextEntries);
end;

procedure TdxPrintStyleManager.LoadFromIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  dxLoadStrings(AIniFile, ASectionName, AutoHFTextEntries);
end;

procedure TdxPrintStyleManager.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  dxSaveStrings(AIniFile, ASectionName, AutoHFTextEntries);
end;

procedure TdxPrintStyleManager.LoadFromRegistry(const APath: string);
var
  AIniFile: TCustomIniFile;
begin
  AIniFile := TRegistryIniFile.Create('');
  try
    LoadFromIniFile(AIniFile, APath);
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPrintStyleManager.SaveToRegistry(const APath: string);
var
  AIniFile: TCustomIniFile;
begin
  AIniFile := TRegistryIniFile.Create('');
  try
    SaveToIniFile(AIniFile, APath);
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPrintStyleManager.DefinePrintStylesDlg(
  out APreviewBtnClicked, APrintBtnClicked: Boolean);
begin
  APreviewBtnClicked := False;
  APrintBtnClicked := False;
  dxDefinePrintStylesDlg(Self, Title, APreviewBtnClicked, APrintBtnClicked);

  DesignerUpdate(nil);
  PostMessage(FWindowHandle, DXM_PS_PRINTSTYLELISTCHANGED, 0, 0);
end;

{ TAbstractdxStyleManagerDesigner }

constructor TAbstractdxStyleManagerDesigner.Create(AStyleManager: TdxPrintStyleManager);
begin
  inherited Create;
  FStyleManager := AStyleManager;
  if FStyleManager <> nil then
    FStyleManager.FDesigner := Self;
end;

destructor TAbstractdxStyleManagerDesigner.Destroy;
begin
  if FStyleManager <> nil then
    FStyleManager.FDesigner := nil;
  inherited Destroy;
end;

{ functions }

function DateFormats: TStrings;
begin
  if FDateFormats = nil then
  begin
    FDateFormats := TStringList.Create;
    if Assigned(dxGetDateFormatsProc) then
      dxGetDateFormatsProc(FDateFormats);
  end;
  Result := FDateFormats;
end;

function TimeFormats: TStrings;
begin
  if FTimeFormats = nil then
  begin
    FTimeFormats := TStringList.Create;
    if Assigned(dxGetTimeFormatsProc) then
      dxGetTimeFormatsProc(FTimeFormats);
  end;
  Result := FTimeFormats;
end;

procedure RefreshDateFormats;
begin
  FreeAndNil(FDateFormats);
end;

procedure RefreshTimeFormats;
begin
  FreeAndNil(FTimeFormats);
end;

procedure DeleteLeadingGarbage(var Source: string);
begin
  while (Length(Source) > 0) and ((Source[1] = '.') or (Source[1] = ',') or (Source[1] = ' ')) do
    Delete(Source, 1, 1);
end;

function ExtractLongMonthFormat(AnExcludeDelimiters: Boolean): string;
const
  ValidChars: array[Boolean] of string = (' M:\.,/', ' M:\');
var
  I: Integer;
  Ch: char;
begin
  Result := '';
  for I := 1 to Length(dxFormatSettings.LongDateFormat) do
  begin
    Ch := dxFormatSettings.LongDateFormat[I];
    if Pos(Ch, ValidChars[AnExcludeDelimiters]) <> 0 then
      Result := Result + Ch;
  end;
  Result := Trim(Result);
  DeleteLeadingGarbage(Result);
end;

function ReducedLongDayFormat: string;
var
  P: Integer;
begin
  Result := dxFormatSettings.LongDateFormat;
  repeat
    P := Pos('dddd', Result);
    if P <> 0 then Delete(Result, P, 4);
  until P = 0;

  Result := Trim(Result);
  DeleteLeadingGarbage(Result);
end;

procedure dxGetDateFormats(AStrings: TStrings);
var
  S: string;
  P: Integer;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
{1} AStrings.Add(dxFormatSettings.ShortDateFormat);

    S := Trim(dxFormatSettings.LongDateFormat);
    if Pos('dddd', S) = 0 then S := 'dddd, ' + S;
{2} AStrings.Add(S);

{3} AStrings.Add(ReducedLongDayFormat);

    S := dxFormatSettings.ShortDateFormat;
    P := Pos('yyyy', S);
    if P <> 0 then
      Delete(S, P, 2)
    else
    begin
      P := Pos('yy', S);
      if P <> 0 then Insert('yy', S, P);
    end;
{4} AStrings.Add(S);

{5} AStrings.Add('yyyy-MM-dd');
{6} AStrings.Add('d-MMM-yy');

    S := dxFormatSettings.ShortDateFormat;
    if dxFormatSettings.DateSeparator <> '/' then
      S := ReplaceSubStr(S, dxFormatSettings.DateSeparator, '/')
    else
      S := ReplaceSubStr(S, dxFormatSettings.DateSeparator, '.');
{7} AStrings.Add(S);

{8} AStrings.Add(ExtractLongMonthFormat(False) + ' yyyy');
{9} AStrings.Add('d MMMM yyyy');
{10}AStrings.Add(ExtractLongMonthFormat(True) + ' yy');
{11}AStrings.Add('MMM-yy');
  finally
    AStrings.EndUpdate;
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}

procedure dxGetTimeFormats(AStrings: TStrings);
const
  HourFormats: array[Boolean] of string = ('hh', 'h');
var
  HourFormat: string;
begin
  HourFormat := HourFormats[StrToIntDef(GetLocaleChar(GetThreadLocale, LOCALE_ITLZERO, '0'), 0) = 0];
  with AStrings do
  begin
    BeginUpdate;
    try
      Clear;
      Add(HourFormat + ':mm tt');
      Add(HourFormat + ':mm:ss tt');
      Add(UpperCase(HourFormat) + ':mm');
      Add(UpperCase(HourFormat) + ':mm:ss');
    finally
      EndUpdate;
    end;
  end;
end;

{$WARN SYMBOL_PLATFORM ON}

function PageNumberFormats: TStrings;
begin
  if FPageNumberFormats = nil then
  begin
    FPageNumberFormats := TStringList.Create;
    FPageNumberFormats.AddObject('1, 2, 3, 4, 5, ...', TObject(Integer(pnfNumeral)));
    FPageNumberFormats.AddObject('a, b, c, d, e, ...', TObject(Integer(pnfChars)));
    FPageNumberFormats.AddObject('A, B, C, D, E, ...', TObject(Integer(pnfUpperChars)));
    FPageNumberFormats.AddObject('i, ii, iii, iv, v, ...', TObject(Integer(pnfRoman)));
    FPageNumberFormats.AddObject('I, II, III, IV, V, ...', TObject(Integer(pnfUpperRoman)));
  end;
  Result := FPageNumberFormats;
end;

function System_GetFormatedDate(const ASystemTime: TSystemTime; const AFormat: string): string;
var
  DefaultLCID: LCID;
  BufferSize: Integer;
  Buffer: array[0..255] of Char;
begin
  if AFormat <> '' then
  begin
    DefaultLCID := GetThreadLocale;
    FillChar(Buffer, SizeOf(Buffer), 0);
    BufferSize := GetDateFormat(DefaultLCID, 0, @ASystemTime, PChar(AFormat), @Buffer, Length(Buffer));
    if BufferSize = 0 then
      Result := ''
    else
      Result := Buffer;
 end
 else
   Result := '';
end;

function System_GetFormatedTime(const ASystemTime: TSystemTime; const AFormat: string): string;
var
  DefaultLCID: LCID;
  BufferSize: Integer;
  Buffer: array[0..255] of Char;
begin
  if AFormat <> '' then
  begin
    DefaultLCID := GetThreadLocale;
    FillChar(Buffer, SizeOf(Buffer), 0);
    BufferSize := GetTimeFormat(DefaultLCID, 0, @ASystemTime, PChar(AFormat), @Buffer, Length(Buffer));
    if BufferSize = 0 then
      Result := ''
    else
      Result := Buffer;
    Result := Buffer;
 end
 else
   Result := '';
end;

function GetFormatedDate(const ADateTime: TDateTime; const AFormat: string): string;
var
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(ADateTime, SystemTime);
  Result := System_GetFormatedDate(SystemTime, AFormat);
end;

function GetFormatedTime(const ADateTime: TDateTime; const AFormat: string): string;
var
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(ADateTime, SystemTime);
  Result := System_GetFormatedTime(SystemTime, AFormat);
end;

procedure GetFormatedDateStrings(const ADateTime: TDateTime;
  ADateFormats, AFormatedStrings: TStrings);
var
  I: Integer;
begin
  with AFormatedStrings do
  begin
    BeginUpdate;
    try
      for I := 0 to ADateFormats.Count - 1 do
        Add(GetFormatedDate(ADateTime, ADateFormats[I]));
    finally
      EndUpdate;
    end;
  end;
end;

procedure GetFormatedTimeStrings(const ADateTime: TDateTime;
  ATimeFormats, AFormatedStrings: TStrings);
var
  I: Integer;
begin
  with AFormatedStrings do
  begin
    BeginUpdate;
    try
      for I := 0 to ATimeFormats.Count - 1 do
        Add(GetFormatedTime(ADateTime, ATimeFormats[I]));
    finally
      EndUpdate;
    end;
  end;
end;

function DefaultAutoHFTextEntries: TStrings;
begin
  if FDefaultAutoHFTextEntries = nil then
  begin
    FDefaultAutoHFTextEntries := TStringList.Create;
    TStringList(FDefaultAutoHFTextEntries).Duplicates := dupIgnore;
    if Assigned(dxGetAutoHFTextEntriesProc) then
      dxGetAutoHFTextEntriesProc(FDefaultAutoHFTextEntries);
  end;
  Result := FDefaultAutoHFTextEntries;
end;

procedure dxGetAutoHFTextEntries(AStrings: TStrings);
var
  F: TdxHFCustomFunction;
  S: string;
begin
  with AStrings do
  begin
    //, -Page #-,
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFPageNumberFunction];
    if F <> nil then
      Add(dxHFFunctionSeparator + ' ' + '-' + F.TemplateString + '-' + ' ' + dxHFFunctionSeparator);

    // Author, Page #, Date
    S := '';
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFUserNameFunction];
    if F <> nil then
      S := F.TemplateString;
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFPageNumberFunction];
    if F <> nil then
      S := S + dxHFFunctionSeparator + ' ' + DropAmpersand(cxGetResourceString(@sdxPage)) + ' ' + F.TemplateString;
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFDateFunction];
    if F <> nil then
      S := S + dxHFFunctionSeparator + ' ' + F.TemplateString;
    Add(S);

    // Confidential, Page #, Date
    S := cxGetResourceString(@sdxConfidential);
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFPageNumberFunction];
    if F <> nil then
      S := S + dxHFFunctionSeparator + ' ' + DropAmpersand(cxGetResourceString(@sdxPage)) + ' ' + F.TemplateString;
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFDateFunction];
    if F <> nil then
      S := S + dxHFFunctionSeparator + ' ' + F.TemplateString;
    Add(S);

    // Created By
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFUserNameFunction];
    if F <> nil then
      Add(cxGetResourceString(@sdxCreatedBy) + F.TemplateString);

    // Created On
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFDateTimeFunction];
    if F <> nil then
      Add(cxGetResourceString(@sdxCreatedOn) + F.TemplateString);

    // Printed By
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFUserNameFunction];
    if F <> nil then
      Add(cxGetResourceString(@sdxPrintedBy) + F.TemplateString);

    // Printed On
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFDateTimeFunction];
    if F <> nil then
      Add(cxGetResourceString(@sdxPrintedOn) + F.TemplateString);

    // Last Printed
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFDateTimeFunction];
    if F <> nil then
      Add(cxGetResourceString(@sdxLastPrinted) + F.TemplateString);

    // Page #
    F := dxHFFunctionLibrary.FuncsByClass[TdxHFPageOfPagesFunction];
    if F <> nil then
      Add(DropAmpersand(cxGetResourceString(@sdxPage)) + ' ' + F.TemplateString);
  end;
end;

procedure TdxfmPageSetupDialog.pgctrlMainPageChanging(Sender: TObject;
  NewPage: TcxTabSheet; var AllowChange: Boolean);
var
  InvalidMarginControl: TWinControl;
begin
  if NewPage.PageIndex = 1 then
  begin
    AllowChange := ValidateMargins(InvalidMarginControl);
    if not AllowChange then
    begin
      Beep;
      case MarginsMessageDlg(cxGetResourceString(@sdxInvalidMarginsMessage)) of
        mrYes:
          FixupMargins;
        mrNo:
          RestoreOriginalMargins;
        else
          Exit;
      end;
      AllowChange := True;
    end;
  end;
end;

procedure TdxfmPageSetupDialog.lbxPaperTypeDrawItem(AControl: TcxListBox;
  ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
begin
  ACanvas.FillRect(ARect, ACanvas.Brush.Color);
  InflateRect(ARect, -ScaleFactor.Apply(2), -ScaleFactor.Apply(1));
  cxDrawImage(ACanvas, cxRectSetWidth(ARect, ScaleFactor.Apply(ilPaperTypes.Width)), nil, ilPapers,
    Ord(dxPrintDevice.IsEnvelopePaper(AIndex)), Enabled, nil, ScaleFactor);
  Inc(ARect.Left, ScaleFactor.Apply(ilPaperTypes.Width) + ScaleFactor.Apply(2));
  if not Enabled then
    ACanvas.Font.Color := clGrayText;
  ACanvas.Canvas.TextRect(ARect, ARect.Left, ARect.Top +
    (ARect.Bottom - ARect.Top - ACanvas.TextHeight(AControl.Items[AIndex])) div 2,
    AControl.Items[AIndex]);
  ACanvas.Font.Color := clWindowText;
end;

procedure TdxfmPageSetupDialog.MarginValueChanged(Sender: TObject);
begin
  if not FControlsUpdating then
  begin
    MarginExit(Sender);
    CheckModified;
  end;
end;

procedure TdxfmPageSetupDialog.cbxPaperSourcePropertiesDrawItem(
  AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer;
  const ARect: TRect; AState: TOwnerDrawState);
var
  R: TRect;
begin
  R := ARect;
  ACanvas.FillRect(R, ACanvas.Brush.Color);
  InflateRect(R, -ScaleFactor.Apply(2), -ScaleFactor.Apply(1));
  cxDrawImage(ACanvas, cxRectSetWidth(R, ScaleFactor.Apply(ilBins.Width)), nil, ilBins,
    Integer(dxPrintDevice.IsAutoSelectBin(AIndex)), True, nil, ScaleFactor);
  Inc(R.Left, ScaleFactor.Apply(ilPaperTypes.Width) + ScaleFactor.Apply(2));
  cxDrawText(ACanvas.Handle, AControl.Properties.Items[AIndex], R, DT_VCENTER or DT_SINGLELINE);
end;

procedure TdxfmPageSetupDialog.memHeaderCenterPropertiesChange(Sender: TObject);
begin
  if not FControlsUpdating then
  begin
    Page.PageHeader.LeftTitle.Text := memHeaderLeft.Text;
    Page.PageHeader.RightTitle.Text := memHeaderRight.Text;
    Page.PageHeader.CenterTitle.Text := memHeaderCenter.Text;

    Page.PageFooter.LeftTitle.Text := memFooterLeft.Text;
    Page.PageFooter.RightTitle.Text := memFooterRight.Text;
    Page.PageFooter.CenterTitle.Text := memFooterCenter.Text;
    CheckModified;
  end;
end;

initialization
  dxPSRegisterPrintStyle(TBasedxPrintStyle, False);
  if dxDefaultPrintStyleClass = nil then
    dxDefaultPrintStyleClass := TBasedxPrintStyle;

  dxGetDateFormatsProc := dxGetDateFormats;
  dxGetTimeFormatsProc := dxGetTimeFormats;
  dxGetAutoHFTextEntriesProc := dxGetAutoHFTextEntries;

finalization
  dxPSUnregisterAllPrintStyles;

  FreeAndNil(FDefaultAutoHFTextEntries);
  FreeAndNil(FDateFormats);
  FreeAndNil(FPageNumberFormats);
  FreeAndNil(FTimeFormats);

end.
