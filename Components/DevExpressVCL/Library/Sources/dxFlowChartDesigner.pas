{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFlowChart                                         }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSFLOWCHART AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxFlowChartDesigner;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxflchrt, cxGraphics, cxControls, cxLookAndFeels, cxTextEdit,
  cxLookAndFeelPainters, dxRibbonSkins, cxClasses,
  dxRibbon, dxBar, dxFcEdit, dxRibbonForm, ImgList, cxImageList, Generics.Collections,
  dxBarApplicationMenu, ActnList, dxDockControl, dxDockPanel, cxPC, dxCore,
  dxGallery, dxGalleryControl, dxFlowChartShapes, cxContainer, cxEdit,
  dxGDIPlusClasses, cxImage, Menus, dxCoreClasses, dxCoreGraphics, dxTypeHelpers,
  dxBarExtItems, cxFontNameComboBox, cxBarEditItem, cxDropDownEdit,
  dxRibbonColorGallery, dxScreenTip, dxColorDialog, dxRibbonGallery,
  ExtCtrls, cxMaskEdit, cxImageComboBox, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, dxLayoutContainer, cxSpinEdit, StdCtrls, cxButtons, cxMemo, dxLayoutControl,
  dxFlowChartColorPicker;

type
  TdxDesignerFlowChart = class;

  TdxFlowChartDesignerMode = (dmStandalone, dmChartDesigner);

  TdxFlowChartForEachStandardShapeProc = procedure(AShapeType: TdxFcShapeType) of object;
  TdxFlowChartForEachSelectedItemProc = reference to procedure(AItem: TdxFcItem);
  TdxFlowChartForEachSelectedObjectProc = reference to procedure(AObject: TdxFcObject);
  TdxFlowChartForEachSelectedConnectionProc = reference to procedure(AConnection: TdxFcConnection);
  TdxFlowChartGroupControlsActionProc = reference to procedure (AControl: TWinControl);
  TdxFlowChartCompareConnectionValueFunc = reference to function (AConnection: TdxFcConnection): Boolean;
  TdxFlowChartCompareObjectValueFunc = reference to function (AObject: TdxFcObject): Boolean;

  TcxFlowChartDesignerMoreShapesDropDownArrowImage = class(TcxImage)
  private
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  end;

  { TdxFlowChartShapeInfo }

  TdxFlowChartShapeInfo = class
  private
    FAdvancedShape: TdxFlowChartObjectAdvancedShape;
    FShapeType: TdxFcShapeType;
  protected
    property AdvancedShape: TdxFlowChartObjectAdvancedShape read FAdvancedShape;
    property ShapeType: TdxFcShapeType read FShapeType;
  public
    constructor Create(AShapeType: TdxFcShapeType; AAdvancedShape: TdxFlowChartObjectAdvancedShape = nil);
  end;

  { TdxDesignerFlowChartDragHelper }

  TdxDesignerFlowChartDragHelper = class(TdxFlowChartDragHelper)
  private
    function GetOwner: TdxDesignerFlowChart;
  public
    procedure DragStart(X, Y: Integer; AKind: TdxFlowChartDragHelper.TFlowChartDragKind); override;
    procedure DragStop(X: Integer; Y: Integer); override;

    property Owner: TdxDesignerFlowChart read GetOwner;
  end;

  { TdxDesignerFlowChartKeyAction }

  TdxDesignerFlowChartKeyAction = class
  private
    FKey: Word;
    FShift: TShiftState;
  protected
    function IsEqual(AKey: Word; AShift: TShiftState): Boolean;
    function IsRepeated(AKey: Word; AShift: TShiftState): Boolean;
  public
    constructor Create(AKey: Word; AShift: TShiftState);
  end;

  { TdxDesignerFlowChart }

  TdxDesignerFlowChart = class(TdxFlowChart)
  private
    FAddingConnection: TdxFcConnection;
    FAddinObject: TdxFcObject;
    FAutoRouteDestObject: TdxFcObject;
    FAutoRouteSourceObject: TdxFcObject;
    FIsKeyActionRepeated: Boolean;
    FLastKeyAction: TdxDesignerFlowChartKeyAction;
    FTransactCount: Integer;
    FUndoRedoManager: TUndoManager;
    FOnTransactChanged: TNotifyEvent;
    FIsAddingConnection: Boolean;

    function GetDragHelper: TdxDesignerFlowChartDragHelper;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    procedure AddConnectionOnDrag(APoint: TPoint);
    procedure AddObjectOnDragFromGallery(APoint: TPoint; AShapeType: TdxFcShapeType; AAdvancedShape: TdxFlowChartObjectAdvancedShape);
    procedure Changed(Item: TdxFcItem); override;
    procedure ConnectionEndpointDragLeave(ACanceled: Boolean);
    function CreateDragHelper: TdxFlowChartDragHelper; override;
    procedure MoveObjectsDragLeave(ACanceled: Boolean);
    procedure SetupAutoRoutingDestinationObject;

    procedure BeginTransactChange;
    procedure CancelTransactChange;
    procedure DoTransactChanged;
    procedure EndTransactChange;
    function IsTransactionProcessing: Boolean;

    function CanRedo: Boolean;
    function CanUndo: Boolean;
    procedure Redo;
    procedure ResetUndoRedo;
    procedure Undo;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    property AddingConnection: TdxFcConnection read FAddingConnection;
    property AddinObject: TdxFcObject read FAddinObject;
    property AutoRouteDestObject: TdxFcObject read FAutoRouteDestObject;
    property AutoRouteSourceObject: TdxFcObject read FAutoRouteSourceObject;
    property DragHelper: TdxDesignerFlowChartDragHelper read GetDragHelper;
    property IsAddingConnection: Boolean read FIsAddingConnection write FIsAddingConnection;
    property UndoRedoManager: TUndoManager read FUndoRedoManager;
    property OnTransactChanged: TNotifyEvent read FOnTransactChanged write FOnTransactChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxFlowChartCustomDesigner }

  TdxFlowChartCustomDesigner = class(TdxRibbonForm, IdxFlowChartDesigner)
  public
    function Execute(AChart: TdxFlowChart): Boolean; virtual; abstract;
  end;
  TcxFlowChartCustomDesignerClass = class of TdxFlowChartCustomDesigner;

  { TdxFlowChartDesigner }

  TdxFlowChartDesigner = class(TdxFlowChartCustomDesigner, IdxFlowChartEditorOptions)
    bmManager: TdxBarManager;
    miOpen: TdxBarLargeButton;
    miSaveAs: TdxBarLargeButton;
    bbUndo: TdxBarLargeButton;
    miCut: TdxBarLargeButton;
    miCopy: TdxBarLargeButton;
    miPaste: TdxBarLargeButton;
    miDelete: TdxBarLargeButton;
    miSelectAll: TdxBarLargeButton;
    miClearSelection: TdxBarLargeButton;
    miBringToFront: TdxBarLargeButton;
    miSendToBack: TdxBarLargeButton;
    miAntialiasing: TdxBarLargeButton;
    miZoomIn: TdxBarLargeButton;
    miZoomOut: TdxBarLargeButton;
    miFit: TdxBarLargeButton;
    miActualSize: TdxBarLargeButton;
    miNewUnion: TdxBarLargeButton;
    miAddToUnion: TdxBarLargeButton;
    miRemoveFromUnion: TdxBarLargeButton;
    miClearUnion: TdxBarLargeButton;
    miClearAllUnions: TdxBarLargeButton;
    miContents: TdxBarLargeButton;
    miProperties: TdxBarLargeButton;
    miRemovePoint: TdxBarLargeButton;
    rtHome: TdxRibbonTab;
    rRibbon: TdxRibbon;
    rtView: TdxRibbonTab;
    rbHomeClipboard: TdxBar;
    rbHomeArrange: TdxBar;
    rbViewZoom: TdxBar;
    rbViewAntialiasing: TdxBar;
    rbHomeEdititng: TdxBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    bpmChart: TdxBarPopupMenu;
    bamRibbonApplicationButtonMenu: TdxBarApplicationMenu;
    miNew: TdxBarLargeButton;
    miSave: TdxBarLargeButton;
    alActions: TActionList;
    acNew: TAction;
    acOpen: TAction;
    acSave: TAction;
    acSaveAs: TAction;
    acContents: TAction;
    acPaste: TAction;
    acCut: TAction;
    acCopy: TAction;
    acSelectAll: TAction;
    acClearSelection: TAction;
    acUndo: TAction;
    acDelete: TAction;
    acBringToFront: TAction;
    acSendToBack: TAction;
    acZoomIn: TAction;
    acZoomOut: TAction;
    acFit: TAction;
    acActualSize: TAction;
    acAntialiasing: TAction;
    acNewUnion: TAction;
    acAddToUnion: TAction;
    acRemoveFromUnion: TAction;
    acClearUnion: TAction;
    acClearAllUnions: TAction;
    acProperties: TAction;
    acRemovePoint: TAction;
    rbViewGridLines: TdxBar;
    miGridLines: TdxBarLargeButton;
    acGridLines: TAction;
    rbHomeDesigner: TdxBar;
    miCloseAndApplyChanges: TdxBarLargeButton;
    acCloseAndApplyChanges: TAction;
    dpShapes: TdxDockPanel;
    dsShapes: TdxDockSite;
    gcShapes: TdxGalleryControl;
    gcgShapes: TdxGalleryControlGroup;
    gcStencils: TdxGalleryControl;
    gcgStencils: TdxGalleryControlGroup;
    gcMoreShapes: TdxGalleryControl;
    gcgMoreShapes: TdxGalleryControlGroup;
    gciMoreShapes: TdxGalleryControlItem;
    imgMoreShapesArrow: TcxImage;
    bpmStensils: TdxBarPopupMenu;
    rbHomeFont: TdxBar;
    bcbFontName: TcxBarEditItem;
    bcbFontSize: TcxBarEditItem;
    bbFontSizeInc: TdxBarLargeButton;
    bbFontSizeDec: TdxBarLargeButton;
    acFontSizeInc: TAction;
    acFontSizeDec: TAction;
    bbFontBold: TdxBarLargeButton;
    bbFontItalic: TdxBarLargeButton;
    bbFontUnderline: TdxBarLargeButton;
    bbFontStrikeout: TdxBarLargeButton;
    acFontBold: TAction;
    acFontItalic: TAction;
    acFontUnderline: TAction;
    acFontStrikeout: TAction;
    rbHomeShapeStyles: TdxBar;
    dmShapes: TdxDockingManager;
    ilLargeIcons: TcxImageList;
    ilSmallIcons: TcxImageList;
    rbHomeParagraph: TdxBar;
    bbTextAlignTop: TdxBarLargeButton;
    bbTextAlignMiddle: TdxBarLargeButton;
    bbTextAlignBottom: TdxBarLargeButton;
    acTextAlignTop: TAction;
    acTextAlignMiddle: TAction;
    acTextAlignBottom: TAction;
    acTextAlignLeft: TAction;
    acTextAlignCenter: TAction;
    acTextAlignRight: TAction;
    bbTextAlignLeft: TdxBarLargeButton;
    bbTextAlignCenter: TdxBarLargeButton;
    bbTextAlignRight: TdxBarLargeButton;
    acFakeTextVertAlign: TAction;
    acFakeTextHorzAlign: TAction;
    bbFakeTextVertAlign: TdxBarLargeButton;
    bbFakeTextHorzAlign: TdxBarLargeButton;
    rbQuickAccessUndoRedo: TdxBar;
    rbHomeTools: TdxBar;
    bbPointerTool: TdxBarLargeButton;
    bbConnector: TdxBarLargeButton;
    bbStandardShape: TdxBarLargeButton;
    bpmStandardShape: TdxBarPopupMenu;
    acPointerTool: TAction;
    acConnector: TAction;
    acStandardShape: TAction;
    acRedo: TAction;
    bbRedo: TdxBarLargeButton;
    acSnapToGrid: TAction;
    rtDesign: TdxRibbonTab;
    bDesignOptions: TdxBar;
    bLayout: TdxBar;
    bbSnapToGrid: TdxBarLargeButton;
    bpmConnectors: TdxBarPopupMenu;
    acRightAngleHorizontal: TAction;
    acCurved: TAction;
    acStraight: TAction;
    bbRightAngle: TdxBarLargeButton;
    bbCurved: TdxBarLargeButton;
    bbStraight: TdxBarLargeButton;
    acRightAngleVertical: TAction;
    bbRightAngleVertical: TdxBarLargeButton;
    bsiConnectors: TdxBarSubItem;
    strScreenTips: TdxBarScreenTipRepository;
    stPaste: TdxScreenTip;
    stCut: TdxScreenTip;
    stCopy: TdxScreenTip;
    stFontName: TdxScreenTip;
    stFontSize: TdxScreenTip;
    stFontSizeInc: TdxScreenTip;
    stFontBold: TdxScreenTip;
    stFontItalic: TdxScreenTip;
    stUnderline: TdxScreenTip;
    stStrikeout: TdxScreenTip;
    stFontColor: TdxScreenTip;
    stFontSizeDec: TdxScreenTip;
    stTextAlignTop: TdxScreenTip;
    stTextAlignMiddle: TdxScreenTip;
    stTextAlignBottom: TdxScreenTip;
    stTextAlignLeft: TdxScreenTip;
    stTextAlignCenter: TdxScreenTip;
    stTextAlignRight: TdxScreenTip;
    stPointerTool: TdxScreenTip;
    stConnector: TdxScreenTip;
    stBackgroundColor: TdxScreenTip;
    stStrokeColor: TdxScreenTip;
    stBringToFront: TdxScreenTip;
    stSendToBack: TdxScreenTip;
    stSnapToGrid: TdxScreenTip;
    stConnectors: TdxScreenTip;
    stGrid: TdxScreenTip;
    stUndo: TdxScreenTip;
    stRedo: TdxScreenTip;
    stNew: TdxScreenTip;
    stOpen: TdxScreenTip;
    stSave: TdxScreenTip;
    stSaveAs: TdxScreenTip;
    ppmFontColor: TdxBarPopupMenu;
    bbFontColor: TdxBarLargeButton;
    rgiFontColor: TdxRibbonGalleryItem;
    rgiColorTheme: TdxRibbonGalleryItem;
    acFontColor: TAction;
    bbBackgroundColor: TdxBarLargeButton;
    acBackgroundColor: TAction;
    rgiBackgroundColor: TdxRibbonGalleryItem;
    rgiBackgroundThemsColor: TdxRibbonGalleryItem;
    rgiObjectBackgroundColor: TdxRibbonGalleryItem;
    rgiObjectBackgroundThemsColor: TdxRibbonGalleryItem;
    ppmBackgroundColor: TdxBarPopupMenu;
    bbStrokeColor: TdxBarLargeButton;
    ppmStrokeColor: TdxBarPopupMenu;
    acStrokeColor: TAction;
    rgiStrokeColor: TdxRibbonGalleryItem;
    rgiStrokeColorTheme: TdxRibbonGalleryItem;
    rgiLineColor: TdxRibbonGalleryItem;
    rgiLineColorTheme: TdxRibbonGalleryItem;
    rgiSourceArrowColor: TdxRibbonGalleryItem;
    rgiSourceArrowColorTheme: TdxRibbonGalleryItem;
    rgiDestArrowColor: TdxRibbonGalleryItem;
    rgiDestArrowColorTheme: TdxRibbonGalleryItem;
    dsProperties: TdxDockSite;
    dpProperties: TdxDockPanel;
    dxLayoutDockSite2: TdxLayoutDockSite;
    ilBeginArrowTypes: TcxImageList;
    icbEndArrow: TcxImageComboBox;
    ilEndArrowTypes: TcxImageList;
    lcProperties: TdxLayoutControl;
    seLineThickness: TcxSpinEdit;
    seDArrowWidth: TcxSpinEdit;
    seSArrowWidth: TcxSpinEdit;
    lcPropertiesGroup_Root: TdxLayoutGroup;
    lgConnections: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    lgSource: TdxLayoutGroup;
    lgTarget: TdxLayoutGroup;
    liStrokeThickness: TdxLayoutItem;
    liDArrowSize: TdxLayoutItem;
    liSArrowWidth: TdxLayoutItem;
    FontDialog: TFontDialog;
    ColorDialog: TdxColorDialog;
    icbBeginArrow: TcxImageComboBox;
    liSArrowStyle: TdxLayoutItem;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lgObjects: TdxLayoutGroup;
    seSArrowHeight: TcxSpinEdit;
    liSArrowHeight: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    seDArrowHeight: TcxSpinEdit;
    dxLayoutItem3: TdxLayoutItem;
    cbSArrowSize: TcxComboBox;
    liSArrowSize: TdxLayoutItem;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutItem4: TdxLayoutItem;
    cbDArrowSize: TcxComboBox;
    btnLineColor: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    ppmLineColor: TdxBarPopupMenu;
    btnSourceArrowColor: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    ppmSourceArrowColor: TdxBarPopupMenu;
    btnDestArrowColor: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    ppmDestArrowColor: TdxBarPopupMenu;
    ppmObjectBkColor: TdxBarPopupMenu;
    btnObjectBkColor: TcxButton;
    dxLayoutItem8: TdxLayoutItem;
    lgLine: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    mText: TcxMemo;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    seShapeHeight: TcxSpinEdit;
    dxLayoutItem10: TdxLayoutItem;
    seAngle: TcxSpinEdit;
    dxLayoutItem11: TdxLayoutItem;
    seShapeWidth: TcxSpinEdit;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    bbUseAdvancedShapesOnly: TdxBarLargeButton;
    acUseAdvancedShapesOnly: TAction;
    lgCommon: TdxLayoutGroup;
    liConnectionType: TdxLayoutItem;
    icbConnectionType: TcxImageComboBox;
    liLineStyle: TdxLayoutItem;
    icbLineStyle: TcxImageComboBox;
    ilLineStyles: TcxImageList;
    dxBarLargeButton1: TdxBarLargeButton;
    acApplyLayeredLayout: TAction;
    stApplyLayeredLayout: TdxScreenTip;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acContentsExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acClearSelectionExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acBringToFrontExecute(Sender: TObject);
    procedure acSendToBackExecute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure acFitExecute(Sender: TObject);
    procedure acActualSizeExecute(Sender: TObject);
    procedure acAntialiasingExecute(Sender: TObject);
    procedure acNewUnionExecute(Sender: TObject);
    procedure acAddToUnionExecute(Sender: TObject);
    procedure acRemoveFromUnionExecute(Sender: TObject);
    procedure acClearUnionExecute(Sender: TObject);
    procedure acClearAllUnionsExecute(Sender: TObject);
    procedure acPropertiesExecute(Sender: TObject);
    procedure acRemovePointExecute(Sender: TObject);
    procedure acGridLinesExecute(Sender: TObject);
    procedure acCloseAndApplyChangesExecute(Sender: TObject);
    procedure gcStencilsItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
    procedure gcMoreShapesItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
    procedure bcbFontNamePropertiesEditValueChanged(Sender: TObject);
    procedure bcbFontNamePropertiesLoadFontComplete(Sender: TObject);
    procedure bcbFontSizePropertiesEditValueChanged(Sender: TObject);
    procedure acFontSizeIncExecute(Sender: TObject);
    procedure acFontSizeDecExecute(Sender: TObject);
    procedure acConnectorExecute(Sender: TObject);
    procedure acStandardShapeExecute(Sender: TObject);
    procedure acPointerToolExecute(Sender: TObject);
    procedure acFontBoldExecute(Sender: TObject);
    procedure acFontItalicExecute(Sender: TObject);
    procedure acFontUnderlineExecute(Sender: TObject);
    procedure acFontStrikeoutExecute(Sender: TObject);
    procedure acTextVertAlignExecute(Sender: TObject);
    procedure acTextHorzAlignExecute(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acSnapToGridExecute(Sender: TObject);
    procedure acRightAngleHorizontalExecute(Sender: TObject);
    procedure acCurvedExecute(Sender: TObject);
    procedure acStraightExecute(Sender: TObject);
    procedure acRightAngleVerticalExecute(Sender: TObject);
    procedure acFontColorExecute(Sender: TObject);
    procedure acBackgroundColorExecute(Sender: TObject);
    procedure acStrokeColorExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure gcMoreShapesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pSourceColorClick(Sender: TObject);
    procedure ConnectionPropertiesChanged(Sender: TObject);
    procedure seLineThicknessPropertiesEditValueChanged(Sender: TObject);
    procedure cbSArrowSizePropertiesEditValueChanged(Sender: TObject);
    procedure cbDArrowSizePropertiesEditValueChanged(Sender: TObject);
    procedure SourceArrowDimensionChanged(Sender: TObject);
    procedure DestArrowDimensionChanged(Sender: TObject);
    procedure icbLineStylePropertiesEditValueChanged(Sender: TObject);
    procedure mTextKeyPress(Sender: TObject; var Key: Char);
    procedure mTextPropertiesEditValueChanged(Sender: TObject);
    procedure acUpdate(Sender: TObject);
    procedure seAnglePropertiesEditValueChanged(Sender: TObject);
    procedure seShapeWidthPropertiesEditValueChanged(Sender: TObject);
    procedure seShapeHeightPropertiesEditValueChanged(Sender: TObject);
    procedure acUseAdvancedShapesOnlyExecute(Sender: TObject);
    procedure acApplyLayeredLayoutExecute(Sender: TObject);
  private
    FBackgroundColorPicker: TdxFlowChartColorPickerController;
    FBuffer: TdxFlowChart;
    FChart: TdxDesignerFlowChart;
    FLineColorPicker: TdxFlowChartColorPickerController;
    FDestArrowColorPicker: TdxFlowChartColorPickerController;
    FDownPoint: TPoint;
    FDragPoint: TPoint;
    FFontColorPicker: TdxFlowChartColorPickerController;
    FModified: Boolean;
    FMoreShapesArrow: TcxFlowChartDesignerMoreShapesDropDownArrowImage;
    FIsInternalUpdatingRibbonItemsByChartItems: Boolean;
    FLastObj: TdxFcObject;
    FMode: TdxFlowChartDesignerMode;
    FObjectBkColorPicker: TdxFlowChartColorPickerController;
    FOldHintHidePause : Integer;
    FPasteCount: Integer;
    FSavedCursor: TCursor;
    FShapeGalleryInfo: TDictionary<TdxGalleryControlItem, TdxFlowChartShapeInfo>;
    FShortCuts: TDictionary<TAction, TShortCut>;
    FStencilGalleryInfo: TDictionary<TdxGalleryControlItem, TdxFlowChartAdvancedShapeStencil>;
    FStencilMenuInfo: TDictionary<TdxBarButton, TdxFlowChartAdvancedShapeStencil>;
    FStrokeColorPicker: TdxFlowChartColorPickerController;
    FSourceArrowColorPicker: TdxFlowChartColorPickerController;
    FUpdatingPropertiesControls: Boolean;
    FUseAdvancedShapesOnly: Boolean;

    function GetActiveChartBackgroundColor: TColor;
    function GetActiveChartFont: TFont;
    function GetActiveChartStrokeColor: TColor;
    procedure SetMode(AValue: TdxFlowChartDesignerMode);

    procedure StartAddObjectOnDrag;
    procedure EndAddObjectOnDrag;

    procedure ChartDblClickHandler(ASender: TObject);
    procedure ChartDragOverHandler(ASender, ASource: TObject; AX, AY: Integer; AState: TDragState; var AAccept: Boolean);
    procedure ChartKeyDownHandler(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure ChartMouseDownHandler(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
    procedure ChartMouseMoveHandler(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
    procedure ChartMouseUpHandler(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
    procedure ChartMouseWheelHandler(ASender: TObject; AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint; var AHandled: Boolean);
    procedure ChartSelectedHandler(ASender: TdxCustomFlowChart; AItem: TdxFcItem);
    procedure ChartTransactChangedHandler(ASender: TObject);
    procedure LineColorChangedHandler(Sender: TObject);
    procedure DestArrowColorChangedHandler(Sender: TObject);
    procedure MoreShapesGalleryHotTrackChangedHandler(ASender: TObject);
    procedure MenuStencilsButtonClickHandler(Sender: TObject);
    procedure ObjectBkColorChangedHandler(Sender: TObject);
    procedure SourceArrowColorChangedHandler(Sender: TObject);
    procedure ToolsStandardShapeItemClickHandler(ASender: TObject);

    procedure ClearBuffer;
    procedure CopySelectedConnectionsToBuffer(ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>);
    procedure CopySelectedObjectsToBuffer(ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>);
    procedure CopyToBuf;
    procedure PasteBufferConnectionsToChart(ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>);
    procedure PasteBufferObjectsToChart(ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>);
    procedure PasteFromBuf;

    function ChartHasUnions: Boolean;
    function FindUnions(FromUnion, Obj: TdxFcObject): TdxFcObject;
    function GetNumberByUnion(Obj: TdxFcObject): Integer;
    function IsChildItemInUnion(Obj : TdxFcObject) : Boolean;
    function IsMainItemInUnion(Obj : TdxFcObject): Boolean;
    procedure UpdateUnionHint(APoint: TPoint);

    procedure ForEachSelectedChartItem(const AProc: TdxFlowChartForEachSelectedItemProc);
    procedure ForEachSelectedConnection(const AProc: TdxFlowChartForEachSelectedConnectionProc);
    procedure ForEachSelectedObject(const AProc: TdxFlowChartForEachSelectedObjectProc);
    procedure ForEachStandardShape(const AProc: TdxFlowChartForEachStandardShapeProc);

    procedure ChangeItemBkColorProc(AItem: TdxFcItem);
    procedure ChangeItemColorProc(AItem: TdxFcItem);
    procedure ChangeItemFontBoldProc(AItem: TdxFcItem);
    procedure ChangeItemFontColorProc(AItem: TdxFcItem);
    procedure ChangeItemFontItalicProc(AItem: TdxFcItem);
    procedure ChangeItemFontNameProc(AItem: TdxFcItem);
    procedure ChangeItemFontSizeProc(AItem: TdxFcItem);
    procedure ChangeItemFontStrikeoutProc(AItem: TdxFcItem);
    procedure ChangeItemFontUnderlineProc(AItem: TdxFcItem);
    procedure ChangeItemTextHorzAlignProc(AItem: TdxFcItem);
    procedure ChangeItemTextVertAlignProc(AItem: TdxFcItem);
    procedure ItemBringToFront(AItem: TdxFcItem);
    procedure ItemCurved(AItem: TdxFcItem);
    procedure ItemRightAngleHorizontal(AItem: TdxFcItem);
    procedure ItemRightAngleVertical(AItem: TdxFcItem);
    procedure ItemSendToBack(AItem: TdxFcItem);
    procedure ItemStraight(AItem: TdxFcItem);
    procedure SetUseAdvancedShapesOnly(AValue: Boolean);
  protected
    procedure AddAdvancedGalleryShapes(AStencil: TdxFlowChartAdvancedShapeStencil; ANeedSetChecked: Boolean);
    procedure AddObjectOnDrag(APoint: TPoint);
    procedure AddStandardGalleryShape(AShapeType: TdxFcShapeType);
    procedure AddStandardGalleryShapes;
    procedure AddToolsStandardShape(AShapeType: TdxFcShapeType);
    function AllowOpenDocument: Boolean;
    procedure BuildMenuStencils;
    procedure BuildToolsStandardShapes;
    procedure ChangeSelectedChartItems(const AProc: TdxFlowChartForEachSelectedItemProc);
    procedure ChangeSelectedConnections(const AProc: TdxFlowChartForEachSelectedConnectionProc);
    procedure ChangeSelectedObjects(const AProc: TdxFlowChartForEachSelectedObjectProc);
    function CheckActiveChartTextHorzAlign(AAlign: TdxFcHorzPos): Boolean;
    function CheckActiveChartTextVertAlign(AAlign: TdxFcVertPos): Boolean;
    procedure ClearShapeGalleryInfo;
    procedure ClearToolsStandardShapes;
    procedure DrawAdvancedGalleryShape(AGalleryItem: TdxGalleryControlItem; AShape: TdxFlowChartObjectAdvancedShape);
    procedure DrawDragObject;
    procedure DrawStandardGalleryShape(AShapeGalleryItem: TdxGalleryControlItem; AShapeType: TdxFcShapeType);
    procedure DrawToolsStandardShape(AMenuItem: TdxBarButton; AShapeType: TdxFcShapeType);
    function GetFocusedGalleryStencil: TdxFlowChartAdvancedShapeStencil;
    function GetStandardGalleryShapeCaption(AShapeType: TdxFcShapeType): string;
    function GetStandardGalleryShapePolygonPoints(AShapeType: TdxFcShapeType; ABounds: TRect): TPoints;
    function GetStandardGalleryShapeShortCut(AShapeType: TdxFcShapeType): TShortCut;
    function GetStandardShapeGlyph(AShapeType: TdxFcShapeType; ASize: TSize): TdxSmartGlyph;
    procedure InitializeChart;
    procedure InitializeArrowTypeImages;
    procedure InitializeMoreShapesArrow;
    procedure InitializeArrowSizes;
    procedure InitializeLineStyleImages;
    procedure InitializeConnectionTypes;
    function IsStandardGalleryShape(AShapeType: TdxFcShapeType): Boolean;
    procedure ModeChanged;
    procedure MultiSelect(ResetOldSelected : Boolean; SelectRect : TRect);
    function NeedAddStandardGalleryShapes(AStencil: TdxFlowChartAdvancedShapeStencil): Boolean;
    procedure PopulateShortCuts;
    procedure PostponedUpdateRibbonItemsEnabled;
    function TryOpenHelpFile(const AFileName: string): Boolean;
    procedure UpdateActiveGalleryShapes;
    procedure UpdateBackgroundColorImage;
    procedure UpdateLineColorImage;
    procedure UpdateDestArrowColorImage;
    procedure UpdateFontColorImage;
    procedure UpdateGalleryStencils(AFocusedStencil: TdxFlowChartAdvancedShapeStencil = nil);
    procedure UpdateObjectBkColorImage;
    procedure UpdateRibbonItemsCaption;
    procedure UpdateRibbonItemsEnabled;
    procedure UpdateRibbonItemsByChartItems;
    procedure UpdateSourceArrowColorImage;
    procedure UpdateStrokeColorImage;

    // properties panel
    function CompareConnectionValue(const ACompare: TdxFlowChartCompareConnectionValueFunc; AStartIndex: Integer): Boolean;
    function CompareObjectValue(const ACompare: TdxFlowChartCompareObjectValueFunc; AStartIndex: Integer): Boolean;
    function FirstSelectedChartItem: TdxFcItem;
    function FirstSelectedChartConnection: TdxFcConnection;
    function FirstSelectedChartObject: TdxFcObject;
    function GetArrowSize(AIndex: Integer): TdxFcaSize;
    procedure IterateGroupControls(AGroup: TdxCustomLayoutGroup; const AAction: TdxFlowChartGroupControlsActionProc);
    procedure UpdatePropertiesPanel;
    procedure UpdateCommonPropertiesControls;
    procedure UpdateObjectsPropertiesControls;
    procedure UpdateConnectionsPropertiesControls;
    procedure UpdateSelectedConnections;

    property ActiveChartBackgroundColor: TColor read GetActiveChartBackgroundColor;
    property ActiveChartFont: TFont read GetActiveChartFont;
    property ActiveChartStrokeColor: TColor read GetActiveChartStrokeColor;
    property Chart: TdxDesignerFlowChart read FChart;
    property Mode: TdxFlowChartDesignerMode read FMode write SetMode;
    property ShapeGalleryInfo: TDictionary<TdxGalleryControlItem, TdxFlowChartShapeInfo> read FShapeGalleryInfo;
    property ShortCuts: TDictionary<TAction, TShortCut> read FShortCuts;
    property StencilGalleryInfo: TDictionary<TdxGalleryControlItem, TdxFlowChartAdvancedShapeStencil> read FStencilGalleryInfo;
    property StencilMenuInfo: TDictionary<TdxBarButton, TdxFlowChartAdvancedShapeStencil> read FStencilMenuInfo;
    property UseAdvancedShapesOnly: Boolean read FUseAdvancedShapesOnly write SetUseAdvancedShapesOnly;
  public
    function Execute(AChart: TdxFlowChart): Boolean; override;
    procedure SetOptions(AOptions: TdxFlowChartEditorOptions);

    procedure LoadFromFile(const AFileName: string);
  end;

implementation

{$R *.dfm}

uses
  RTLConsts, Math, dxSelUnion, dxFcStrs, dxEditObj, dxEditCon, dxThreading, cxGeometry, dxLines, dxFlowChartArrows;

const
  dxActualZoom = 100;
  dxMaxZoom = 490;
  dxMinZoom = 20;
  dxZoomStep = 10;
  dxDefaultTextColor = clHighlightText;
  dxDefaultShapeColor = $C8C8C8;
  dxDefaultShapeBkColor = $D59B5B;
  dxDefaultArrowColor = dxDefaultShapeBkColor;
  dxDefaultArrowSize = 8;
  dxHelp = 'ExpressFlowChartEditor.chm';
  dxToolStandardShapesGroupIndex = 100;
  dxToolStandardShapesGlyphSize = 16;
  dxUndoStepCount = 100;
  dxPasteDelta = 10;
  dxExitConfirmation = 'Do you want to apply the changes?';
  dxNewConfirmation = 'The document you are about to close has unsaved changes. Do you want to save the document?';

  dxStandardShapeShortCutText: array[TdxFcShapeType] of string = ('', 'Ctrl+3', 'Ctrl+4', 'Ctrl+5', 'Ctrl+6',
    'Ctrl+Shift+1', 'Ctrl+Shift+2', 'Ctrl+Shift+3', 'Ctrl+Shift+4', 'Ctrl+Shift+5', '', '');

procedure DrawHelpedColorLine(AGlyph: TdxSmartGlyph ; AColor: TColor; AImageBasicGlyph: TcxImageList; AIndexBasicGlyph: Integer);
const
  ALineWidth = 14;
  ALineHeight = 3;
var
  ALineBounds: TRect;
  ALineGlyph, ABasicGlyph: TcxAlphaBitmap;
begin
  ABasicGlyph := TcxAlphaBitmap.Create;
  try
    AImageBasicGlyph.GetImageInfo(AIndexBasicGlyph, ABasicGlyph, nil);
    ALineGlyph := TcxAlphaBitmap.CreateSize(ALineWidth, ALineHeight);
    try
      ALineGlyph.cxCanvas.FillRect(Rect(0, 0, ALineWidth, ALineHeight), AColor);
      ALineGlyph.MakeOpaque;
      ALineBounds := cxRectBounds(1, 13, ALineWidth, ALineHeight);
      cxBitBlt(ABasicGlyph.Canvas.Handle, ALineGlyph.Canvas.Handle, ALineBounds, Point(0, 0), SRCCOPY);
    finally
      ALineGlyph.Free;
    end;
    AGlyph.Assign(ABasicGlyph);
  finally
    ABasicGlyph.Free;
  end;
end;

type
  { TdxFlowChartHelper }

  TdxFlowChartHelper = class helper for TdxFlowChart
  public
    function GetScaleFactor: TdxScaleFactor;
  end;

  { TdxFlowChartObjectAdvancedShapeHelper }

  TdxFlowChartObjectAdvancedShapeHelper = class helper for TdxFlowChartObjectAdvancedShape
  public
    procedure PaintShape(AGraphics: TdxGPCanvas; const ABounds: TRect; APenColor, ABrushColor: TdxAlphaColor);
    function IsLegacyReplacement: Boolean;
  end;

  { TdxGalleryControlHelper }

  TdxGalleryControlHelper = class helper for TdxGalleryControl
  private
    function GetDragMode: TDragMode;
    function GetOnMouseEnter: TNotifyEvent;
    function GetOnMouseLeave: TNotifyEvent;
    function GetOnStartDrag: TStartDragEvent;
    procedure SetDragMode(AValue: TDragMode);
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    procedure SetOnStartDrag(const Value: TStartDragEvent);
  public
    property DragMode: TDragMode read GetDragMode write SetDragMode default dmManual;
    property OnMouseEnter: TNotifyEvent read GetOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read GetOnMouseLeave write SetOnMouseLeave;
    property OnStartDrag: TStartDragEvent read GetOnStartDrag write SetOnStartDrag;
  end;

  { TdxGalleryControlItemHelper }

  TdxGalleryControlItemHelper = class helper for TdxGalleryControlItem
  private
    function GetViewInfo: TdxGalleryItemViewInfo;
  public
    property ViewInfo: TdxGalleryItemViewInfo read GetViewInfo;
  end;

  { TdxFcConnectionHelper }

  TdxFcConnectionHelper = class helper for TdxFcConnection
  public
    procedure AssignPattern(ASource: TdxFcConnection);
    function GetFirstRealPoint: TPoint;
    function GetLastRealPoint: TPoint;
    procedure Offset(ADelta: Integer);
  end;

  { TdxFcObjectHelper }

  TdxFcObjectHelper = class helper for TdxFcObject
  public
    function AutoRoutingPoint: TPoint;
  end;

  { TdxFcConnectionArrowHelper }

  TdxFcConnectionArrowHelper = class helper for TdxFcConnectionArrow
  public
    class function GetRepository: TdxFlowChartArrowShapeRepository;
    function HasBackground: Boolean;
  end;

  { TcxMemoHelper }

  TcxMemoHelper = class helper for TcxMemo
  public
    class function GetShiftState: TShiftState; static;
    procedure KeyPressShiftEnterHandler(var AKey: Char);
    procedure KeyPressCtrlEnterHandler(var AKey: Char);
  end;

{ TdxFlowChartHelper }

function TdxFlowChartHelper.GetScaleFactor: TdxScaleFactor;
begin
  Result := inherited ScaleFactor;
end;

{ TdxFlowChartObjectAdvancedShapeHelper }

function TdxFlowChartObjectAdvancedShapeHelper.IsLegacyReplacement: Boolean;
begin
  Result := inherited LegacyReplacement;
end;

procedure TdxFlowChartObjectAdvancedShapeHelper.PaintShape(AGraphics: TdxGPCanvas;
  const ABounds: TRect; APenColor, ABrushColor: TdxAlphaColor);
begin
  FShape.PaintDefaultShape(AGraphics, ABounds, APenColor, ABrushColor);
end;

{ TdxGalleryControlHelper }

function TdxGalleryControlHelper.GetDragMode: TDragMode;
begin
  Result := inherited DragMode;
end;

function TdxGalleryControlHelper.GetOnMouseEnter: TNotifyEvent;
begin
  Result := inherited OnMouseEnter;
end;

function TdxGalleryControlHelper.GetOnMouseLeave: TNotifyEvent;
begin
  Result := inherited OnMouseLeave;
end;

function TdxGalleryControlHelper.GetOnStartDrag: TStartDragEvent;
begin
  Result := inherited OnStartDrag;
end;

procedure TdxGalleryControlHelper.SetDragMode(AValue: TDragMode);
begin
  inherited DragMode := AValue;
end;

procedure TdxGalleryControlHelper.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  inherited OnMouseEnter := Value;
end;

procedure TdxGalleryControlHelper.SetOnMouseLeave(const Value: TNotifyEvent);
begin
  inherited OnMouseLeave := Value;
end;

procedure TdxGalleryControlHelper.SetOnStartDrag(const Value: TStartDragEvent);
begin
  inherited OnStartDrag := Value;
end;

{ TdxGalleryControlItemHelper }

function TdxGalleryControlItemHelper.GetViewInfo: TdxGalleryItemViewInfo;
begin
  Result := inherited ViewInfo;
end;

{ TdxFcConnectionHelper }

procedure TdxFcConnectionHelper.AssignPattern(ASource: TdxFcConnection);
begin
  AssignAttributes(ASource);
  AssignGeometry(ASource);
end;

function TdxFcConnectionHelper.GetFirstRealPoint: TPoint;
begin
  Result := FRealPoints.Points[0];
end;

function TdxFcConnectionHelper.GetLastRealPoint: TPoint;
begin
  Result := FRealPoints.Points[FRealPoints.Count - 1];
end;

procedure TdxFcConnectionHelper.Offset(ADelta: Integer);
var
  I: Integer;
begin
  for I := 0 to PointCount - 1 do
    Points[I] := cxPointOffset(Points[I], ADelta, ADelta);
end;

{ TdxFcObjectHelper }

function TdxFcObjectHelper.AutoRoutingPoint: TPoint;
begin
  Result := Owner.ChartPoint(DisplayRect.CenterPoint);
end;

{ TdxFcConnectionArrowHelper }

class function TdxFcConnectionArrowHelper.GetRepository: TdxFlowChartArrowShapeRepository;
begin
  Result := Repository;
end;

function TdxFcConnectionArrowHelper.HasBackground: Boolean;
begin
  Result := (Shape <> nil) and Shape.HasBackground;
end;

{ TcxMemoHelper }

class function TcxMemoHelper.GetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Result := Result + [ssShift];
  if GetKeyState(VK_CONTROL) < 0 then Result := Result + [ssCtrl];
  if GetKeyState(VK_MENU) < 0 then Result := Result + [ssAlt];
end;

procedure TcxMemoHelper.KeyPressCtrlEnterHandler(var AKey: Char);
begin
  Assert(Properties.WantReturns);
  if (AKey = #10) and (GetShiftState = [ssCtrl]) and ModifiedAfterEnter then
  begin
    AKey := #0;
    ModifiedAfterEnter := True;
    PostEditValue;
  end;
end;

procedure TcxMemoHelper.KeyPressShiftEnterHandler(var AKey: Char);
var
  AState: TShiftState;
begin
  Assert(not Properties.WantReturns);
  AState := GetShiftState;
  if (AKey = #10) and (AState = [ssCtrl]) then
  begin
    AKey := #0;
    Exit;
  end;
  if AKey = #13 then
  begin
    AKey := #0;
    if AState = [ssShift] then
      AKey := #10
    else
      if (AState = []) and ModifiedAfterEnter then
      begin
        DoEditValueChanged;
        ModifiedAfterEnter := False;
      end;
  end;
end;

{ TcxFlowChartDesignerMoreShapesDropDownArrowImage }

procedure TcxFlowChartDesignerMoreShapesDropDownArrowImage.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  Message.Result := HTTRANSPARENT;
end;

{ TdxFlowChartShapeInfo }

constructor TdxFlowChartShapeInfo.Create(AShapeType: TdxFcShapeType; AAdvancedShape: TdxFlowChartObjectAdvancedShape = nil);
begin
  inherited Create;
  FShapeType := AShapeType;
  FAdvancedShape := AAdvancedShape;
end;

{ TdxDesignerFlowChartDragHelper }

procedure TdxDesignerFlowChartDragHelper.DragStart(X, Y: Integer; AKind: TdxFlowChartDragHelper.TFlowChartDragKind);
begin
  Owner.BeginTransactChange;
  try
    inherited DragStart(X, Y, AKind);
  finally
    Owner.CancelTransactChange;
  end;
  if not (DragKind in [TFlowChartDragKind.None, TFlowChartDragKind.Selection]) then
    Owner.BeginTransactChange;
end;

procedure TdxDesignerFlowChartDragHelper.DragStop(X, Y: Integer);
var
  ADragKind: TFlowChartDragKind;
begin
  ADragKind := DragKind;
  if ADragKind = TFlowChartDragKind.Endpoint then
    Owner.SetupAutoRoutingDestinationObject;
  inherited DragStop(X, Y);
  if ADragKind = TFlowChartDragKind.Endpoint then
    Owner.ConnectionEndpointDragLeave(Cancelled)
  else if ADragKind = TFlowChartDragKind.Move then
    Owner.MoveObjectsDragLeave(Cancelled);
  if not (ADragKind in [TFlowChartDragKind.None, TFlowChartDragKind.Selection]) then
    if Cancelled then
      Owner.CancelTransactChange
    else
      Owner.EndTransactChange;
end;

function TdxDesignerFlowChartDragHelper.GetOwner: TdxDesignerFlowChart;
begin
  Result := TdxDesignerFlowChart(inherited Owner);
end;

{ TdxDesignerFlowChartKeyAction }

constructor TdxDesignerFlowChartKeyAction.Create(AKey: Word; AShift: TShiftState);
begin
  inherited Create;
  FKey := AKey;
  FShift := AShift;
end;

function TdxDesignerFlowChartKeyAction.IsEqual(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := (FKey = AKey) and (FShift = AShift);
end;

function TdxDesignerFlowChartKeyAction.IsRepeated(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := (AKey in [VK_RIGHT, VK_LEFT, VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR, VK_HOME, VK_END]) and
    ((AShift = [ssShift]) or (AShift = [ssAlt])) and IsEqual(AKey, AShift);
end;

{ TdxDesignerFlowChart }

constructor TdxDesignerFlowChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUndoRedoManager := TUndoManager.Create(Self, dxUndoStepCount);
  UndoRedoManager.Store;
end;

destructor TdxDesignerFlowChart.Destroy;
begin
  FreeAndNil(FLastKeyAction);
  FreeAndNil(FUndoRedoManager);
  inherited Destroy;
end;

procedure TdxDesignerFlowChart.AddConnectionOnDrag(APoint: TPoint);
var
  ASourcePoint: Integer;
  ASourceObject: TdxFcObject;
begin
  if AutoRouting and (GetEndpointHighlightedObject = nil) then
    Exit;
  BeginTransactChange;
  try
    FAutoRouteSourceObject := ConnectionHighlights.&Object;
    if ConnectionHighlights.PointInfo <> nil then
    begin
      ASourceObject := ConnectionHighlights.PointInfo.&Object;
      ASourcePoint := ConnectionHighlights.PointInfo.Index;
    end
    else
    begin
      ASourcePoint := 0;
      ASourceObject := nil;
    end;
    FAddingConnection := CreateConnection(ASourceObject, nil, ASourcePoint, 0);
    AddingConnection.Color := clBlack;
    AddingConnection.PenStyle := psDash;
    if ASourceObject = nil then
    begin
      if FAutoRouteSourceObject <> nil then
        AddingConnection.AddPoint(FAutoRouteSourceObject.AutoRoutingPoint)
      else
        AddingConnection.AddPoint(ChartPoint(APoint));
    end;
    AddingConnection.AddPoint(ChartPoint(APoint));
    DragHelper.SetupDragConnection(AddingConnection, 1);
    DragHelper.DragStart(APoint.X, APoint.Y, TdxFlowChartDragHelper.TFlowChartDragKind.Endpoint);
  finally
    EndTransactChange;
  end;
end;

procedure TdxDesignerFlowChart.AddObjectOnDragFromGallery(APoint: TPoint; AShapeType: TdxFcShapeType;
  AAdvancedShape: TdxFlowChartObjectAdvancedShape);
begin
  BeginTransactChange;
  try
    StartExternalDrag(APoint,
    function (): TdxFcObject
    begin
      if AShapeType = fcsAdvanced then
        FAddinObject := CreateObject(0, 0, 0, 0, AAdvancedShape)
      else
        FAddinObject := CreateObject(0, 0, 0, 0, AShapeType);
      AddinObject.ShapeColor := dxDefaultShapeColor;
      AddinObject.BkColor := dxDefaultShapeBkColor;
      Result := AddinObject;
    end);
  finally
    EndTransactChange;
  end;
end;

procedure TdxDesignerFlowChart.Changed(Item: TdxFcItem);
begin
  inherited Changed(Item);
  DoTransactChanged;
end;

procedure TdxDesignerFlowChart.ConnectionEndpointDragLeave(ACanceled: Boolean);
begin
  if AddingConnection <> nil then
  begin
    if ACanceled or (AutoRouting and (AutoRouteDestObject = nil)) then
      FAddingConnection.Free
    else
    begin
      ClearSelection;
      AddingConnection.Color := dxDefaultArrowColor;
      AddingConnection.PenStyle := psSolid;
      AddingConnection.ArrowDest.ArrowType := fcaArrow;
      AddingConnection.ArrowDest.Height := 8;
      AddingConnection.ArrowDest.Width := 8;
      AddingConnection.Selected := True;
      if AutoRouting then
      begin
        AddingConnection.SetObjectDest(AutoRouteDestObject, 0);
        AddingConnection.SetObjectSource(AutoRouteSourceObject, 0);
      end;
    end;
    FAddingConnection := nil;
  end
  else
    InvalidateRouting;
end;

function TdxDesignerFlowChart.CreateDragHelper: TdxFlowChartDragHelper;
begin
  Result := TdxDesignerFlowChartDragHelper.Create(Self);
end;

procedure TdxDesignerFlowChart.MoveObjectsDragLeave(ACanceled: Boolean);
begin
  if ACanceled then
    FAddinObject.Free;
  FAddinObject := nil;
end;

procedure TdxDesignerFlowChart.BeginTransactChange;
begin
  Inc(FTransactCount);
end;

procedure TdxDesignerFlowChart.CancelTransactChange;
begin
  Dec(FTransactCount);
end;

procedure TdxDesignerFlowChart.DoTransactChanged;
begin
  if IsTransactionProcessing then
    Exit;
  if not IsDestroying then
  begin
    if not FIsKeyActionRepeated then
      FreeAndNil(FLastKeyAction);
    UndoRedoManager.Store(FLastKeyAction <> nil)
  end;
  if Assigned(OnTransactChanged) then
    OnTransactChanged(Self);
end;

procedure TdxDesignerFlowChart.EndTransactChange;
begin
  Dec(FTransactCount);
  DoTransactChanged;
end;

function TdxDesignerFlowChart.IsTransactionProcessing: Boolean;
begin
  Result := FTransactCount > 0;
end;

function TdxDesignerFlowChart.CanRedo: Boolean;
begin
  Result := UndoRedoManager.CanRedo;
end;

function TdxDesignerFlowChart.CanUndo: Boolean;
begin
  Result := UndoRedoManager.CanUndo;
end;

procedure TdxDesignerFlowChart.Redo;
begin
  UndoRedoManager.Redo;
  FreeAndNil(FLastKeyAction);
end;

procedure TdxDesignerFlowChart.ResetUndoRedo;
begin
  UndoRedoManager.Clear;
  UndoRedoManager.Store;
end;

procedure TdxDesignerFlowChart.SetupAutoRoutingDestinationObject;
begin
  FAutoRouteDestObject := GetEndpointHighlightedObject;
end;

procedure TdxDesignerFlowChart.Undo;
begin
  UndoRedoManager.Undo;
  FreeAndNil(FLastKeyAction);
end;

procedure TdxDesignerFlowChart.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FIsKeyActionRepeated := (FLastKeyAction <> nil) and FLastKeyAction.IsRepeated(Key, Shift);
  try
    inherited KeyDown(Key, Shift);
    if FLastKeyAction = nil then
      FLastKeyAction := TdxDesignerFlowChartKeyAction.Create(Key, Shift);
  finally
    FIsKeyActionRepeated := False;
  end;
end;

procedure TdxDesignerFlowChart.WMSetCursor(var Message: TWMSetCursor);
begin
  if FIsAddingConnection then
  begin
    if AutoRouting and (GetEndpointHighlightedObject = nil) then
      Windows.SetCursor(Screen.Cursors[crDefault])
    else
      Windows.SetCursor(Screen.Cursors[crFlChartDrawFreeConnector]);
  end
  else
    inherited;
end;

function TdxDesignerFlowChart.GetDragHelper: TdxDesignerFlowChartDragHelper;
begin
  Result := TdxDesignerFlowChartDragHelper(inherited DragHelper);
end;

{ TdxFlowChartRibbonStyleEditor }

function TdxFlowChartDesigner.Execute(AChart: TdxFlowChart): Boolean;
var
  ASavedContent: TMemoryStream;
  ASavedEvent: TdxFcEvent;
begin
  Result := False;
  acUseAdvancedShapesOnly.Visible := AChart.IsDesigning;
  Mode := dmChartDesigner;
  ASavedEvent := AChart.OnDeletion;
  AChart.OnDeletion := nil;
  ASavedContent := TMemoryStream.Create;
  try
    AChart.SaveToStream(ASavedContent);
    ASavedContent.Position := 0;

    acAntialiasing.Checked := AChart.Antialiasing;
    Chart.Antialiasing := AChart.Antialiasing;
    SetControlLookAndFeel(Self, AChart.LookAndFeel);
    if AChart.LookAndFeel.SkinPainter <> nil then
      rRibbon.ColorSchemeName := Chart.LookAndFeel.SkinName;
    Chart.Images := AChart.Images;
    Chart.Color := AChart.Color;
    Chart.Font := AChart.Font;
    Chart.Font.Height := ScaleFactor.Apply(AChart.Font.Height, AChart.GetScaleFactor);
    Chart.Zoom := 100;
    Chart.LoadFromStream(ASavedContent);
    UpdateRibbonItemsEnabled;
    UpdateRibbonItemsByChartItems;
    UpdatePropertiesPanel;
    Chart.ResetUndoRedo;
    if ShowModal = mrOK then
    begin
      ASavedContent.Position := 0;
      Chart.SaveToStream(ASavedContent);
      ASavedContent.Position := 0;
      AChart.LoadFromStream(ASavedContent);
      AChart.Color := Chart.Color;
      Result := True;
    end;
  finally
    ASavedContent.Free;
    AChart.OnDeletion := ASavedEvent;
  end;
end;

procedure TdxFlowChartDesigner.SetOptions(AOptions: TdxFlowChartEditorOptions);
begin
  UseAdvancedShapesOnly := (AOptions = nil) or AOptions.UseAdvancedShapesOnly;
end;

procedure TdxFlowChartDesigner.LoadFromFile(const AFileName: string);
begin
  Chart.LoadFromFile(AFileName);
  Chart.ResetUndoRedo;
  UpdateRibbonItemsEnabled;
  SaveDialog.FileName := OpenDialog.FileName;
end;

procedure TdxFlowChartDesigner.mTextKeyPress(Sender: TObject; var Key: Char);
var
  AMemo: TcxMemo;
begin
  AMemo := Safe<TcxMemo>.Cast(Sender);
  if AMemo <> nil then
    AMemo.KeyPressShiftEnterHandler(Key);
end;

procedure TdxFlowChartDesigner.mTextPropertiesEditValueChanged(Sender: TObject);
var
  AText: string;
begin
  if FUpdatingPropertiesControls then
    Exit;
  AText := mText.Text;
  ForEachSelectedChartItem(
    procedure (AItem: TdxFcItem)
    begin
      AItem.Text := AText;
    end);
end;

procedure TdxFlowChartDesigner.AddAdvancedGalleryShapes(AStencil: TdxFlowChartAdvancedShapeStencil;
  ANeedSetChecked: Boolean);
var
  I: Integer;
  AItem: TdxGalleryControlItem;
  AShape: TdxFlowChartObjectAdvancedShape;
begin
  for I := 0 to AStencil.Count - 1 do
  begin
    AShape := AStencil.Shapes[I];
    if not UseAdvancedShapesOnly and AShape.IsLegacyReplacement then
      Continue;
    AItem := gcgShapes.Items.Add;
    AItem.Caption := AShape.Caption;
    if ANeedSetChecked then
      AItem.Checked := I = 0;
    ShapeGalleryInfo.Add(AItem, TdxFlowChartShapeInfo.Create(fcsAdvanced, AShape));
    DrawAdvancedGalleryShape(AItem, AShape);
  end;
end;

procedure TdxFlowChartDesigner.AddObjectOnDrag(APoint: TPoint);
var
  AObject: TdxFcObject;
  AShapeType: TdxFcShapeType;
  ADownPoint, AUpPoint: TPoint;
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  Chart.BeginTransactChange;
  try
    AShapeType := TdxFlowChartShapeInfo(bbStandardShape.Data).ShapeType;
    ADownPoint := Chart.ChartPoint(FDownPoint);
    AUpPoint := Chart.ChartPoint(APoint);
    AHeight := Abs(ADownPoint.Y - AUpPoint.Y);
    AWidth := Abs(ADownPoint.X - AUpPoint.X);
    ALeft := Min(ADownPoint.X, AUpPoint.X);
    ATop := Min(ADownPoint.Y, AUpPoint.Y);
    AObject := Chart.CreateObject(ALeft, ATop, AWidth, AHeight, AShapeType);
    AObject.Selected := True;
    AObject.BkColor := dxDefaultShapeBkColor;
    AObject.ShapeColor := dxDefaultShapeColor;
    acPointerTool.Execute;
  finally
    Chart.EndTransactChange;
  end;
end;

procedure TdxFlowChartDesigner.AddStandardGalleryShape(AShapeType: TdxFcShapeType);
var
  AItem: TdxGalleryControlItem;
begin
  AItem := gcgShapes.Items.Add;
  AItem.Caption := GetStandardGalleryShapeCaption(AShapeType);
  AItem.Checked := AShapeType = fcsRectangle;
  ShapeGalleryInfo.Add(AItem, TdxFlowChartShapeInfo.Create(AShapeType));
  DrawStandardGalleryShape(AItem, AShapeType);
end;

procedure TdxFlowChartDesigner.AddStandardGalleryShapes;
begin
  ForEachStandardShape(AddStandardGalleryShape);
end;

procedure TdxFlowChartDesigner.AddToolsStandardShape(AShapeType: TdxFcShapeType);
var
  AMenuItem: TdxBarButton;
begin
  AMenuItem := TdxBarButton(bpmStandardShape.ItemLinks.AddButton.Item);
  AMenuItem.Caption := GetStandardGalleryShapeCaption(AShapeType);
  AMenuItem.ShortCut := GetStandardGalleryShapeShortCut(AShapeType);
  AMenuItem.ButtonStyle := bsChecked;
  AMenuItem.GroupIndex := dxToolStandardShapesGroupIndex;
  AMenuItem.Down := AShapeType = fcsRectangle;
  AMenuItem.Data := TdxFlowChartShapeInfo.Create(AShapeType);
  DrawToolsStandardShape(AMenuItem, AShapeType);
  AMenuItem.OnClick := ToolsStandardShapeItemClickHandler;
  if AMenuItem.Down then
    AMenuItem.Click;
end;

function TdxFlowChartDesigner.AllowOpenDocument: Boolean;
var
  AMsgRes: Integer;
begin
  Result := True;
  AMsgRes := MessageDlg(dxNewConfirmation, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  case AMsgRes of
    mrYes:
      begin
        acSave.Execute;
        if FModified then
          Result := False
        else
          if Mode = dmChartDesigner then
            FModified := True;
      end;
    mrCancel:
      Result := False;
  end;
end;

procedure TdxFlowChartDesigner.BuildMenuStencils;
var
  I: Integer;
  AMenuButton: TdxBarButton;
  AStencil: TdxFlowChartAdvancedShapeStencil;
begin
  bpmStensils.ItemLinks.BeginUpdate;
  try
    for I := 0 to Chart.Repository.StencilCount - 1 do
    begin
      AStencil := Chart.Repository.Stencils[I];
      if not AStencil.HasVisibleShapes then
        Continue;
      AMenuButton := TdxBarButton(bpmStensils.ItemLinks.AddButton.Item);
      AMenuButton.Caption := AStencil.Caption;
      AMenuButton.ButtonStyle := bsChecked;
      AMenuButton.Down := (AStencil = Chart.Repository.BasicShapes) or (AStencil = Chart.Repository.BasicFlowchartShapes);
      AMenuButton.OnClick := MenuStencilsButtonClickHandler;
      StencilMenuInfo.Add(AMenuButton, AStencil);
    end;
  finally
    bpmStensils.ItemLinks.EndUpdate;
  end;
end;

procedure TdxFlowChartDesigner.BuildToolsStandardShapes;
begin
  bpmStandardShape.ItemLinks.BeginUpdate;
  try
    ForEachStandardShape(AddToolsStandardShape);
  finally
    bpmStandardShape.ItemLinks.EndUpdate;
  end;
end;

procedure TdxFlowChartDesigner.cbDArrowSizePropertiesEditValueChanged(Sender: TObject);
var
  ASize: TdxFcaSize;
begin
  if FUpdatingPropertiesControls then
    Exit;
  ASize := GetArrowSize(cbDArrowSize.ItemIndex);
  ForEachSelectedConnection(
    procedure (AConnection: TdxFcConnection)
    begin
      AConnection.ArrowDest.Size := ASize;
    end);
  UpdateConnectionsPropertiesControls;
end;

procedure TdxFlowChartDesigner.icbLineStylePropertiesEditValueChanged(Sender: TObject);
var
  APenStyle: TPenStyle;
begin
  if FUpdatingPropertiesControls then
    Exit;

  APenStyle := TPenStyle(EnsureRange(icbLineStyle.ItemIndex, 0, 4));
  ChangeSelectedChartItems(
    procedure (AItem: TdxFcItem)
    var
      AConnection: TdxFcConnection;
      AObject: TdxFcObject;
    begin
      AObject := Safe<TdxFcObject>.Cast(AItem);
      if AObject <> nil then
        AObject.ShapeStyle := APenStyle;
      AConnection := Safe<TdxFcConnection>.Cast(AItem);
      if AConnection <> nil then
        AConnection.PenStyle := APenStyle;
    end);
end;

procedure TdxFlowChartDesigner.cbSArrowSizePropertiesEditValueChanged(Sender: TObject);
var
  ASize: TdxFcaSize;
begin
  if FUpdatingPropertiesControls then
    Exit;
  ASize := GetArrowSize(cbSArrowSize.ItemIndex);
  ForEachSelectedConnection(
    procedure (AConnection: TdxFcConnection)
    begin
      AConnection.ArrowSource.Size := ASize;
    end);
  UpdateConnectionsPropertiesControls;
end;

procedure TdxFlowChartDesigner.ChangeSelectedChartItems(const AProc: TdxFlowChartForEachSelectedItemProc);
begin
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self,
    procedure ()
    begin
      Chart.BeginTransactChange;
      try
        ForEachSelectedChartItem(AProc);
      finally
        Chart.EndTransactChange;
      end;
    end);
end;

procedure TdxFlowChartDesigner.ChangeSelectedConnections(const AProc: TdxFlowChartForEachSelectedConnectionProc);
begin
  Chart.BeginTransactChange;
  try
    ForEachSelectedConnection(AProc);
  finally
    Chart.EndTransactChange;
  end;
end;

procedure TdxFlowChartDesigner.ChangeSelectedObjects(const AProc: TdxFlowChartForEachSelectedObjectProc);
begin
  Chart.BeginTransactChange;
  try
    ForEachSelectedObject(AProc);
  finally
    Chart.EndTransactChange;
  end;
end;

function TdxFlowChartDesigner.CheckActiveChartTextHorzAlign(AAlign: TdxFcHorzPos): Boolean;
begin
  if FirstSelectedChartItem is TdxFcObject then
    Result := FirstSelectedChartObject.HorzTextPos = AAlign
  else
    Result := False;
end;

function TdxFlowChartDesigner.CheckActiveChartTextVertAlign(AAlign: TdxFcVertPos): Boolean;
begin
  if FirstSelectedChartItem is TdxFcObject then
    Result := FirstSelectedChartObject.VertTextPos = AAlign
  else
    Result := False;
end;

procedure TdxFlowChartDesigner.ClearShapeGalleryInfo;
begin
  ShapeGalleryInfo.Clear;
end;

procedure TdxFlowChartDesigner.ClearToolsStandardShapes;
var
  I: Integer;
begin
  for I := 0 to bpmStandardShape.ItemLinks.Count - 1 do
    bpmStandardShape.ItemLinks[I].Item.Data.Free;
end;

procedure TdxFlowChartDesigner.DrawAdvancedGalleryShape(AGalleryItem: TdxGalleryControlItem; AShape: TdxFlowChartObjectAdvancedShape);
var
  AGlyph: TdxSmartGlyph;
  ACanvas: TdxGpCanvas;
begin
  AGlyph := TdxSmartGlyph.CreateSize(gcShapes.OptionsView.Item.Image.Size.Size);
  try
    ACanvas := AGlyph.CreateCanvas;
    try
      AShape.PaintShape(ACanvas, cxRectInflate(AGlyph.ClientRect, -1),
        TdxAlphaColors.FromColor(dxDefaultShapeColor),
        TdxAlphaColors.FromColor(dxDefaultShapeBkColor));
      AGalleryItem.Glyph := AGlyph;
    finally
      ACanvas.Free;
    end;
  finally
    AGlyph.Free;
  end;
end;

procedure TdxFlowChartDesigner.DrawDragObject;
var
  ARect: TRect;
  AShapeType: TdxFcShapeType;
begin
  Chart.Canvas.SaveState;
  try
    Chart.Canvas.Pen.Color := clBlack;
    Chart.Canvas.Pen.Mode := pmNotXor;
    Chart.Canvas.Pen.Style := psSolid;
    Chart.Canvas.Brush.Style := bsClear;
    ARect := Rect(FDownPoint, FDragPoint);
    AShapeType := TdxFlowChartShapeInfo(bbStandardShape.Data).ShapeType;
    case AShapeType of
      fcsRectangle:
        Chart.Canvas.Rectangle(ARect);
      fcsEllipse:
        Chart.Canvas.Ellipse(ARect);
      fcsRoundRect:
        Chart.Canvas.RoundRect(ARect, ARect.Width shr 1, ARect.Height shr 1);
      else
        Chart.Canvas.Polygon(GetStandardGalleryShapePolygonPoints(AShapeType, ARect));
    end;
  finally
    Chart.Canvas.RestoreState;
  end;
end;

procedure TdxFlowChartDesigner.DrawStandardGalleryShape(AShapeGalleryItem: TdxGalleryControlItem; AShapeType: TdxFcShapeType);
var
  AGlyph: TdxSmartGlyph;
begin
  AGlyph := GetStandardShapeGlyph(AShapeType, gcShapes.OptionsView.Item.Image.Size.Size);
  try
    AShapeGalleryItem.Glyph := AGlyph;
  finally
    AGlyph.Free;
  end;
end;

procedure TdxFlowChartDesigner.DrawToolsStandardShape(AMenuItem: TdxBarButton; AShapeType: TdxFcShapeType);
var
  AGlyph: TdxSmartGlyph;
begin
  AGlyph := GetStandardShapeGlyph(AShapeType, cxSize(dxToolStandardShapesGlyphSize));
  try
    AMenuItem.Glyph := AGlyph;
  finally
    AGlyph.Free;
  end;
end;

function TdxFlowChartDesigner.GetFocusedGalleryStencil: TdxFlowChartAdvancedShapeStencil;
begin
  StencilGalleryInfo.TryGetValue(gcStencils.Gallery.GetCheckedItem, Result);
end;

function TdxFlowChartDesigner.GetStandardGalleryShapeCaption(AShapeType: TdxFcShapeType): string;
begin
  Result := cxGetResourceString(dxFlowChartShapeNamesMap[AShapeType]);
end;

function TdxFlowChartDesigner.GetStandardGalleryShapeShortCut(AShapeType: TdxFcShapeType): TShortCut;
begin
  Result := TextToShortCut(dxStandardShapeShortCutText[AShapeType]);
end;

function TdxFlowChartDesigner.GetStandardGalleryShapePolygonPoints(AShapeType: TdxFcShapeType; ABounds: TRect): TPoints;
begin
  case AShapeType of
    fcsDiamond:
      begin
        SetLength(Result, 4);
        Result[0] := Point(ABounds.Left + ABounds.Width shr 1, ABounds.Top);
        Result[1] := Point(ABounds.Right, ABounds.Top + ABounds.Height shr 1);
        Result[2] := Point(ABounds.Left + ABounds.Width shr 1, ABounds.Bottom);
        Result[3] := Point(ABounds.Left, ABounds.Top + ABounds.Height shr 1);
      end;
    fcsNorthTriangle:
      begin
        SetLength(Result, 3);
        Result[0] := Point(ABounds.Left + ABounds.Width shr 1, ABounds.Top);
        Result[1] := Point(ABounds.Right, ABounds.Bottom);
        Result[2] := Point(ABounds.Left, ABounds.Bottom);
      end;
    fcsSouthTriangle:
      begin
        SetLength(Result, 3);
        Result[0] := Point(ABounds.Left, ABounds.Top);
        Result[1] := Point(ABounds.Right, ABounds.Top);
        Result[2] := Point(ABounds.Left + ABounds.Width shr 1, ABounds.Bottom);
      end;
    fcsEastTriangle:
      begin
        SetLength(Result, 3);
        Result[0] := Point(ABounds.Left, ABounds.Top);
        Result[1] := Point(ABounds.Right,  ABounds.Top + ABounds.Height shr 1);
        Result[2] := Point(ABounds.Left, ABounds.Bottom);
      end;
    fcsWestTriangle:
      begin
        SetLength(Result, 3);
        Result[0] := Point(ABounds.Left, ABounds.Top + ABounds.Height shr 1);
        Result[1] := Point(ABounds.Right, ABounds.Top);
        Result[2] := Point(ABounds.Right, ABounds.Bottom);
      end;
    fcsHexagon:
      begin
        SetLength(Result, 6);
        Result[0] := Point(ABounds.Left + ABounds.Width shr 2, ABounds.Top);
        Result[1] := Point(ABounds.Right - ABounds.Width shr 2, ABounds.Top);
        Result[2] := Point(ABounds.Right, ABounds.Top + ABounds.Height shr 1);
        Result[3] := Point(ABounds.Right - ABounds.Width shr 2, ABounds.Bottom);
        Result[4] := Point(ABounds.Left + ABounds.Width shr 2, ABounds.Bottom);
        Result[5] := Point(ABounds.Left, ABounds.Top + ABounds.Height shr 1);
      end;
    else
      Result := nil;
  end;
end;

function TdxFlowChartDesigner.GetStandardShapeGlyph(AShapeType: TdxFcShapeType; ASize: TSize): TdxSmartGlyph;
var
  ARect: TRect;
  ACanvas: TdxGpCanvas;
begin
  Result := TdxSmartGlyph.CreateSize(ASize);
  ACanvas := Result.CreateCanvas;
  try
    ACanvas.SmoothingMode := smAntiAlias;
    ARect := cxRectInflate(Result.ClientRect, -1);
    case AShapeType of
      fcsRectangle:
        ACanvas.Rectangle(ARect,
          dxDefaultShapeColor,
          dxDefaultShapeBkColor,
          1, psSolid, 255, 255);
      fcsEllipse:
        ACanvas.Ellipse(ARect,
          dxDefaultShapeColor,
          dxDefaultShapeBkColor,
          1, psSolid, 255, 255);
      fcsRoundRect:
        ACanvas.RoundRect(ARect,
          TdxAlphaColors.FromColor(dxDefaultShapeColor),
          TdxAlphaColors.FromColor(dxDefaultShapeBkColor),
          ARect.Width shr 1, ARect.Height shr 1);
      else
        ACanvas.Polygon(GetStandardGalleryShapePolygonPoints(AShapeType, ARect),
          dxDefaultShapeColor,
          dxDefaultShapeBkColor,
          1, psSolid, 255, 255);
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TdxFlowChartDesigner.InitializeChart;
begin
  Chart.Align := alClient;
  Chart.Antialiasing := True;
  Chart.GridLineOptions.ShowLines := True;
  Chart.OnSelected := ChartSelectedHandler;
  Chart.Options := [fcoCanDelete, fcoCanDrag, fcoCanSelect, fcoMultiSelect, fcoDelOnClick, fcoCanRotate,
    fcoUseShapeParameters, fcoSnapToGuides];
  Chart.PopupMenu := bpmChart;
  Chart.OnDblClick := ChartDblClickHandler;
  Chart.OnDragOver := ChartDragOverHandler;
  Chart.OnKeyDown := ChartKeyDownHandler;
  Chart.OnMouseDown := ChartMouseDownHandler;
  Chart.OnMouseMove := ChartMouseMoveHandler;
  Chart.OnMouseUp := ChartMouseUpHandler;
  Chart.OnMouseWheel := ChartMouseWheelHandler;
  Chart.OnTransactChanged := ChartTransactChangedHandler;
  Chart.Parent := Self;
end;

procedure TdxFlowChartDesigner.InitializeConnectionTypes;

  procedure Add(AIndex, AImageIndex: Integer; const AText: string);
  begin
    with icbConnectionType.Properties.Items.Add do
    begin
      Value := AIndex;
      Description := AText;
      ImageIndex := AImageIndex;
    end;
  end;

begin
  Add(0, 44, cxGetResourceString(@sdxFlowChartConnectionStyleStraight));
  Add(1, 43, cxGetResourceString(@sdxFlowChartConnectionStyleCurved));
  Add(2, 41, cxGetResourceString(@sdxFlowChartConnectionStyleRectHorizontal));
  Add(3, 42, cxGetResourceString(@sdxFlowChartConnectionStyleRectVertical));
end;

procedure TdxFlowChartDesigner.InitializeArrowTypeImages;
var
  I: TdxFcaType;
  B: TcxBitmap32;
  G: TdxGPGraphics;
  H, W: Integer;
  ADescription: string;
begin
  ilBeginArrowTypes.Clear;
  ilEndArrowTypes.Clear;
  H := ilBeginArrowTypes.Height;
  W := ilBeginArrowTypes.Width;
  B := TcxBitmap32.CreateSize(W, H);
  G := dxGpBeginPaint(B.Canvas.Handle, B.ClientRect);
  try
    G.FillRectangle(b.ClientRect, TdxAlphaColors.White);
    for I := Low(TdxFcaType) to High(TdxFcaType) do
    begin
      if I <> fcaNone then
        TdxFcConnectionArrow.GetRepository.DrawPreview(Ord(I), G, B.ClientRect, Chart.ScaleFactor.ApplyF(1.0), True);
      ilBeginArrowTypes.Add(B, nil);
      ADescription := cxGetResourceString(dxFlowChartArrowStyleNamesMap[I]);
      with icbBeginArrow.Properties.Items.Add do
      begin
        Value := Ord(I);
        Description := ADescription;
        ImageIndex := Index;
      end;

      if I <> fcaNone then
        TdxFcConnectionArrow.GetRepository.DrawPreview(Ord(I), G, B.ClientRect, Chart.ScaleFactor.ApplyF(1.0), False);
      ilEndArrowTypes.Add(B, nil);
      with icbEndArrow.Properties.Items.Add do
      begin
        Value := Ord(I);
        Description := ADescription;
        ImageIndex := Index;
      end;
    end;
  finally
    dxGpEndPaint(G);
    B.Free;
  end;
end;

procedure TdxFlowChartDesigner.InitializeMoreShapesArrow;
begin
  FMoreShapesArrow.Left := 74;
  FMoreShapesArrow.Top := 7;
  FMoreShapesArrow.Height := 13;
  FMoreShapesArrow.Width := 13;
  FMoreShapesArrow.TabStop := False;
  FMoreShapesArrow.Transparent := True;
  FMoreShapesArrow.Style := imgMoreShapesArrow.Style;
  FMoreShapesArrow.Properties := imgMoreShapesArrow.Properties;
  FMoreShapesArrow.Picture := imgMoreShapesArrow.Picture;
  FMoreShapesArrow.Parent := imgMoreShapesArrow.Parent;
end;

procedure TdxFlowChartDesigner.InitializeArrowSizes;
var
  I: TdxFcaSize;
begin
  for I := Succ(fcasCustom) to High(TdxFcaSize) do
    cbSArrowSize.Properties.Items.Add(cxGetResourceString(dxFlowChartConnectionArrowSizeNamesMap[I]));
  cbSArrowSize.Properties.Items.Add(cxGetResourceString(dxFlowChartConnectionArrowSizeNamesMap[fcasCustom]));
  cbDArrowSize.Properties.Items.Assign(cbSArrowSize.Properties.Items);
end;

procedure TdxFlowChartDesigner.InitializeLineStyleImages;

  procedure Add(AIndex: Integer; const AText: string);
  begin
    with icbLineStyle.Properties.Items.Add do
    begin
      Value := AIndex;
      Description := AText;
      ImageIndex := AIndex;
    end;
  end;

begin
  Add(0, cxGetResourceString(@sdxFlowChartEditorLineSolid));
  Add(1, cxGetResourceString(@sdxFlowChartEditorLineDashed));
  Add(2, cxGetResourceString(@sdxFlowChartEditorLineDotted));
  Add(3, cxGetResourceString(@sdxFlowChartEditorLineDashDotted));
  Add(4, cxGetResourceString(@sdxFlowChartEditorLineDashDoubleDotted));
end;

function TdxFlowChartDesigner.IsStandardGalleryShape(AShapeType: TdxFcShapeType): Boolean;
begin
  Result := AShapeType in [fcsRectangle..fcsHexagon];
end;

procedure TdxFlowChartDesigner.ModeChanged;
begin
  acCloseAndApplyChanges.Visible := Mode = dmChartDesigner;
  rbHomeDesigner.Visible := Mode = dmChartDesigner;
  acSave.Visible := Mode = dmStandalone;
end;

procedure TdxFlowChartDesigner.MultiSelect(ResetOldSelected: Boolean; SelectRect: TRect);
var
  I: Integer;
begin
  if ResetOldSelected then
    Chart.ClearSelection;
  for I := 0 to Chart.ObjectCount - 1 do
  begin
    if Chart.Objects[i].InRect(SelectRect) then
      Chart.Objects[i].Selected := not Chart.Objects[i].Selected;
  end;
  for I := 0 to Chart.ConnectionCount - 1 do
  begin
    if Chart.Connections[i].InRect(SelectRect) then
      Chart.Connections[i].Selected := not Chart.Connections[i].Selected;
  end;
end;

function TdxFlowChartDesigner.NeedAddStandardGalleryShapes(AStencil: TdxFlowChartAdvancedShapeStencil): Boolean;
begin
  Result := (AStencil = Chart.Repository.BasicShapes) and not UseAdvancedShapesOnly;
end;

procedure TdxFlowChartDesigner.PopulateShortCuts;
begin
  ShortCuts.Add(acActualSize, ShortCut(VK_NUMPAD0, [ssCtrl]));
  ShortCuts.Add(acCopy, ShortCut(Ord('C'), [ssCtrl]));
  ShortCuts.Add(acCut, ShortCut(Ord('X'), [ssCtrl]));
  ShortCuts.Add(acDelete, ShortCut(VK_DELETE, []));
  ShortCuts.Add(acPaste, ShortCut(Ord('V'), [ssCtrl]));
  ShortCuts.Add(acSelectAll, ShortCut(Ord('A'), [ssCtrl]));
end;

procedure TdxFlowChartDesigner.PostponedUpdateRibbonItemsEnabled;
begin
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self,
    procedure ()
    begin
      UpdateRibbonItemsEnabled;
    end);
end;

procedure TdxFlowChartDesigner.pSourceColorClick(Sender: TObject);
begin
//  icbBeginArrow.EditValue := Null;
//  icbBeginArrow.EditText := 'test';
//  icbBeginArrow.Properties.DefaultDescription := 'Default';
//  icbBeginArrow.ItemIndex := -1;
end;

procedure TdxFlowChartDesigner.seAnglePropertiesEditValueChanged(Sender: TObject);
var
  AAngle: Integer;
begin
  if FUpdatingPropertiesControls then
    Exit;

  AAngle := seAngle.Value;
  ChangeSelectedObjects(
    procedure (AObject: TdxFcObject)
    begin
      AObject.Angle := AAngle;
    end);
end;

procedure TdxFlowChartDesigner.seLineThicknessPropertiesEditValueChanged(Sender: TObject);
var
  AThickness: Integer;
begin
  if FUpdatingPropertiesControls then
    Exit;

  AThickness := seLineThickness.Value;
  ChangeSelectedChartItems(
    procedure (AItem: TdxFcItem)
    var
      AConnection: TdxFcConnection;
      AObject: TdxFcObject;
    begin
      AObject := Safe<TdxFcObject>.Cast(AItem);
      if AObject <> nil then
        AObject.ShapeWidth := AThickness;
      AConnection := Safe<TdxFcConnection>.Cast(AItem);
      if AConnection <> nil then
        AConnection.PenWidth := AThickness;
    end);
end;

procedure TdxFlowChartDesigner.seShapeHeightPropertiesEditValueChanged(Sender: TObject);
var
  AHeight: Integer;
begin
  if FUpdatingPropertiesControls then
    Exit;

  AHeight := seShapeHeight.Value;
  ChangeSelectedObjects(
    procedure (AObject: TdxFcObject)
    begin
      AObject.Height := AHeight;
    end);
end;

procedure TdxFlowChartDesigner.seShapeWidthPropertiesEditValueChanged(Sender: TObject);
var
  AWidth: Integer;
begin
  if FUpdatingPropertiesControls then
    Exit;

  AWidth := seShapeWidth.Value;
  ChangeSelectedObjects(
    procedure (AObject: TdxFcObject)
    begin
      AObject.Width := AWidth;
    end);
end;

function TdxFlowChartDesigner.TryOpenHelpFile(const AFileName: string): Boolean;
begin
  Result := FileExists(AFileName) and dxShellExecute(Handle, AFileName, SW_SHOWMAXIMIZED);
end;

procedure TdxFlowChartDesigner.UpdateActiveGalleryShapes;
var
  AStencil: TdxFlowChartAdvancedShapeStencil;
begin
  gcShapes.BeginUpdate;
  try
    ClearShapeGalleryInfo;
    gcgShapes.Items.Clear;
    AStencil := GetFocusedGalleryStencil;
    if AStencil <> nil then
    begin
      if NeedAddStandardGalleryShapes(AStencil) then
        AddStandardGalleryShapes;
      AddAdvancedGalleryShapes(AStencil, NeedAddStandardGalleryShapes(AStencil));
    end;
  finally
    gcShapes.EndUpdate;
  end;
end;

procedure TdxFlowChartDesigner.UpdateBackgroundColorImage;
begin
  DrawHelpedColorLine(bbBackgroundColor.Glyph, FBackgroundColorPicker.Color, ilSmallIcons, acBackgroundColor.ImageIndex);
end;

procedure TdxFlowChartDesigner.UpdateLineColorImage;
begin
  DrawHelpedColorLine(btnLineColor.OptionsImage.Glyph, FLineColorPicker.Color,
    ilSmallIcons, btnLineColor.OptionsImage.ImageIndex);
end;

procedure TdxFlowChartDesigner.UpdateDestArrowColorImage;
begin
  DrawHelpedColorLine(btnDestArrowColor.OptionsImage.Glyph, FDestArrowColorPicker.Color,
    ilSmallIcons, btnDestArrowColor.OptionsImage.ImageIndex);
end;

procedure TdxFlowChartDesigner.UpdateFontColorImage;
begin
  DrawHelpedColorLine(bbFontColor.Glyph, FFontColorPicker.Color, ilSmallIcons, acFontColor.ImageIndex);
end;

procedure TdxFlowChartDesigner.UpdateGalleryStencils(AFocusedStencil: TdxFlowChartAdvancedShapeStencil = nil);
var
  I: Integer;
  AMenuButton: TdxBarButton;
  AStencilGalleryItem: TdxGalleryControlItem;
  AStencil: TdxFlowChartAdvancedShapeStencil;
begin
  gcStencils.BeginUpdate;
  try
    if AFocusedStencil = nil then
      AFocusedStencil := GetFocusedGalleryStencil;
    gcgStencils.Items.Clear;
    StencilGalleryInfo.Clear;
    for I := 0 to bpmStensils.ItemLinks.Count - 1 do
    begin
      AMenuButton := TdxBarButton(bpmStensils.ItemLinks[I].Item);
      if AMenuButton.Down and StencilMenuInfo.TryGetValue(AMenuButton, AStencil) then
      begin
        AStencilGalleryItem := gcgStencils.Items.Add;
        AStencilGalleryItem.Caption := AStencil.Caption;
        StencilGalleryInfo.Add(AStencilGalleryItem, AStencil);
        AStencilGalleryItem.Checked := AStencil = AFocusedStencil;
      end;
    end;
    if gcgStencils.ItemCount = 0 then
      UpdateActiveGalleryShapes
    else
      if gcStencils.Gallery.GetCheckedItem = nil then
        gcgStencils.Items[0].Checked := True;
  finally
    gcStencils.EndUpdate;
  end;
end;

procedure TdxFlowChartDesigner.UpdateObjectBkColorImage;
begin
  DrawHelpedColorLine(btnObjectBkColor.OptionsImage.Glyph, FObjectBkColorPicker.Color, ilSmallIcons,
    btnObjectBkColor.OptionsImage.ImageIndex);
end;

procedure TdxFlowChartDesigner.UpdateRibbonItemsCaption;
begin
  acOpen.Caption := cxGetResourceString(@sdxFlowChartEditorFileOpen);
  acSaveAs.Caption := cxGetResourceString(@sdxFlowChartEditorFileSave);

  acBringToFront.Caption := cxGetResourceString(@sdxFlowChartEditorEditBringToFront);
  acClearSelection.Caption := cxGetResourceString(@sdxFlowChartEditorEditClearSelection);
  acCopy.Caption := cxGetResourceString(@sdxFlowChartEditorEditCopy);
  acCut.Caption := cxGetResourceString(@sdxFlowChartEditorEditCut);
  acDelete.Caption := cxGetResourceString(@sdxFlowChartEditorEditDelete);
  acPaste.Caption := cxGetResourceString(@sdxFlowChartEditorEditPaste);
  acSelectAll.Caption := cxGetResourceString(@sdxFlowChartEditorEditSelectAll);
  acSendToBack.Caption := cxGetResourceString(@sdxFlowChartEditorEditSendToBack);
  acUndo.Caption := cxGetResourceString(@sdxFlowChartEditorEditUndo);

  acProperties.Caption := cxGetResourceString(@sdxFlowChartEditorProperties);

  acActualSize.Caption := cxGetResourceString(@sdxFlowChartEditorViewActualSize);
  acAntialiasing.Caption := cxGetResourceString(@sdxFlowChartEditorViewAntialiasing);
  acFit.Caption := cxGetResourceString(@sdxFlowChartEditorViewFit);
  acZoomIn.Caption := cxGetResourceString(@sdxFlowChartEditorViewZoomIn);
  acZoomOut.Caption := cxGetResourceString(@sdxFlowChartEditorViewZoomOut);

  acAddToUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsAdd);
  acClearAllUnions.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsClearAll);
  acClearUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsClear);
  acNewUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsNew);
  acRemoveFromUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsRemove);

  acContents.Caption := cxGetResourceString(@sdxFlowChartEditorHelpContents);
end;

procedure TdxFlowChartDesigner.UpdateRibbonItemsEnabled;
var
  I: Integer;
  AHasSelectedItem, AHasSelectedObject: Boolean;
begin
  acCut.Enabled := Chart.SelCount > 0;
  acCopy.Enabled := Chart.SelCount > 0;
  acClearSelection.Enabled := Chart.SelCount > 0;
  acDelete.Enabled := Chart.SelCount > 0;
  acProperties.Enabled := Chart.SelCount > 0;
  acPaste.Enabled := FBuffer.ObjectCount + FBuffer.ConnectionCount > 0;
  AHasSelectedItem := FirstSelectedChartItem <> nil;
  AHasSelectedObject := Chart.SelectedObjectCount > 0;
  acBringToFront.Enabled := AHasSelectedObject;
  acSendToBack.Enabled := AHasSelectedObject;
  acNewUnion.Enabled := Chart.SelectedObjectCount > 1;
  acAddToUnion.Enabled := ChartHasUnions and AHasSelectedObject;
  acRemoveFromUnion.Enabled := False;
  for I := 0 to Chart.SelectedObjectCount - 1 do
    if IsChildItemInUnion(Chart.SelectedObjects[I]) then
    begin
      acRemoveFromUnion.Enabled := True;
      Break;
    end;
  acClearUnion.Enabled := False;
  for I := 0 to Chart.SelectedObjectCount - 1 do
    if IsMainItemInUnion(Chart.SelectedObjects[I]) then
      acClearUnion.Enabled := True;
  acClearAllUnions.Enabled := ChartHasUnions;
  acZoomOut.Enabled := not acFit.Checked and (Chart.Zoom > dxMinZoom);
  acZoomIn.Enabled := not acFit.Checked and (Chart.Zoom < dxMaxZoom);
  acActualSize.Enabled := not acFit.Checked and (Chart.Zoom <> dxActualZoom);
  acFontBold.Enabled := AHasSelectedItem;
  acFontItalic.Enabled := AHasSelectedItem;
  acFontUnderline.Enabled := AHasSelectedItem;
  acFontStrikeout.Enabled := AHasSelectedItem;
  acFontSizeInc.Enabled := AHasSelectedItem;
  acFontSizeDec.Enabled := AHasSelectedItem;
  acTextAlignTop.Enabled := AHasSelectedObject;
  acTextAlignMiddle.Enabled := AHasSelectedObject;
  acTextAlignBottom.Enabled := AHasSelectedObject;
  acTextAlignLeft.Enabled := AHasSelectedObject;
  acTextAlignCenter.Enabled := AHasSelectedObject;
  acTextAlignRight.Enabled := AHasSelectedObject;
  acFakeTextVertAlign.Checked := not (acTextAlignTop.Enabled or acTextAlignMiddle.Enabled or acTextAlignBottom.Enabled);
  acFakeTextHorzAlign.Checked := not (acTextAlignLeft.Enabled or acTextAlignCenter.Enabled or acTextAlignRight.Enabled);
  bcbFontName.Enabled := AHasSelectedItem;
  bcbFontSize.Enabled := AHasSelectedItem;
  bbFontColor.Enabled := AHasSelectedItem;
  bbBackgroundColor.Enabled := FirstSelectedChartItem is TdxFcObject;
  bbStrokeColor.Enabled := AHasSelectedItem;
  acUndo.Enabled := Chart.CanUndo;
  acRedo.Enabled := Chart.CanRedo;
  acRemovePoint.Enabled := Chart.SelectedConnectionCount > 0;
  acApplyLayeredLayout.Enabled := Chart.ObjectCount > 1;
end;

procedure TdxFlowChartDesigner.UpdateRibbonItemsByChartItems;
begin
  FIsInternalUpdatingRibbonItemsByChartItems := True;
  try
    acFontBold.Checked := fsBold in ActiveChartFont.Style;
    acFontItalic.Checked := fsItalic in ActiveChartFont.Style;
    acFontUnderline.Checked := fsUnderline in ActiveChartFont.Style;
    acFontStrikeout.Checked := fsStrikeOut in ActiveChartFont.Style;
    acTextAlignTop.Checked := CheckActiveChartTextVertAlign(fcvpUp);
    acTextAlignMiddle.Checked := CheckActiveChartTextVertAlign(fcvpCenter);
    acTextAlignBottom.Checked := CheckActiveChartTextVertAlign(fcvpDown);
    acTextAlignLeft.Checked := CheckActiveChartTextHorzAlign(fchpLeft);
    acTextAlignCenter.Checked := CheckActiveChartTextHorzAlign(fchpCenter);
    acTextAlignRight.Checked := CheckActiveChartTextHorzAlign(fchpRight);
    bcbFontName.EditValue := ActiveChartFont.Name;
    bcbFontSize.EditValue := IntToStr(ActiveChartFont.Size);
  finally
    FIsInternalUpdatingRibbonItemsByChartItems := False;
  end;
end;

procedure TdxFlowChartDesigner.UpdateSourceArrowColorImage;
begin
  DrawHelpedColorLine(btnSourceArrowColor.OptionsImage.Glyph, FSourceArrowColorPicker.Color,
    ilSmallIcons, btnSourceArrowColor.OptionsImage.ImageIndex);
end;

function TdxFlowChartDesigner.GetArrowSize(AIndex: Integer): TdxFcaSize;
begin
  if AIndex in [Ord(Low(TdxFcaSize))..Pred(Ord(High(TdxFcaSize)))] then
    Result := TdxFcaSize(AIndex + 1)
  else
    Result := TdxFcaSize.fcasCustom;
end;

procedure TdxFlowChartDesigner.UpdatePropertiesPanel;
begin
  FUpdatingPropertiesControls := True;
  try
    UpdateCommonPropertiesControls;
    UpdateObjectsPropertiesControls;
    UpdateConnectionsPropertiesControls;
  finally
    FUpdatingPropertiesControls := False;
  end;
end;

procedure TdxFlowChartDesigner.IterateGroupControls(AGroup: TdxCustomLayoutGroup; const AAction: TdxFlowChartGroupControlsActionProc);
var
  I: Integer;
  AControl: TWinControl;
  AItem: TdxLayoutItem;
  AChildGroup: TdxCustomLayoutGroup;
begin
  for I := 0 to AGroup.Count - 1 do
  begin
    AChildGroup := Safe<TdxCustomLayoutGroup>.Cast(AGroup[I]);
    if AChildGroup <> nil then
      IterateGroupControls(AChildGroup, AAction)
    else
    begin
      AItem := Safe<TdxLayoutItem>.Cast(AGroup[I]);
      if AItem <> nil then
      begin
        AControl := Safe<TWinControl>.Cast(AItem.Control);
        if AControl <> nil then
          AAction(AControl);
      end;
    end;
  end;
end;

function TdxFlowChartDesigner.CompareObjectValue(const ACompare: TdxFlowChartCompareObjectValueFunc; AStartIndex: Integer): Boolean;
var
  I: Integer;
begin
  if Assigned(ACompare) then
  begin
    for I := AStartIndex to Chart.SelectedObjectCount - 1 do
      if not ACompare(Chart.SelectedObjects[I]) then
        Exit(False);
  end;
  Result := True;
end;

function TdxFlowChartDesigner.CompareConnectionValue(const ACompare: TdxFlowChartCompareConnectionValueFunc; AStartIndex: Integer): Boolean;
var
  I: Integer;
begin
  if Assigned(ACompare) then
  begin
    for I := AStartIndex to Chart.SelectedConnectionCount - 1 do
      if not ACompare(Chart.SelectedConnections[I]) then
        Exit(False);
  end;
  Result := True;
end;

function TdxFlowChartDesigner.FirstSelectedChartObject: TdxFcObject;
begin
  if Chart.SelectedObjectCount > 0 then
    Result := Chart.SelectedObjects[0]
  else
    Result := nil;
end;

function TdxFlowChartDesigner.FirstSelectedChartItem: TdxFcItem;
begin
  Result := FirstSelectedChartObject;
  if Result = nil then
    Result := FirstSelectedChartConnection;
end;

function TdxFlowChartDesigner.FirstSelectedChartConnection: TdxFcConnection;
begin
  if Chart.SelectedConnectionCount > 0 then
    Result := Chart.SelectedConnections[0]
  else
    Result := nil;
end;

procedure TdxFlowChartDesigner.UpdateCommonPropertiesControls;

  function AreAllEqual(const ACompareObjectValue: TdxFlowChartCompareObjectValueFunc;
    const ACompareConnectionValue: TdxFlowChartCompareConnectionValueFunc): Boolean;
  begin
    Result := CompareObjectValue(ACompareObjectValue, 0) and
      CompareConnectionValue(ACompareConnectionValue, 0);
  end;

  procedure SetMemoText(const AText: string);
  begin
    if AreAllEqual(
     function (AObject: TdxFcObject): Boolean
     begin
       Result := AObject.Text = AText;
     end,
     function (AConnection: TdxFcConnection): Boolean
     begin
       Result := AConnection.Text = AText;
     end) then
      mText.EditValue := AText
    else
      mText.EditValue := Null;
    mText.ModifiedAfterEnter := False;
  end;

  procedure SetLineStyle(APenStyle: TPenStyle);
  begin
    if AreAllEqual(
     function (AObject: TdxFcObject): Boolean
     begin
       Result := AObject.ShapeStyle = APenStyle;
     end,
     function (AConnection: TdxFcConnection): Boolean
     begin
       Result := AConnection.PenStyle = APenStyle;
     end) then
      icbLineStyle.ItemIndex := Ord(APenStyle)
    else
      icbLineStyle.EditValue := Null;
    mText.ModifiedAfterEnter := False;
  end;

  procedure SetLineThickness(AThickness: Integer);
  begin
    if AreAllEqual(
     function (AObject: TdxFcObject): Boolean
     begin
       Result := AObject.ShapeWidth = AThickness;
     end,
     function (AConnection: TdxFcConnection): Boolean
     begin
       Result := AConnection.PenWidth = AThickness;
     end) then
      seLineThickness.EditValue := AThickness
    else
      seLineThickness.EditValue := Null;
    seLineThickness.ModifiedAfterEnter := False;
  end;

var
  AEnabled: Boolean;
  APenStyle: TPenStyle;
  AText: string;
  AThickness: Integer;
begin
  AEnabled := (Chart.SelectedConnectionCount > 0) or (Chart.SelectedObjectCount > 0);
  IterateGroupControls(lgCommon,
    procedure (AControl: TWinControl)
    var
      AEdit: TcxCustomEdit;
    begin
      AControl.Enabled := AEnabled;
      if not AEnabled then
      begin
        AEdit := Safe<TcxCustomEdit>.Cast(AControl);
        if AEdit <> nil then
          AEdit.EditValue := Null;
      end;
    end);
  lgCommon.Enabled := AEnabled;
  if not AEnabled then
    Exit;

  if Chart.SelectedObjectCount > 0 then
  begin
    AText := FirstSelectedChartObject.Text;
    APenStyle := FirstSelectedChartObject.ShapeStyle;
    AThickness := FirstSelectedChartObject.ShapeWidth;
    FLineColorPicker.Color := FirstSelectedChartObject.ShapeColor;
  end
  else
  begin
    AText := FirstSelectedChartConnection.Text;
    APenStyle := FirstSelectedChartConnection.PenStyle;
    AThickness := FirstSelectedChartConnection.PenWidth;
    FLineColorPicker.Color := FirstSelectedChartConnection.Color;
  end;
  SetMemoText(AText);
  SetLineStyle(APenStyle);
  SetLineThickness(AThickness);

  IterateGroupControls(lgLine,
    procedure (AControl: TWinControl)
    var
      AEdit: TcxCustomEdit;
    begin
      AEdit := Safe<TcxCustomEdit>.Cast(AControl);
      if AEdit <> nil then
        AEdit.ModifiedAfterEnter := False;
    end);
end;

procedure TdxFlowChartDesigner.UpdateObjectsPropertiesControls;

  procedure SetSpinEdit(AValue: Integer; ASpinEdit: TcxSpinEdit; const ACompare: TdxFlowChartCompareObjectValueFunc);
  begin
    if CompareObjectValue(ACompare, 1) then
      ASpinEdit.EditValue := AValue
    else
      ASpinEdit.EditValue := Null;
  end;

var
  AEnabled: Boolean;
  AAngle, AHeight, AWidth: Integer;
  AFirstObject: TdxFcObject;
begin
  AEnabled := Chart.SelectedObjectCount > 0;
  IterateGroupControls(lgObjects,
    procedure (AControl: TWinControl)
    var
      AEdit: TcxCustomEdit;
    begin
      AControl.Enabled := AEnabled;
      if not AEnabled then
      begin
        AEdit := Safe<TcxCustomEdit>.Cast(AControl);
        if AEdit <> nil then
          AEdit.EditValue := Null;
      end;
    end);
  lgObjects.Enabled := AEnabled;
  if not AEnabled then
    Exit;
  AFirstObject := FirstSelectedChartObject;
  FObjectBkColorPicker.Color := AFirstObject.BkColor;
  AAngle := Round(AFirstObject.Angle);
  SetSpinEdit(AAngle, seAngle,
    function (AObject: TdxFcObject): Boolean
    begin
      Result := Round(AObject.Angle) = AAngle;
    end);
  AWidth := AFirstObject.Width;
  SetSpinEdit(AWidth, seShapeWidth,
    function (AObject: TdxFcObject): Boolean
    begin
      Result := AObject.Width = AWidth;
    end);
  AHeight := AFirstObject.Height;
  SetSpinEdit(AHeight, seShapeHeight,
    function (AObject: TdxFcObject): Boolean
    begin
      Result := AObject.Height = AHeight;
    end);
end;

procedure TdxFlowChartDesigner.UpdateConnectionsPropertiesControls;

  function AreAllEqual(const ACompare: TdxFlowChartCompareConnectionValueFunc): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Chart.SelectedConnectionCount - 1 do
      if not ACompare(Chart.SelectedConnections[I]) then
        Exit(False);
    Result := True;
  end;

  procedure SetArrowSize(AArrowSize: TdxFcaSize; AComboBox: TcxComboBox; const ACompare: TdxFlowChartCompareConnectionValueFunc);
  begin
    if CompareConnectionValue(ACompare, 1) then
    begin
      if AArrowSize = fcasCustom then
        AComboBox.ItemIndex := Ord(High(TdxFcaSize))
      else
        AComboBox.ItemIndex := Ord(AArrowSize) - 1
    end
    else
      AComboBox.ItemIndex := -1;
  end;

  procedure SetArrowType(AArrowType: TdxFcaType; AComboBox: TcxImageComboBox; const ACompare: TdxFlowChartCompareConnectionValueFunc);
  begin
    if CompareConnectionValue(ACompare, 1) then
      AComboBox.EditValue := Ord(AArrowType)
    else
      AComboBox.EditValue := Null;
  end;

  procedure SetSpinEdit(AValue: Integer; ASpinEdit: TcxSpinEdit; const ACompare: TdxFlowChartCompareConnectionValueFunc);
  begin
    if CompareConnectionValue(ACompare, 1) then
      ASpinEdit.EditValue := AValue
    else
      ASpinEdit.EditValue := Null;
  end;

  procedure SetStyle(AStyle: TdxFclStyle; AComboBox: TcxImageComboBox; const ACompare: TdxFlowChartCompareConnectionValueFunc);
  begin
    if CompareConnectionValue(ACompare, 1) then
      AComboBox.EditValue := Ord(AStyle)
    else
      AComboBox.EditValue := Null;
  end;

var
  AEnabled: Boolean;
  AArrowType: TdxFcaType;
  AArrowSize: TdxFcaSize;
  AArrowWidth, AArrowHeight: Integer;
  AFirstConnection: TdxFcConnection;
  AStyle: TdxFclStyle;
begin
  AEnabled := Chart.SelectedConnectionCount > 0;
  IterateGroupControls(lgConnections,
    procedure (AControl: TWinControl)
    var
      AEdit: TcxCustomEdit;
    begin
      AControl.Enabled := AEnabled;
      if not AEnabled then
      begin
        AEdit := Safe<TcxCustomEdit>.Cast(AControl);
        if AEdit <> nil then
          AEdit.EditValue := Null;
      end;
    end);

  lgConnections.Enabled := AEnabled;
  if not AEnabled then
    Exit;
  AFirstConnection := FirstSelectedChartConnection;
  btnSourceArrowColor.Enabled := not AreAllEqual(
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := not AConnection.ArrowSource.HasBackground;
    end);
  btnDestArrowColor.Enabled := not AreAllEqual(
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := not AConnection.ArrowDest.HasBackground;
    end);

  FSourceArrowColorPicker.Color := AFirstConnection.ArrowSource.Color;
  FDestArrowColorPicker.Color := AFirstConnection.ArrowDest.Color;

  AArrowType := AFirstConnection.ArrowSource.ArrowType;
  SetArrowType(AArrowType, icbBeginArrow,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.ArrowSource.ArrowType = AArrowType;
    end);
  AArrowType := AFirstConnection.ArrowDest.ArrowType;
  SetArrowType(AArrowType, icbEndArrow,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.ArrowDest.ArrowType = AArrowType;
    end);
  AArrowSize := AFirstConnection.ArrowSource.Size;
  SetArrowSize(AArrowSize, cbSArrowSize,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.ArrowSource.Size = AArrowSize;
    end);
  AArrowSize := AFirstConnection.ArrowDest.Size;
  SetArrowSize(AArrowSize, cbDArrowSize,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.ArrowDest.Size = AArrowSize;
    end);
  AArrowWidth := AFirstConnection.ArrowSource.Width;
  SetSpinEdit(AArrowWidth, seSArrowWidth,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.ArrowSource.Width = AArrowWidth;
    end);
  AArrowHeight := AFirstConnection.ArrowSource.Height;
  SetSpinEdit(AArrowHeight, seSArrowHeight,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.ArrowSource.Height = AArrowHeight;
    end);
  AArrowWidth := AFirstConnection.ArrowDest.Width;
  SetSpinEdit(AArrowWidth, seDArrowWidth,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.ArrowDest.Width = AArrowWidth;
    end);
  AArrowHeight := AFirstConnection.ArrowDest.Height;
  SetSpinEdit(AArrowHeight, seDArrowHeight,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.ArrowDest.Height = AArrowHeight;
    end);
  AStyle := AFirstConnection.Style;
  SetStyle(AStyle, icbConnectionType,
    function (AConnection: TdxFcConnection): Boolean
    begin
      Result := AConnection.Style = AStyle;
    end);

  IterateGroupControls(lgConnections,
    procedure (AControl: TWinControl)
    var
      AEdit: TcxCustomEdit;
    begin
      AEdit := Safe<TcxCustomEdit>.Cast(AControl);
      if AEdit <> nil then
        AEdit.ModifiedAfterEnter := False;
    end);
end;

procedure TdxFlowChartDesigner.UpdateSelectedConnections;
begin
  ForEachSelectedConnection(
    procedure (AConnection: TdxFcConnection)
    begin
      if icbBeginArrow.ModifiedAfterEnter then
        AConnection.ArrowSource.ArrowType := TdxFcaType(icbBeginArrow.EditValue);
      if icbEndArrow.ModifiedAfterEnter then
        AConnection.ArrowDest.ArrowType := TdxFcaType(icbEndArrow.EditValue);
      if icbConnectionType.ModifiedAfterEnter then
        AConnection.Style := TdxFclStyle(icbConnectionType.ItemIndex);
    end);
end;

procedure TdxFlowChartDesigner.UpdateStrokeColorImage;
begin
  DrawHelpedColorLine(bbStrokeColor.Glyph, FStrokeColorPicker.Color, ilSmallIcons, acStrokeColor.ImageIndex);
end;

function TdxFlowChartDesigner.GetActiveChartBackgroundColor: TColor;
begin
  if FirstSelectedChartItem is TdxFcObject then
    Result := TdxFcObject(FirstSelectedChartItem).BkColor
  else
    Result := clDefault;
end;

function TdxFlowChartDesigner.GetActiveChartFont: TFont;
begin
  if FirstSelectedChartItem <> nil then
    Result := FirstSelectedChartItem.Font
  else
    Result := Chart.Font;
end;

function TdxFlowChartDesigner.GetActiveChartStrokeColor: TColor;
begin
  if FirstSelectedChartItem = nil then
    Result := clDefault
  else
    if FirstSelectedChartItem is TdxFcObject then
      Result := FirstSelectedChartObject.ShapeColor
    else
      Result := FirstSelectedChartConnection.Color;
end;

procedure TdxFlowChartDesigner.SetMode(AValue: TdxFlowChartDesignerMode);
begin
  if Mode <> AValue then
  begin
    FMode := AValue;
    ModeChanged;
  end;
end;

procedure TdxFlowChartDesigner.SourceArrowDimensionChanged(Sender: TObject);
var
  AValue: Integer;
begin
  if FUpdatingPropertiesControls then
    Exit;

  AValue := (Sender as TcxSpinEdit).Value;
  ForEachSelectedConnection(
    procedure (AConnection: TdxFcConnection)
    begin
      if Sender = seSArrowHeight then
        AConnection.ArrowSource.Height := AValue
      else
        AConnection.ArrowSource.Width := AValue;
    end);
  FUpdatingPropertiesControls := True;
  try
    cbSArrowSize.ItemIndex := cbSArrowSize.Properties.Items.Count - 1;
  finally
    FUpdatingPropertiesControls := False;
  end;
end;

procedure TdxFlowChartDesigner.DestArrowDimensionChanged(Sender: TObject);
var
  AValue: Integer;
begin
  if FUpdatingPropertiesControls then
    Exit;

  AValue := (Sender as TcxSpinEdit).Value;
  ForEachSelectedConnection(
    procedure (AConnection: TdxFcConnection)
    begin
      if Sender = seDArrowHeight then
        AConnection.ArrowDest.Height := AValue
      else
        AConnection.ArrowDest.Width := AValue;
    end);
  FUpdatingPropertiesControls := True;
  cbDArrowSize.ItemIndex := cbDArrowSize.Properties.Items.Count - 1;
  FUpdatingPropertiesControls := False;
end;

procedure TdxFlowChartDesigner.StartAddObjectOnDrag;
begin
  FDragPoint := FDownPoint;
  Chart.Options := Chart.Options - [fcoMultiSelect];
  FSavedCursor := Screen.Cursor;
end;

procedure TdxFlowChartDesigner.EndAddObjectOnDrag;
begin
  AddObjectOnDrag(FDragPoint);
  Screen.Cursor := FSavedCursor;
  Chart.Options := Chart.Options + [fcoMultiSelect];
  FDragPoint := cxInvalidPoint;
end;

procedure TdxFlowChartDesigner.ChartDblClickHandler(ASender: TObject);
begin
  acProperties.Execute;
end;

procedure TdxFlowChartDesigner.ChartDragOverHandler(ASender, ASource: TObject; AX, AY: Integer; AState: TDragState; var AAccept: Boolean);
var
  AItem: TdxGalleryControlItem;
  AShapeInfo: TdxFlowChartShapeInfo;
begin
  if (ASource is TcxDragControlObject) and (TcxDragControlObject(ASource).Control = gcShapes) and (AState = dsDragEnter) then
  begin
    CancelDrag;
    AItem := gcShapes.Gallery.GetCheckedItem;
    if ShapeGalleryInfo.TryGetValue(AItem, AShapeInfo) then
      Chart.AddObjectOnDragFromGallery(cxInvisiblePoint, AShapeInfo.ShapeType, AShapeInfo.AdvancedShape);
  end;
end;

procedure TdxFlowChartDesigner.ChartKeyDownHandler(ASender: TObject; var AKey: Word; AShift: TShiftState);
var
  P: TPoint;
begin
  if AKey = VK_APPS then
  begin
    GetCursorPos(P);
    P := Chart.ScreenToClient(P);
    FDownPoint := Point(P.X, P.Y);
    GetCursorPos(P);
    bpmChart.Popup(P.X, P.Y);
  end;
  if (AKey = VK_INSERT) and (ssCtrl in AShift) and acCopy.Enabled then
    acCopy.Execute;
  if (AKey = VK_INSERT) and (ssShift in AShift) and acPaste.Enabled then
    acPaste.Execute;
  if (AKey = VK_DELETE) and (ssShift in AShift) and acCut.Enabled then
    acCut.Execute;
end;

procedure TdxFlowChartDesigner.ChartMouseDownHandler(ASender: TObject;
  AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
begin
  FDownPoint := Point(AX, AY);
  if AButton <> mbLeft then
    Exit;
  if acStandardShape.Checked then
    StartAddObjectOnDrag
  else
    if acConnector.Checked then
      Chart.AddConnectionOnDrag(FDownPoint);
end;

procedure TdxFlowChartDesigner.ChartMouseMoveHandler(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
var
  APoint: TPoint;
begin
  APoint := Point(AX, AY);
  if acPointerTool.Checked then
    UpdateUnionHint(APoint)
  else
    if acConnector.Checked then
    begin
      if Chart.ConnectionHighlights <> nil then
      begin
        Chart.ConnectionHighlights.Update(AX, AY);
        Chart.Invalidate;
      end;
    end
    else
      if (ssLeft in AShift) and not cxPointIsEqual(APoint, FDragPoint) then
      begin
        Screen.Cursor := crFlChartCross;
        DrawDragObject;
        FDragPoint := APoint;
        DrawDragObject;
      end
end;

procedure TdxFlowChartDesigner.ChartMouseUpHandler(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
begin
  if acStandardShape.Checked then
  begin
    DrawDragObject;
    EndAddObjectOnDrag;
  end;
  acPointerTool.Execute;
end;

procedure TdxFlowChartDesigner.ChartMouseWheelHandler(ASender: TObject; AShift: TShiftState; AWheelDelta: Integer;
  AMousePos: TPoint; var AHandled: Boolean);
begin
  PostponedUpdateRibbonItemsEnabled;
end;

procedure TdxFlowChartDesigner.ChartSelectedHandler(ASender: TdxCustomFlowChart; AItem: TdxFcItem);
begin
  UpdateRibbonItemsEnabled;
  UpdateRibbonItemsByChartItems;
  UpdatePropertiesPanel;
end;

procedure TdxFlowChartDesigner.ChartTransactChangedHandler(ASender: TObject);
begin
  if Chart.IsDestroying then
    Exit;
  UpdateRibbonItemsEnabled;
  UpdateRibbonItemsByChartItems;
  UpdatePropertiesPanel;
  FModified := True;
end;

procedure TdxFlowChartDesigner.LineColorChangedHandler(Sender: TObject);
var
  AColor: TColor;
begin
  UpdateLineColorImage;
  if FUpdatingPropertiesControls then
    Exit;

  AColor := FLineColorPicker.Color;
  ChangeSelectedChartItems(
    procedure (AItem: TdxFcItem)
    var
      AConnection: TdxFcConnection;
      AObject: TdxFcObject;
    begin
      AObject := Safe<TdxFcObject>.Cast(AItem);
      if AObject <> nil then
        AObject.ShapeColor := AColor;
      AConnection := Safe<TdxFcConnection>.Cast(AItem);
      if AConnection <> nil then
        AConnection.Color := AColor;
    end);
end;

procedure TdxFlowChartDesigner.DestArrowColorChangedHandler(Sender: TObject);
begin
  UpdateDestArrowColorImage;
  if FUpdatingPropertiesControls then
    Exit;

  ChangeSelectedConnections(
    procedure (AConnection: TdxFcConnection)
    begin
      AConnection.ArrowDest.Color := FDestArrowColorPicker.Color;
    end);
end;

procedure TdxFlowChartDesigner.MoreShapesGalleryHotTrackChangedHandler(ASender: TObject);
begin
  FMoreShapesArrow.Invalidate;
end;

procedure TdxFlowChartDesigner.MenuStencilsButtonClickHandler(Sender: TObject);
var
  AMenuButton: TdxBarButton;
  AFocusedStencil: TdxFlowChartAdvancedShapeStencil;
begin
  AMenuButton := TdxBarButton(Sender);
  if AMenuButton.Down then
    StencilMenuInfo.TryGetValue(AMenuButton, AFocusedStencil)
  else
    AFocusedStencil := nil;
  UpdateGalleryStencils(AFocusedStencil);
end;

procedure TdxFlowChartDesigner.ObjectBkColorChangedHandler(Sender: TObject);
begin
  UpdateObjectBkColorImage;
  if FUpdatingPropertiesControls then
    Exit;
  ChangeSelectedObjects(
    procedure (AObject: TdxFcObject)
    begin
      AObject.BkColor := FObjectBkColorPicker.Color;
    end);
end;

procedure TdxFlowChartDesigner.SourceArrowColorChangedHandler(Sender: TObject);
begin
  UpdateSourceArrowColorImage;
  if FUpdatingPropertiesControls then
    Exit;
  ChangeSelectedConnections(
    procedure (AConnection: TdxFcConnection)
    begin
      AConnection.ArrowSource.Color := FSourceArrowColorPicker.Color;
    end);
end;

procedure TdxFlowChartDesigner.ToolsStandardShapeItemClickHandler(ASender: TObject);
var
  AMenuItem: TdxBarButton;
begin
  AMenuItem := TdxBarButton(ASender);
  acStandardShape.Caption := AMenuItem.Caption;
  acStandardShape.Checked := True;
  acStandardShape.ShortCut := AMenuItem.ShortCut;
  bbStandardShape.Data := AMenuItem.Data;
  bbStandardShape.Glyph := AMenuItem.Glyph;
end;

procedure TdxFlowChartDesigner.ClearBuffer;
begin
  FBuffer.Clear;
end;

procedure TdxFlowChartDesigner.ConnectionPropertiesChanged(Sender: TObject);
var
  AEdit: TcxCustomEdit;
  AEnabled: Boolean;
begin
  if FUpdatingPropertiesControls then
    Exit;
  AEdit := Safe<TcxCustomEdit>.Cast(Sender);
  if (AEdit <> nil) and AEdit.ModifiedAfterEnter then
  begin
    if AEdit = icbEndArrow then
    begin
      AEnabled := not VarIsNull(AEdit.EditValue);
      seDArrowWidth.Enabled := AEnabled;
      seDArrowHeight.Enabled := AEnabled;
      if not AEnabled then
      begin
        FUpdatingPropertiesControls := True;
        seDArrowWidth.EditValue := Null;
        seDArrowHeight.EditValue := Null;
        seDArrowWidth.ModifiedAfterEnter := False;
        seDArrowHeight.ModifiedAfterEnter := False;
        FUpdatingPropertiesControls := False;
      end;
    end;
    if AEdit = icbBeginArrow then
    begin
      AEnabled := not VarIsNull(AEdit.EditValue);
      seSArrowWidth.Enabled := AEnabled;
      seSArrowHeight.Enabled := AEnabled;
      if not AEnabled then
      begin
        FUpdatingPropertiesControls := True;
        seSArrowWidth.EditValue := Null;
        seSArrowHeight.EditValue := Null;
        seSArrowWidth.ModifiedAfterEnter := False;
        seSArrowHeight.ModifiedAfterEnter := False;
        FUpdatingPropertiesControls := False;
      end;
    end;
  end;
  UpdateSelectedConnections;
  UpdateConnectionsPropertiesControls;
end;

procedure TdxFlowChartDesigner.CopySelectedConnectionsToBuffer(ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>);
var
  I: Integer;
  AConnection, ABufferConnection: TdxFcConnection;
  ABufferObject: TdxFcObject;
begin
  for I := 0 to Chart.SelectedConnectionCount - 1 do
  begin
    AConnection := Chart.SelectedConnections[I];
    ABufferConnection := TdxFcConnection.Create(FBuffer);
    ABufferConnection.AssignPattern(AConnection);
    ALinkObjects.TryGetValue(AConnection.ObjectSource, ABufferObject);
    ABufferConnection.SetObjectSource(ABufferObject, AConnection.PointSource);
    ALinkObjects.TryGetValue(AConnection.ObjectDest, ABufferObject);
    ABufferConnection.SetObjectDest(ABufferObject, AConnection.PointDest);
  end;
end;

procedure TdxFlowChartDesigner.CopySelectedObjectsToBuffer(ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>);
var
  I: Integer;
  AObject, ABufferObject: TdxFcObject;
begin
  for I := 0 to Chart.SelectedObjectCount - 1 do
  begin
    AObject := Chart.SelectedObjects[I];
    ABufferObject := TdxFcObject.Create(FBuffer);
    ALinkObjects.Add(AObject, ABufferObject);
    ABufferObject.Assign(AObject);
  end;
end;

procedure TdxFlowChartDesigner.CopyToBuf;
var
  ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>;
begin
  ALinkObjects := TDictionary<TdxFcObject, TdxFcObject>.Create;
  try
    FPasteCount := 0;
    ClearBuffer;
    CopySelectedObjectsToBuffer(ALinkObjects);
    CopySelectedConnectionsToBuffer(ALinkObjects);
  finally
    ALinkObjects.Free;
  end;
end;

procedure TdxFlowChartDesigner.PasteBufferConnectionsToChart(ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>);
var
  I: Integer;
  AObject: TdxFcObject;
  AConnection, ABufferConnection: TdxFcConnection;
begin
  for I := 0 to FBuffer.ConnectionCount - 1 do
  begin
    ABufferConnection := FBuffer.Connections[I];
    AConnection := TdxFcConnection.Create(Chart);
    AConnection.AssignPattern(ABufferConnection);
    AConnection.Selected := True;
    ALinkObjects.TryGetValue(ABufferConnection.ObjectSource, AObject);
    AConnection.SetObjectSource(AObject, ABufferConnection.PointSource);
    ALinkObjects.TryGetValue(ABufferConnection.ObjectDest, AObject);
    AConnection.SetObjectDest(AObject, ABufferConnection.PointDest);
    AConnection.Offset(dxPasteDelta * FPasteCount);
  end;
end;

procedure TdxFlowChartDesigner.PasteBufferObjectsToChart(ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>);
var
  I: Integer;
  AObject, ABufferObject: TdxFcObject;
begin
  for I := 0 to FBuffer.ObjectCount - 1 do
  begin
    AObject := TdxFcObject.Create(Chart);
    ABufferObject := FBuffer.Objects[I];
    ALinkObjects.Add(ABufferObject, AObject);
    AObject.Assign(ABufferObject);
    AObject.Left := AObject.Left + dxPasteDelta * FPasteCount;
    AObject.Top := AObject.Top + dxPasteDelta * FPasteCount;
    AObject.BringToFront;
    AObject.Selected := True;
  end;
end;

procedure TdxFlowChartDesigner.PasteFromBuf;
var
  ALinkObjects: TDictionary<TdxFcObject, TdxFcObject>;
begin
  ALinkObjects := TDictionary<TdxFcObject, TdxFcObject>.Create;
  try
    Inc(FPasteCount);
    Chart.ClearSelection;
    PasteBufferObjectsToChart(ALinkObjects);
    PasteBufferConnectionsToChart(ALinkObjects);
  finally
    ALinkObjects.Free;
  end;
end;

function TdxFlowChartDesigner.ChartHasUnions: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Chart.ObjectCount - 1 do
    if Chart.Objects[I].IsUnion then
      Exit(True);
end;

function TdxFlowChartDesigner.FindUnions(FromUnion, Obj: TdxFcObject): TdxFcObject;
var
  I: integer;
  FFind : Boolean;
begin
  Result := nil;
  FFind := FromUnion = nil;
  for I := 0 to Chart.ObjectCount - 1 do
    if Chart.Objects[i].IsUnion and (Obj <> Chart.Objects[i]) then
      if Chart.Objects[i].HasInUnion(Obj) then
      begin
        if FFind then
        begin
          Result := Chart.Objects[i];
          break;
        end;
        FFind := Chart.Objects[i] = FromUnion;
      end;
end;

function TdxFlowChartDesigner.GetNumberByUnion(Obj: TdxFcObject): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Chart.ObjectCount - 1 do
    if Chart.Objects[I].IsUnion then
    begin
      Inc(Result);
      if Chart.Objects[I] = Obj then
        Break;
    end;
end;

function TdxFlowChartDesigner.IsChildItemInUnion(Obj: TdxFcObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Chart.ObjectCount - 1 do
    if Chart.Objects[I].IsUnion then
      if Chart.Objects[I].HasInUnion(Obj) then
      begin
        Result := True;
        Break;
      end;
end;

function TdxFlowChartDesigner.IsMainItemInUnion(Obj: TdxFcObject): Boolean;
begin
  Result := Obj.IsUnion;
end;

procedure TdxFlowChartDesigner.UpdateUnionHint(APoint: TPoint);

  function GetUnionString(ACount: Integer): string;
  begin
    if ACount > 1 then
      Result := cxGetResourceString(@sdxFlowChartUnions)
    else
      Result := cxGetResourceString(@sdxFlowChartUnion);
  end;

var
  Obj : TdxFcObject;
  AText, CountText : String;
  FirstObj : TdxFcObject;
  Count : integer;
begin
  Obj := Chart.GetObjectAt(APoint.X, APoint.Y);
    AText := '';
    if Obj <> nil then
    begin
      if IsMainItemInUnion(Obj) then
        AText := Format(cxGetResourceString(@sdxFlowChartEditorMainItemOfUnion), [GetNumberByUnion(Obj)]);

      if IsChildItemInUnion(Obj) then
      begin
        Count := 0;
        CountText := '';
        FirstObj := nil;
        repeat
          FirstObj := FindUnions(FirstObj, Obj);
          if FirstObj <> nil then
          begin
            if Count = 0 then
              CountText := CountText + IntToStr(GetNumberByUnion(FirstObj))
            else
              CountText := CountText + ', ' + IntToStr(GetNumberByUnion(FirstObj));
            Inc(Count);
          end;
        until FirstObj = nil;

        if Count > 0 then
        begin
          if AText <> '' then
            AText := AText + ', ';
          AText := AText + cxGetResourceString(@sdxFlowChartEditorChildItem);
          AText := Format(AText, [GetUnionString(Count)]);
          AText := AText + CountText;
        end;
      end;
    end;
    if AText = '' then
      Obj := nil;
    if (Obj <> nil) and (Obj <> FLastObj) and not Chart.DragHelper.Dragging then
    begin
      Application.HintHidePause := 5000;
      Chart.Hint := AText;
      Chart.ShowHint := True;
      FLastObj := Obj;
    end
    else
      if Obj <> FLastObj then
      begin
        Chart.ShowHint := False;
        FLastObj := nil;
        Application.HintHidePause := FOldHintHidePause;
      end;
end;

procedure TdxFlowChartDesigner.ForEachSelectedChartItem(const AProc: TdxFlowChartForEachSelectedItemProc);
var
  I: Integer;
begin
  for I := 0 to Chart.SelectedObjectCount - 1 do
    AProc(Chart.SelectedObjects[I]);
  for I := 0 to Chart.SelectedConnectionCount - 1 do
    AProc(Chart.SelectedConnections[I]);
end;

procedure TdxFlowChartDesigner.ForEachSelectedConnection(const AProc: TdxFlowChartForEachSelectedConnectionProc);
var
  I: Integer;
begin
  for I := 0 to Chart.SelectedConnectionCount - 1 do
    AProc(Chart.SelectedConnections[I]);
end;

procedure TdxFlowChartDesigner.ForEachSelectedObject(const AProc: TdxFlowChartForEachSelectedObjectProc);
var
  I: Integer;
begin
  for I := 0 to Chart.SelectedObjectCount - 1 do
    AProc(Chart.SelectedObjects[I]);
end;

procedure TdxFlowChartDesigner.ForEachStandardShape(const AProc: TdxFlowChartForEachStandardShapeProc);
var
  AShapeType: TdxFcShapeType;
begin
  for AShapeType := Low(TdxFcShapeType) to High(TdxFcShapeType) do
    if IsStandardGalleryShape(AShapeType) then
      AProc(AShapeType);
end;

procedure TdxFlowChartDesigner.ChangeItemBkColorProc(AItem: TdxFcItem);
begin
  if AItem is TdxFcObject then
    TdxFcObject(AItem).BkColor := FBackgroundColorPicker.Color;
end;

procedure TdxFlowChartDesigner.ChangeItemColorProc(AItem: TdxFcItem);
begin
  if AItem is TdxFcObject then
    TdxFcObject(AItem).ShapeColor := FStrokeColorPicker.Color
  else
    TdxFcConnection(AItem).Color := FStrokeColorPicker.Color;
end;

procedure TdxFlowChartDesigner.ChangeItemFontBoldProc(AItem: TdxFcItem);
begin
  if acFontBold.Checked then
    AItem.Font.Style := AItem.Font.Style + [fsBold]
  else
    AItem.Font.Style := AItem.Font.Style - [fsBold];
end;

procedure TdxFlowChartDesigner.ChangeItemFontColorProc(AItem: TdxFcItem);
begin
  AItem.Font.Color := FFontColorPicker.Color;
end;

procedure TdxFlowChartDesigner.ChangeItemFontItalicProc(AItem: TdxFcItem);
begin
  if acFontItalic.Checked then
    AItem.Font.Style := AItem.Font.Style + [fsItalic]
  else
    AItem.Font.Style := AItem.Font.Style - [fsItalic];
end;

procedure TdxFlowChartDesigner.ChangeItemFontNameProc(AItem: TdxFcItem);
begin
  AItem.Font.Name := bcbFontName.EditValue;
end;

procedure TdxFlowChartDesigner.ChangeItemFontSizeProc(AItem: TdxFcItem);
begin
  AItem.Font.Size := bcbFontSize.EditValue;
end;

procedure TdxFlowChartDesigner.ChangeItemFontStrikeoutProc(AItem: TdxFcItem);
begin
  if acFontStrikeout.Checked then
    AItem.Font.Style := AItem.Font.Style + [fsStrikeOut]
  else
    AItem.Font.Style := AItem.Font.Style - [fsStrikeOut];
end;

procedure TdxFlowChartDesigner.ChangeItemFontUnderlineProc(AItem: TdxFcItem);
begin
  if acFontUnderline.Checked then
    AItem.Font.Style := AItem.Font.Style + [fsUnderline]
  else
    AItem.Font.Style := AItem.Font.Style - [fsUnderline];
end;

procedure TdxFlowChartDesigner.ChangeItemTextHorzAlignProc(AItem: TdxFcItem);
var
  AObject: TdxFcObject;
begin
  if AItem is TdxFcObject then
  begin
    AObject := TdxFcObject(AItem);
    if acTextAlignLeft.Checked then
      AObject.HorzTextPos := fchpLeft
    else
      if acTextAlignCenter.Checked then
        AObject.HorzTextPos := fchpCenter
      else
        AObject.HorzTextPos := fchpRight;
  end;
end;

procedure TdxFlowChartDesigner.ChangeItemTextVertAlignProc(AItem: TdxFcItem);
var
  AObject: TdxFcObject;
begin
  if AItem is TdxFcObject then
  begin
    AObject := TdxFcObject(AItem);
    if acTextAlignTop.Checked then
      AObject.VertTextPos := fcvpUp
    else
      if acTextAlignMiddle.Checked then
        AObject.VertTextPos := fcvpCenter
      else
        AObject.VertTextPos := fcvpDown;
  end;
end;

procedure TdxFlowChartDesigner.ItemBringToFront(AItem: TdxFcItem);
begin
  if AItem is TdxFcObject then
    TdxFcObject(AItem).BringToFront;
end;

procedure TdxFlowChartDesigner.ItemCurved(AItem: TdxFcItem);
begin
  if AItem is TdxFcConnection then
    TdxFcConnection(AItem).Style := fclCurved;
end;

procedure TdxFlowChartDesigner.ItemRightAngleHorizontal(AItem: TdxFcItem);
begin
  if AItem is TdxFcConnection then
    TdxFcConnection(AItem).Style := fclRectH;
end;

procedure TdxFlowChartDesigner.ItemRightAngleVertical(AItem: TdxFcItem);
begin
  if AItem is TdxFcConnection then
    TdxFcConnection(AItem).Style := fclRectV;
end;

procedure TdxFlowChartDesigner.ItemSendToBack(AItem: TdxFcItem);
begin
  if AItem is TdxFcObject then
    TdxFcObject(AItem).SendToBack;
end;

procedure TdxFlowChartDesigner.ItemStraight(AItem: TdxFcItem);
begin
  if AItem is TdxFcConnection then
    TdxFcConnection(AItem).Style := fclStraight;
end;

procedure TdxFlowChartDesigner.SetUseAdvancedShapesOnly(AValue: Boolean);
begin
  if AValue <> UseAdvancedShapesOnly then
  begin
    FUseAdvancedShapesOnly := AValue;
    acUseAdvancedShapesOnly.Checked := UseAdvancedShapesOnly;
    if GetFocusedGalleryStencil = Chart.Repository.BasicShapes then
      UpdateActiveGalleryShapes;
  end;
end;

procedure TdxFlowChartDesigner.bcbFontNamePropertiesEditValueChanged(Sender: TObject);
begin
  if FIsInternalUpdatingRibbonItemsByChartItems then
    Exit;
  ChangeSelectedChartItems(ChangeItemFontNameProc);
end;

procedure TdxFlowChartDesigner.bcbFontNamePropertiesLoadFontComplete(Sender: TObject);
var
  AProperties: TcxFontNameComboBoxProperties;
begin
  AProperties := TcxFontNameComboBoxProperties(Sender);
  while (AProperties.Items.Count > 0) and (AProperties.Items[0][1] = '@') do
    AProperties.Items.Delete(0);
end;

procedure TdxFlowChartDesigner.bcbFontSizePropertiesEditValueChanged(Sender: TObject);
begin
  if FIsInternalUpdatingRibbonItemsByChartItems then
    Exit;
  ChangeSelectedChartItems(ChangeItemFontSizeProc);
end;

procedure TdxFlowChartDesigner.acActualSizeExecute(Sender: TObject);
begin
  Chart.Zoom := dxActualZoom;
  UpdateRibbonItemsEnabled;
end;

procedure TdxFlowChartDesigner.acAddToUnionExecute(Sender: TObject);
var
  I: Integer;
  FFind: Boolean;
  Obj, FirstObj: TdxFcObject;
begin
  if not ChartHasUnions then
    Exit;
  Obj := SelectUnion(Chart, nil);
  if Obj <> nil then
    for i := 0 to Chart.SelectedObjectCount - 1 do
    begin
      FirstObj := nil;
      FFind := False;
      repeat
        FirstObj := FindUnions(FirstObj, Obj);
        if FirstObj = Chart.SelectedObjects[i] then
        begin
          FFind := True;
          break;
        end;
      until FirstObj = nil;
      if not FFind then
        Obj.AddToUnion(Chart.SelectedObjects[i]);
    end;
  Chart.Changed(nil);
end;

procedure TdxFlowChartDesigner.acAntialiasingExecute(Sender: TObject);
begin
  Chart.Antialiasing := acAntialiasing.Checked;
end;

procedure TdxFlowChartDesigner.acApplyLayeredLayoutExecute(Sender: TObject);
begin
  Chart.BeginTransactChange;
  try
    Chart.ApplyLayeredLayout;
  finally
    Chart.EndTransactChange;
  end;
end;

procedure TdxFlowChartDesigner.acFontBoldExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ChangeItemFontBoldProc);
end;

procedure TdxFlowChartDesigner.acFontColorExecute(Sender: TObject);
begin
  UpdateFontColorImage;
  ChangeSelectedChartItems(ChangeItemFontColorProc);
end;

procedure TdxFlowChartDesigner.acFontItalicExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ChangeItemFontItalicProc);
end;

procedure TdxFlowChartDesigner.acFontSizeDecExecute(Sender: TObject);
var
  I: Integer;
  AItems: TStrings;
begin
  AItems := TcxComboBoxProperties(bcbFontSize.Properties).Items;
  for I := AItems.Count - 1 downto 0 do
    if StrToInt(AItems[I]) < bcbFontSize.EditValue then
    begin
      bcbFontSize.EditValue := StrToInt(AItems[I]);
      ChangeSelectedChartItems(ChangeItemFontSizeProc);
      Break;
    end;
end;

procedure TdxFlowChartDesigner.acFontSizeIncExecute(Sender: TObject);
var
  I: Integer;
  AItems: TStrings;
begin
  AItems := TcxComboBoxProperties(bcbFontSize.Properties).Items;
  for I := 0 to AItems.Count - 1 do
    if StrToInt(AItems[I]) > bcbFontSize.EditValue then
    begin
      bcbFontSize.EditValue := StrToInt(AItems[I]);
      ChangeSelectedChartItems(ChangeItemFontSizeProc);
      Break;
    end;
end;

procedure TdxFlowChartDesigner.acFontStrikeoutExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ChangeItemFontStrikeoutProc);
end;

procedure TdxFlowChartDesigner.acFontUnderlineExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ChangeItemFontUnderlineProc);
end;

procedure TdxFlowChartDesigner.acBackgroundColorExecute(Sender: TObject);
begin
  UpdateBackgroundColorImage;
  ChangeSelectedChartItems(ChangeItemBkColorProc);
end;

procedure TdxFlowChartDesigner.acBringToFrontExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ItemBringToFront);
end;

procedure TdxFlowChartDesigner.acClearAllUnionsExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Chart.ObjectCount - 1 do
    if Chart.Objects[i].IsUnion then
      Chart.Objects[i].ClearUnion;
  Chart.Changed(nil);
end;

procedure TdxFlowChartDesigner.acClearSelectionExecute(Sender: TObject);
begin
  Chart.ClearSelection;
end;

procedure TdxFlowChartDesigner.acClearUnionExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Chart.SelectedObjectCount - 1 do
    if Chart.SelectedObjects[I].IsUnion then
      Chart.SelectedObjects[I].ClearUnion;
  Chart.Changed(nil);
end;

procedure TdxFlowChartDesigner.acCloseAndApplyChangesExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TdxFlowChartDesigner.acConnectorExecute(Sender: TObject);
begin
  Chart.ClearSelection;
  Chart.IsAddingConnection := True;
  Chart.StartConnectionEndpointHighlight(cxInvisiblePoint.X, cxInvisiblePoint.Y);
end;

procedure TdxFlowChartDesigner.acContentsExecute(Sender: TObject);
begin
  if not TryOpenHelpFile(dxHelp) then
    TryOpenHelpFile(ExtractFilePath(Application.HelpFile) + dxHelp);
end;

procedure TdxFlowChartDesigner.acCopyExecute(Sender: TObject);
begin
  CopyToBuf;
  UpdateRibbonItemsEnabled;
end;

procedure TdxFlowChartDesigner.acCurvedExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ItemCurved);
end;

procedure TdxFlowChartDesigner.acCutExecute(Sender: TObject);
begin
  Chart.BeginTransactChange;
  try
    CopyToBuf;
    acDelete.Execute;
  finally
    Chart.EndTransactChange;
  end;
end;

procedure TdxFlowChartDesigner.acDeleteExecute(Sender: TObject);
begin
  if not Chart.IsFocused then
    Exit;
  Chart.BeginTransactChange;
  try
    Chart.DeleteSelection;
  finally
    Chart.EndTransactChange;
  end;
end;

procedure TdxFlowChartDesigner.acUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Safe<TAction>.Cast(Sender);
  if AAction <> nil then
    if Chart.IsFocused then
      AAction.ShortCut := ShortCuts[AAction]
    else
      AAction.ShortCut := 0;
end;

procedure TdxFlowChartDesigner.acUseAdvancedShapesOnlyExecute(Sender: TObject);
begin
  UseAdvancedShapesOnly := acUseAdvancedShapesOnly.Checked;
end;

procedure TdxFlowChartDesigner.acFitExecute(Sender: TObject);
begin
  if acFit.Checked then
    Chart.Zoom := 0
  else
    Chart.Zoom := 100;
  UpdateRibbonItemsEnabled;
end;

procedure TdxFlowChartDesigner.acGridLinesExecute(Sender: TObject);
begin
  Chart.GridLineOptions.ShowLines := acGridLines.Checked;
end;

procedure TdxFlowChartDesigner.acNewExecute(Sender: TObject);
begin
  if FModified and not AllowOpenDocument then
    Exit;
  Chart.BeginTransactChange;
  try
    Chart.Clear;
    Chart.ResetUndoRedo;
    UpdateRibbonItemsEnabled;
    SaveDialog.FileName := '';
    OpenDialog.FileName := '';
  finally
    Chart.CancelTransactChange;
  end;
end;

procedure TdxFlowChartDesigner.acNewUnionExecute(Sender: TObject);
var
  I: Integer;
  MainObj, FirstObj: TdxFcObject;
  FFind: Boolean;
begin
  MainObj := nil;
  if Chart.SelectedObjectCount > 0 then
    MainObj := FirstSelectedChartObject;
  for I := 0 to Chart.SelectedObjectCount - 1 do
    if MainObj <> nil then
    begin
      FFind := False;
      FirstObj := nil;
      repeat
        FirstObj := FindUnions(FirstObj, MainObj);
        if FirstObj = Chart.SelectedObjects[i] then
        begin
          FFind := True;
          break;
        end;
      until FirstObj = nil;
      if not FFind then
        MainObj.AddToUnion(Chart.SelectedObjects[i]);
    end;
  Chart.Changed(nil);
end;

procedure TdxFlowChartDesigner.acOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute and (not FModified or AllowOpenDocument) then
    LoadFromFile(OpenDialog.FileName);
end;

procedure TdxFlowChartDesigner.acPasteExecute(Sender: TObject);
begin
  Chart.BeginTransactChange;
  try
    PasteFromBuf;
  finally
    Chart.EndTransactChange;
  end;
end;

procedure TdxFlowChartDesigner.acPointerToolExecute(Sender: TObject);
begin
  Chart.StopConnectionEndpointHighlight;
  Chart.IsAddingConnection := False;
  Chart.Cursor := FSavedCursor;
end;

procedure TdxFlowChartDesigner.acPropertiesExecute(Sender: TObject);
var
  hTest: TdxFcHitTest;
begin
  Chart.BeginTransactChange;
  try
    hTest := Chart.GetHitTestAt(FDownPoint.X, FDownPoint.Y);
    if ((hTest * [htByObject,htOnObject]) <> []) and (Chart.SelectedObjectCount > 0) then
      ObjectEditor(Chart, FirstSelectedChartObject)
    else
      if ((hTest * [htOnConnection, htOnConLabel,htOnArrowSrc,htOnArrowDst]) <> []) and
        (Chart.SelectedConnectionCount > 0) then
        ConnectEditor(Chart, FirstSelectedChartConnection);
  finally
    Chart.EndTransactChange;
  end;
end;

procedure TdxFlowChartDesigner.acRedoExecute(Sender: TObject);
begin
  Chart.Redo;
  UpdateRibbonItemsEnabled;
end;

procedure TdxFlowChartDesigner.acRemoveFromUnionExecute(Sender: TObject);
var
  FFind: Boolean;
  I: integer;
  Obj: TdxFcObject;
begin
  FFind := False;
  for I := 0 to Chart.SelectedObjectCount - 1 do
    if IsChildItemInUnion(Chart.SelectedObjects[i]) then
      FFind := True;
  if FFind then
  begin
    Obj := SelectUnion(Chart, FirstSelectedChartObject);
    if Obj <> nil then
      for i := 0 to Chart.SelectedObjectCount - 1 do
        Obj.RemoveFromUnion(Chart.SelectedObjects[i]);
  end;
  Chart.Changed(nil);
end;

procedure TdxFlowChartDesigner.acRemovePointExecute(Sender: TObject);
var
  I, J, B, K: Integer;
  P1 : TPoint;
begin
  P1 := FDownPoint;
  for I := 0 to Chart.SelectedConnectionCount - 1 do
  begin
    J := Chart.SelectedConnections[I].GetNearestPoint(P1.X, P1.Y);
    B := 1;
    K := Chart.SelectedConnections[I].PointCount - 2;
    if Chart.SelectedConnections[I].ObjectSource <> nil then
      Dec(b);
    if Chart.SelectedConnections[I].ObjectDest <> nil then
      Inc(K);
    if (J >= B) and (J <= K) then
       Chart.SelectedConnections[I].RemovePoint(J);
  end;
end;

procedure TdxFlowChartDesigner.acRightAngleHorizontalExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ItemRightAngleHorizontal);
end;

procedure TdxFlowChartDesigner.acRightAngleVerticalExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ItemRightAngleVertical);
end;

procedure TdxFlowChartDesigner.acSaveAsExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    acSave.Execute;
end;

procedure TdxFlowChartDesigner.acSaveExecute(Sender: TObject);
begin
  if SaveDialog.FileName <> '' then
  begin
    Chart.SaveToFile(SaveDialog.FileName);
    OpenDialog.FileName := SaveDialog.FileName;
    UpdateRibbonItemsEnabled;
    FModified := False;
  end
  else
    acSaveAs.Execute;
end;

procedure TdxFlowChartDesigner.acSelectAllExecute(Sender: TObject);
begin
  Chart.SelectAll;
end;

procedure TdxFlowChartDesigner.acSendToBackExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ItemSendToBack);
end;

procedure TdxFlowChartDesigner.acSnapToGridExecute(Sender: TObject);
begin
  if acSnapToGrid.Checked then
    Chart.Options := Chart.Options + [fcoAlignWithGrid]
  else
    Chart.Options := Chart.Options - [fcoAlignWithGrid];
end;

procedure TdxFlowChartDesigner.acStandardShapeExecute(Sender: TObject);
begin
  Chart.StopConnectionEndpointHighlight;
  Chart.IsAddingConnection := False;
  Chart.Cursor := crFlChartCross;
end;

procedure TdxFlowChartDesigner.acStraightExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ItemStraight);
end;

procedure TdxFlowChartDesigner.acStrokeColorExecute(Sender: TObject);
begin
  UpdateStrokeColorImage;
  ChangeSelectedChartItems(ChangeItemColorProc);
end;

procedure TdxFlowChartDesigner.acTextHorzAlignExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ChangeItemTextHorzAlignProc);
end;

procedure TdxFlowChartDesigner.acTextVertAlignExecute(Sender: TObject);
begin
  ChangeSelectedChartItems(ChangeItemTextVertAlignProc);
end;

procedure TdxFlowChartDesigner.acUndoExecute(Sender: TObject);
begin
  Chart.Undo;
  UpdateRibbonItemsEnabled;
end;

procedure TdxFlowChartDesigner.acZoomInExecute(Sender: TObject);
begin
  Chart.Zoom := Min(Chart.Zoom + dxZoomStep, dxMaxZoom);
  UpdateRibbonItemsEnabled;
end;

procedure TdxFlowChartDesigner.acZoomOutExecute(Sender: TObject);
begin
  Chart.Zoom := Max(Chart.Zoom - dxZoomStep, dxMinZoom);
  UpdateRibbonItemsEnabled;
end;

procedure TdxFlowChartDesigner.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  AMsgRes: Integer;
begin
  if not ((ModalResult = mrCancel) and FModified) then
    Exit;
  AMsgRes := MessageDlg(dxExitConfirmation, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  case AMsgRes of
    mrYes:
      if Mode = dmChartDesigner then
        acCloseAndApplyChanges.Execute
      else
      begin
        acSave.Execute;
        CanClose := not FModified;
      end;
    mrCancel:
      CanClose := False;
  end;
end;

type
  TdxDockPanelAccess = class(TdxDockPanel);

procedure TdxFlowChartDesigner.FormCreate(Sender: TObject);
begin
  FChart := TdxDesignerFlowChart.Create(Self);
  InitializeChart;
  FMoreShapesArrow := TcxFlowChartDesignerMoreShapesDropDownArrowImage.Create(Self);
  InitializeMoreShapesArrow;
  InitializeArrowTypeImages;
  InitializeArrowSizes;
  InitializeLineStyleImages;
  InitializeConnectionTypes;
  DisableAero := True;
  Constraints.MinHeight := MulDiv(Constraints.MinHeight, PixelsPerInch, 96);
  Constraints.MinWidth := MulDiv(Constraints.MinWidth, PixelsPerInch, 96);
  FBuffer := TdxFlowChart.Create(Self);
  FUseAdvancedShapesOnly := True;
  FShapeGalleryInfo := TObjectDictionary<TdxGalleryControlItem, TdxFlowChartShapeInfo>.Create([doOwnsValues]);
  FStencilGalleryInfo := TDictionary<TdxGalleryControlItem, TdxFlowChartAdvancedShapeStencil>.Create;
  FStencilMenuInfo := TDictionary<TdxBarButton, TdxFlowChartAdvancedShapeStencil>.Create;
  FLastObj := nil;
  FOldHintHidePause := Application.HintHidePause;
  BuildMenuStencils;
  BuildToolsStandardShapes;
  acPointerTool.Checked := True;
  UpdateGalleryStencils(Chart.Repository.BasicShapes);
  UpdateRibbonItemsCaption;
  UpdateRibbonItemsEnabled;
  UpdateRibbonItemsByChartItems;
  UpdatePropertiesPanel;
  gcShapes.DragMode := dmAutomatic;
  gcMoreShapes.OnMouseEnter := MoreShapesGalleryHotTrackChangedHandler;
  gcMoreShapes.OnMouseLeave := MoreShapesGalleryHotTrackChangedHandler;
  TcxFontNameComboBoxProperties(bcbFontName.Properties).DropDownListStyle := lsEditFixedList;
  FShortCuts := TDictionary<TAction, TShortCut>.Create;
  PopulateShortCuts;
  acActualSize.ShortCut := ShortCuts[acActualSize];
  FDragPoint := cxInvalidPoint;
  FFontColorPicker := TdxFlowChartColorPickerController.Create(ppmFontColor, rgiFontColor, rgiColorTheme);
  FFontColorPicker.Color := dxDefaultTextColor;
  FFontColorPicker.OnColorChanged := acFontColorExecute;
  UpdateFontColorImage;
  FBackgroundColorPicker := TdxFlowChartColorPickerController.Create(ppmBackgroundColor,
    rgiBackgroundColor, rgiBackgroundThemsColor);
  FBackgroundColorPicker.Color := dxDefaultShapeBkColor;
  FBackgroundColorPicker.OnColorChanged := acBackgroundColorExecute;
  UpdateBackgroundColorImage;
  FStrokeColorPicker := TdxFlowChartColorPickerController.Create(ppmStrokeColor, rgiStrokeColor, rgiStrokeColorTheme);
  FStrokeColorPicker.Color := dxDefaultShapeColor;
  FStrokeColorPicker.OnColorChanged := acStrokeColorExecute;
  UpdateStrokeColorImage;
  FLineColorPicker := TdxFlowChartColorPickerController.Create(
    ppmLineColor, rgiLineColor, rgiLineColorTheme);
  FLineColorPicker.Color := dxDefaultArrowColor;
  FLineColorPicker.OnColorChanged := LineColorChangedHandler;
  UpdateLineColorImage;
  FSourceArrowColorPicker := TdxFlowChartColorPickerController.Create(ppmSourceArrowColor,
    rgiSourceArrowColor, rgiSourceArrowColorTheme);
  FSourceArrowColorPicker.Color := dxDefaultArrowColor;
  FSourceArrowColorPicker.OnColorChanged := SourceArrowColorChangedHandler;
  UpdateSourceArrowColorImage;
  FDestArrowColorPicker := TdxFlowChartColorPickerController.Create(ppmDestArrowColor,
    rgiDestArrowColor, rgiDestArrowColorTheme);
  FDestArrowColorPicker.Color := dxDefaultArrowColor;
  FDestArrowColorPicker.OnColorChanged := DestArrowColorChangedHandler;
  UpdateDestArrowColorImage;
  FObjectBkColorPicker := TdxFlowChartColorPickerController.Create(
    ppmObjectBkColor, rgiObjectBackgroundColor, rgiObjectBackgroundThemsColor);
  FObjectBkColorPicker.Color := dxDefaultShapeBkColor;
  FObjectBkColorPicker.OnColorChanged := ObjectBkColorChangedHandler;
  UpdateObjectBkColorImage;
end;

procedure TdxFlowChartDesigner.FormDestroy(Sender: TObject);
begin
  ClearShapeGalleryInfo;
  ClearToolsStandardShapes;
  FreeAndNil(FBuffer);
  FreeAndNil(FBuffer);
  FreeAndNil(FShapeGalleryInfo);
  FreeAndNil(FStencilMenuInfo);
  FreeAndNil(FStencilGalleryInfo);
  FreeAndNil(FFontColorPicker);
  FreeAndNil(FBackgroundColorPicker);
  FreeAndNil(FStrokeColorPicker);
  FreeAndNil(FLineColorPicker);
  FreeAndNil(FSourceArrowColorPicker);
  FreeAndNil(FDestArrowColorPicker);
  FreeAndNil(FObjectBkColorPicker);
  FreeAndNil(FShortCuts);
  Application.HintHidePause := FOldHintHidePause;
end;

procedure TdxFlowChartDesigner.gcMoreShapesItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
var
  ARect: TRect;
begin
  ARect := dxMapWindowRect(gcMoreShapes.Handle, 0, gciMoreShapes.ViewInfo.Bounds);
  bpmStensils.Popup(ARect.Right, ARect.Top);
end;

procedure TdxFlowChartDesigner.gcMoreShapesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMoreShapesArrow.Invalidate;
end;

procedure TdxFlowChartDesigner.gcStencilsItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
begin
  UpdateActiveGalleryShapes;
end;

initialization
  dxFlowChartCustomizeFormManager.Register(TdxFlowChartDesigner);

  Screen.Cursors[crFlChartZoomIn] := LoadCursor(HInstance, 'ZOOMIN');
  Screen.Cursors[crFlChartZoomOut] := LoadCursor(HInstance, 'ZOOMOUT');

finalization
  dxFlowChartCustomizeFormManager.Unregister(TdxFlowChartDesigner);

end.
