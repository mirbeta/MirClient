{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Ruler;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, ActiveX, Windows, Messages, Generics.Defaults, Generics.Collections,
  Controls, SyncObjs, Classes, Graphics, dxCore, dxGDIPlusClasses,
  cxGraphics, dxCoreClasses, dxCoreGraphics, cxControls, cxGeometry, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, dxSkinInfo,

  dxRichEdit.Types,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.Commands.Tables,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Control,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Control.Mouse,
  dxRichEdit.Control.Mouse.AutoScroller,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.DocumentModel.Section;

type
  TdxOrientation = class;

  TdxRulerControlBase = class;
  TdxHorizontalRulerControl = class;
  TdxVerticalRulerControl = class;

  TdxRulerPainterBase = class;
  TdxHorizontalRulerPainter = class;
  TdxVerticalRulerPainter = class;
  TdxRulerMouseController = class;

  TdxRulerTickmark = class;
  TdxRulerTickmarkNumber = class;
  TdxHorizontalRulerHotZone = class;
  TdxRulerHotZone = class;
  TdxLeftBottomHotZone = class;
  TdxRulerViewInfoBase = class;
  TdxHorizontalRulerViewInfo = class;
  TdxVerticalRulerViewInfo = class;

  TdxGenerateImageDelegate = reference to procedure (AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);

  TdxRulerFont = TFont;

  TdxRulerTickmarkType = (
    Tick,
    HalfTick,
    QuarterTick
  );

  { TdxOrientation }

  TdxOrientation = class abstract
  public
    function CreatePoint(APrimary: Integer; ASecondary: Integer): TPoint; virtual; abstract;
    function GetFarPrimaryCoordinate(const ABounds: TRect): Integer; overload; virtual; abstract;
    function GetFarPrimaryCoordinate(const ABounds: TdxRectF): Single; overload; virtual; abstract;
    function GetNearPrimaryCoordinate(const ABounds: TRect): Integer; overload; virtual; abstract;
    function GetNearPrimaryCoordinate(const ABounds: TdxRectF): Single; overload; virtual; abstract;
    function GetPrimaryCoordinate(const APoint: TPoint): Integer; virtual; abstract;
    function GetPrimaryCoordinateExtent(const ABounds: TdxRectF): Single; virtual; abstract;
    function GetSecondaryCoordinate(const APoint: TPoint): Integer; virtual; abstract;
    function SetNearPrimaryCoordinate(const ABounds: TdxRectF; AValue: Single): TdxRectF; virtual; abstract;
  end;

  { TdxHorizontalOrientation }

  TdxHorizontalOrientation = class(TdxOrientation)
  public
    function GetPrimaryCoordinate(const APoint: TPoint): Integer; override;
    function GetSecondaryCoordinate(const APoint: TPoint): Integer; override;
    function CreatePoint(APrimary: Integer; ASecondary: Integer): TPoint; override;
    function GetNearPrimaryCoordinate(const ABounds: TdxRectF): Single; override;
    function GetFarPrimaryCoordinate(const ABounds: TdxRectF): Single; override;
    function GetPrimaryCoordinateExtent(const ABounds: TdxRectF): Single; override;
    function GetNearPrimaryCoordinate(const ABounds: TRect): Integer; override;
    function GetFarPrimaryCoordinate(const ABounds: TRect): Integer; override;
    function SetNearPrimaryCoordinate(const ABounds: TdxRectF; AValue: Single): TdxRectF; override;
  end;

  { TdxVerticalOrientation }

  TdxVerticalOrientation = class(TdxOrientation)
  public
    function GetPrimaryCoordinate(const APoint: TPoint): Integer; override;
    function GetSecondaryCoordinate(const APoint: TPoint): Integer; override;
    function CreatePoint(APrimary: Integer; ASecondary: Integer): TPoint; override;
    function GetNearPrimaryCoordinate(const ABounds: TdxRectF): Single; override;
    function GetFarPrimaryCoordinate(const ABounds: TdxRectF): Single; override;
    function GetPrimaryCoordinateExtent(const ABounds: TdxRectF): Single; override;
    function GetNearPrimaryCoordinate(const ABounds: TRect): Integer; override;
    function GetFarPrimaryCoordinate(const ABounds: TRect): Integer; override;
    function SetNearPrimaryCoordinate(const ABounds: TdxRectF; AValue: Single): TdxRectF; override;
  end;

  { TdxRulerPainterBase }

  TdxRulerPainterBase = class abstract
  strict private
    FRuler: TdxRulerControlBase;
    FForeColorBrush: TdxGPBrush;
    FGraphics: TdxGraphics;
    FView: TdxRichEditView;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
  private
    function GetSkinInfo: TdxSkinInfo;
  protected
    function GetForeColor: TdxAlphaColor; virtual;
    function GetVerticalTextPaddingBottom: Integer; virtual;
    function GetVerticalTextPaddingTop: Integer; virtual;
    function GetPaddingTop: Integer; virtual;
    function GetPaddingBottom: Integer; virtual;
    procedure BeginDraw(AGraphics: TdxGraphics); virtual;
    procedure EndDraw;
    procedure DrawActiveAreas;
    procedure DrawSpaceAreas;
    procedure DrawTickMarks;
    procedure DrawHotZones;

    procedure DrawBackground(AGraphics: TdxGraphics); virtual;
    procedure DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect); virtual; abstract;
    procedure DrawInPixels(AGraphics: TdxGraphics); virtual; abstract;
    procedure DrawTickMark(ATickMark: TdxRulerTickmark); virtual; abstract;
    procedure DrawTickmarkNumber(ATickmark: TdxRulerTickmarkNumber); virtual; abstract;
    procedure DrawActiveArea(const ABounds: TdxRectF); virtual; abstract;
    procedure DrawSpaceArea(const ABounds: TdxRectF); virtual; abstract;
    procedure DrawHotZone(AHotZone: TdxRulerHotZone); virtual; abstract;
    procedure DrawRulerAreaBackground(const ABounds: TRect); virtual; abstract;

    property ForeColor: TdxAlphaColor read GetForeColor;
    property VerticalTextPaddingBottom: Integer read GetVerticalTextPaddingBottom;
    property VerticalTextPaddingTop: Integer read GetVerticalTextPaddingTop;
    property View: TdxRichEditView read FView;
    property PaddingTop: Integer read GetPaddingTop;
    property PaddingBottom: Integer read GetPaddingBottom;

    property ForeColorBrush: TdxGPBrush read FForeColorBrush;
    property Graphics: TdxGraphics read FGraphics;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property Ruler: TdxRulerControlBase read FRuler;
    property SkinInfo: TdxSkinInfo read GetSkinInfo;
  public
    constructor Create(ARuler: TdxRulerControlBase);
    destructor Destroy; override;

    function CalculateTotalRulerSize(ATextSize: Integer): Integer; virtual; abstract;
    procedure Draw(AGraphics: TdxGraphics);
    class procedure DrawImage(AGraphics: TdxGraphics; AImage: TdxGPImage; X, Y: Integer); overload; static;
    class procedure DrawImage(AGraphics: TdxGraphics; AImage: TdxGPImage; const ABounds: TRect); overload; static;
    class function GenerateImage(const ABounds: TRect; AColor: TdxAlphaColor; const AGenerateImage: TdxGenerateImageDelegate): TdxGPImage;
    procedure Initialize; virtual;
  end;

  { TdxRulerAutoScroller }

  TdxRulerAutoScroller = class(TdxAutoScroller)
  protected
    procedure PopulateHotZones; override;
  end;

  { TdxRulerMouseController }

  TdxRulerMouseController = class abstract (TdxRichEditMouseController)
  strict private
    FControl: TdxRulerControlBase;
    function GetOrientation: TdxOrientation;
  protected
    procedure CalculateAndSaveHitInfo(const E: TdxMouseEventArgs); override;
    function CreateAutoScroller: TdxAutoScroller; override;
    procedure HandleClickTimerTick;
    procedure HandleMouseWheel(const E: TdxMouseEventArgs); override;
    function GetSnappedPositionCore(APos: Integer): Integer; virtual;
    function GetNearestIndex(APos: Integer; ATickmarks: TdxList<TdxRulerTickmark>; AIndex: Integer): Integer;
    function GetTickMiddlePoint(const ATickmarkBounds: TdxRectF): Single;
    procedure Cancel;
  public
    constructor Create(AControl: TdxRulerControlBase); reintroduce; virtual;

    procedure CaptureMouse; virtual;
    function CreateLineVisualFeedback(
      const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState): IdxVisualFeedback; virtual; abstract;
    function GetHotZoneBounds(AHotZone: TdxRulerHotZone): TRect; virtual;
    function GetSnappedPosition(APos: Integer): Integer; virtual;
    procedure ReleaseMouseCapture; virtual;
    procedure SetMouseCursor(ACursor: TCursor); virtual;

    procedure Initialize; virtual;
    procedure SwitchToDefaultState; override;
    function GetPrimaryCoordinate(const APoint: TPoint): Integer;
    function GetSecondaryCoordinate(const APoint: TPoint): Integer;
    function CreatePoint(APrimary: Integer; ASecondary: Integer): TPoint;
    function GetNearPrimaryCoordinate(const ABounds: TdxRectF): Single; overload;
    function GetFarPrimaryCoordinate(const ABounds: TdxRectF): Single; overload;
    function GetPrimaryCoordinateExtent(const ABounds: TdxRectF): Single;
    function GetNearPrimaryCoordinate(const ABounds: TRect): Integer; overload;
    function GetFarPrimaryCoordinate(const ABounds: TRect): Integer; overload;
    function SetNearPrimaryCoordinate(const ABounds: TdxRectF; AValue: Single): TdxRectF;

    property Control: TdxRulerControlBase read FControl;
    property Orientation: TdxOrientation read GetOrientation;
  end;

  { TdxRulerHotZone }

  TdxRulerHotZone = class abstract(TdxReferencedObject)
  strict private
    FControl: TdxRulerControlBase;
    FEnabled: Boolean;
    FVisible: Boolean;
    FIsNew: Boolean;
    FBounds: TRect;
    FDisplayBounds: TRect;
    procedure SetBounds(const AValue: TRect);
    function GetRichEditControl: IdxRichEditControl;
    function GetDocumentModel: TdxDocumentModel;
    function GetRulerViewInfo: TdxRulerViewInfoBase;
    function GetRulerClientBounds: TRect;
    function GetZoomFactor: Single;
  protected
    function GetWeight: Integer; virtual; abstract;
    function GetCursor: TCursor; virtual;

    property Control: TdxRulerControlBase read FControl;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property RulerViewInfo: TdxRulerViewInfoBase read GetRulerViewInfo;
    property RulerClientBounds: TRect read GetRulerClientBounds;
    property ZoomFactor: Single read GetZoomFactor;
    property Weight: Integer read GetWeight;
  public
    constructor Create(const AControl: TdxRulerControlBase);
    procedure AddFakeHotZone; virtual;
    function GetVisualFeedbackValue(const AMousePosition: TPoint;
      APageViewInfo: TdxPageViewInfo): Integer; virtual; abstract;
    procedure OnMouseDoubleClick; virtual; abstract;
    procedure OnMove(const AMousePosition: TPoint); virtual; abstract;
    function CanActivate(const Args: TdxMouseEventArgs): Boolean; virtual;
    function CanEdit: Boolean; virtual;
    procedure Commit(const AMousePosition: TPoint); virtual; abstract;
    function CreateEmptyClone: TdxRulerHotZone; virtual; abstract;
    function Clone: TdxRulerHotZone; virtual;
    procedure Activate(AHandler: TdxRulerMouseController; const AMousePosition: TPoint); virtual;
    procedure SetNewValue(ANewValue: Integer); virtual; abstract;

    property Bounds: TRect read FBounds write SetBounds;
    property DisplayBounds: TRect read FDisplayBounds;
    property Cursor: TCursor read GetCursor;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Visible: Boolean read FVisible write FVisible;
    property IsNew: Boolean read FIsNew write FIsNew;
    property RulerControl: TdxRulerControlBase read FControl;
    property RichEditControl: IdxRichEditControl read GetRichEditControl;
  end;

  { TdxRulerTickmark }

  TdxRulerTickmark = class abstract
  strict private
    FBounds: TdxRectF;
    FDisplayBounds: TdxRectF;
    FVisible: Boolean;
  protected
    function GetRulerTickmarkType: TdxRulerTickmarkType; virtual; abstract;
  public
    constructor Create(const AControl: TdxRulerControlBase; const ABounds: TdxRectF);
    procedure Draw(APainter: TdxHorizontalRulerPainter); overload; virtual; abstract;
    procedure Draw(APainter: TdxVerticalRulerPainter); overload; virtual; abstract;

    property Visible: Boolean read FVisible write FVisible;
    property DisplayBounds: TdxRectF read FDisplayBounds;
    property Bounds: TdxRectF read FBounds;
    property RulerTickmarkType: TdxRulerTickmarkType read GetRulerTickmarkType;
  end;

  { TdxRulerTickmarkNumber }

  TdxRulerTickmarkNumber = class(TdxRulerTickmark)
  strict private
    FNumber: string;
  protected
    function GetRulerTickmarkType: TdxRulerTickmarkType; override;
  public
    constructor Create(const AControl: TdxRulerControlBase; const ABounds: TdxRectF;
      const ANumber: string);
    procedure Draw(APainter: TdxHorizontalRulerPainter); override;
    procedure Draw(APainter: TdxVerticalRulerPainter); override;

    property Number: string read FNumber;
  end;

  { TdxRulerTickmarkHalf }

  TdxRulerTickmarkHalf = class(TdxRulerTickmark)
  protected
    function GetRulerTickmarkType: TdxRulerTickmarkType; override;
  public
    procedure Draw(APainter: TdxHorizontalRulerPainter); override;
    procedure Draw(APainter: TdxVerticalRulerPainter); override;
  end;

  { TdxRulerTickmarkQuarter }

  TdxRulerTickmarkQuarter = class(TdxRulerTickmark)
  protected
    function GetRulerTickmarkType: TdxRulerTickmarkType; override;
  public
    procedure Draw(APainter: TdxHorizontalRulerPainter); override;
    procedure Draw(APainter: TdxVerticalRulerPainter); override;
  end;

  { TdxHorizontalRulerHotZone }

  TdxHorizontalRulerHotZone = class abstract(TdxRulerHotZone)
  strict private
    function GetHorizontalRulerControl: TdxHorizontalRulerControl;
    function GetHorizontalRulerViewInfo: TdxHorizontalRulerViewInfo;
  protected
    function CalculateBounds(const ABounds: TRect): TRect; virtual; abstract;
    function GetDocumentLayoutDistanceToCurrentAreaStart(AMouseX: Integer): Integer; virtual;
    function GetDocumentLayoutDistanceToCellLeft(AMouseX: Integer): Integer; virtual;
    function GetDocumentLayoutDistanceToCurrentAreaEnd(AMouseX: Integer): Integer; virtual;
    function CalculateSize: TSize;

    property HorizontalRulerViewInfo: TdxHorizontalRulerViewInfo read GetHorizontalRulerViewInfo;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl);
    function GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer; override;

    property HorizontalRulerControl: TdxHorizontalRulerControl read GetHorizontalRulerControl;
  end;

  { TdxEmptyHorizontalRulerHotZone }

  TdxEmptyHorizontalRulerHotZone = class(TdxHorizontalRulerHotZone)
  protected
    function GetWeight: Integer; override;
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect);
    function GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer; override;

    procedure OnMouseDoubleClick; override;
    procedure OnMove(const AMousePosition: TPoint); override;
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    procedure Activate(AHandler: TdxRulerMouseController; const AMousePosition: TPoint); override;
    procedure SetNewValue(ANewValue: Integer); override;
  end;

  { TdxTabHotZone }

  TdxTabHotZone = class abstract(TdxHorizontalRulerHotZone)
  strict private
    FOldTabInfo: TdxTabInfo;
    FTabInfo: TdxTabInfo;
  protected
    function GetWeight: Integer; override;
    function CanDropTab(const AMousePosition: TPoint): Boolean;
    procedure AddNewTab;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; const ATabInfo: TdxTabInfo);
    function CanActivate(const Args: TdxMouseEventArgs): Boolean; override;
    procedure OnMove(const AMousePosition: TPoint); override;
    procedure Commit(const AMousePosition: TPoint); override;
    procedure OnMouseDoubleClick; override;
    procedure SetNewValue(ANewValue: Integer); override;

    property TabInfo: TdxTabInfo read FTabInfo;
  end;

  { TdxLeftTabHotZone }

  TdxLeftTabHotZone = class(TdxTabHotZone)
  protected
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    function CreateEmptyClone: TdxRulerHotZone; override;
  end;

  { TdxRightTabHotZone }

  TdxRightTabHotZone = class(TdxTabHotZone)
  protected
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    function CreateEmptyClone: TdxRulerHotZone; override;
  end;

  { TdxCenterTabHotZone }

  TdxCenterTabHotZone = class(TdxTabHotZone)
  protected
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    function CreateEmptyClone: TdxRulerHotZone; override;
  end;

  { TdxDecimalTabHotZone }

  TdxDecimalTabHotZone = class(TdxTabHotZone)
  protected
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    function CreateEmptyClone: TdxRulerHotZone; override;
  end;

  { TdxTableLeftBorderHotZone }

  TdxTableLeftBorderHotZone = class(TdxHorizontalRulerHotZone)
  strict private
    FTableViewInfo: TdxTableViewInfo;
    FLeftBorder: Integer;
    FRightBorder: Single;
    FNewValue: Integer;
    FColumn: TdxVirtualTableColumn;
    FTableCell: TdxTableCell;
    FBorderIndex: Integer;
  protected
    function GetWeight: Integer; override;
    function GetCursor: TCursor; override;
    procedure CalculateBorders;
    function GetVirtualTableColumn: TdxVirtualTableColumn; virtual;
    function GetElementAccessor: TdxTableElementAccessorBase; virtual;
    function CalculateBounds(const ABounds: TRect): TRect; override;

    property TableViewInfo: TdxTableViewInfo read FTableViewInfo;
    property Column: TdxVirtualTableColumn read FColumn;
    property TableCell: TdxTableCell read FTableCell;
    property NewValue: Integer read FNewValue;
    property BorderIndex: Integer read FBorderIndex;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect;
      ABorderIndex: Integer; ATableViewInfo: TdxTableViewInfo; ATableCell: TdxTableCell);
    destructor Destroy; override;
    procedure OnMove(const AMousePosition: TPoint); override;
    function CanEdit: Boolean; override;
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    procedure OnMouseDoubleClick; override;
    procedure SetNewValue(ANewValue: Integer); override;
  end;

  { TdxTableHotZone }

  TdxTableHotZone = class(TdxTableLeftBorderHotZone)
  protected
    function GetElementAccessor: TdxTableElementAccessorBase; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect;
      ABorderIndex: Integer; ATableViewInfo: TdxTableViewInfo; ATableCell: TdxTableCell);
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
  end;

  { TdxIndentHotZone }

  TdxIndentHotZone = class abstract(TdxHorizontalRulerHotZone)
  strict private
    FIndent: Integer;
    FRightBorder: Integer;
    FLeftBorder: Integer;
    FNewIndent: Integer;
  protected
    function GetWeight: Integer; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AIndent: Integer);
    procedure OnMove(const AMousePosition: TPoint); override;
    procedure OnMouseDoubleClick; override;
    procedure SetNewValue(ANewIndent: Integer); override;

    property NewIndent: Integer read FNewIndent write FNewIndent;
    property Indent: Integer read FIndent;
    property LeftBorder: Integer read FLeftBorder write FLeftBorder;
    property RightBorder: Integer read FRightBorder write FRightBorder;
  end;

  { TdxLeftIndentHotZone }

  TdxLeftIndentHotZone = class(TdxIndentHotZone)
  strict private
    FLeftBottomHotZone: TdxLeftBottomHotZone;
    FFirstLineIndent: Integer;
  protected
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect;
      AIndent: Integer; ALeftBottomHotZone: TdxLeftBottomHotZone; AFirstLineIndent: Integer);

    procedure Activate(AHandler: TdxRulerMouseController; const AMousePosition: TPoint); override;
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    procedure OnMove(const AMousePosition: TPoint); override;
  end;

  { TdxRightIndentHotZone }

  TdxRightIndentHotZone = class(TdxIndentHotZone)
  protected
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AIndent: Integer);
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    procedure OnMove(const AMousePosition: TPoint); override;
    procedure SetNewValue(ANewIndent: Integer); override;
  end;

  { TdxFirstLineIndentHotZone }

  TdxFirstLineIndentHotZone = class(TdxIndentHotZone)
  strict private
    FLeftIndent: Integer;
  protected
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect;
      AIndent: Integer; ALeftIndent: Integer);
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
  end;

  { TdxLeftBottomHotZone }

  TdxLeftBottomHotZone = class(TdxIndentHotZone)
  strict private
    FFirstLineHotZone: TdxFirstLineIndentHotZone;
    FLeftIndentHotZone: TdxLeftIndentHotZone;
  protected
    procedure AddLeftHotZone(const ABounds: TRect);
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect;
      AIndent: Integer; ARightBorder: Integer; AFirstLineIndentHotZone: TdxFirstLineIndentHotZone);
    procedure AddFakeHotZone; override;
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    procedure OnMove(const AMousePosition: TPoint); override;
  end;

  { TdxColumnResizerHotZone }

  TdxColumnResizerHotZone = class abstract(TdxHorizontalRulerHotZone)
  strict private
    FColumnIndex: Integer;
    function GetSectionProperties: TdxSectionProperties;
  protected
    function GetCursor: TCursor; override;
    function GetWeight: Integer; override;
    procedure OnMoveCore(const AMousePosition: TPoint); virtual; abstract;
    function CalculateNewValue(const AMousePosition: TPoint): Integer; virtual; abstract;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
    procedure AddFakeHotZone; override;
    procedure OnMouseDoubleClick; override;
    procedure OnMove(const AMousePosition: TPoint); override;
    procedure Commit(const AMousePosition: TPoint); override;
    procedure SetNewValue(ANewPosition: Integer); override;

    property ColumnIndex: Integer read FColumnIndex;
    property SectionProperties: TdxSectionProperties read GetSectionProperties;
  end;

  { TdxMiddleColumnResizerHotZone }

  TdxMiddleColumnResizerHotZone = class(TdxColumnResizerHotZone)
  protected
    procedure OnMoveCore(const AMousePosition: TPoint); override;
    function CalculateBounds(const ABounds: TRect): TRect; override;
    function CalculateNewValue(const AMousePosition: TPoint): Integer; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    function GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer; override;
  end;

  { TdxLeftColumnResizerHotZone }

  TdxLeftColumnResizerHotZone = class(TdxColumnResizerHotZone)
  protected
    procedure OnMoveCore(const AMousePosition: TPoint); override;
    function CalculateBounds(const ABounds: TRect): TRect; override;
    function CalculateNewValue(const AMousePosition: TPoint): Integer; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateCommand(const AMousePosition: TPoint): TdxChangeColumnSizeCommand;
    function CreateEmptyClone: TdxRulerHotZone; override;
    function GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer; override;
    procedure OnMouseDoubleClick; override;
  end;

  { TdxRightColumnResizerHotZone }

  TdxRightColumnResizerHotZone = class(TdxColumnResizerHotZone)
  protected
    procedure OnMoveCore(const AMousePosition: TPoint); override;
    function CalculateBounds(const ABounds: TRect): TRect; override;
    function CalculateNewValue(const AMousePosition: TPoint): Integer; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    function GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer; override;
  end;

  { TdxTabTypeToggleBackgroundHotZone }

  TdxTabTypeToggleBackgroundHotZone = class(TdxHorizontalRulerHotZone)
  protected
    function GetWeight: Integer; override;
    function CalculateBounds(const ABounds: TRect): TRect; override;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect);
    procedure OnMove(const AMousePosition: TPoint); override;
    procedure Commit(const AMousePosition: TPoint); override;
    procedure OnMouseDoubleClick; override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    procedure SetNewValue(ANewValue: Integer); override;
  end;

  { TdxTabTypeToggleHotZone }

  TdxTabTypeToggleHotZone = class(TdxHorizontalRulerHotZone)
  strict private
    FHotZones: TdxReferencedObjectList<TdxHorizontalRulerHotZone>;
    FHotZoneBounds: TRect;
    function GetHotZone: TdxHorizontalRulerHotZone;
  protected
    function GetWeight: Integer; override;
    function CreateHotZones: TdxReferencedObjectList<TdxHorizontalRulerHotZone>;
    function CalculateBounds(const ABounds: TRect): TRect; override;
    function CalculateHotZoneBounds: TRect;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect);
    destructor Destroy; override;
    procedure OnMove(const AMousePosition: TPoint); override;
    procedure Commit(const AMousePosition: TPoint); override;
    procedure OnMouseDoubleClick; override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    procedure SetNewValue(ANewValue: Integer); override;

    property HotZone: TdxHorizontalRulerHotZone read GetHotZone;
    property HotZoneBounds: TRect read FHotZoneBounds;
  end;

  { TdxVerticalRulerHotZone }

  TdxVerticalRulerHotZone = class abstract(TdxRulerHotZone)
  strict private
    function GetVerticalRulerControl: TdxVerticalRulerControl;
  protected
    function GetWeight: Integer; override;
    function CalculateNewValue(const AMousePosition: TPoint): Integer; virtual; abstract;
  public
    constructor Create(const AControl: TdxVerticalRulerControl);
    procedure AddFakeHotZone; override;
    function GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer; override;
    procedure SetNewValue(ANewValue: Integer); override;

    property VerticalRulerControl: TdxVerticalRulerControl read GetVerticalRulerControl;
  end;

  { TdxSectionResizerHotZone }

  TdxSectionResizerHotZone = class abstract(TdxVerticalRulerHotZone)
  strict private
    FColumnIndex: Integer;
    function GetSectionProperties: TdxSectionProperties;
  protected
    function GetCursor: TCursor; override;
    procedure OnMoveCore(const AMousePosition: TPoint); virtual; abstract;
  public
    constructor Create(const AControl: TdxVerticalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
    procedure OnMouseDoubleClick; override;
    procedure OnMove(const AMousePosition: TPoint); override;

    property ColumnIndex: Integer read FColumnIndex;
    property SectionProperties: TdxSectionProperties read GetSectionProperties;
  end;

  { TdxSectionTopResizerHotZone }

  TdxSectionTopResizerHotZone = class(TdxSectionResizerHotZone)
  protected
    procedure OnMoveCore(const AMousePosition: TPoint); override;
    function CalculateNewValue(const AMousePosition: TPoint): Integer; override;
  public
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    function GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer; override;
    procedure OnMouseDoubleClick; override;
  end;

  { TdxSectionBottomResizerHotZone }

  TdxSectionBottomResizerHotZone = class(TdxSectionResizerHotZone)
  protected
    procedure OnMoveCore(const AMousePosition: TPoint); override;
    function CalculateNewValue(const AMousePosition: TPoint): Integer; override;
  public
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    procedure OnMouseDoubleClick; override;
  end;

  { TdxVerticalTableHotZone }

  TdxVerticalTableHotZone = class(TdxVerticalRulerHotZone)
  strict private
    FTableViewInfo: TdxTableViewInfo;
    FAnchorIndex: Integer;
  protected
    function GetCursor: TCursor; override;
    function CalculateBounds(const ABounds: TRect): TRect; virtual;
    function CalculateNewValue(const AMousePosition: TPoint): Integer; override;

    property TableViewInfo: TdxTableViewInfo read FTableViewInfo;
  public
    constructor Create(const AControl: TdxVerticalRulerControl; const ABounds: TRect;
      ATableViewInfo: TdxTableViewInfo; AAnchorIndex: Integer);
    destructor Destroy; override;
    function CanEdit: Boolean; override;
    procedure Commit(const AMousePosition: TPoint); override;
    function CreateEmptyClone: TdxRulerHotZone; override;
    function GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer; override;
    procedure OnMove(const AMousePosition: TPoint); override;
    procedure OnMouseDoubleClick; override;
  end;

  { TdxRulerViewInfoBase }

  TdxRulerViewInfoBase = class abstract
  strict private
    class var
      FMeasureGraphics: TdxGraphics;
    class function GetMeasureGraphics: TdxGraphics; static; inline;
  protected
    class procedure FinalizeMeasureGraphics; static;
  strict private
    FHotZones: TdxReferencedObjectList<TdxRulerHotZone>;
    FSectionProperties: TdxSectionProperties;
    FBounds: TRect;
    FRulerTickmarks: TdxObjectList<TdxRulerTickmark>;
    FTextSize: TSize;
    FControl: TdxRulerControlBase;
    FActiveAreaCollection: TList<TdxRectF>;
    FDisplayActiveAreaCollection: TList<TdxRectF>;
    FSpaceAreaCollection: TList<TdxRectF>;
    FDisplaySpaceAreaCollection: TList<TdxRectF>;
    FClientBounds: TRect;
    FHalfTickBounds: TdxRectF;
    FQuarterTickBounds: TdxRectF;
    FDisplayClientBounds: TRect;
    FDisplayHalfTickBounds: TRect;
    FDisplayQuarterTickBounds: TRect;
    FIsMainPieceTable: Boolean;
    FIsReady: Boolean;
    function GetToDocumentLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    function GetDocumentModel: TdxDocumentModel;
    function GetOrientation: TdxOrientation;
    function GetMaxRulerDimensionInModelUnits: Integer;
  protected
    procedure CalculateBounds(AUnitConverter: TdxDocumentLayoutUnitConverter);
    procedure Initialize; virtual;
    procedure SetClientBounds(const ABounds: TRect);
    function GetCurrentActiveAreaIndex: Integer; virtual;
    function MeasureString(const AValue: string): TSize; virtual;
    function CalculateTickStep(AConverter: TdxUnitConverter): Single; virtual;
    procedure ResampleDownRulerTickmarks; virtual;
    procedure ResampleDownTicks;
    procedure ResampleDownTicksCore(AIsDelete: Boolean);
    function ShouldResampleDownTicks: Boolean; virtual;
    function AreRulerTickmarksOverlapped: Boolean; virtual;
    procedure DeleteRulerTickmarks(AType: TdxRulerTickmarkType); virtual;
    procedure CalculateRulerSeparators; virtual;
    procedure AddTicks(AStep: Single; U: Single);
    procedure AddQuarterTicks(AStep: Single; U: Single); virtual;
    procedure AddHalfTick(U: Single);
    procedure AddQuarterTick(U: Single);
    procedure AddTickNumber(U: Single; AValue: Integer); virtual; abstract;
    function CalculatePageClientBounds(APageCalculator: TdxPageBoundsCalculator; AProperties: TdxSectionProperties): TRect;
    procedure CalculateSpaceAreaCollection; virtual; abstract;
    procedure CalculateActiveAreaCollection; virtual; abstract;
    procedure CalculateTableHotZone; virtual; abstract;
    function CalculateQuarterTickBounds: TdxRectF; virtual; abstract;
    function CalculateHalfTickBounds: TdxRectF; virtual; abstract;
    function CalculateClientBounds(const AControl: TdxRulerControlBase): TRect; virtual; abstract;
    function GetRulerLayoutPosition(AModelPosition: Integer): Integer; virtual; abstract;

    property IsMainPieceTable: Boolean read FIsMainPieceTable;
    property ToDocumentLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter read GetToDocumentLayoutUnitConverter;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Orientation: TdxOrientation read GetOrientation;
    property MaxRulerDimensionInModelUnits: Integer read GetMaxRulerDimensionInModelUnits;
    property IsReady: Boolean read FIsReady write FIsReady;
  public
    constructor Create(const AControl: TdxRulerControlBase; ASectionProperties: TdxSectionProperties; AIsMainPieceTable: Boolean);
    destructor Destroy; override;
    function GetRulerModelPosition(ALayoutPosition: Integer): Integer; virtual; abstract;

    property Control: TdxRulerControlBase read FControl;
    property SectionProperties: TdxSectionProperties read FSectionProperties;
    property Bounds: TRect read FBounds write FBounds;
    property RulerTickmarks: TdxObjectList<TdxRulerTickmark> read FRulerTickmarks;
    property TextSize: TSize read FTextSize;
    property SpaceAreaCollection: TList<TdxRectF> read FSpaceAreaCollection;
    property ActiveAreaCollection: TList<TdxRectF> read FActiveAreaCollection;
    property ClientBounds: TRect read FClientBounds;
    property HalfTickBounds: TdxRectF read FHalfTickBounds;
    property QuarterTickBounds: TdxRectF read FQuarterTickBounds;
    property DisplaySpaceAreaCollection: TList<TdxRectF> read FDisplaySpaceAreaCollection;
    property DisplayActiveAreaCollection: TList<TdxRectF> read FDisplayActiveAreaCollection;
    property DisplayClientBounds: TRect read FDisplayClientBounds;
    property DisplayHalfTickBounds: TRect read FDisplayHalfTickBounds;
    property DisplayQuarterTickBounds: TRect read FDisplayQuarterTickBounds;
    property CurrentActiveAreaIndex: Integer read GetCurrentActiveAreaIndex;
    property HotZones: TdxReferencedObjectList<TdxRulerHotZone> read FHotZones;
    class property MeasureGraphics: TdxGraphics read GetMeasureGraphics;
  end;

  { TdxHorizontalRulerViewInfo }

  TdxHorizontalRulerViewInfo = class(TdxRulerViewInfoBase)
  strict private const
  {$REGION 'private const'}
    MinDistanceBetweenIndents = 10;
  {$ENDREGION}
  strict private
    FControl: TdxHorizontalRulerControl;
    FDefaultTabsCollection: TdxRectList;
    FDisplayDefaultTabsCollection: TdxRectList;
    FDefaultTabBounds: TRect;
    FDisplayDefaultTabBounds: TRect;
    FTabTypeToggle: TdxTabTypeToggleHotZone;
    FTabTypeToggleBackground: TdxTabTypeToggleBackgroundHotZone;
    FDefaultTabWidth: Integer;
    FParagraph: TdxParagraph;
    FCurrentActiveAreaIndex: Integer;
    FTableCellViewInfo: TdxTableCellViewInfo;
    FTableAlignedPositions: TdxSortedList<Integer>;
    function GetCellTextRight(ACellViewInfo: TdxTableCellViewInfo; AApplyZoomFactor: Boolean): Integer;
    function GetRightIndentBasePosition: Single;
  protected
    procedure Initialize; override;
    function GetCurrentActiveAreaIndex: Integer; override;
    procedure CalculateBounds;
    function GetRulerSize: Integer;
    function CalculateClientBounds(const AControl: TdxRulerControlBase): TRect; override;
    procedure CalculateActiveAreaCollection; override;
    procedure AddActiveArea(const ABounds: TRect);
    function GetActiveAreaBounds: TRect;
    function GetRulerAreaCollection(ASectionProperties: TdxSectionProperties; const ABounds: TRect): TdxRectList;
    procedure CalculateSpaceAreaCollection; override;
    procedure CalculateTableHotZone; override;
    procedure AddTableLeftBorderHotZone; virtual;
    function GetTableHotZoneLeft(ATableAlignedPositionsIndex: Integer): Integer; virtual;
    function ApplyZoomFactor(const AArea: TRect): TRect;
    procedure AddTickNumber(X: Single; AValue: Integer); override;
    procedure CalculateTabsBounds; virtual;
    procedure CalculateTabsBoundsCore(ATabsController: TdxTabsController); virtual;
    procedure AddTabHotZone(const ATabInfo: TdxTabInfo; APos: Integer);
    function AddTabHotZoneCore(const ATabInfo: TdxTabInfo; APos: Integer): TdxTabHotZone;
    function CreateTabHotZone(const ATabInfo: TdxTabInfo; const ABounds: TRect): TdxTabHotZone;
    function GetTabBounds(APosition: Integer): TRect;
    procedure AddDefaultTab(APos: Integer);
    function GetRulerLayoutPosition(AModelPosition: Integer): Integer; override;
    function GetRulerModelPositionRelativeToTableCellViewInfo(ALayoutPosition: Integer): Integer;
    procedure CalculateIndentElementsBounds;
    function GetRightIndentLayoutPosition(AModelPosition: Integer): Integer;
    function GetRightIndentModelPosition(ALayoutPosition: Integer): Integer;
    function GetIndentInModelUnits(AParagraph: TdxParagraph): Integer;
    procedure AddLeftIndentHotZone(AParagraph: TdxParagraph; ALeftIndent: Integer; ARightBorder: Integer; AFirstIndent: Integer);
    function AddFirstLineHotZone(AParagraph: TdxParagraph; ARightBorder: Integer; AFirstIndent: Integer;
      AHeight: Integer): TdxFirstLineIndentHotZone;
    procedure AddRightIndentHotZone(AParagraph: TdxParagraph; ARightIndent: Integer; ALeftBorder: Integer);
    procedure CalculateSectionSeparator;
    procedure AddMiddleColumnResizer(AIndex: Integer);
    procedure AddLeftColumnResizer(AIndex: Integer);
    procedure AddRightColumnResizer(AIndex: Integer);
    function GetDefaultTabWidth: Integer;
    procedure CalculateTabTypeToggle;
    function CalculateHalfTickBounds: TdxRectF; override;
    function CalculateQuarterTickBounds: TdxRectF; override;
    function CalculateDefaultTabBounds: TRect; virtual;
    procedure SortingHotZones; virtual;
  public
    constructor Create(const AControl: TdxHorizontalRulerControl; ASectionProperties: TdxSectionProperties;
      ACurrentParagraph: TdxParagraph; ACurrentActiveAreaIndex: Integer; AIsMainPieceTable: Boolean;
      ATableCellViewInfo: TdxTableCellViewInfo; ATableAlignedPositions: TdxSortedList<Integer>);
    destructor Destroy; override;

    function GetAdditionalCellIndent(ACellViewInfo: TdxTableCellViewInfo; AApplyZoomFactor: Boolean): Integer; overload;
    function GetAdditionalCellIndent(AApplyZoomFactor: Boolean): Integer; overload;
    function GetAdditionalParentCellIndent(AApplyZoomFactor: Boolean): Integer;
    function GetRulerModelPosition(ALayoutPosition: Integer): Integer; override;

    property Paragraph: TdxParagraph read FParagraph;
    property TabTypeToggleBackground: TdxTabTypeToggleBackgroundHotZone read FTabTypeToggleBackground;
    property TabTypeToggleHotZone: TdxTabTypeToggleHotZone read FTabTypeToggle;
    property Control: TdxHorizontalRulerControl read FControl;
    property DefaultTabBounds: TRect read FDefaultTabBounds;
    property DisplayDefaultTabBounds: TRect read FDisplayDefaultTabBounds;
    property DefaultTabsCollection: TdxRectList read FDefaultTabsCollection;
    property DisplayDefaultTabsCollection: TdxRectList read FDisplayDefaultTabsCollection;
    property TableCellViewInfo: TdxTableCellViewInfo read FTableCellViewInfo;
  end;

  { TdxRulerHotZonesComparer }

  TdxRulerHotZonesComparer = class(TcxIUnknownObject, IComparer<TdxRulerHotZone>)
  public
    function Compare(const AHotZone, ANextHotZone: TdxRulerHotZone): Integer;
  end;

  { TdxVerticalRulerViewInfo }

  TdxVerticalRulerViewInfo = class(TdxRulerViewInfoBase)
  strict private
    FControl: TdxVerticalRulerControl;
    FTableVerticalPositions: TdxSortedList<Integer>;
    FTableCellViewInfo: TdxTableCellViewInfo;
  protected
    procedure Initialize; override;
    function CalculateClientBounds(const AControl: TdxRulerControlBase): TRect; override;
    procedure CalculateBounds;
    function GetRulerSize: Integer;
    function CalculateHalfTickBounds: TdxRectF; override;
    function CalculateQuarterTickBounds: TdxRectF; override;
    procedure CalculateActiveAreaCollection; override;
    function GetActiveBounds: TRect;
    function GetPageBounds: TRect;
    procedure CalculateSpaceAreaCollection; override;
    procedure CalculateTableHotZone; override;
    function ApplyZoomFactor(const AArea: TRect): TRect;
    procedure AddTickNumber(Y: Single; AValue: Integer); override;
    procedure CalculateSectionSeparator;
    procedure AddBottomSectionResizer(AIndex: Integer);
    procedure AddTopSectionResizer(AIndex: Integer);
    function GetRulerLayoutPosition(AModelPosition: Integer): Integer; override;

    property TableVerticalPositions: TdxSortedList<Integer> read FTableVerticalPositions;
    property TableCellViewInfo: TdxTableCellViewInfo read FTableCellViewInfo;
  public
    constructor Create(const AControl: TdxVerticalRulerControl; ASectionProperties: TdxSectionProperties;
      AIsMainPieceTable: Boolean; ATableVerticalPositions: TdxSortedList<Integer>; ATableCellViewInfo: TdxTableCellViewInfo);
    destructor Destroy; override;
    function GetRulerModelPosition(ALayoutPosition: Integer): Integer; override;

    property Control: TdxVerticalRulerControl read FControl;
  end;

  { TdxRulerElementPainter }

  TdxRulerElementPainter = class abstract
  strict private type
    TImages = class(TObjectDictionary<TClass, TdxGPImage>);
    TScaleImages = class(TObjectDictionary<TSize, TImages>);
  strict private
    FRuler: TdxHorizontalRulerControl;
    FEnabledElementImages: TScaleImages;
    FDisabledElementImages: TScaleImages;
    function CanDrawHotZone(AHotZone: TdxRulerHotZone; out AImage: TdxGPImage): Boolean;
    function GetScaleFactor: TdxScaleFactor;
  protected
    function CreateImage(AHotZone: TdxRulerHotZone): TdxGPImage; virtual;
    function GetImage(AHotZone: TdxRulerHotZone): TdxGPImage;

    procedure DrawHotZone(AHotZone: TdxRulerHotZone; AGraphics: TdxGraphics; const ALocation: TPoint); overload;
    procedure DrawHotZone(AHotZone: TdxRulerHotZone; AGraphics: TdxGraphics; const ABounds: TRect); overload;
    function CalculateHotZoneSize(AHotZone: TdxHorizontalRulerHotZone): TSize; virtual;

    property Ruler: TdxHorizontalRulerControl read FRuler;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(ARuler: TdxHorizontalRulerControl);
    destructor Destroy; override;
    procedure Initialize; virtual;
  end;

  { TdxColorBasedRulerElementPainter }

  TdxColorBasedRulerElementPainter = class(TdxRulerElementPainter)
  strict private
    FEnabledElementColor: TdxAlphaColor;
    FDisabledElementColor: TdxAlphaColor;
  protected
    function CreateImage(AHotZone: TdxRulerHotZone): TdxGPImage; override;
    function GenerateTabImage(const AGenerateImage: TdxGenerateImageDelegate; AColor: TdxAlphaColor): TdxGPImage; virtual;
    procedure DrawTabImageBottom(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
    procedure DrawLeftTabImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
    procedure DrawRightTabImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
    procedure DrawCenterTabImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
    procedure DrawDecimalTabImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);

    property EnabledElementColor: TdxAlphaColor read FEnabledElementColor;
    property DisabledElementColor: TdxAlphaColor read FDisabledElementColor;
  public
    constructor Create(ARuler: TdxHorizontalRulerControl; AElementColor: TdxAlphaColor);
  end;

  { TdxHorizontalRulerPainter }

  TdxHorizontalRulerPainter = class(TdxRulerPainterBase)
  strict private
    FRuler: TdxHorizontalRulerControl;
    FQuarterTickImage: TdxGPImage;
    FHalfTickImage: TdxGPImage;
    FDefaultTabImage: TdxGPImage;
    FElementPainter: TdxRulerElementPainter;
  protected
    function CreateElementPainter: TdxRulerElementPainter; virtual; abstract;
    procedure DrawTickmarkImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
    procedure DrawInPixels(AGraphics: TdxGraphics); override;
    procedure DrawRuler; virtual;
    procedure DrawDefaultTabMarks;
    procedure DrawHotZone(AHotZone: TdxRulerHotZone); override;
    procedure DrawTickMark(ATickMark: TdxRulerTickmark); override;
    procedure DrawTickmarkNumber(ATickmark: TdxRulerTickmarkNumber); override;
    procedure DrawTickmarkHalf(ATickmark: TdxRulerTickmarkHalf); virtual;
    procedure DrawTickmarkQuarter(ATickmark: TdxRulerTickmarkQuarter); virtual;
    procedure DrawDefaultTabMark(const ABounds: TRect); virtual;
    procedure DrawTabTypeToggleBackground(AGraphics: TdxGraphics; const ABounds: TRect); virtual; abstract;
    procedure DrawTabTypeToggleActiveArea(AGraphics: TdxGraphics; const ABounds: TRect); virtual; abstract;
    function GetDefaultTabColor: TdxAlphaColor; virtual;
    function GenerateQuarterTickImage: TdxGPImage; virtual;
    function GenerateHalfTickImage: TdxGPImage; virtual;
    function GenerateDefaultTabImage: TdxGPImage; virtual;
    function GenerateTickImageCore(const ABounds: TRect): TdxGPImage;
    function GetTabTypeToggleActiveAreaSize: TSize; virtual;

    property DefaultTabColor: TdxAlphaColor read GetDefaultTabColor;
  public
    constructor Create(ARuler: TdxHorizontalRulerControl);
    destructor Destroy; override;

    procedure Initialize; override;
    function CalculateTotalRulerSize(ATextSize: Integer): Integer; override;
    procedure DrawTabTypeToggle(AGraphics: TdxGraphics); virtual;
    procedure Draw(AGraphics: TdxGraphics);

    property ElementPainter: TdxRulerElementPainter read FElementPainter;
    property Ruler: TdxHorizontalRulerControl read FRuler;
    property QuarterTickImage: TdxGPImage read FQuarterTickImage;
    property HalfTickImage: TdxGPImage read FHalfTickImage;
  end;

  { TdxHorizontalRulerColorBasedPainter }

  TdxHorizontalRulerColorBasedPainter = class(TdxHorizontalRulerPainter)
  protected
    function GetControlBackgroundColor: TdxAlphaColor; virtual;
    function GetRulerAreaBackgroundColor: TdxAlphaColor; virtual;
    function GetActiveAreaColor: TdxAlphaColor; virtual;
    function GetTabTypeToggleBorderColor: TdxAlphaColor; virtual;

    procedure DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect); override;
    procedure DrawTabTypeToggleBackground(AGraphics: TdxGraphics; const ABounds: TRect); override;
    procedure DrawTabTypeToggleActiveArea(AGraphics: TdxGraphics; const ABounds: TRect); override;
    procedure DrawRulerAreaBackground(const ABounds: TRect); override;
    procedure DrawActiveArea(const ABounds: TdxRectF); override;
    procedure DrawSpaceArea(const ABounds: TdxRectF); override;
    function CreateElementPainter: TdxRulerElementPainter; override;

    property ControlBackgroundColor: TdxAlphaColor read GetControlBackgroundColor;
    property RulerAreaBackgroundColor: TdxAlphaColor read GetRulerAreaBackgroundColor;
    property ActiveAreaColor: TdxAlphaColor read GetActiveAreaColor;
    property TabTypeToggleBorderColor: TdxAlphaColor read GetTabTypeToggleBorderColor;
  end;

  { TdxRulerElementSkinPainter }

  TdxRulerElementSkinPainter = class(TdxRulerElementPainter)
  strict private
    FSkinInfo: TdxSkinInfo;
  protected
    function CreateImage(AHotZone: TdxRulerHotZone): TdxGPImage; override;
    function GetSkinElementImage(AElement: TdxSkinElement; AImageIndex: Integer; AState: TdxSkinElementState): TdxGPImage; virtual;

    property SkinInfo: TdxSkinInfo read FSkinInfo;
  public
    constructor Create(ARuler: TdxHorizontalRulerControl; ASkinInfo: TdxSkinInfo);
  end;

  { TdxHorizontalRulerSkinPainter }

  TdxHorizontalRulerSkinPainter = class(TdxHorizontalRulerPainter)
  protected
    function GetForeColor: TdxAlphaColor; override;
    function GetPaddingTop: Integer; override;
    function GetPaddingBottom: Integer; override;
    function GetVerticalTextPaddingBottom: Integer; override;
    function GetVerticalTextPaddingTop: Integer; override;
    procedure DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect); override;
    procedure DrawTabTypeToggleBackground(AGraphics: TdxGraphics; const ABounds: TRect); override;
    procedure DrawTabTypeToggleActiveArea(AGraphics: TdxGraphics; const ABounds: TRect); override;
    procedure DrawRulerAreaBackground(const ABounds: TRect); override;
    procedure DrawActiveArea(const ABounds: TdxRectF); override;
    procedure DrawSpaceArea(const ABounds: TdxRectF); override;
    function CreateElementPainter: TdxRulerElementPainter; override;
  public
    function CalculateTotalRulerSize(ATextSize: Integer): Integer; override;
  end;

  { TdxVerticalRulerPainter }

  TdxVerticalRulerPainter = class(TdxRulerPainterBase)
  strict private
    FRuler: TdxVerticalRulerControl;
    FQuarterTickImage: TdxGPImage;
    FHalfTickImage: TdxGPImage;
  protected
    function GenerateQuarterTickImage: TdxGPImage; virtual;
    function GenerateHalfTickImage: TdxGPImage; virtual;
    function GenerateTickImageCore(const ABounds: TRect): TdxGPImage;
    procedure DrawTickmarkImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
    procedure DrawRuler; virtual;
    procedure DrawTickMark(ATickMark: TdxRulerTickmark); override;
    procedure DrawTickmarkNumber(ATickmark: TdxRulerTickmarkNumber); override;
    procedure DrawTickmarkHalf(ATickmark: TdxRulerTickmarkHalf); virtual;
    procedure DrawTickmarkQuarter(ATickmark: TdxRulerTickmarkQuarter); virtual;
    procedure DrawInPixels(AGraphics: TdxGraphics); override;
  public
    constructor Create(ARuler: TdxVerticalRulerControl);
    destructor Destroy; override;

    procedure Initialize; override;
    function CalculateTotalRulerSize(ATextSize: Integer): Integer; override;

    property Ruler: TdxVerticalRulerControl read FRuler;
    property QuarterTickImage: TdxGPImage read FQuarterTickImage;
    property HalfTickImage: TdxGPImage read FHalfTickImage;
  end;

  { TdxVerticalRulerColorBasedPainter }

  TdxVerticalRulerColorBasedPainter = class abstract(TdxVerticalRulerPainter)
  protected
    function GetControlBackgroundColor: TdxAlphaColor; virtual;
    function GetRulerAreaBackgroundColor: TdxAlphaColor; virtual;
    function GetActiveAreaColor: TdxAlphaColor; virtual;
    function GetTableRowAreaColor: TdxAlphaColor; virtual;
    procedure DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect); override;
    procedure DrawRulerAreaBackground(const ABounds: TRect); override;
    procedure DrawActiveArea(const ABounds: TdxRectF); override;
    procedure DrawSpaceArea(const ABounds: TdxRectF); override;
    procedure DrawHotZone(AHotZone: TdxRulerHotZone); override;

    property ControlBackgroundColor: TdxAlphaColor read GetControlBackgroundColor;
    property RulerAreaBackgroundColor: TdxAlphaColor read GetRulerAreaBackgroundColor;
    property ActiveAreaColor: TdxAlphaColor read GetActiveAreaColor;
    property TableRowAreaColor: TdxAlphaColor read GetTableRowAreaColor;
  end;

  { TdxVerticalRulerSkinPainter }

  TdxVerticalRulerSkinPainter = class(TdxVerticalRulerPainter)
  protected
    function GetForeColor: TdxAlphaColor; override;
    function GetPaddingTop: Integer; override;
    function GetPaddingBottom: Integer; override;
    function GetVerticalTextPaddingBottom: Integer; override;
    function GetVerticalTextPaddingTop: Integer; override;
    procedure DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect); override;
    procedure DrawRulerAreaBackground(const ABounds: TRect); override;
    procedure DrawActiveArea(const ABounds: TdxRectF); override;
    procedure DrawSpaceArea(const ABounds: TdxRectF); override;
    procedure DrawHotZone(AHotZone: TdxRulerHotZone); override;
  public
    function CalculateTotalRulerSize(ATextSize: Integer): Integer; override;
  end;

  { TdxRulerControlBase }

  TdxRulerControlBase = class abstract(TdxCustomRichEditRulerControl,
    IdxRulerControl
  )
  strict private const
    TickMarkFontSize = 7;
  strict private
    FRichEditControl: TdxCustomRichEditControl;
    FOrientation: TdxOrientation;
    FMinWidth: Integer;
    FMinHeight: Integer;
    FPainter: TdxRulerPainterBase;
    FDpiX: Single;
    FDpiY: Single;
    FMouseController: TdxRulerMouseController;
    FPageIndex: Integer;
    FColumnIndex: Integer;
    FParagraphIndex: TdxParagraphIndex;
    FViewInfo: TdxRulerViewInfoBase;
    FDirtyViewInfos: TdxFastObjectList;
    FTickMarkFont: TdxGPFont;
    FSkinInfo: TdxSkinInfo;
    function GetZoomFactor: Single;
    function GetIgnoreChildren: Boolean;
    function GetDocumentModel: TdxDocumentModel;
    procedure UpdateSkinInfo;
  protected
    function GetRichEditControl: IdxRichEditControl; override;

    function CreateMouseController: TdxRulerMouseController; virtual; abstract;
    function CreatePainter: TdxRulerPainterBase; virtual; abstract;
    function CreateOrientation: TdxOrientation; virtual; abstract;
    function CreateTickMarkFont: TdxGPFont; virtual;
    function CreateViewInfo: TdxRulerViewInfoBase; virtual; abstract;
    procedure CheckReleaseDirtyViewInfos;
    procedure SetViewInfo(ANewViewInfo: TdxRulerViewInfoBase);

    procedure InvalidateRulerViewInfo; virtual;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoCancelMode; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoPaint; override;
    procedure ScaleFactorChanged; override;

    function CalculateParagraphIndex: TdxParagraphIndex; virtual;
    function GetSection(APieceTable: TdxPieceTable): TdxSection;
    function LayoutUnitsToPixelsH(AValue: Integer): Integer;
    function LayoutUnitsToPixelsV(AValue: Integer): Integer;
    function PixelsToLayoutUnitsH(AValue: Integer): Integer;
    function PixelsToLayoutUnitsV(AValue: Integer): Integer;
    function PixelsToLayoutUnits(const AValue: TSize): TSize; overload;
    procedure RecreatePainter; override;
    procedure RecreateTickMarkFont;
    procedure InitializeMouseHandler; virtual;

    function CanFocusOnClick(X, Y: Integer): Boolean; override;
    function CanHandleMouseEvent: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;

    procedure DrawRuler(AGraphics: TdxGraphics); virtual;
    function UpdatePageIndex: Boolean;
    function UpdateColumnIndex: Boolean;
    function UpdateParagraphIndex: Boolean;
    function FindEndParagraphIndex: TdxParagraphIndex; virtual;
    function GetTableAlignedPositions(ATableViewInfo: TdxTableViewInfo): TdxSortedList<Integer>;
    function GetTableCellViewInfo(AIsMainPieceTable: Boolean): TdxTableCellViewInfo;
    function GetTableVerticalPositions(AIsMainPieceTable: Boolean): TdxSortedList<Integer>;

    property SkinInfo: TdxSkinInfo read FSkinInfo;
  public
    constructor Create(ARichEditControl: TdxCustomRichEditControl); reintroduce; virtual;
    destructor Destroy; override;

    function GetPhysicalPoint(const APoint: TPoint): TPoint; virtual; abstract;
    procedure Reset; override;
    function LayoutUnitsToPixels(const AValue: TRect): TRect;
    function PixelsToLayoutUnits(const AValue: TRect): TRect; overload;
    procedure OnLookAndFeelChanged; override;

    procedure AddKeyboardService(const AService: IdxKeyboardHandlerService);
    procedure RemoveKeyboardService(const AService: IdxKeyboardHandlerService);

    property DpiX: Single read FDpiX;
    property DpiY: Single read FDpiY;
    property ZoomFactor: Single read GetZoomFactor;
    property IgnoreChildren: Boolean read GetIgnoreChildren;
    property Painter: TdxRulerPainterBase read FPainter;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property MouseController: TdxRulerMouseController read FMouseController;
    property Orientation: TdxOrientation read FOrientation;
    property RichEditControl: TdxCustomRichEditControl read FRichEditControl;
    property TickMarkFont: TdxGPFont read FTickMarkFont;
    property ViewInfo: TdxRulerViewInfoBase read FViewInfo;
  end;

  { TdxHorizontalRulerControl }

  TdxHorizontalRulerControl = class(TdxRulerControlBase)
  strict private
    FTabTypeIndex: Integer;
    function GetPainter: TdxHorizontalRulerPainter;
    function GetViewInfo: TdxHorizontalRulerViewInfo;
  protected
    function GetIsVisible: Boolean; override;
    function GetParagraph: TdxParagraph;
    function GetColumnIndex(AIsMainPieceTable: Boolean): Integer;

    function CreateMouseController: TdxRulerMouseController; override;
    function CreateViewInfo: TdxRulerViewInfoBase; override;
    function CreateOrientation: TdxOrientation; override;
    function CreatePainter: TdxRulerPainterBase; override;
    procedure RecreatePainter; override;

    function GetRulerSizeInPixels: Integer; override;
    function GetRulerHeightInPixels: Integer; override;
    procedure DrawRuler(AGraphics: TdxGraphics); override;
    function CanUpdate: Boolean; override;

    property Painter: TdxHorizontalRulerPainter read GetPainter;
  public
    constructor Create(ARichEditControl: TdxCustomRichEditControl); override;
    function GetPhysicalPoint(const APoint: TPoint): TPoint; override;
    procedure SetViewInfo(ANewViewInfo: TdxHorizontalRulerViewInfo);
    function CreateViewInfoCore(ASectionProperties: TdxSectionProperties;
      AIsMainPieceTable: Boolean): TdxHorizontalRulerViewInfo; virtual;
    function CalculateHotZoneSize(AHotZone: TdxHorizontalRulerHotZone): TSize; virtual;
    function GetTabTypeToggleActiveAreaSize: TSize;
    function CalculateTabTypeToggleBackgroundBounds: TRect;

    property TabTypeIndex: Integer read FTabTypeIndex write FTabTypeIndex;
    property ViewInfo: TdxHorizontalRulerViewInfo read GetViewInfo;
  end;

  { TdxVerticalRulerControl }

  TdxVerticalRulerControl = class(TdxRulerControlBase)
  private
    function GetPainter: TdxVerticalRulerPainter;
    function GetViewInfo: TdxVerticalRulerViewInfo;
  protected
    function CreateOrientation: TdxOrientation; override;
    function CreatePainter: TdxRulerPainterBase; override;
    function CreateViewInfo: TdxRulerViewInfoBase; override;
    function CreateMouseController: TdxRulerMouseController; override;
    function GetIsVisible: Boolean; override;
    function GetRulerSizeInPixels: Integer; override;
    function GetRulerWidthInPixels: Integer; override;
    procedure RecreatePainter; override;

    procedure DrawRuler(AGraphics: TdxGraphics); override;
    function CanUpdate: Boolean; override;
  public
    constructor Create(ARichEditControl: TdxCustomRichEditControl); override;
    function GetPhysicalPoint(const APoint: TPoint): TPoint; override;
    procedure SetViewInfo(ANewViewInfo: TdxVerticalRulerViewInfo);

    property Painter: TdxVerticalRulerPainter read GetPainter;
    property ViewInfo: TdxVerticalRulerViewInfo read GetViewInfo;
  end;

implementation

{$R 'dxRichEdit.Control.Ruler.RES'}

uses
  Contnrs, Math,
  dxTypeHelpers, cxDrawTextUtils,

  dxRichEdit.Utils.Cursors,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.Delete,
  dxRichEdit.Commands.Dialogs,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.Ruler.Mouse, dxRichEdit.Control.Keyboard;

type
  { TdxTickMarkPositionComparable }

  TdxTickMarkPositionComparable = class(TcxIUnknownObject, IdxComparable<TdxRulerTickmark>)
  strict private
    FOrientation: TdxOrientation;
    FPos: Integer;
  public
    constructor Create(const AOrientation: TdxOrientation; APos: Integer);
    function CompareTo(const ATickmark: TdxRulerTickmark): Integer;
  end;

function CreateRotatedFont(AFont: TFont; ADirection: TcxVerticalTextOutDirection): HFONT;
const
  Angles: array[TcxVerticalTextOutDirection] of Integer = (-900, 900);
var
  ALogFontW: TLogFontW;
begin
  FillChar(ALogFontW, SizeOf(ALogFontW), 0);
  GetObject(AFont.Handle, SizeOf(TLogFontW), @ALogFontW);
  ALogFontW.lfEscapement := Angles[ADirection];
  ALogFontW.lfOutPrecision := OUT_TT_ONLY_PRECIS;
  Result := CreateFontIndirectW(ALogFontW);
end;

{ TdxHorizontalOrientation }

function TdxHorizontalOrientation.GetPrimaryCoordinate(const APoint: TPoint): Integer;
begin
  Result := APoint.X;
end;

function TdxHorizontalOrientation.GetSecondaryCoordinate(const APoint: TPoint): Integer;
begin
  Result := APoint.Y;
end;

function TdxHorizontalOrientation.CreatePoint(APrimary: Integer; ASecondary: Integer): TPoint;
begin
  Result.Init(APrimary, ASecondary);
end;

function TdxHorizontalOrientation.GetNearPrimaryCoordinate(const ABounds: TdxRectF): Single;
begin
  Result := ABounds.Left;
end;

function TdxHorizontalOrientation.GetFarPrimaryCoordinate(const ABounds: TdxRectF): Single;
begin
  Result := ABounds.Right;
end;

function TdxHorizontalOrientation.GetPrimaryCoordinateExtent(const ABounds: TdxRectF): Single;
begin
  Result := ABounds.Width;
end;

function TdxHorizontalOrientation.GetNearPrimaryCoordinate(const ABounds: TRect): Integer;
begin
  Result := ABounds.Left;
end;

function TdxHorizontalOrientation.GetFarPrimaryCoordinate(const ABounds: TRect): Integer;
begin
  Result := ABounds.Right;
end;

function TdxHorizontalOrientation.SetNearPrimaryCoordinate(const ABounds: TdxRectF; AValue: Single): TdxRectF;
begin
  Result.Init(AValue, ABounds.Top, AValue + (ABounds.Right - ABounds.Left), ABounds.Bottom);
end;

{ TdxVerticalOrientation }

function TdxVerticalOrientation.GetPrimaryCoordinate(const APoint: TPoint): Integer;
begin
  Result := APoint.Y;
end;

function TdxVerticalOrientation.GetSecondaryCoordinate(const APoint: TPoint): Integer;
begin
  Result := APoint.X;
end;

function TdxVerticalOrientation.CreatePoint(APrimary: Integer; ASecondary: Integer): TPoint;
begin
  Result.Init(ASecondary, APrimary);
end;

function TdxVerticalOrientation.GetNearPrimaryCoordinate(const ABounds: TdxRectF): Single;
begin
  Result := ABounds.Top;
end;

function TdxVerticalOrientation.GetFarPrimaryCoordinate(const ABounds: TdxRectF): Single;
begin
  Result := ABounds.Bottom;
end;

function TdxVerticalOrientation.GetPrimaryCoordinateExtent(const ABounds: TdxRectF): Single;
begin
  Result := ABounds.Height;
end;

function TdxVerticalOrientation.GetNearPrimaryCoordinate(const ABounds: TRect): Integer;
begin
  Result := ABounds.Top;
end;

function TdxVerticalOrientation.GetFarPrimaryCoordinate(const ABounds: TRect): Integer;
begin
  Result := ABounds.Bottom;
end;

function TdxVerticalOrientation.SetNearPrimaryCoordinate(const ABounds: TdxRectF; AValue: Single): TdxRectF;
begin
  Result.Init(ABounds.Left, AValue, ABounds.Right, AValue + (ABounds.Bottom - ABounds.Top));
end;

{ TdxRulerPainterBase }

constructor TdxRulerPainterBase.Create(ARuler: TdxRulerControlBase);
begin
  inherited Create;
  FRuler := ARuler;
  FView := ARuler.RichEditControl.ActiveView;
end;

destructor TdxRulerPainterBase.Destroy;
begin
  FForeColorBrush.Free;
  inherited Destroy;
end;

procedure TdxRulerPainterBase.Draw(AGraphics: TdxGraphics);
begin
  DrawInPixels(AGraphics);
end;

class procedure TdxRulerPainterBase.DrawImage(AGraphics: TdxGraphics; AImage: TdxGPImage; X, Y: Integer);
begin
  AGraphics.Draw(AImage, TRect.CreateSize(X, Y, AImage.Width, AImage.Height));
end;

class procedure TdxRulerPainterBase.DrawImage(AGraphics: TdxGraphics; AImage: TdxGPImage; const ABounds: TRect);
begin
  AGraphics.Draw(AImage, ABounds);
end;

class function TdxRulerPainterBase.GenerateImage(const ABounds: TRect; AColor: TdxAlphaColor;
  const AGenerateImage: TdxGenerateImageDelegate): TdxGPImage;
var
  ASize: TSize;
  AImage: TdxSmartImage;
  AGpCanvas: TdxGPCanvas;
begin
  ASize := ABounds.Size;
  ASize.cx := Max(1, ASize.cx);
  ASize.cy := Max(1, ASize.cy);
  AImage := TdxSmartImage.CreateSize(ASize);
  AGpCanvas := AImage.CreateCanvas;
  try
    AGenerateImage(AGpCanvas, AImage.ClientRect, AColor);
  finally
    AGpCanvas.Free;
  end;
  Result := AImage;
end;

procedure TdxRulerPainterBase.BeginDraw(AGraphics: TdxGraphics);
begin
  FGraphics := AGraphics;
end;

procedure TdxRulerPainterBase.EndDraw;
begin
  FGraphics := nil;
end;

procedure TdxRulerPainterBase.DrawActiveAreas;
var
  I: Integer;
begin
  for I := 0 to Ruler.ViewInfo.DisplayActiveAreaCollection.Count - 1 do
    DrawActiveArea(Ruler.ViewInfo.DisplayActiveAreaCollection[I]);
end;

procedure TdxRulerPainterBase.DrawSpaceAreas;
var
  I: Integer;
begin
  for I := 0 to Ruler.ViewInfo.DisplaySpaceAreaCollection.Count - 1 do
    DrawSpaceArea(Ruler.ViewInfo.DisplaySpaceAreaCollection[I]);
end;

procedure TdxRulerPainterBase.DrawTickMarks;
var
  I: Integer;
begin
  for I := 0 to Ruler.ViewInfo.RulerTickmarks.Count - 1 do
    DrawTickMark(Ruler.ViewInfo.RulerTickmarks[I]);
end;

procedure TdxRulerPainterBase.DrawHotZones;
var
  I: Integer;
begin
  for I := 0 to Ruler.ViewInfo.HotZones.Count - 1 do
    DrawHotZone(Ruler.ViewInfo.HotZones[I]);
end;

procedure TdxRulerPainterBase.DrawBackground(AGraphics: TdxGraphics);
begin
  DrawControlBackground(AGraphics, Ruler.ClientRect);
end;

procedure TdxRulerPainterBase.Initialize;
begin
  FForeColorBrush := TdxGPBrush.Create;
  FForeColorBrush.Color := ForeColor;
end;

function TdxRulerPainterBase.GetForeColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.WindowText;
end;

function TdxRulerPainterBase.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Ruler.LookAndFeelPainter;
end;

function TdxRulerPainterBase.GetVerticalTextPaddingBottom: Integer;
begin
  Result := Ruler.PixelsToLayoutUnitsV(Ruler.ScaleFactor.Apply(2));
end;

function TdxRulerPainterBase.GetVerticalTextPaddingTop: Integer;
begin
  Result := Ruler.PixelsToLayoutUnitsV(Ruler.ScaleFactor.Apply(2));
end;

function TdxRulerPainterBase.GetPaddingTop: Integer;
begin
  Result := Ruler.PixelsToLayoutUnitsV(Ruler.ScaleFactor.Apply(5));
end;

function TdxRulerPainterBase.GetSkinInfo: TdxSkinInfo;
begin
  Result := Ruler.SkinInfo;
end;

function TdxRulerPainterBase.GetPaddingBottom: Integer;
begin
  Result := Ruler.PixelsToLayoutUnitsV(Ruler.ScaleFactor.Apply(5));
end;

{ TdxTickMarkPositionComparable }

constructor TdxTickMarkPositionComparable.Create(const AOrientation: TdxOrientation; APos: Integer);
begin
  FOrientation := AOrientation;
  FPos := APos;
end;

function TdxTickMarkPositionComparable.CompareTo(const ATickmark: TdxRulerTickmark): Integer;
begin
  if FPos < FOrientation.GetNearPrimaryCoordinate(ATickmark.Bounds) then
    Result := 1
  else
    if FPos > FOrientation.GetFarPrimaryCoordinate(ATickmark.Bounds) then
      Result := -1
    else
      Result := 0;
end;

{ TdxRulerAutoScroller }

procedure TdxRulerAutoScroller.PopulateHotZones;
begin
end;

{ TdxRulerMouseController }

constructor TdxRulerMouseController.Create(AControl: TdxRulerControlBase);
begin
  inherited Create(AControl.RichEditControl);
  FControl := AControl;
end;

function TdxRulerMouseController.CreateAutoScroller: TdxAutoScroller;
begin
  Result := TdxRulerAutoScroller.Create(Self);
end;

procedure TdxRulerMouseController.Initialize;
begin
end;

function TdxRulerMouseController.GetOrientation: TdxOrientation;
begin
  Result := Control.Orientation;
end;

procedure TdxRulerMouseController.CalculateAndSaveHitInfo(const E: TdxMouseEventArgs);
begin
end;

procedure TdxRulerMouseController.HandleClickTimerTick;
begin
end;

procedure TdxRulerMouseController.HandleMouseWheel(const E: TdxMouseEventArgs);
begin
end;

procedure TdxRulerMouseController.SetMouseCursor(ACursor: TCursor);
begin
  Control.Cursor := ACursor;
end;

procedure TdxRulerMouseController.Cancel;
var
  ADragState: TdxDragAndDropMouseHandlerState;
begin
  ADragState := Safe<TdxDragAndDropMouseHandlerState>.Cast(State);
  if ADragState <> nil then
    ADragState.Cancel;
end;

procedure TdxRulerMouseController.CaptureMouse;
begin
end;

procedure TdxRulerMouseController.ReleaseMouseCapture;
begin
end;

function TdxRulerMouseController.GetHotZoneBounds(AHotZone: TdxRulerHotZone): TRect;
begin
  Result := AHotZone.Bounds;
end;

procedure TdxRulerMouseController.SwitchToDefaultState;
var
  ANewState: TdxDefaultRulerMouseHandlerState;
begin
  ANewState := TdxDefaultRulerMouseHandlerState.Create(Self);
  SwitchStateCore(ANewState, cxNullPoint);
end;

function TdxRulerMouseController.GetSnappedPosition(APos: Integer): Integer;
begin
  if TdxKeyboardHelper.IsAltPressed then
    Exit(APos);
  Result := GetSnappedPositionCore(APos);
end;

function TdxRulerMouseController.GetSnappedPositionCore(APos: Integer): Integer;
var
  ATickmarks: TdxObjectList<TdxRulerTickmark>;
  AIndex: Integer;
  AComparer: TdxTickMarkPositionComparable;
begin
  ATickmarks := Control.ViewInfo.RulerTickmarks;
  AComparer := TdxTickMarkPositionComparable.Create(Orientation, APos);
  try
    if not TdxAlgorithms1<TdxRulerTickmark>.BinarySearch(ATickmarks, AComparer, AIndex) then
    begin
      if (AIndex >= ATickmarks.Count) or (AIndex = 0) then
        Exit(APos);
      AIndex := GetNearestIndex(APos, ATickmarks, AIndex);
    end;
  finally
    AComparer.Free;
  end;
  Result := Trunc(GetTickMiddlePoint(ATickmarks[AIndex].Bounds));
end;

function TdxRulerMouseController.GetNearestIndex(APos: Integer; ATickmarks: TdxList<TdxRulerTickmark>; AIndex: Integer): Integer;
var
  ANearInterval, AFarInterval: Single;
begin
  if AIndex > 0 then
  begin
    ANearInterval := APos - GetFarPrimaryCoordinate(ATickmarks[AIndex - 1].Bounds);
    AFarInterval := GetNearPrimaryCoordinate(ATickmarks[AIndex].Bounds) - APos;
    if ANearInterval <= AFarInterval then
      Dec(AIndex, 1);
  end;
  Result := AIndex;
end;

function TdxRulerMouseController.GetTickMiddlePoint(const ATickmarkBounds: TdxRectF): Single;
begin
  Result := GetNearPrimaryCoordinate(ATickmarkBounds) + ATickmarkBounds.Width / 2;
end;

function TdxRulerMouseController.GetPrimaryCoordinate(const APoint: TPoint): Integer;
begin
  Result := Orientation.GetPrimaryCoordinate(APoint);
end;

function TdxRulerMouseController.GetSecondaryCoordinate(const APoint: TPoint): Integer;
begin
  Result := Orientation.GetSecondaryCoordinate(APoint);
end;

function TdxRulerMouseController.CreatePoint(APrimary: Integer; ASecondary: Integer): TPoint;
begin
  Result := Orientation.CreatePoint(APrimary, ASecondary);
end;

function TdxRulerMouseController.GetNearPrimaryCoordinate(const ABounds: TdxRectF): Single;
begin
  Result := Orientation.GetNearPrimaryCoordinate(ABounds);
end;

function TdxRulerMouseController.GetFarPrimaryCoordinate(const ABounds: TdxRectF): Single;
begin
  Result := Orientation.GetFarPrimaryCoordinate(ABounds);
end;

function TdxRulerMouseController.GetPrimaryCoordinateExtent(const ABounds: TdxRectF): Single;
begin
  Result := Orientation.GetPrimaryCoordinateExtent(ABounds);
end;

function TdxRulerMouseController.GetNearPrimaryCoordinate(const ABounds: TRect): Integer;
begin
  Result := Orientation.GetNearPrimaryCoordinate(ABounds);
end;

function TdxRulerMouseController.GetFarPrimaryCoordinate(const ABounds: TRect): Integer;
begin
  Result := Orientation.GetFarPrimaryCoordinate(ABounds);
end;

function TdxRulerMouseController.SetNearPrimaryCoordinate(const ABounds: TdxRectF; AValue: Single): TdxRectF;
begin
  Result := Orientation.SetNearPrimaryCoordinate(ABounds, AValue);
end;

{ TdxRulerElementPainter }

constructor TdxRulerElementPainter.Create(ARuler: TdxHorizontalRulerControl);
begin
  FRuler := ARuler;
  FEnabledElementImages := TScaleImages.Create([doOwnsValues]);
  FDisabledElementImages := TScaleImages.Create([doOwnsValues]);
end;

procedure TdxRulerElementPainter.Initialize;
begin
end;

destructor TdxRulerElementPainter.Destroy;
begin
  FEnabledElementImages.Free;
  FDisabledElementImages.Free;
  inherited Destroy;
end;

function TdxRulerElementPainter.CanDrawHotZone(AHotZone: TdxRulerHotZone; out AImage: TdxGPImage): Boolean;
begin
  AImage := nil;
  Result := AHotZone.Visible;
  if Result then
  begin
    AImage := GetImage(AHotZone);
    Result := AImage <> nil;
  end;
end;

function TdxRulerElementPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := Ruler.ScaleFactor;
end;

function TdxRulerElementPainter.GetImage(AHotZone: TdxRulerHotZone): TdxGPImage;
var
  AScaleImages: TScaleImages;
  AImages: TImages;
  AScaleFactor: TSize;
begin
  if AHotZone.Enabled then
    AScaleImages := FEnabledElementImages
  else
    AScaleImages := FDisabledElementImages;
  AScaleFactor := TSize.Create(ScaleFactor.Denominator, ScaleFactor.Numerator);
  if not AScaleImages.TryGetValue(AScaleFactor, AImages) then
  begin
    AImages := TImages.Create([doOwnsValues]);
    AScaleImages.Add(AScaleFactor, AImages);
  end;
  if not AImages.TryGetValue(AHotZone.ClassType, Result) then
  begin
    Result := CreateImage(AHotZone);
    if Result <> nil then
      AImages.Add(AHotZone.ClassType, Result);
  end;
end;

function TdxRulerElementPainter.CreateImage(AHotZone: TdxRulerHotZone): TdxGPImage;
begin
  Result := nil;
end;

procedure TdxRulerElementPainter.DrawHotZone(AHotZone: TdxRulerHotZone;
  AGraphics: TdxGraphics; const ALocation: TPoint);
var
  AImage: TdxGPImage;
begin
  if not CanDrawHotZone(AHotZone, AImage) then
    Exit;
  TdxHorizontalRulerPainter.DrawImage(AGraphics, AImage, ALocation.X, ALocation.Y);
end;

procedure TdxRulerElementPainter.DrawHotZone(AHotZone: TdxRulerHotZone;
  AGraphics: TdxGraphics; const ABounds: TRect);
var
  AImage: TdxGPImage;
begin
  if not CanDrawHotZone(AHotZone, AImage) then
    Exit;
  TdxHorizontalRulerPainter.DrawImage(AGraphics, AImage, ABounds);
end;

function TdxRulerElementPainter.CalculateHotZoneSize(AHotZone: TdxHorizontalRulerHotZone): TSize;
var
  AImage: TdxGPImage;
begin
  AImage := GetImage(AHotZone);
  if AImage <> nil then
    Result := Ruler.PixelsToLayoutUnits(TSize.Create(AImage.Width, AImage.Height))
  else
    Result := Ruler.PixelsToLayoutUnits(ScaleFactor.Apply(TSize.Create(16, 16)));
end;

{ TdxColorBasedRulerElementPainter }

constructor TdxColorBasedRulerElementPainter.Create(ARuler: TdxHorizontalRulerControl; AElementColor: TdxAlphaColor);
begin
  inherited Create(ARuler);
  FEnabledElementColor := AElementColor;
  FDisabledElementColor := TdxAlphaColors.FromColor(dxGetLighterColor(TdxAlphaColors.ToColor(AElementColor), 33));
end;

function TdxColorBasedRulerElementPainter.CreateImage(AHotZone: TdxRulerHotZone): TdxGPImage;

  function GetResourceName: string;
  begin
    if AHotZone.ClassType = TdxLeftIndentHotZone then
      Result := 'RECRULERLEFTINDENT'
    else if AHotZone.ClassType = TdxLeftBottomHotZone then
      Result := 'RECRULERLEFTINDENTBOTTOM'
    else if AHotZone.ClassType = TdxRightIndentHotZone then
      Result := 'RECRULERRIGHTINDENT'
    else if AHotZone.ClassType = TdxFirstLineIndentHotZone then
      Result := 'RECRULERFIRSTLINEINDENT'
    else if AHotZone.ClassType = TdxTableLeftBorderHotZone then
      Result := 'RECRULERTABLECOLUMNBORDER'
    else if AHotZone.ClassType = TdxTableHotZone then
      Result := 'RECRULERTABLECOLUMNBORDER'
    else
      Exit('');
    if not AHotZone.Enabled then
      Result := Result + 'DISABLED';
  end;

var
  AColor: TdxAlphaColor;
  AResourceName: string;
  ASize: TSize;
begin
  if AHotZone.Enabled then
    AColor := EnabledElementColor
  else
    AColor := DisabledElementColor;
  if AHotZone.ClassType = TdxLeftTabHotZone then
    Result := GenerateTabImage(DrawLeftTabImage, AColor)
  else if AHotZone.ClassType = TdxRightTabHotZone then
    Result := GenerateTabImage(DrawRightTabImage, AColor)
  else if AHotZone.ClassType = TdxCenterTabHotZone then
    Result := GenerateTabImage(DrawCenterTabImage, AColor)
  else if AHotZone.ClassType = TdxDecimalTabHotZone then
    Result := GenerateTabImage(DrawDecimalTabImage, AColor)
  else
  begin
    AResourceName := GetResourceName;
    if AResourceName <> '' then
    begin
      Result := CreateImageFromResource(HInstance, AResourceName);
      if (Result <> nil) and ScaleFactor.Assigned then
      begin
        ASize := ScaleFactor.Apply(Result.Size);
        Result.Resize(ASize);
      end;
    end
    else
      Result := nil;
  end;
end;

function TdxColorBasedRulerElementPainter.GenerateTabImage(const AGenerateImage: TdxGenerateImageDelegate; AColor: TdxAlphaColor): TdxGPImage;
var
  AHeight: Integer;
begin
  AHeight := Max(ScaleFactor.Apply(6), Ruler.LayoutUnitsToPixelsH(Round(2 * Ruler.ViewInfo.ClientBounds.Height / 5)));
  Result := TdxRulerPainterBase.GenerateImage(TRect.CreateSize(0, 0, 2 * AHeight, AHeight), AColor, AGenerateImage);
end;

procedure TdxColorBasedRulerElementPainter.DrawTabImageBottom(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
var
  ARect: TRect;
begin
  ARect := ABounds;
  ARect.Top := ARect.Bottom - ScaleFactor.Apply(2);
  ARect.Height := ScaleFactor.Apply(2);
  AGpCanvas.FillRectangle(ARect, AColor);
end;

procedure TdxColorBasedRulerElementPainter.DrawLeftTabImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
var
  ARect: TRect;
begin
  ARect := ABounds;
  ARect.Width := ScaleFactor.Apply(2);
  ARect.Offset(ScaleFactor.Apply(2), 0);
  AGpCanvas.FillRectangle(ARect, AColor);
  ARect := ABounds;
  ARect.Width := ARect.Width - ABounds.Height;
  ARect.Offset(ScaleFactor.Apply(2), 0);
  DrawTabImageBottom(AGpCanvas, ARect, AColor);
end;

procedure TdxColorBasedRulerElementPainter.DrawRightTabImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
var
  ARect: TRect;
begin
  ARect := ABounds;
  ARect.X := ARect.Right - ScaleFactor.Apply(2);
  ARect.Width := ScaleFactor.Apply(2);
  ARect.Offset(ScaleFactor.Apply(-1), 0);
  AGpCanvas.FillRectangle(ARect, AColor);
  ARect := ABounds;
  ARect.X := ABounds.X + ABounds.Height;
  ARect.Width := ABounds.Width - ABounds.Height;
  ARect.Offset(ScaleFactor.Apply(-1), 0);
  DrawTabImageBottom(AGpCanvas, ARect, AColor);
end;

procedure TdxColorBasedRulerElementPainter.DrawCenterTabImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
var
  ARect: TRect;
  AWidth: Integer;
begin
  ARect := ABounds;
  ARect.X := ARect.X + ARect.Width div 2 - ScaleFactor.Apply(1);
  ARect.Width := ScaleFactor.Apply(2);
  AGpCanvas.FillRectangle(ARect, AColor);
  AWidth := 2 * ABounds.Height - ScaleFactor.Apply(2);
  ARect := ABounds;
  ARect.X := ABounds.X + (ABounds.Width - AWidth) div 2;
  ARect.Width := AWidth;
  DrawTabImageBottom(AGpCanvas, ARect, AColor);
end;

procedure TdxColorBasedRulerElementPainter.DrawDecimalTabImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
var
  ARect: TRect;
begin
  DrawCenterTabImage(AGpCanvas, ABounds, AColor);
  ARect := ABounds;
  ARect.X := ARect.X + ARect.Width div 2 - ScaleFactor.Apply(1);
  ARect.X := ARect.X + ScaleFactor.Apply(3);
  ARect.Y := ARect.Bottom - ScaleFactor.Apply(2);
  ARect.Y := ARect.Y - ScaleFactor.Apply(3);
  ARect.Height := ScaleFactor.Apply(2);
  ARect.Width := ScaleFactor.Apply(2);
  AGpCanvas.FillRectangle(ARect, AColor);
end;

{ TdxHorizontalRulerPainter }

constructor TdxHorizontalRulerPainter.Create(ARuler: TdxHorizontalRulerControl);
begin
  inherited Create(ARuler);
  FRuler := ARuler;
end;

destructor TdxHorizontalRulerPainter.Destroy;
begin
  FreeAndNil(FElementPainter);
  FreeAndNil(FHalfTickImage);
  FreeAndNil(FQuarterTickImage);
  FreeAndNil(FDefaultTabImage);
  inherited Destroy;
end;

procedure TdxHorizontalRulerPainter.Initialize;
begin
  inherited Initialize;
  FElementPainter := CreateElementPainter;
  FElementPainter.Initialize;
  FHalfTickImage := GenerateHalfTickImage;
  FQuarterTickImage := GenerateQuarterTickImage;
  FDefaultTabImage := GenerateDefaultTabImage;
end;

function TdxHorizontalRulerPainter.CalculateTotalRulerSize(ATextSize: Integer): Integer;
begin
  Result := ATextSize + PaddingTop + PaddingBottom + VerticalTextPaddingBottom + VerticalTextPaddingTop;
end;

function TdxHorizontalRulerPainter.GenerateQuarterTickImage: TdxGPImage;
begin
  Result := GenerateTickImageCore(Ruler.ViewInfo.DisplayQuarterTickBounds);
end;

function TdxHorizontalRulerPainter.GenerateHalfTickImage: TdxGPImage;
begin
  Result := GenerateTickImageCore(Ruler.ViewInfo.DisplayHalfTickBounds);
end;

function TdxHorizontalRulerPainter.GenerateDefaultTabImage: TdxGPImage;
begin
  Result := TdxRulerPainterBase.GenerateImage(Ruler.ViewInfo.DisplayDefaultTabBounds, DefaultTabColor, DrawTickmarkImage);
end;

function TdxHorizontalRulerPainter.GenerateTickImageCore(const ABounds: TRect): TdxGPImage;
begin
  Result := TdxRulerPainterBase.GenerateImage(ABounds, ForeColor, DrawTickmarkImage);
end;

function TdxHorizontalRulerPainter.GetDefaultTabColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerDefaultTabColor);
end;

function TdxHorizontalRulerPainter.GetTabTypeToggleActiveAreaSize: TSize;
var
  AWidth: Integer;
begin
  AWidth := FRuler.ScaleFactor.Apply(14);
  Result.Init(AWidth, AWidth);
end;

procedure TdxHorizontalRulerPainter.DrawTickmarkImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
begin
  AGpCanvas.FillRectangle(ABounds, AColor);
end;

procedure TdxHorizontalRulerPainter.DrawTabTypeToggle(AGraphics: TdxGraphics);
begin
  DrawTabTypeToggleBackground(AGraphics, Ruler.ViewInfo.TabTypeToggleBackground.Bounds);
  DrawTabTypeToggleActiveArea(AGraphics, Ruler.ViewInfo.TabTypeToggleHotZone.DisplayBounds);
  Ruler.Painter.ElementPainter.DrawHotZone(Ruler.ViewInfo.TabTypeToggleHotZone.HotZone, AGraphics, Ruler.ViewInfo.TabTypeToggleHotZone.HotZoneBounds.Location);
end;

procedure TdxHorizontalRulerPainter.Draw(AGraphics: TdxGraphics);
begin
  DrawInPixels(AGraphics);
end;

procedure TdxHorizontalRulerPainter.DrawInPixels(AGraphics: TdxGraphics);
var
  AControl: TdxCustomRichEditControl;
  ACaretPosition: TdxCaretPosition;
  AViewBoundsLeft, APhysicalLeftInvisibleWidth: Integer;
begin
  AControl := Ruler.RichEditControl;
  ACaretPosition := AControl.ActiveView.CaretPosition;
  ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column);
  if ACaretPosition.PageViewInfo = nil then
    Exit;
  try
    APhysicalLeftInvisibleWidth := View.HorizontalScrollController.GetPhysicalLeftInvisibleWidth;
    AViewBoundsLeft := Ruler.LayoutUnitsToPixelsH(AControl.ViewBounds.Left - APhysicalLeftInvisibleWidth);
    AGraphics.TranslateWorldTransform(AViewBoundsLeft, 0);
    BeginDraw(AGraphics);
    DrawRuler;
    EndDraw;
  finally
    AGraphics.ResetTransform;
  end;
end;

procedure TdxHorizontalRulerPainter.DrawRuler;
begin
  DrawRulerAreaBackground(Ruler.ViewInfo.DisplayClientBounds);
  DrawActiveAreas;
  DrawTickMarks;
  DrawSpaceAreas;
  DrawDefaultTabMarks;
  DrawHotZones;
end;

procedure TdxHorizontalRulerPainter.DrawDefaultTabMarks;
var
  I: Integer;
begin
  for I := 0 to Ruler.ViewInfo.DisplayDefaultTabsCollection.Count - 1 do
    DrawDefaultTabMark(Ruler.ViewInfo.DisplayDefaultTabsCollection[I]);
end;

procedure TdxHorizontalRulerPainter.DrawHotZone(AHotZone: TdxRulerHotZone);
begin
  if AHotZone.Visible then
    FElementPainter.DrawHotZone(AHotZone, Graphics, AHotZone.DisplayBounds);
end;

procedure TdxHorizontalRulerPainter.DrawTickMark(ATickMark: TdxRulerTickmark);
begin
  ATickMark.Draw(Self);
end;

procedure TdxHorizontalRulerPainter.DrawTickmarkNumber(ATickmark: TdxRulerTickmarkNumber);
begin
  Graphics.DrawString(ATickmark.Number, Ruler.TickMarkFont,
    ForeColorBrush, ATickmark.DisplayBounds.Location, TdxGPStringFormat.GenericTypographic);
end;

procedure TdxHorizontalRulerPainter.DrawTickmarkHalf(ATickmark: TdxRulerTickmarkHalf);
var
  ABounds: TdxRectF;
begin
  ABounds := ATickmark.DisplayBounds;
  DrawImage(Graphics, FHalfTickImage, Trunc(ABounds.Left), Trunc(ABounds.Top));
end;

procedure TdxHorizontalRulerPainter.DrawTickmarkQuarter(ATickmark: TdxRulerTickmarkQuarter);
var
  ABounds: TdxRectF;
begin
  ABounds := ATickmark.DisplayBounds;
  DrawImage(Graphics, FQuarterTickImage, Trunc(ABounds.Left), Trunc(ABounds.Top));
end;

procedure TdxHorizontalRulerPainter.DrawDefaultTabMark(const ABounds: TRect);
begin
  DrawImage(Graphics, FDefaultTabImage, Trunc(ABounds.Left), Trunc(ABounds.Top));
end;

{ TdxHorizontalRulerColorBasedPainter }

procedure TdxHorizontalRulerColorBasedPainter.DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  AGraphics.FillRectangle(ABounds, ControlBackgroundColor);
end;

procedure TdxHorizontalRulerColorBasedPainter.DrawTabTypeToggleBackground(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  AGraphics.FillRectangle(ABounds, ControlBackgroundColor);
end;

procedure TdxHorizontalRulerColorBasedPainter.DrawTabTypeToggleActiveArea(AGraphics: TdxGraphics; const ABounds: TRect);
var
  R: TRect;
begin
  R := ABounds;
  R.Inflate(1, 1);
  AGraphics.Rectangle(R, TabTypeToggleBorderColor, ControlBackgroundColor);
end;

procedure TdxHorizontalRulerColorBasedPainter.DrawRulerAreaBackground(const ABounds: TRect);
begin
  Graphics.FillRectangle(ABounds, RulerAreaBackgroundColor);
end;

procedure TdxHorizontalRulerColorBasedPainter.DrawActiveArea(const ABounds: TdxRectF);
begin
  Graphics.FillRectangle(TRect.Round(ABounds), ActiveAreaColor);
end;

procedure TdxHorizontalRulerColorBasedPainter.DrawSpaceArea(const ABounds: TdxRectF);
begin
  Graphics.FillRectangle(TRect.Round(ABounds), ControlBackgroundColor);
end;

function TdxHorizontalRulerColorBasedPainter.CreateElementPainter: TdxRulerElementPainter;
begin
  Result := TdxColorBasedRulerElementPainter.Create(Ruler, ForeColor);
end;

function TdxHorizontalRulerColorBasedPainter.GetActiveAreaColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerActiveAreaColor);
end;

function TdxHorizontalRulerColorBasedPainter.GetControlBackgroundColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerControlColor);
end;

function TdxHorizontalRulerColorBasedPainter.GetRulerAreaBackgroundColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerInactiveAreaColor);
end;

function TdxHorizontalRulerColorBasedPainter.GetTabTypeToggleBorderColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerTabTypeToggleBorderColor);
end;

{ TdxRulerElementSkinPainter }

constructor TdxRulerElementSkinPainter.Create(ARuler: TdxHorizontalRulerControl;
  ASkinInfo: TdxSkinInfo);
begin
  inherited Create(ARuler);
  FSkinInfo := ASkinInfo;
end;

function TdxRulerElementSkinPainter.GetSkinElementImage(AElement: TdxSkinElement; AImageIndex: Integer; AState: TdxSkinElementState): TdxGPImage;
begin
  if AElement = nil then
    Result := nil
  else
    Result := AElement.GetImage(AImageIndex, cxNullSize, ScaleFactor, AState);
end;

function TdxRulerElementSkinPainter.CreateImage(AHotZone: TdxRulerHotZone): TdxGPImage;
const
  ASkinElementStateMap: array[Boolean] of TdxSkinElementState = (esDisabled, esNormal);

  function GetSkinElement: TdxSkinElement;
  begin
    if AHotZone.ClassType = TdxLeftTabHotZone then
      Result := SkinInfo.RichEditRulerTab
    else if AHotZone.ClassType = TdxRightTabHotZone then
      Result := SkinInfo.RichEditRulerTab
    else if AHotZone.ClassType = TdxCenterTabHotZone then
      Result := SkinInfo.RichEditRulerTab
    else if AHotZone.ClassType = TdxDecimalTabHotZone then
      Result := SkinInfo.RichEditRulerTab
    else if AHotZone.ClassType = TdxLeftIndentHotZone then
      Result := SkinInfo.RichEditRulerIndent
    else if AHotZone.ClassType = TdxRightIndentHotZone then
      Result := SkinInfo.RichEditRulerIndent
    else if AHotZone.ClassType = TdxFirstLineIndentHotZone then
      Result := SkinInfo.RichEditRulerIndent
    else if AHotZone.ClassType = TdxLeftBottomHotZone then
      Result := SkinInfo.RichEditRulerIndentBottom
    else if AHotZone.ClassType = TdxRightColumnResizerHotZone then
      Result := SkinInfo.RichEditRulerColumnResizer
    else if AHotZone.ClassType = TdxLeftColumnResizerHotZone then
      Result := SkinInfo.RichEditRulerColumnResizer
    else if AHotZone.ClassType = TdxMiddleColumnResizerHotZone then
      Result := SkinInfo.RichEditRulerColumnResizer
    else if AHotZone.ClassType = TdxTableHotZone then
      Result := SkinInfo.RichEditRulerColumnResizer
    else if AHotZone.ClassType = TdxTableLeftBorderHotZone then
      Result := SkinInfo.RichEditRulerColumnResizer
    else
      Result := nil;
  end;

  function GetImageIndex: Integer;
  begin
    if AHotZone.ClassType = TdxLeftTabHotZone then
      Result := 1
    else if AHotZone.ClassType = TdxRightTabHotZone then
      Result := 0
    else if AHotZone.ClassType = TdxCenterTabHotZone then
      Result := 3
    else if AHotZone.ClassType = TdxDecimalTabHotZone then
      Result := 4
    else if AHotZone.ClassType = TdxLeftIndentHotZone then
      Result := 1
    else if AHotZone.ClassType = TdxRightIndentHotZone then
      Result := 1
    else if AHotZone.ClassType = TdxFirstLineIndentHotZone then
      Result := 0
    else if AHotZone.ClassType = TdxLeftBottomHotZone then
      Result := 0
    else if AHotZone.ClassType = TdxRightColumnResizerHotZone then
      Result := 1
    else if AHotZone.ClassType = TdxLeftColumnResizerHotZone then
      Result := 1
    else
      Result := 0
  end;

begin
  Result := GetSkinElementImage(GetSkinElement, GetImageIndex, ASkinElementStateMap[AHotZone.Enabled]);
end;

{ TdxHorizontalRulerSkinPainter }

function TdxHorizontalRulerSkinPainter.GetForeColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerTextColor);
end;

function TdxHorizontalRulerSkinPainter.GetPaddingTop: Integer;
begin
  Result := Ruler.ScaleFactor.Apply(SkinInfo.RichEditRulerBackgroundHorz.ContentOffset.Top);
  Result := Ruler.PixelsToLayoutUnitsV(Result);
end;

function TdxHorizontalRulerSkinPainter.GetPaddingBottom: Integer;
begin
  Result := Ruler.ScaleFactor.Apply(SkinInfo.RichEditRulerBackgroundHorz.ContentOffset.Bottom);
  Result := Ruler.PixelsToLayoutUnitsV(Result);
end;

function TdxHorizontalRulerSkinPainter.GetVerticalTextPaddingBottom: Integer;
begin
  Result := Ruler.ScaleFactor.Apply(SkinInfo.RichEditRulerSection.ContentOffset.Bottom);
  Result := Ruler.PixelsToLayoutUnitsV(Result + 1);
end;

function TdxHorizontalRulerSkinPainter.GetVerticalTextPaddingTop: Integer;
begin
  Result := Ruler.ScaleFactor.Apply(SkinInfo.RichEditRulerSection.ContentOffset.Top);
  Result := Ruler.PixelsToLayoutUnitsV(Result + 1);
end;

function TdxHorizontalRulerSkinPainter.CalculateTotalRulerSize(ATextSize: Integer): Integer;
var
  AAreaSize, AActiveAreaSize: TSize;
  ATextAreaHeight: Integer;
begin
  AAreaSize := SkinInfo.RichEditRulerRightMargin.CalculateMinSize;
  AActiveAreaSize := SkinInfo.RichEditRulerSection.CalculateMinSize;
  ATextAreaHeight := Max(ATextSize, Ruler.PixelsToLayoutUnitsV(Max(AAreaSize.Height, AActiveAreaSize.Height)));
  Result := ATextAreaHeight + PaddingTop + PaddingBottom + VerticalTextPaddingBottom + VerticalTextPaddingTop;
end;

procedure TdxHorizontalRulerSkinPainter.DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  SkinInfo.RichEditRulerBackgroundHorz.DrawEx(AGraphics, ABounds);
end;

procedure TdxHorizontalRulerSkinPainter.DrawTabTypeToggleBackground(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  SkinInfo.RichEditCornerPanel.DrawEx(AGraphics, ABounds);
end;

procedure TdxHorizontalRulerSkinPainter.DrawTabTypeToggleActiveArea(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  SkinInfo.RichEditRulerTabTypeBackground.DrawEx(AGraphics, ABounds);
end;

procedure TdxHorizontalRulerSkinPainter.DrawRulerAreaBackground(const ABounds: TRect);
begin
  SkinInfo.RichEditRulerRightMargin.DrawEx(Graphics, ABounds);
end;

procedure TdxHorizontalRulerSkinPainter.DrawActiveArea(const ABounds: TdxRectF);
begin
  SkinInfo.RichEditRulerSection.DrawEx(Graphics, TRect.Round(ABounds));
end;

procedure TdxHorizontalRulerSkinPainter.DrawSpaceArea(const ABounds: TdxRectF);
begin
  SkinInfo.RichEditRulerRightMargin.DrawEx(Graphics, TRect.Round(ABounds));
end;

function TdxHorizontalRulerSkinPainter.CreateElementPainter: TdxRulerElementPainter;
begin
  Result := TdxRulerElementSkinPainter.Create(Ruler, SkinInfo);
end;

{ TdxVerticalRulerPainter }

constructor TdxVerticalRulerPainter.Create(ARuler: TdxVerticalRulerControl);
begin
  inherited Create(ARuler);
  FRuler := ARuler;
end;

destructor TdxVerticalRulerPainter.Destroy;
begin
  FQuarterTickImage.Free;
  FHalfTickImage.Free;
  inherited Destroy;
end;

procedure TdxVerticalRulerPainter.Initialize;
begin
  inherited Initialize;
  FHalfTickImage := GenerateHalfTickImage;
  FQuarterTickImage := GenerateQuarterTickImage;
end;

function TdxVerticalRulerPainter.CalculateTotalRulerSize(ATextSize: Integer): Integer;
begin
  Result := ATextSize + PaddingTop + PaddingBottom + VerticalTextPaddingBottom + VerticalTextPaddingTop;
end;

function TdxVerticalRulerPainter.GenerateQuarterTickImage: TdxGPImage;
begin
  Result := GenerateTickImageCore(Ruler.ViewInfo.DisplayQuarterTickBounds);
end;

function TdxVerticalRulerPainter.GenerateHalfTickImage: TdxGPImage;
begin
  Result := GenerateTickImageCore(FRuler.ViewInfo.DisplayHalfTickBounds);
end;

function TdxVerticalRulerPainter.GenerateTickImageCore(const ABounds: TRect): TdxGPImage;
begin
  Result := GenerateImage(ABounds, ForeColor, DrawTickmarkImage);
end;

procedure TdxVerticalRulerPainter.DrawTickmarkImage(AGpCanvas: TdxGPCanvas; const ABounds: TRect; AColor: TdxAlphaColor);
begin
  AGpCanvas.FillRectangle(ABounds, AColor);
end;

procedure TdxVerticalRulerPainter.DrawInPixels(AGraphics: TdxGraphics);
var
  AControl: TdxCustomRichEditControl;
  ACaretPosition: TdxCaretPosition;
  AViewBoundsTop: Integer;
begin
  AControl := FRuler.RichEditControl;
  ACaretPosition := AControl.ActiveView.CaretPosition;
  ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column);
  if ACaretPosition.PageViewInfo = nil then
    Exit;
  AViewBoundsTop := FRuler.LayoutUnitsToPixelsV(AControl.ViewBounds.Top + ACaretPosition.PageViewInfo.ClientBounds.Top);
  AGraphics.TranslateWorldTransform(0, AViewBoundsTop);
  try
    BeginDraw(AGraphics);
    DrawRuler;
    EndDraw;
  finally
    AGraphics.ResetTransform;
  end;
end;

procedure TdxVerticalRulerPainter.DrawRuler;
begin
  DrawRulerAreaBackground(FRuler.ViewInfo.DisplayClientBounds);
  DrawActiveAreas;
  DrawTickMarks;
  DrawSpaceAreas;
  DrawHotZones;
end;

procedure TdxVerticalRulerPainter.DrawTickMark(ATickMark: TdxRulerTickmark);
begin
  ATickMark.Draw(Self);
end;

procedure TdxVerticalRulerPainter.DrawTickmarkNumber(ATickmark: TdxRulerTickmarkNumber);
begin
  Graphics.DrawVString(ATickmark.Number, Ruler.TickMarkFont,
    ForeColorBrush, TRect.Round(ATickmark.DisplayBounds), TdxGPStringFormat.GenericTypographic, 270);
end;

procedure TdxVerticalRulerPainter.DrawTickmarkHalf(ATickmark: TdxRulerTickmarkHalf);
var
  ABounds: TdxRectF;
begin
  ABounds := ATickmark.DisplayBounds;
  DrawImage(Graphics, FHalfTickImage, Trunc(ABounds.Left), Trunc(ABounds.Top));
end;

procedure TdxVerticalRulerPainter.DrawTickmarkQuarter(ATickmark: TdxRulerTickmarkQuarter);
var
  ABounds: TdxRectF;
begin
  ABounds := ATickmark.DisplayBounds;
  DrawImage(Graphics, FQuarterTickImage, Trunc(ABounds.Left), Trunc(ABounds.Top));
end;

{ TdxVerticalRulerColorBasedPainter }

function TdxVerticalRulerColorBasedPainter.GetActiveAreaColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerActiveAreaColor);
end;

function TdxVerticalRulerColorBasedPainter.GetControlBackgroundColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerControlColor);
end;

function TdxVerticalRulerColorBasedPainter.GetRulerAreaBackgroundColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerInactiveAreaColor);
end;

function TdxVerticalRulerColorBasedPainter.GetTableRowAreaColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeelPainter.RichEditRulerInactiveAreaColor);
end;

procedure TdxVerticalRulerColorBasedPainter.DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  AGraphics.FillRectangle(ABounds, ControlBackgroundColor);
end;

procedure TdxVerticalRulerColorBasedPainter.DrawRulerAreaBackground(const ABounds: TRect);
begin
  Graphics.FillRectangle(ABounds, RulerAreaBackgroundColor);
end;

procedure TdxVerticalRulerColorBasedPainter.DrawActiveArea(const ABounds: TdxRectF);
begin
  Graphics.FillRectangle(TRect.Round(ABounds), ActiveAreaColor);
end;

procedure TdxVerticalRulerColorBasedPainter.DrawSpaceArea(const ABounds: TdxRectF);
begin
  Graphics.FillRectangle(TRect.Round(ABounds), ControlBackgroundColor);
end;

procedure TdxVerticalRulerColorBasedPainter.DrawHotZone(AHotZone: TdxRulerHotZone);
begin
  if AHotZone.ClassType = TdxVerticalTableHotZone then
    Graphics.FillRectangle(AHotZone.DisplayBounds, TableRowAreaColor);
end;

{ TdxVerticalRulerSkinPainter }

function TdxVerticalRulerSkinPainter.GetForeColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(SkinInfo.RichEditRulerTextColor.Value);
end;

function TdxVerticalRulerSkinPainter.GetPaddingTop: Integer;
begin
  Result := Ruler.ScaleFactor.Apply(SkinInfo.RichEditRulerBackgroundVert.ContentOffset.Top);
  Result := Ruler.PixelsToLayoutUnitsV(Result);
end;

function TdxVerticalRulerSkinPainter.GetPaddingBottom: Integer;
begin
  Result := Ruler.ScaleFactor.Apply(SkinInfo.RichEditRulerBackgroundVert.ContentOffset.Bottom);
  Result := Ruler.PixelsToLayoutUnitsV(Result);
end;

function TdxVerticalRulerSkinPainter.GetVerticalTextPaddingBottom: Integer;
begin
  Result := Ruler.ScaleFactor.Apply(SkinInfo.RichEditRulerSection.ContentOffset.Bottom);
  Result := Ruler.PixelsToLayoutUnitsV(Result + 1);
end;

function TdxVerticalRulerSkinPainter.GetVerticalTextPaddingTop: Integer;
begin
  Result := Ruler.ScaleFactor.Apply(SkinInfo.RichEditRulerSection.ContentOffset.Top);
  Result := Ruler.PixelsToLayoutUnitsV(Result + 1);
end;

function TdxVerticalRulerSkinPainter.CalculateTotalRulerSize(ATextSize: Integer): Integer;
var
  AAreaSize, AActiveAreaSize: TSize;
  ATextAreaHeight: Integer;
begin
  AAreaSize := SkinInfo.RichEditRulerRightMargin.CalculateMinSize;
  AActiveAreaSize := SkinInfo.RichEditRulerSection.CalculateMinSize;
  ATextAreaHeight := Max(ATextSize, Ruler.PixelsToLayoutUnitsV(Math.Max(AAreaSize.Height, AActiveAreaSize.Height)));
  Result := ATextAreaHeight + PaddingTop + PaddingBottom + VerticalTextPaddingBottom + VerticalTextPaddingTop;
end;

procedure TdxVerticalRulerSkinPainter.DrawControlBackground(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  SkinInfo.RichEditRulerBackgroundVert.DrawEx(AGraphics, ABounds);
end;

procedure TdxVerticalRulerSkinPainter.DrawRulerAreaBackground(const ABounds: TRect);
begin
  SkinInfo.RichEditRulerRightMargin.DrawEx(Graphics, ABounds);
end;

procedure TdxVerticalRulerSkinPainter.DrawActiveArea(const ABounds: TdxRectF);
begin
  SkinInfo.RichEditRulerSection.DrawEx(Graphics, TRect.Round(ABounds));
end;

procedure TdxVerticalRulerSkinPainter.DrawSpaceArea(const ABounds: TdxRectF);
begin
  SkinInfo.RichEditRulerRightMargin.DrawEx(Graphics, TRect.Round(ABounds));
end;

procedure TdxVerticalRulerSkinPainter.DrawHotZone(AHotZone: TdxRulerHotZone);
begin
  if AHotZone.ClassType = TdxVerticalTableHotZone then
    SkinInfo.RichEditRulerRightMargin.DrawEx(Graphics, AHotZone.DisplayBounds);
end;

{ TdxRulerHotZone }

constructor TdxRulerHotZone.Create(const AControl: TdxRulerControlBase);
begin
  inherited Create;
  FControl := AControl;
  FEnabled := True;
  FVisible := True;
end;

procedure TdxRulerHotZone.SetBounds(const AValue: TRect);
begin
  if AValue.IsEqual(FBounds) then
    Exit;
  FBounds := AValue;
  FDisplayBounds := RulerControl.LayoutUnitsToPixels(FBounds);
end;

function TdxRulerHotZone.GetCursor: TCursor;
begin
  Result := TdxRichEditCursors.Default;
end;

function TdxRulerHotZone.GetRichEditControl: IdxRichEditControl;
begin
  Result := RulerControl.RichEditControl;
end;

function TdxRulerHotZone.GetDocumentModel: TdxDocumentModel;
begin
  Result := RulerControl.DocumentModel;
end;

function TdxRulerHotZone.GetRulerViewInfo: TdxRulerViewInfoBase;
begin
  Result := RulerControl.ViewInfo;
end;

function TdxRulerHotZone.GetRulerClientBounds: TRect;
begin
  Result := RulerViewInfo.ClientBounds;
end;

function TdxRulerHotZone.GetZoomFactor: Single;
begin
  Result := RulerControl.ZoomFactor;
end;

function TdxRulerHotZone.Clone: TdxRulerHotZone;
begin
  if Self = nil then
    Exit(nil);
  Result := CreateEmptyClone;
  Result.Bounds := Bounds;
end;

procedure TdxRulerHotZone.AddFakeHotZone;
var
  AHotZone: TdxRulerHotZone;
  AHotZones: TdxList<TdxRulerHotZone>;
  I: Integer;
begin
  AHotZone := Clone;
  AHotZone.Enabled := False;
  AHotZones := RulerViewInfo.HotZones;
  for I := AHotZones.Count - 1 downto 0 do
  begin
    if (AHotZone.ClassType = AHotZones[I].ClassType) and not AHotZones[I].Enabled then
      AHotZones.Delete(I);
  end;
  AHotZones.Insert(0, AHotZone);
end;

function TdxRulerHotZone.CanActivate(const Args: TdxMouseEventArgs): Boolean;
begin
  Result := Args.Buttons = [mbLeft];
end;

function TdxRulerHotZone.CanEdit: Boolean;
begin
  Result := True;
end;

procedure TdxRulerHotZone.Activate(AHandler: TdxRulerMouseController; const AMousePosition: TPoint);
var
  AState: TdxDragAndDropMouseHandlerState;
  AHelperState: TdxBeginDragHotZoneMouseDragHelperState;
begin
  AState := TdxDragAndDropMouseHandlerState.Create(AHandler, Self, AMousePosition);
  AHelperState := TdxBeginDragHotZoneMouseDragHelperState.Create(AHandler, AState, AMousePosition, Self);
  AHandler.SwitchStateCore(AHelperState, AMousePosition);
end;

{ TdxRulerTickmark }

constructor TdxRulerTickmark.Create(const AControl: TdxRulerControlBase; const ABounds: TdxRectF);
begin
  inherited Create;
  FBounds := ABounds;
  FDisplayBounds.Assign(AControl.LayoutUnitsToPixels(TRect.Round(ABounds)));
  FVisible := True;
end;

{ TdxRulerTickmarkNumber }

constructor TdxRulerTickmarkNumber.Create(const AControl: TdxRulerControlBase; const ABounds: TdxRectF; const ANumber: string);
begin
  inherited Create(AControl, ABounds);
  FNumber := ANumber;
end;

function TdxRulerTickmarkNumber.GetRulerTickmarkType: TdxRulerTickmarkType;
begin
  Result := TdxRulerTickmarkType.Tick;
end;

procedure TdxRulerTickmarkNumber.Draw(APainter: TdxHorizontalRulerPainter);
begin
  APainter.DrawTickmarkNumber(Self);
end;

procedure TdxRulerTickmarkNumber.Draw(APainter: TdxVerticalRulerPainter);
begin
  APainter.DrawTickmarkNumber(Self);
end;

{ TdxRulerTickmarkHalf }

function TdxRulerTickmarkHalf.GetRulerTickmarkType: TdxRulerTickmarkType;
begin
  Result := TdxRulerTickmarkType.HalfTick;
end;

procedure TdxRulerTickmarkHalf.Draw(APainter: TdxHorizontalRulerPainter);
begin
  APainter.DrawTickmarkHalf(Self);
end;

procedure TdxRulerTickmarkHalf.Draw(APainter: TdxVerticalRulerPainter);
begin
  APainter.DrawTickmarkHalf(Self);
end;

{ TdxRulerTickmarkQuarter }

function TdxRulerTickmarkQuarter.GetRulerTickmarkType: TdxRulerTickmarkType;
begin
  Result := TdxRulerTickmarkType.QuarterTick;
end;

procedure TdxRulerTickmarkQuarter.Draw(APainter: TdxHorizontalRulerPainter);
begin
  APainter.DrawTickmarkQuarter(Self);
end;

procedure TdxRulerTickmarkQuarter.Draw(APainter: TdxVerticalRulerPainter);
begin
  APainter.DrawTickmarkQuarter(Self);
end;

{ TdxHorizontalRulerHotZone }

constructor TdxHorizontalRulerHotZone.Create(const AControl: TdxHorizontalRulerControl);
begin
  inherited Create(AControl);
end;

function TdxHorizontalRulerHotZone.GetHorizontalRulerControl: TdxHorizontalRulerControl;
begin
  Result := TdxHorizontalRulerControl(RulerControl);
end;

function TdxHorizontalRulerHotZone.GetHorizontalRulerViewInfo: TdxHorizontalRulerViewInfo;
begin
  Result := HorizontalRulerControl.ViewInfo;
end;

function TdxHorizontalRulerHotZone.GetDocumentLayoutDistanceToCurrentAreaStart(AMouseX: Integer): Integer;
begin
  Result := Trunc((AMouseX - RulerViewInfo.ActiveAreaCollection[RulerViewInfo.CurrentActiveAreaIndex].Left) / ZoomFactor);
end;

function TdxHorizontalRulerHotZone.GetDocumentLayoutDistanceToCellLeft(AMouseX: Integer): Integer;
var
  AHorizontalRulerViewInfo: TdxHorizontalRulerViewInfo;
begin
  if RulerViewInfo is TdxHorizontalRulerViewInfo then
     AHorizontalRulerViewInfo := TdxHorizontalRulerViewInfo(RulerViewInfo)
  else
     AHorizontalRulerViewInfo := nil;
  if (AHorizontalRulerViewInfo <> nil) and (AHorizontalRulerViewInfo.TableCellViewInfo <> nil) then
    Result := Trunc((AMouseX - AHorizontalRulerViewInfo.TableCellViewInfo.TextLeft) / ZoomFactor)
  else
    Result := GetDocumentLayoutDistanceToCurrentAreaStart(AMouseX);
end;

function TdxHorizontalRulerHotZone.GetDocumentLayoutDistanceToCurrentAreaEnd(AMouseX: Integer): Integer;
begin
  Result := Trunc((RulerViewInfo.ActiveAreaCollection[RulerViewInfo.CurrentActiveAreaIndex].Right - AMouseX) / ZoomFactor);
end;

function TdxHorizontalRulerHotZone.CalculateSize: TSize;
begin
  Result := HorizontalRulerControl.CalculateHotZoneSize(Self);
end;

function TdxHorizontalRulerHotZone.GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer;
var
  X: Integer;
begin
  if APageViewInfo = nil then
    Exit(0);
  X := Trunc((AMousePosition.X - RulerViewInfo.ActiveAreaCollection[0].Left) / RulerControl.ZoomFactor);
  Result := APageViewInfo.Page.ClientBounds.Left + X;
end;

{ TdxEmptyHorizontalRulerHotZone }

constructor TdxEmptyHorizontalRulerHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect);
begin
  inherited Create(AControl);
  Bounds := CalculateBounds(ABounds);
end;

function TdxEmptyHorizontalRulerHotZone.GetWeight: Integer;
begin
  Result := 0;
end;

function TdxEmptyHorizontalRulerHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
begin
  ASize := CalculateSize;
  Result := TRect.CreateSize(ABounds.Left, RulerClientBounds.Bottom - ASize.cy, ASize.cx, ASize.cy);
end;

procedure TdxEmptyHorizontalRulerHotZone.OnMouseDoubleClick;
begin
end;

procedure TdxEmptyHorizontalRulerHotZone.OnMove(const AMousePosition: TPoint);
begin
end;

procedure TdxEmptyHorizontalRulerHotZone.Commit(const AMousePosition: TPoint);
begin
end;

procedure TdxEmptyHorizontalRulerHotZone.SetNewValue(ANewValue: Integer);
begin
end;

function TdxEmptyHorizontalRulerHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxEmptyHorizontalRulerHotZone.Create(HorizontalRulerControl, Bounds);
end;

function TdxEmptyHorizontalRulerHotZone.GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer;
begin
  Result := 0;
end;

procedure TdxEmptyHorizontalRulerHotZone.Activate(AHandler: TdxRulerMouseController; const AMousePosition: TPoint);
begin
end;

{ TdxTabHotZone }

constructor TdxTabHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; const ATabInfo: TdxTabInfo);
begin
  inherited Create(AControl);
  FTabInfo := ATabInfo;
  FOldTabInfo := ATabInfo;
  Bounds := CalculateBounds(ABounds);
end;

function TdxTabHotZone.GetWeight: Integer;
begin
  Result := 1;
end;

procedure TdxTabHotZone.OnMove(const AMousePosition: TPoint);
var
  ANewTabPosition: Integer;
begin
  ANewTabPosition := HorizontalRulerViewInfo.GetRulerModelPositionRelativeToTableCellViewInfo(AMousePosition.X);
  SetNewValue(ANewTabPosition);
  Visible := CanDropTab(AMousePosition);
end;

procedure TdxTabHotZone.SetNewValue(ANewValue: Integer);
begin
  FTabInfo := TdxTabInfo.Create(ANewValue, FTabInfo.Alignment, FTabInfo.Leader, FTabInfo.Deleted, False);

  Bounds := TRect.CreateSize(RulerViewInfo.GetRulerLayoutPosition(ANewValue) + HorizontalRulerViewInfo.GetAdditionalCellIndent(True),
    Bounds.Top, Bounds.Width, Bounds.Height);
  Bounds := CalculateBounds(Bounds);
end;

function TdxTabHotZone.CanActivate(const Args: TdxMouseEventArgs): Boolean;
begin
  Result := (Args.Buttons = [mbLeft]) or (Args.Buttons = [mbRight]);
end;

function TdxTabHotZone.CanDropTab(const AMousePosition: TPoint): Boolean;
var
  AClientBounds, ABounds: TRect;
begin
  AClientBounds := RulerClientBounds;
  ABounds := RulerViewInfo.Bounds;
  AClientBounds.Top := ABounds.Top;
  AClientBounds.Bottom := ABounds.Bottom;
  Result := AClientBounds.Contains(AMousePosition);
end;

procedure TdxTabHotZone.Commit(const AMousePosition: TPoint);
var
  ACommands: TdxCommandCollection;
  ACommand: TdxCustomTransactedMultiCommand;
begin
  ACommands := TdxCommandCollection.Create;
  try
    ACommands.OwnsObjects := False;
    ACommands.Add(TdxDeleteTabAtParagraphCommand.Create(RichEditControl, FOldTabInfo));
    if CanDropTab(AMousePosition) then
      ACommands.Add(TdxInsertTabToParagraphCommand.Create(RichEditControl, FTabInfo));

    ACommand := TdxCustomTransactedMultiCommand.Create(RichEditControl, ACommands,
      TdxMultiCommandExecutionMode.ExecuteAllAvailable, TdxMultiCommandUpdateUIStateMode.EnableIfAllAvailable);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
    RulerControl.Reset;
  finally
    ACommands.Free;
  end;
end;

procedure TdxTabHotZone.OnMouseDoubleClick;
var
  ACommand: TdxShowTabsFormCommand;
begin
  ACommand := TdxShowTabsFormCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxTabHotZone.AddNewTab;
var
  ACommand: TdxInsertTabToParagraphCommand;
begin
  ACommand := TdxInsertTabToParagraphCommand.Create(RichEditControl, FTabInfo);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

{ TdxLeftTabHotZone }

function TdxLeftTabHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
begin
  ASize := CalculateSize;
  Result := TRect.CreateSize(ABounds.Left, RulerClientBounds.Bottom - ASize.cy, ASize.cx, ASize.cy);
end;

function TdxLeftTabHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxLeftTabHotZone.Create(HorizontalRulerControl, Bounds, TabInfo);
end;

{ TdxRightTabHotZone }

function TdxRightTabHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
begin
  ASize := CalculateSize;
  Result := TRect.CreateSize(ABounds.Left - ASize.Width, RulerClientBounds.Bottom - ASize.cy, ASize.cx, ASize.cy);
end;

function TdxRightTabHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxRightTabHotZone.Create(HorizontalRulerControl, Bounds, TabInfo);
end;

{ TdxCenterTabHotZone }

function TdxCenterTabHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
begin
  ASize := CalculateSize;
  Result := TRect.CreateSize(ABounds.Left - ASize.cx div 2, RulerClientBounds.Bottom - ASize.cy, ASize.cx, ASize.cy);
end;

function TdxCenterTabHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxCenterTabHotZone.Create(HorizontalRulerControl, Bounds, TabInfo);
end;

{ TdxDecimalTabHotZone }

function TdxDecimalTabHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
begin
  ASize := CalculateSize;
  Result := TRect.CreateSize(ABounds.Left - ASize.cx div 2, RulerClientBounds.Bottom - ASize.cy, ASize.cx, ASize.cy);
end;

function TdxDecimalTabHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxDecimalTabHotZone.Create(HorizontalRulerControl, Bounds, TabInfo);
end;

{ TdxTableLeftBorderHotZone }

constructor TdxTableLeftBorderHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect;
  ABorderIndex: Integer; ATableViewInfo: TdxTableViewInfo; ATableCell: TdxTableCell);
begin
  inherited Create(AControl);
  Bounds := CalculateBounds(ABounds);
  FTableCell := ATableCell;
  FTableViewInfo := ATableViewInfo;
  TdxTableViewInfo.AddReference(FTableViewInfo);
  FBorderIndex := ABorderIndex;
  FColumn := GetVirtualTableColumn;
  CalculateBorders;
  FNewValue := ABounds.Left;
end;

destructor TdxTableLeftBorderHotZone.Destroy;
begin
  FColumn.Free;
  TdxTableViewInfo.Release(FTableViewInfo);
  inherited Destroy;
end;

function TdxTableLeftBorderHotZone.GetWeight: Integer;
begin
  Result := 3;
end;

function TdxTableLeftBorderHotZone.GetCursor: TCursor;
begin
  Result := TdxRichEditCursors.ResizeTableColumn;
end;

procedure TdxTableLeftBorderHotZone.CalculateBorders;
var
  ACurrentColumnIndex, AMinCellWidth, AAdditionalIndent: Integer;
begin
  ACurrentColumnIndex := RulerViewInfo.CurrentActiveAreaIndex;
  AMinCellWidth := RulerViewInfo.ToDocumentLayoutUnitConverter.ToLayoutUnits(FTableCell.GetMinCellWidth);
  AAdditionalIndent := HorizontalRulerViewInfo.GetAdditionalParentCellIndent(False);
  FLeftBorder := Trunc(((FColumn.MaxLeftBorder + AMinCellWidth - TableViewInfo.Column.Bounds.Left + AAdditionalIndent) * ZoomFactor) + RulerViewInfo.ActiveAreaCollection[ACurrentColumnIndex].Left);
  FRightBorder := ((FColumn.MaxRightBorder - AMinCellWidth - TableViewInfo.Column.Bounds.Left + AAdditionalIndent) * ZoomFactor) + RulerViewInfo.ActiveAreaCollection[ACurrentColumnIndex].Left;
end;

function TdxTableLeftBorderHotZone.GetVirtualTableColumn: TdxVirtualTableColumn;
var
  AElementAccessor: TdxTableElementAccessorBase;
  AEnhancedSelectionManager: TdxEnhancedSelectionManager;
begin
  if FTableCell = nil then
    Exit(nil);
  AElementAccessor := GetElementAccessor;
  AEnhancedSelectionManager := TdxEnhancedSelectionManager.Create(DocumentModel.ActivePieceTable);
  try
    Result := AEnhancedSelectionManager.CalculateTableCellsToResizeHorizontallyCore(TableViewInfo, FTableCell.Row,
      AElementAccessor, FBorderIndex, FBorderIndex);
  finally
    AEnhancedSelectionManager.Free;
  end;
end;

function TdxTableLeftBorderHotZone.GetElementAccessor: TdxTableElementAccessorBase;
begin
  Result := TdxTableRowBeforeAccessor.Create(FTableCell.Row);
end;

function TdxTableLeftBorderHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
begin
  ASize := CalculateSize;
  Result := TRect.CreateSize(ABounds.Left - ASize.cx div 2, ABounds.Top, ASize.cx, ASize.cy);
end;

procedure TdxTableLeftBorderHotZone.OnMove(const AMousePosition: TPoint);
var
  ANewBounds: TRect;
begin
  if (AMousePosition.X <= FLeftBorder) or (AMousePosition.X >= FRightBorder) then
    Exit;
  FNewValue := AMousePosition.X;
  ANewBounds := TRect.CreateSize(FNewValue, Bounds.Top, Bounds.Width, Bounds.Height);
  Bounds := CalculateBounds(ANewBounds);
end;

function TdxTableLeftBorderHotZone.CanEdit: Boolean;
var
  ACommand: TdxChangeTableVirtualColumnRightCommand;
begin
  if not inherited CanEdit then
    Exit(False);

  ACommand := TdxChangeTableVirtualColumnRightCommand.Create(RichEditControl, Column, 100);
  try
    Result := ACommand.CanExecute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxTableLeftBorderHotZone.Commit(const AMousePosition: TPoint);
var
  ATable: TdxTable;
  ATableIndentValue: Integer;
begin
  ATableIndentValue := RulerViewInfo.ToDocumentLayoutUnitConverter.ToModelUnits(
      GetDocumentLayoutDistanceToCurrentAreaStart(NewValue) -
      HorizontalRulerViewInfo.GetAdditionalParentCellIndent(False) -
      TableViewInfo.TextAreaOffset
    ) + TableCell.GetActualLeftMargin.Value;
  ATable := TableViewInfo.Table;
  ATable.TableProperties.TableIndent.&Type := TdxWidthUnitType.ModelUnits;
  ATable.TableProperties.TableIndent.Value := ATableIndentValue;
end;

function TdxTableLeftBorderHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxTableLeftBorderHotZone.Create(HorizontalRulerControl, Bounds, FBorderIndex, FTableViewInfo, TableCell);
end;

procedure TdxTableLeftBorderHotZone.OnMouseDoubleClick;
begin
end;

procedure TdxTableLeftBorderHotZone.SetNewValue(ANewValue: Integer);
begin
end;

{ TdxTableHotZone }

constructor TdxTableHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; ABorderIndex: Integer; ATableViewInfo: TdxTableViewInfo; ATableCell: TdxTableCell);
begin
  inherited Create(AControl, ABounds, ABorderIndex, ATableViewInfo, ATableCell);
end;

function TdxTableHotZone.GetElementAccessor: TdxTableElementAccessorBase;
begin
  Result := TdxTableCellAccessor.Create(TableCell);
end;

procedure TdxTableHotZone.Commit(const AMousePosition: TPoint);
var
  ACommand: TdxChangeTableVirtualColumnRightCommand;
begin
  if Column = nil then
    Exit;
  ACommand := TdxChangeTableVirtualColumnRightCommand.Create(RichEditControl, Column,
    GetDocumentLayoutDistanceToCurrentAreaStart(NewValue) + TableViewInfo.Column.Bounds.Left -
    HorizontalRulerViewInfo.GetAdditionalParentCellIndent(False));
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxTableHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxTableHotZone.Create(HorizontalRulerControl, Bounds, BorderIndex, TableViewInfo, TableCell);
end;

{ TdxIndentHotZone }

constructor TdxIndentHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AIndent: Integer);
begin
  inherited Create(AControl);
  FIndent := AIndent;
  FNewIndent := AIndent;
  FRightBorder := RulerClientBounds.Right;
  FLeftBorder := RulerClientBounds.Left;
end;

function TdxIndentHotZone.GetWeight: Integer;
begin
  Result := 2;
end;

procedure TdxIndentHotZone.OnMove(const AMousePosition: TPoint);
var
  ANewIndent: Integer;
begin
  if (AMousePosition.X >= LeftBorder) and (AMousePosition.X <= RightBorder) then
  begin
    ANewIndent := HorizontalRulerViewInfo.GetRulerModelPositionRelativeToTableCellViewInfo(AMousePosition.X);
    SetNewValue(ANewIndent);
  end;
end;

procedure TdxIndentHotZone.SetNewValue(ANewIndent: Integer);
begin
  FNewIndent := ANewIndent;
  Bounds := TRect.CreateSize(RulerViewInfo.GetRulerLayoutPosition(ANewIndent) + HorizontalRulerViewInfo.GetAdditionalCellIndent(True), Bounds.Top, Bounds.Width, Bounds.Height);
  Bounds := CalculateBounds(Bounds);
end;

procedure TdxIndentHotZone.OnMouseDoubleClick;
var
  ACommand: TdxShowParagraphFormCommand;
begin
  ACommand := TdxShowParagraphFormCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

{ TdxLeftIndentHotZone }

constructor TdxLeftIndentHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AIndent: Integer; ALeftBottomHotZone: TdxLeftBottomHotZone; AFirstLineIndent: Integer);
begin
  inherited Create(AControl, ABounds, AIndent);
  FLeftBottomHotZone := ALeftBottomHotZone;
  SetNewValue(AIndent);
  FFirstLineIndent := AFirstLineIndent;
end;

procedure TdxLeftIndentHotZone.Activate(AHandler: TdxRulerMouseController; const AMousePosition: TPoint);
begin
  inherited Activate(AHandler, AMousePosition);
  if not IsNew then
    FLeftBottomHotZone.AddFakeHotZone;
end;

procedure TdxLeftIndentHotZone.Commit(const AMousePosition: TPoint);
var
  ATransaction: TdxHistoryTransaction;
  AFirstLineCommand: TdxChangeParagraphFirstLineIndentCommand;
  ACommand: TdxChangeParagraphLeftIndentCommand;
begin
  DocumentModel.BeginUpdate;
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AFirstLineCommand := TdxChangeParagraphFirstLineIndentCommand.Create(RichEditControl, FFirstLineIndent - (NewIndent - Indent));
    try
      AFirstLineCommand.Execute;
    finally
      AFirstLineCommand.Free;
    end;
    ACommand := TdxChangeParagraphLeftIndentCommand.Create(RichEditControl, NewIndent);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  finally
    ATransaction.Free;
  end;
  DocumentModel.EndUpdate;
end;

function TdxLeftIndentHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxLeftIndentHotZone.Create(HorizontalRulerControl, Bounds, Indent, FLeftBottomHotZone, FFirstLineIndent);
end;

function TdxLeftIndentHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize, ASizeBottom: TSize;
  Y: Integer;
begin
  ASize := CalculateSize;
  ASizeBottom := FLeftBottomHotZone.CalculateSize;
  Y := RulerClientBounds.Bottom + HorizontalRulerControl.Painter.PaddingBottom - ASizeBottom.cy - ASize.cy;
  Result := TRect.CreateSize(ABounds.Left - ASize.Width div 2, Y, ASize.cx, ASize.cy);
end;

procedure TdxLeftIndentHotZone.OnMove(const AMousePosition: TPoint);
begin
  inherited OnMove(AMousePosition);
  FLeftBottomHotZone.SetNewValue(NewIndent);
end;

{ TdxRightIndentHotZone }

constructor TdxRightIndentHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AIndent: Integer);
begin
  inherited Create(AControl, ABounds, AIndent);
  Bounds := CalculateBounds(ABounds);
end;

procedure TdxRightIndentHotZone.Commit(const AMousePosition: TPoint);
var
  ACommand: TdxChangeParagraphRightIndentCommand;
begin
  ACommand := TdxChangeParagraphRightIndentCommand.Create(RichEditControl, NewIndent);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxRightIndentHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
  Y: Integer;
begin
  ASize := CalculateSize;
  Y := RulerClientBounds.Bottom - ASize.cy;
  Result := TRect.CreateSize(ABounds.Left - ASize.cx div 2, Y, ASize.cx, ASize.cy);
end;

function TdxRightIndentHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxRightIndentHotZone.Create(HorizontalRulerControl, Bounds, Indent);
end;

procedure TdxRightIndentHotZone.OnMove(const AMousePosition: TPoint);
begin
  if (AMousePosition.X >= LeftBorder) and (AMousePosition.X <= RightBorder) then
  begin
    NewIndent := HorizontalRulerControl.ViewInfo.GetRightIndentModelPosition(AMousePosition.X);
    Bounds := TRect.CreateSize(HorizontalRulerControl.ViewInfo.GetRightIndentLayoutPosition(NewIndent), Bounds.Top, Bounds.Width, Bounds.Height);
    Bounds := CalculateBounds(Bounds);
  end;
end;

procedure TdxRightIndentHotZone.SetNewValue(ANewIndent: Integer);
begin
  NewIndent := ANewIndent;
  Bounds := TRect.CreateSize(HorizontalRulerControl.ViewInfo.GetRightIndentLayoutPosition(ANewIndent), Bounds.Top, Bounds.Width, Bounds.Height);
  Bounds := CalculateBounds(Bounds);
end;

{ TdxFirstLineIndentHotZone }

constructor TdxFirstLineIndentHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AIndent: Integer; ALeftIndent: Integer);
begin
  inherited Create(AControl, ABounds, AIndent);
  SetNewValue(AIndent);
  FLeftIndent := ALeftIndent;
end;

procedure TdxFirstLineIndentHotZone.Commit(const AMousePosition: TPoint);
var
  ACommand: TdxChangeParagraphFirstLineIndentCommand;
begin
  ACommand := TdxChangeParagraphFirstLineIndentCommand.Create(RichEditControl, NewIndent - FLeftIndent);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxFirstLineIndentHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
  Y: Integer;
begin
  ASize := CalculateSize;
  Y := RulerClientBounds.Top - HorizontalRulerControl.Painter.PaddingTop;
  Result := TRect.CreateSize(ABounds.Left - ASize.Width div 2, Y, ASize.Width, ASize.Height);
end;

function TdxFirstLineIndentHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxFirstLineIndentHotZone.Create(HorizontalRulerControl, Bounds, Indent, FLeftIndent);
end;

{ TdxLeftBottomHotZone }

constructor TdxLeftBottomHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AIndent: Integer; ARightBorder: Integer; AFirstLineIndentHotZone: TdxFirstLineIndentHotZone);
begin
  inherited Create(AControl, ABounds, AIndent);
  SetNewValue(AIndent);
  FFirstLineHotZone := AFirstLineIndentHotZone;
  RightBorder := ARightBorder;
end;

procedure TdxLeftBottomHotZone.AddLeftHotZone(const ABounds: TRect);
begin
  FLeftIndentHotZone := TdxLeftIndentHotZone.Create(HorizontalRulerControl, ABounds, Indent, Self, FFirstLineHotZone.Indent - Indent);
  FLeftIndentHotZone.RightBorder := RightBorder;
  RulerViewInfo.HotZones.Add(FLeftIndentHotZone);
end;

procedure TdxLeftBottomHotZone.AddFakeHotZone;
begin
  FFirstLineHotZone.AddFakeHotZone;
  FLeftIndentHotZone.AddFakeHotZone;
  inherited AddFakeHotZone;
end;

procedure TdxLeftBottomHotZone.Commit(const AMousePosition: TPoint);
var
  ACommand: TdxChangeParagraphLeftIndentCommand;
begin
  ACommand := TdxChangeParagraphLeftIndentCommand.Create(RichEditControl, NewIndent);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxLeftBottomHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
  Y: Integer;
begin
  ASize := CalculateSize;
  Y := RulerClientBounds.Bottom + HorizontalRulerControl.Painter.PaddingBottom - ASize.Height;
  Result := TRect.CreateSize(ABounds.Left - ASize.Width div 2, Y, ASize.Width, ASize.Height);
end;

function TdxLeftBottomHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxLeftBottomHotZone.Create(HorizontalRulerControl, Bounds, Indent, RightBorder, FFirstLineHotZone);
end;

procedure TdxLeftBottomHotZone.OnMove(const AMousePosition: TPoint);
var
  ABounds: TRect;
  X: Integer;
begin
  ABounds := FFirstLineHotZone.Bounds;
  X := AMousePosition.X + ABounds.Left - Bounds.Left;
  if (X >= LeftBorder) and (X <= RightBorder) then
    inherited OnMove(AMousePosition);
  FLeftIndentHotZone.SetNewValue(NewIndent);
  FFirstLineHotZone.SetNewValue(FFirstLineHotZone.Indent + (NewIndent - Indent));
end;

{ TdxColumnResizerHotZone }

constructor TdxColumnResizerHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
begin
  inherited Create(AControl);
  FColumnIndex := AColumnIndex;
  Bounds := CalculateBounds(ABounds);
end;

function TdxColumnResizerHotZone.GetSectionProperties: TdxSectionProperties;
begin
  Result := RulerViewInfo.SectionProperties;
end;

function TdxColumnResizerHotZone.GetCursor: TCursor;
begin
  Result := TdxRichEditCursors.SizeWE;
end;

function TdxColumnResizerHotZone.GetWeight: Integer;
begin
  Result := 4;
end;

procedure TdxColumnResizerHotZone.OnMouseDoubleClick;
begin
  // do nothing
end;

procedure TdxColumnResizerHotZone.SetNewValue(ANewPosition: Integer);
begin
  // do nothing
end;

procedure TdxColumnResizerHotZone.AddFakeHotZone;
begin
  // do nothing
end;

procedure TdxColumnResizerHotZone.OnMove(const AMousePosition: TPoint);
var
  ASectionProperties: TdxSectionProperties;
begin
  OnMoveCore(AMousePosition);
  ASectionProperties := TdxSectionProperties.Create(SectionProperties);
  HorizontalRulerControl.SetViewInfo(HorizontalRulerControl.CreateViewInfoCore(ASectionProperties, RulerViewInfo.IsMainPieceTable));
end;

procedure TdxColumnResizerHotZone.Commit(const AMousePosition: TPoint);
begin
  if not RichEditControl.InnerControl.ActiveView.CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column) then
    Exit;
  RulerControl.Reset;
end;

{ TdxMiddleColumnResizerHotZone }

constructor TdxMiddleColumnResizerHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
begin
  inherited Create(AControl, ABounds, AColumnIndex);
end;

procedure TdxMiddleColumnResizerHotZone.OnMoveCore(const AMousePosition: TPoint);
var
  AOffset: Integer;
  ACommand: TdxMoveColumnCommand;
begin
  AOffset := CalculateNewValue(AMousePosition);
  ACommand := TdxMoveColumnCommand.Create(RichEditControl, SectionProperties, ColumnIndex, AOffset);
  try
    ACommand.GetNewSection(SectionProperties);
  finally
    ACommand.Free;
  end;
end;

function TdxMiddleColumnResizerHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
  Y: Integer;
begin
  ASize := CalculateSize;
  Y := RulerClientBounds.Top + RulerClientBounds.Height div 2 - ASize.Height div 2;
  Result := TRect.CreateSize(ABounds.Left - ASize.Width div 2, Y, ASize.Width, ASize.Height);
end;

procedure TdxMiddleColumnResizerHotZone.Commit(const AMousePosition: TPoint);
var
  AOffset: Integer;
  ACommand: TdxMoveColumnCommand;
begin
  AOffset := CalculateNewValue(AMousePosition);
  ACommand := TdxMoveColumnCommand.Create(RichEditControl, SectionProperties, ColumnIndex, AOffset);
  try
    ACommand.Execute;
    inherited Commit(AMousePosition);
  finally
    ACommand.Free;
  end;
end;

function TdxMiddleColumnResizerHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxMiddleColumnResizerHotZone.Create(HorizontalRulerControl, Bounds, ColumnIndex);
end;

function TdxMiddleColumnResizerHotZone.CalculateNewValue(const AMousePosition: TPoint): Integer;
var
  AAreaCollection: TList<TdxRectF>;
  APos: Single;
begin
  AAreaCollection := RulerViewInfo.ActiveAreaCollection;
  APos := AMousePosition.X - (AAreaCollection[ColumnIndex].Right + (AAreaCollection[ColumnIndex + 1].Left - AAreaCollection[ColumnIndex].Right) / 2);
  Result := Trunc((APos / ZoomFactor));
end;

function TdxMiddleColumnResizerHotZone.GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer;
var
  AMiddle: Single;
begin
  if APageViewInfo <> nil then
  begin
    AMiddle := (RulerViewInfo.ActiveAreaCollection[ColumnIndex].Right + RulerViewInfo.ActiveAreaCollection[ColumnIndex + 1].Left) / 2.0;
    Result := Trunc(((AMiddle - RulerViewInfo.ActiveAreaCollection[0].Left) / ZoomFactor)) + APageViewInfo.Page.ClientBounds.Left;
  end
  else
    Result := inherited GetVisualFeedbackValue(AMousePosition, APageViewInfo);
end;

{ TdxLeftColumnResizerHotZone }

constructor TdxLeftColumnResizerHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
begin
  inherited Create(AControl, ABounds, AColumnIndex);
end;

procedure TdxLeftColumnResizerHotZone.OnMoveCore(const AMousePosition: TPoint);
var
  ACommand: TdxChangeColumnSizeCommand;
begin
  ACommand := CreateCommand(AMousePosition);
  try
    ACommand.GetNewSection(SectionProperties);
  finally
    ACommand.Free;
  end;
end;

procedure TdxLeftColumnResizerHotZone.Commit(const AMousePosition: TPoint);
var
  ACommand: TdxChangeColumnSizeCommand;
begin
  ACommand := CreateCommand(AMousePosition);
  try
    ACommand.Execute;
    inherited Commit(AMousePosition);
  finally
    ACommand.Free;
  end;
end;

function TdxLeftColumnResizerHotZone.CreateCommand(const AMousePosition: TPoint): TdxChangeColumnSizeCommand;
var
  AOffset: Integer;
begin
  AOffset := CalculateNewValue(AMousePosition);
  if SectionProperties.EqualWidthColumns then
    Result := TdxChangeWidthEqualWidthColumnsByLeftCommand.Create(RichEditControl, SectionProperties, ColumnIndex, AOffset)
  else
    Result := TdxChangeColumnSizeByLeftCommand.Create(RichEditControl, SectionProperties, ColumnIndex, AOffset);
end;

function TdxLeftColumnResizerHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxLeftColumnResizerHotZone.Create(HorizontalRulerControl, Bounds, ColumnIndex);
end;

function TdxLeftColumnResizerHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
  Y: Integer;
begin
  ASize := CalculateSize;
  Y := RulerClientBounds.Top + RulerClientBounds.Height div 2 - ASize.Height div 2;
  Result := TRect.CreateSize(ABounds.Left - ASize.Width, Y, ASize.Width, ASize.Height);
end;

function TdxLeftColumnResizerHotZone.CalculateNewValue(const AMousePosition: TPoint): Integer;
var
  AOffset: Integer;
begin
  AOffset := Trunc((RulerViewInfo.ActiveAreaCollection[ColumnIndex].Left - AMousePosition.X) / ZoomFactor);
  Result := RulerViewInfo.ToDocumentLayoutUnitConverter.ToModelUnits(AOffset);
end;

function TdxLeftColumnResizerHotZone.GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer;
begin
  if ColumnIndex = 0 then
    Result := RulerViewInfo.ToDocumentLayoutUnitConverter.ToLayoutUnits(SectionProperties.LeftMargin)
  else
  begin
    if APageViewInfo <> nil then
      Result := Trunc((RulerViewInfo.ActiveAreaCollection[ColumnIndex].Left - RulerViewInfo.ActiveAreaCollection[0].Left) / ZoomFactor) + APageViewInfo.Page.ClientBounds.Left
    else
      Result := inherited GetVisualFeedbackValue(AMousePosition, APageViewInfo);
  end;
end;

procedure TdxLeftColumnResizerHotZone.OnMouseDoubleClick;
var
  ACommand: TdxShowPageSetupFormCommand;
begin
  if ColumnIndex = 0 then
  begin
    ACommand := TdxShowPageSetupFormCommand.Create(RichEditControl);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

{ TdxRightColumnResizerHotZone }

constructor TdxRightColumnResizerHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
begin
  inherited Create(AControl, ABounds, AColumnIndex);
end;

procedure TdxRightColumnResizerHotZone.OnMoveCore(const AMousePosition: TPoint);
var
  AOffset, AIndex: Integer;
  ACommand: TdxChangeColumnSizeCommand;
begin
  AOffset := CalculateNewValue(AMousePosition);
  if RulerViewInfo.IsMainPieceTable then
    AIndex := ColumnIndex
  else
    AIndex := RulerViewInfo.SectionProperties.ColumnCount - 1;
  if SectionProperties.EqualWidthColumns then
    ACommand := TdxChangeWidthEqualWidthColumnsByRightCommand.Create(RichEditControl, SectionProperties, AIndex, AOffset)
  else
    ACommand := TdxChangeColumnWidthCommand.Create(RichEditControl, SectionProperties, AIndex, AOffset);
  try
    ACommand.GetNewSection(SectionProperties);
  finally
    ACommand.Free;
  end;
end;

procedure TdxRightColumnResizerHotZone.Commit(const AMousePosition: TPoint);
var
  AOffset: Integer;
  ACommand: TdxChangeColumnSizeCommand;
begin
  AOffset := CalculateNewValue(AMousePosition);
  if SectionProperties.EqualWidthColumns then
    ACommand := TdxChangeWidthEqualWidthColumnsByRightCommand.Create(RichEditControl, SectionProperties, ColumnIndex, AOffset)
  else
    ACommand := TdxChangeColumnWidthCommand.Create(RichEditControl, SectionProperties, ColumnIndex, AOffset);
  try
    ACommand.Execute;
    inherited Commit(AMousePosition);
  finally
    ACommand.Free;
  end;
end;

function TdxRightColumnResizerHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxRightColumnResizerHotZone.Create(HorizontalRulerControl, Bounds, ColumnIndex);
end;

function TdxRightColumnResizerHotZone.CalculateBounds(const ABounds: TRect): TRect;
var
  ASize: TSize;
  Y: Integer;
begin
  ASize := CalculateSize;
  Y := RulerClientBounds.Top + RulerClientBounds.Height div 2 - ASize.Height div 2;
  Result := TRect.CreateSize(ABounds.Left, Y, ASize.Width, ASize.Height);
end;

function TdxRightColumnResizerHotZone.CalculateNewValue(const AMousePosition: TPoint): Integer;
begin
  Result := Trunc((RulerViewInfo.ActiveAreaCollection[ColumnIndex].Right - AMousePosition.X) / ZoomFactor);
end;

function TdxRightColumnResizerHotZone.GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer;
begin
  if APageViewInfo <> nil then
    Result := Trunc((RulerViewInfo.ActiveAreaCollection[ColumnIndex].Right - RulerViewInfo.ActiveAreaCollection[0].Left) / ZoomFactor) + APageViewInfo.Page.ClientBounds.Left
  else
    Result := inherited GetVisualFeedbackValue(AMousePosition, APageViewInfo);
end;

{ TdxTabTypeToggleBackgroundHotZone }

constructor TdxTabTypeToggleBackgroundHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect);
begin
  inherited Create(AControl);
  Bounds := CalculateBounds(ABounds);
end;

function TdxTabTypeToggleBackgroundHotZone.GetWeight: Integer;
begin
  Result := 5;
end;

function TdxTabTypeToggleBackgroundHotZone.CalculateBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
end;

procedure TdxTabTypeToggleBackgroundHotZone.OnMove(const AMousePosition: TPoint);
begin
  // do nothing
end;

procedure TdxTabTypeToggleBackgroundHotZone.Commit(const AMousePosition: TPoint);
begin
  // do nothing
end;

procedure TdxTabTypeToggleBackgroundHotZone.OnMouseDoubleClick;
begin
  // do nothing
end;

function TdxTabTypeToggleBackgroundHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxTabTypeToggleBackgroundHotZone.Create(HorizontalRulerControl, Bounds);
end;

procedure TdxTabTypeToggleBackgroundHotZone.SetNewValue(ANewValue: Integer);
begin
  // do nothing
end;

{ TdxTabTypeToggleHotZone }

constructor TdxTabTypeToggleHotZone.Create(const AControl: TdxHorizontalRulerControl; const ABounds: TRect);
begin
  inherited Create(AControl);
  Bounds := CalculateBounds(ABounds);
  FHotZones := CreateHotZones;
  FHotZoneBounds := CalculateHotZoneBounds;
end;

destructor TdxTabTypeToggleHotZone.Destroy;
begin
  FHotZones.Free;
  inherited Destroy;
end;

function TdxTabTypeToggleHotZone.GetHotZone: TdxHorizontalRulerHotZone;
var
  AIndex: Integer;
begin
  AIndex := HorizontalRulerControl.TabTypeIndex;
  if AIndex >= FHotZones.Count then
    Result := FHotZones[0]
  else
    Result := FHotZones[AIndex];
end;

function TdxTabTypeToggleHotZone.GetWeight: Integer;
begin
  Result := 0;
end;

function TdxTabTypeToggleHotZone.CreateHotZones: TdxReferencedObjectList<TdxHorizontalRulerHotZone>;
begin
  Result := TdxReferencedObjectList<TdxHorizontalRulerHotZone>.Create;
  if DocumentModel.DocumentCapabilities.ParagraphTabsAllowed and RichEditControl.InnerControl.Options.HorizontalRuler.ShowTabs then
  begin
    Result.Add(TdxLeftTabHotZone.Create(HorizontalRulerControl, Bounds, TdxTabInfo.Create(-1, TdxTabAlignmentType.Left)));
    Result.Add(TdxRightTabHotZone.Create(HorizontalRulerControl, Bounds, TdxTabInfo.Create(-1, TdxTabAlignmentType.Right)));
    Result.Add(TdxCenterTabHotZone.Create(HorizontalRulerControl, Bounds, TdxTabInfo.Create(-1, TdxTabAlignmentType.Center)));
    Result.Add(TdxDecimalTabHotZone.Create(HorizontalRulerControl, Bounds, TdxTabInfo.Create(-1, TdxTabAlignmentType.Decimal)));
  end;
  if DocumentModel.DocumentCapabilities.ParagraphFormattingAllowed and RichEditControl.InnerControl.Options.HorizontalRuler.ShowLeftIndent then
    Result.Add(TdxFirstLineIndentHotZone.Create(HorizontalRulerControl, Bounds, 0, 0));
  if Result.Count <= 0 then
    Result.Add(TdxEmptyHorizontalRulerHotZone.Create(HorizontalRulerControl, Bounds));
end;

function TdxTabTypeToggleHotZone.CalculateBounds(const ABounds: TRect): TRect;
begin
  Result := RulerControl.PixelsToLayoutUnits(ABounds);
end;

function TdxTabTypeToggleHotZone.CalculateHotZoneBounds: TRect;
var
  ASize: TSize;
  ABounds: TRect;
begin
  ASize := HotZone.CalculateSize;
  ABounds := TRect.CreateSize(Bounds.Left + (Bounds.Width - ASize.Width) div 2, Bounds.Top + (Bounds.Height - ASize.Height) div 2, ASize.Width, ASize.Height);
  Result := RulerControl.LayoutUnitsToPixels(ABounds);
end;

procedure TdxTabTypeToggleHotZone.OnMove(const AMousePosition: TPoint);
begin
  // do nothing
end;

procedure TdxTabTypeToggleHotZone.Commit(const AMousePosition: TPoint);
begin
  HorizontalRulerControl.TabTypeIndex := HorizontalRulerControl.TabTypeIndex + 1;
  if HorizontalRulerControl.TabTypeIndex >= FHotZones.Count then
    HorizontalRulerControl.TabTypeIndex := 0;
  FHotZoneBounds := CalculateHotZoneBounds;
end;

procedure TdxTabTypeToggleHotZone.OnMouseDoubleClick;
begin
  // do nothing
end;

function TdxTabTypeToggleHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxTabTypeToggleHotZone.Create(HorizontalRulerControl, Bounds);
end;

procedure TdxTabTypeToggleHotZone.SetNewValue(ANewValue: Integer);
begin
  // do nothing
end;

{ TdxVerticalRulerHotZone }

constructor TdxVerticalRulerHotZone.Create(const AControl: TdxVerticalRulerControl);
begin
  inherited Create(AControl);
end;

function TdxVerticalRulerHotZone.GetVerticalRulerControl: TdxVerticalRulerControl;
begin
  Result := TdxVerticalRulerControl(RulerControl);
end;

function TdxVerticalRulerHotZone.GetWeight: Integer;
begin
  Result := 0;
end;

procedure TdxVerticalRulerHotZone.SetNewValue(ANewValue: Integer);
begin
  // do nothing
end;

procedure TdxVerticalRulerHotZone.AddFakeHotZone;
begin
end;

function TdxVerticalRulerHotZone.GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer;
var
  Y: Integer;
begin
  if APageViewInfo = nil then
    Exit(0);

  Y := Trunc((AMousePosition.Y - RulerViewInfo.ActiveAreaCollection[0].Top) / RulerControl.ZoomFactor);
  Result := APageViewInfo.Page.ClientBounds.Top + Y;
end;

{ TdxSectionResizerHotZone }

constructor TdxSectionResizerHotZone.Create(const AControl: TdxVerticalRulerControl; const ABounds: TRect; AColumnIndex: Integer);
begin
  inherited Create(AControl);
  FColumnIndex := AColumnIndex;
  Bounds := ABounds;
end;

function TdxSectionResizerHotZone.GetSectionProperties: TdxSectionProperties;
begin
  Result := RulerViewInfo.SectionProperties;
end;

function TdxSectionResizerHotZone.GetCursor: TCursor;
begin
  Result := TdxRichEditCursors.SizeNS;
end;

procedure TdxSectionResizerHotZone.OnMouseDoubleClick;
begin
end;

procedure TdxSectionResizerHotZone.OnMove(const AMousePosition: TPoint);
var
  ASectionProperties: TdxSectionProperties;
  ATableVerticalPositions: TdxSortedList<Integer>;
begin
  OnMoveCore(AMousePosition);
  ASectionProperties := TdxSectionProperties.Create(SectionProperties);
  ATableVerticalPositions := TdxSortedList<Integer>.Create;
  ATableVerticalPositions.CopyCore(VerticalRulerControl.ViewInfo.TableVerticalPositions);

  VerticalRulerControl.SetViewInfo(TdxVerticalRulerViewInfo.Create(VerticalRulerControl, ASectionProperties,
    RulerViewInfo.IsMainPieceTable, ATableVerticalPositions, VerticalRulerControl.ViewInfo.TableCellViewInfo));
end;

{ TdxSectionTopResizerHotZone }

procedure TdxSectionTopResizerHotZone.OnMoveCore(const AMousePosition: TPoint);
var
  AOffset: Integer;
  ACommand: TdxChangeSectionHeightByTopCommand;
begin
  AOffset := CalculateNewValue(AMousePosition);
  ACommand := TdxChangeSectionHeightByTopCommand.Create(RichEditControl, SectionProperties, 0, AOffset);
  try
    ACommand.GetNewSection(SectionProperties);
  finally
    ACommand.Free;
  end;
end;

procedure TdxSectionTopResizerHotZone.Commit(const AMousePosition: TPoint);
var
  AOffset: Integer;
  ACommand: TdxChangeSectionHeightByTopCommand;
begin
  AOffset := CalculateNewValue(AMousePosition);
  ACommand := TdxChangeSectionHeightByTopCommand.Create(RichEditControl, SectionProperties, 0, AOffset);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxSectionTopResizerHotZone.CalculateNewValue(const AMousePosition: TPoint): Integer;
var
  AOffset: Integer;
begin
  AOffset := Trunc((RulerViewInfo.ActiveAreaCollection[0].Top - AMousePosition.Y) / ZoomFactor);
  Result := RulerViewInfo.ToDocumentLayoutUnitConverter.ToModelUnits(AOffset);
end;

function TdxSectionTopResizerHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxSectionTopResizerHotZone.Create(VerticalRulerControl, Bounds, ColumnIndex);
end;

function TdxSectionTopResizerHotZone.GetVisualFeedbackValue(const AMousePosition: TPoint; APageViewInfo: TdxPageViewInfo): Integer;
begin
  if ColumnIndex = 0 then
    Result := RulerViewInfo.ToDocumentLayoutUnitConverter.ToLayoutUnits(SectionProperties.TopMargin)
  else
    Result := inherited GetVisualFeedbackValue(AMousePosition, APageViewInfo);
end;

procedure TdxSectionTopResizerHotZone.OnMouseDoubleClick;
var
  ACommand: TdxShowPageSetupFormCommand;
begin
  ACommand := TdxShowPageSetupFormCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

{ TdxSectionBottomResizerHotZone }

procedure TdxSectionBottomResizerHotZone.OnMoveCore(const AMousePosition: TPoint);
var
  AOffset: Integer;
  ACommand: TdxChangeSectionHeightByBottomCommand;
begin
  AOffset := CalculateNewValue(AMousePosition);
  ACommand := TdxChangeSectionHeightByBottomCommand.Create(RichEditControl, SectionProperties, 0, AOffset);
  try
    ACommand.GetNewSection(SectionProperties);
  finally
    ACommand.Free;
  end;
end;

procedure TdxSectionBottomResizerHotZone.Commit(const AMousePosition: TPoint);
var
  AOffset: Integer;
  ACommand: TdxChangeSectionHeightByBottomCommand;
begin
  AOffset := CalculateNewValue(AMousePosition);
  ACommand := TdxChangeSectionHeightByBottomCommand.Create(RichEditControl, SectionProperties, 0, AOffset);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxSectionBottomResizerHotZone.CalculateNewValue(const AMousePosition: TPoint): Integer;
var
  AOffset: Integer;
begin
  AOffset := Trunc((RulerViewInfo.ActiveAreaCollection[0].Bottom - AMousePosition.Y) / ZoomFactor);
  Result := RulerViewInfo.ToDocumentLayoutUnitConverter.ToModelUnits(AOffset);
end;

function TdxSectionBottomResizerHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxSectionBottomResizerHotZone.Create(VerticalRulerControl, Bounds, ColumnIndex);
end;

procedure TdxSectionBottomResizerHotZone.OnMouseDoubleClick;
var
  ACommand: TdxShowPageSetupFormCommand;
begin
  ACommand := TdxShowPageSetupFormCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

{ TdxVerticalTableHotZone }

constructor TdxVerticalTableHotZone.Create(const AControl: TdxVerticalRulerControl; const ABounds: TRect; ATableViewInfo: TdxTableViewInfo; AAnchorIndex: Integer);
begin
  inherited Create(AControl);
  Bounds := CalculateBounds(ABounds);
  FTableViewInfo := ATableViewInfo;
  TdxTableViewInfo.AddReference(FTableViewInfo);
  FAnchorIndex := AAnchorIndex;
end;

destructor TdxVerticalTableHotZone.Destroy;
begin
  TdxTableViewInfo.Release(FTableViewInfo);
  inherited Destroy;
end;

function TdxVerticalTableHotZone.GetCursor: TCursor;
begin
  Result := TdxRichEditCursors.ResizeTableRow;
end;

function TdxVerticalTableHotZone.GetVisualFeedbackValue(const AMousePosition: TPoint;
  APageViewInfo: TdxPageViewInfo): Integer;
begin
  if APageViewInfo = nil then
    Exit(0);
  Result := Trunc(AMousePosition.Y / RulerControl.ZoomFactor);
end;

function TdxVerticalTableHotZone.CalculateBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
end;

function TdxVerticalTableHotZone.CalculateNewValue(const AMousePosition: TPoint): Integer;
begin
  Result := Trunc((AMousePosition.Y - RulerViewInfo.ActiveAreaCollection[0].Top) / ZoomFactor);
end;

function TdxVerticalTableHotZone.CanEdit: Boolean;
var
  ACommand: TdxChangeTableRowHeightCommand;
  ARowIndex: Integer;
  ARow: TdxTableRow;
begin
  if not inherited CanEdit then
    Exit(False);

  ARowIndex := FAnchorIndex + FTableViewInfo.TopRowIndex;
  if ARowIndex < 0 then
    Exit(False);

  ARow := TableViewInfo.Table.Rows[ARowIndex];
  ACommand := TdxChangeTableRowHeightCommand.Create(RichEditControl, ARow, 0);
  try
    Result := ACommand.CanExecute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxVerticalTableHotZone.OnMove(const AMousePosition: TPoint);
var
  ANewValue: Integer;
  ANewBounds: TRect;
begin
  ANewValue := AMousePosition.Y;
  ANewBounds := TRect.CreateSize(Bounds.Left, ANewValue, Bounds.Width, Bounds.Height);
  Bounds := CalculateBounds(ANewBounds);
end;

procedure TdxVerticalTableHotZone.Commit(const AMousePosition: TPoint);
var
  ATable: TdxTable;
  ARowIndex, AValue, AMinValue, AHeightIndex, AVerticalPosition: Integer;
begin
  ARowIndex := FAnchorIndex + FTableViewInfo.TopRowIndex;
  if ARowIndex < 0 then
    Exit;
  ATable := TableViewInfo.Table;
  AVerticalPosition := TableViewInfo.Anchors[FAnchorIndex].VerticalPosition;
  ATable.Rows[ARowIndex].Properties.Height.&Type := TdxHeightUnitType.Minimum;
  AValue := Trunc(AMousePosition.Y / ZoomFactor) - AVerticalPosition;
  AMinValue := DocumentModel.LayoutUnitConverter.PointsToFontUnits(1);
  AHeightIndex := RulerViewInfo.ToDocumentLayoutUnitConverter.ToModelUnits(Math.Max(AValue, AMinValue));
  ATable.Rows[ARowIndex].Properties.Height.Value := AHeightIndex;
end;

procedure TdxVerticalTableHotZone.OnMouseDoubleClick;
begin
  // do nothing
end;

function TdxVerticalTableHotZone.CreateEmptyClone: TdxRulerHotZone;
begin
  Result := TdxVerticalTableHotZone.Create(VerticalRulerControl, Bounds, TableViewInfo, FAnchorIndex);
end;

{ TdxRulerViewInfoBase }

constructor TdxRulerViewInfoBase.Create(const AControl: TdxRulerControlBase; ASectionProperties: TdxSectionProperties; AIsMainPieceTable: Boolean);
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  inherited Create;
  FIsMainPieceTable := AIsMainPieceTable;
  FControl := AControl;
  AUnitConverter := AControl.DocumentModel.LayoutUnitConverter;
  FBounds := AUnitConverter.PixelsToLayoutUnits(AControl.Bounds, Control.DpiX, Control.DpiY);
  FRulerTickmarks := TdxObjectList<TdxRulerTickmark>.Create;
  FSectionProperties := ASectionProperties;
  FTextSize := MeasureString('yW');
  FActiveAreaCollection := TList<TdxRectF>.Create;
  FDisplayActiveAreaCollection := TList<TdxRectF>.Create;
  FSpaceAreaCollection := TList<TdxRectF>.Create;
  FDisplaySpaceAreaCollection := TList<TdxRectF>.Create;
  FClientBounds := CalculateClientBounds(AControl);
  CalculateBounds(AUnitConverter);
  FHotZones := TdxReferencedObjectList<TdxRulerHotZone>.Create;
end;

destructor TdxRulerViewInfoBase.Destroy;
begin
  FreeAndNil(FSectionProperties);
  FreeAndNil(FActiveAreaCollection);
  FreeAndNil(FDisplayActiveAreaCollection);
  FreeAndNil(FSpaceAreaCollection);
  FreeAndNil(FDisplaySpaceAreaCollection);
  FreeAndNil(FHotZones);
  FreeAndNil(FRulerTickmarks);
  inherited Destroy;
end;

class function TdxRulerViewInfoBase.GetMeasureGraphics: TdxGraphics;
begin
  if FMeasureGraphics = nil then
    FMeasureGraphics := TdxGraphics.Create;
  Result := FMeasureGraphics;
end;

class procedure TdxRulerViewInfoBase.FinalizeMeasureGraphics;
begin
  FreeAndNil(FMeasureGraphics);
end;

procedure TdxRulerViewInfoBase.CalculateBounds(AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  FDisplayClientBounds := AUnitConverter.LayoutUnitsToPixels(FClientBounds, Control.DpiX, Control.DpiY);
  FHalfTickBounds := CalculateHalfTickBounds;
  FDisplayHalfTickBounds := AUnitConverter.LayoutUnitsToPixels(TRect.Round(FHalfTickBounds), Control.DpiX, Control.DpiY);
  FQuarterTickBounds := CalculateQuarterTickBounds;
  FDisplayQuarterTickBounds := AUnitConverter.LayoutUnitsToPixels(TRect.Round(FQuarterTickBounds), Control.DpiX, Control.DpiY);
end;

procedure TdxRulerViewInfoBase.Initialize;
begin
  CalculateActiveAreaCollection;
  CalculateSpaceAreaCollection;
  CalculateRulerSeparators;
end;

procedure TdxRulerViewInfoBase.SetClientBounds(const ABounds: TRect);
begin
  FClientBounds := ABounds;
end;

function TdxRulerViewInfoBase.GetToDocumentLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := FControl.DocumentModel.ToDocumentLayoutUnitConverter;
end;

function TdxRulerViewInfoBase.GetCurrentActiveAreaIndex: Integer;
begin
  Result := 0;
end;

function TdxRulerViewInfoBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.RichEditControl.InnerControl.DocumentModel;
end;

function TdxRulerViewInfoBase.GetOrientation: TdxOrientation;
begin
  Result := Control.Orientation;
end;

function TdxRulerViewInfoBase.GetMaxRulerDimensionInModelUnits: Integer;
begin
  Result := DocumentModel.UnitConverter.DocumentsToModelUnits(300 * 50);
end;

function TdxRulerViewInfoBase.MeasureString(const AValue: string): TSize;
var
  AModifier: TdxGraphicsToLayoutUnitsModifier;
  ASize: TdxSizeF;
begin
  System.TMonitor.Enter(MeasureGraphics);
  try
    AModifier := TdxGraphicsToLayoutUnitsModifier.Create(MeasureGraphics, Control.DocumentModel.LayoutUnitConverter);
    try
      ASize := MeasureGraphics.MeasureString(AValue, Control.TickMarkFont, MaxInt, TdxGPStringFormat.GenericTypographic);
      Result.Init(Ceil(ASize.cx), Ceil(ASize.cy));
    finally
      AModifier.Free;
    end;
  finally
    System.TMonitor.Exit(MeasureGraphics);
  end;
end;

function TdxRulerViewInfoBase.CalculateTickStep(AConverter: TdxUnitConverter): Single;
var
  AUnit: TdxMeasurementUnit;
  AValue: Single;
begin
  AUnit := FControl.RichEditControl.InnerControl.UIUnit;
  if AUnit = TdxMeasurementUnit.Millimeter then
    AValue := 20
  else
    if AUnit = TdxMeasurementUnit.Point then
      AValue := 72
    else
      AValue := 1;
  Result := ToDocumentLayoutUnitConverter.ToLayoutUnits(Trunc(AConverter.ToUnits(AValue))) * Control.ZoomFactor;
end;

procedure TdxRulerViewInfoBase.ResampleDownRulerTickmarks;
begin
  ResampleDownTicks;
  if AreRulerTickmarksOverlapped then
    DeleteRulerTickmarks(TdxRulerTickmarkType.QuarterTick);
  if AreRulerTickmarksOverlapped then
    DeleteRulerTickmarks(TdxRulerTickmarkType.HalfTick);
end;

procedure TdxRulerViewInfoBase.ResampleDownTicks;
var
  AIsDelete: Boolean;
begin
  AIsDelete := True;
  while True do
  begin
    if not ShouldResampleDownTicks then
      Exit;
    ResampleDownTicksCore(AIsDelete);
  end;
end;

procedure TdxRulerViewInfoBase.ResampleDownTicksCore(AIsDelete: Boolean);
var
  I: Integer;
begin
  for I := RulerTickmarks.Count - 1 downto 0 do
  begin
    if RulerTickmarks[I].RulerTickmarkType = TdxRulerTickmarkType.Tick then
    begin
      if AIsDelete then
      begin
        RulerTickmarks.Remove(RulerTickmarks[I]);
        AIsDelete := False;
      end
      else
        AIsDelete := True;
    end;
  end;
end;

function TdxRulerViewInfoBase.ShouldResampleDownTicks: Boolean;
var
  ATotalTickExtent: Single;
  I: Integer;
begin
  ATotalTickExtent := 0;
  for I := 0 to RulerTickmarks.Count - 1 do
  begin
    if RulerTickmarks[I].RulerTickmarkType = TdxRulerTickmarkType.Tick then
      ATotalTickExtent := ATotalTickExtent + Orientation.GetPrimaryCoordinateExtent(RulerTickmarks[I].Bounds);
  end;
  if ATotalTickExtent <= 0 then
    Result := False
  else
    Result := Orientation.GetPrimaryCoordinateExtent(ClientBounds.ToRectF) / ATotalTickExtent < 1.65;
end;

function TdxRulerViewInfoBase.AreRulerTickmarksOverlapped: Boolean;
var
  I: Integer;
begin
  for I := 1 to RulerTickmarks.Count - 1 do
    if Orientation.GetFarPrimaryCoordinate(RulerTickmarks[I - 1].Bounds) >= Orientation.GetNearPrimaryCoordinate(RulerTickmarks[I].Bounds) then
      Exit(True);
  Result := False;
end;

procedure TdxRulerViewInfoBase.DeleteRulerTickmarks(AType: TdxRulerTickmarkType);
var
  I: Integer;
begin
  for I := RulerTickmarks.Count - 1 downto 0 do
  begin
    if RulerTickmarks[I].RulerTickmarkType = AType then
      RulerTickmarks.Remove(RulerTickmarks[I]);
  end;
end;

procedure TdxRulerViewInfoBase.CalculateRulerSeparators;
var
  AUiUnitConverter: TdxUnitConverter;
  AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AStep, AActiveAreaCollectionNear, ADistanceToZero, AOffset, AStartPos, U, AValueInLayoutUnits, AValueInModelUnits: Single;
  AClientBoundsNear, AClientBoundsFar, ADistanceToZeroInSteps, AValue: Integer;
begin
  AUiUnitConverter := Control.DocumentModel.InternalAPI.UnitConverters[Control.RichEditControl.InnerControl.UIUnit];
  AToLayoutUnitConverter := Control.DocumentModel.ToDocumentLayoutUnitConverter;
  AStep := CalculateTickStep(AUiUnitConverter);
  AActiveAreaCollectionNear := Orientation.GetNearPrimaryCoordinate(ActiveAreaCollection[CurrentActiveAreaIndex]);
  AClientBoundsNear := Orientation.GetNearPrimaryCoordinate(ClientBounds);
  AClientBoundsFar := Orientation.GetFarPrimaryCoordinate(ClientBounds);
  ADistanceToZero := Abs(AClientBoundsNear - AActiveAreaCollectionNear);
  ADistanceToZeroInSteps := Trunc(ADistanceToZero / AStep);
  AOffset := ADistanceToZero - ADistanceToZeroInSteps * AStep;
  AStartPos := AClientBoundsNear + AOffset;
  AddTicks(AStep, AStartPos - AStep);

  U := AStartPos;
  while U < AClientBoundsFar do
  begin
    AValueInLayoutUnits := (U - AActiveAreaCollectionNear) / Control.ZoomFactor;
    AValueInModelUnits := AToLayoutUnitConverter.ToModelUnits(AValueInLayoutUnits);
    AValue := Abs(Round(AUiUnitConverter.FromUnits(AValueInModelUnits)));
    AddTickNumber(U, AValue);
    AddTicks(AStep, U);
    U := U + AStep;
  end;
  ResampleDownRulerTickmarks;
end;

procedure TdxRulerViewInfoBase.AddTicks(AStep: Single; U: Single);
begin
  AddQuarterTicks(AStep / 2, U);
  AddHalfTick(U + AStep / 2.0);
  AddQuarterTicks(AStep / 2, U + AStep / 2);
end;

procedure TdxRulerViewInfoBase.AddQuarterTicks(AStep: Single; U: Single);
begin
  if Control.RichEditControl.InnerControl.UIUnit = TdxMeasurementUnit.Centimeter then
  begin
    AddQuarterTick(U + AStep / 2.0);
    Exit;
  end;
  AddQuarterTick(U + AStep / 4.0);
  AddQuarterTick(U + AStep / 2.0);
  AddQuarterTick(U + AStep * 3 / 4.0);
end;

procedure TdxRulerViewInfoBase.AddHalfTick(U: Single);
var
  ABounds: TdxRectF;
begin
  if (U < Orientation.GetFarPrimaryCoordinate(ClientBounds)) and (U > Orientation.GetNearPrimaryCoordinate(ClientBounds)) then
  begin
    ABounds := Orientation.SetNearPrimaryCoordinate(HalfTickBounds, U);
    RulerTickmarks.Add(TdxRulerTickmarkHalf.Create(FControl, ABounds));
  end;
end;

procedure TdxRulerViewInfoBase.AddQuarterTick(U: Single);
var
  ABounds: TdxRectF;
begin
  if (U < Orientation.GetFarPrimaryCoordinate(ClientBounds)) and (U > Orientation.GetNearPrimaryCoordinate(ClientBounds)) then
  begin
    ABounds := Orientation.SetNearPrimaryCoordinate(QuarterTickBounds, U);
    RulerTickmarks.Add(TdxRulerTickmarkQuarter.Create(FControl, ABounds));
  end;
end;

function TdxRulerViewInfoBase.CalculatePageClientBounds(APageCalculator: TdxPageBoundsCalculator; AProperties: TdxSectionProperties): TRect;
begin
  Result := APageCalculator.CalculatePageClientBoundsCore(AProperties.PageWidth, AProperties.PageHeight, AProperties.LeftMargin, AProperties.TopMargin, AProperties.RightMargin, AProperties.BottomMargin);
end;

{ TdxHorizontalRulerViewInfo }

constructor TdxHorizontalRulerViewInfo.Create(const AControl: TdxHorizontalRulerControl;
  ASectionProperties: TdxSectionProperties; ACurrentParagraph: TdxParagraph;
  ACurrentActiveAreaIndex: Integer; AIsMainPieceTable: Boolean; ATableCellViewInfo: TdxTableCellViewInfo;
  ATableAlignedPositions: TdxSortedList<Integer>);
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  inherited Create(AControl, ASectionProperties, AIsMainPieceTable);
  FControl := AControl;
  FCurrentActiveAreaIndex := ACurrentActiveAreaIndex;
  FParagraph := ACurrentParagraph;
  FDefaultTabBounds := CalculateDefaultTabBounds;
  AUnitConverter := AControl.DocumentModel.LayoutUnitConverter;
  FDisplayDefaultTabBounds := AUnitConverter.LayoutUnitsToPixels(FDefaultTabBounds, Control.DpiX, Control.DpiY);
  FDefaultTabWidth := GetDefaultTabWidth;
  FDefaultTabsCollection := TdxRectList.Create;
  FDisplayDefaultTabsCollection := TdxRectList.Create;
  FTableCellViewInfo := ATableCellViewInfo;
  FTableAlignedPositions := ATableAlignedPositions;
end;

destructor TdxHorizontalRulerViewInfo.Destroy;
begin
  FreeAndNil(FTabTypeToggleBackground);
  FreeAndNil(FTabTypeToggle);
  FreeAndNil(FTableAlignedPositions);
  FreeAndNil(FDefaultTabsCollection);
  FreeAndNil(FDisplayDefaultTabsCollection);
  inherited Destroy;
end;

procedure TdxHorizontalRulerViewInfo.Initialize;
begin
  CalculateClientBounds(FControl);
  inherited Initialize;
  CalculateTabsBounds;
  CalculateIndentElementsBounds;
  CalculateSectionSeparator;
  CalculateBounds;
  CalculateTabTypeToggle;
  CalculateTableHotZone;
  SortingHotZones;
end;

function TdxHorizontalRulerViewInfo.GetCurrentActiveAreaIndex: Integer;
begin
  Result := FCurrentActiveAreaIndex;
end;

procedure TdxHorizontalRulerViewInfo.CalculateBounds;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  AHeight: Integer;
begin
  Control.Height := GetRulerSize;
  AUnitConverter := FControl.DocumentModel.LayoutUnitConverter;
  AHeight := DisplayClientBounds.Height + AUnitConverter.LayoutUnitsToPixels(FControl.Painter.PaddingTop + FControl.Painter.PaddingBottom, FControl.DpiY);
  Control.Height := AHeight;
end;

function TdxHorizontalRulerViewInfo.GetRulerSize: Integer;
begin
  Result := Control.Painter.CalculateTotalRulerSize(TextSize.Height);
end;

function TdxHorizontalRulerViewInfo.CalculateClientBounds(const AControl: TdxRulerControlBase): TRect;
var
  APageClientBounds: TRect;
  ACaretPosition: TdxCaretPosition;
  AClientHeight, Y: Integer;
begin
  IsReady := False;
  ACaretPosition := AControl.RichEditControl.InnerControl.ActiveView.CaretPosition;
  if not ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Page) then
    Exit(TRect.CreateSize(0, 0, 100, 100));
  IsReady := True;
  APageClientBounds := ACaretPosition.PageViewInfo.ClientBounds;
  if AControl.RichEditControl.InnerControl.ActiveViewType = TdxRichEditViewType.PrintLayout then
    APageClientBounds.Width := Round(ToDocumentLayoutUnitConverter.ToLayoutUnits(Math.Min(MaxRulerDimensionInModelUnits, SectionProperties.PageWidth)) * AControl.ZoomFactor)
  else
    if AControl.RichEditControl.InnerControl.ActiveViewType = TdxRichEditViewType.Draft then
      APageClientBounds.Width := Round(ToDocumentLayoutUnitConverter.ToLayoutUnits(Math.Min(MaxRulerDimensionInModelUnits, SectionProperties.PageWidth - SectionProperties.LeftMargin)) * AControl.ZoomFactor);
  AClientHeight := TextSize.Height + 2 * AControl.Painter.VerticalTextPaddingBottom;
  Y := AControl.Painter.PaddingTop;
  Result := TRect.CreateSize(APageClientBounds.Left, Y, APageClientBounds.Width, AClientHeight);
end;

procedure TdxHorizontalRulerViewInfo.CalculateActiveAreaCollection;
var
  ABounds: TRect;
  ARulerAreaCollection: TdxRectList;
  I: Integer;
begin
  ABounds := GetActiveAreaBounds;
  if IsMainPieceTable then
  begin
    ARulerAreaCollection := GetRulerAreaCollection(SectionProperties, ABounds);
    try
      for I := 0 to ARulerAreaCollection.Count - 1 do
        AddActiveArea(ARulerAreaCollection[I]);
    finally
      ARulerAreaCollection.Free;
    end;
  end
  else
    AddActiveArea(ABounds);
end;

procedure TdxHorizontalRulerViewInfo.AddActiveArea(const ABounds: TRect);
var
  ALayoutUnitConverter: TdxDocumentLayoutUnitConverter;
  ARulerArea: TRect;
begin
  ALayoutUnitConverter := FControl.DocumentModel.LayoutUnitConverter;
  ARulerArea := ApplyZoomFactor(ABounds);
  ARulerArea.Offset(ClientBounds.Left, 0);
  ActiveAreaCollection.Add(ARulerArea.ToRectF);
  DisplayActiveAreaCollection.Add(ALayoutUnitConverter.LayoutUnitsToPixels(ARulerArea, Control.DpiX, Control.DpiY).ToRectF);
end;

function TdxHorizontalRulerViewInfo.GetActiveAreaBounds: TRect;
var
  AControl: IdxRichEditControl;
  ADocumentModel: TdxDocumentModel;
  APageCalculator, AActiveViewPageCalculator: TdxPageBoundsCalculator;
  APrintLayoutViewPageClientBounds, AActiveViewPageClientBounds: TRect;
  ADeltaX: Integer;
begin
  AControl := Control.RichEditControl;
  ADocumentModel := Control.DocumentModel;
  APageCalculator := TdxPageBoundsCalculator.Create(ADocumentModel.ToDocumentLayoutUnitConverter);
  try
    APrintLayoutViewPageClientBounds.Empty;
    if AControl.InnerControl.ActiveViewType = TdxRichEditViewType.Simple then
      APrintLayoutViewPageClientBounds := AControl.InnerControl.ActiveView.PageViewInfos.First.ClientBounds
    else
      APrintLayoutViewPageClientBounds := CalculatePageClientBounds(APageCalculator, SectionProperties);
  finally
    APageCalculator.Free;
  end;
  AActiveViewPageCalculator := AControl.InnerControl.Formatter.DocumentFormatter.Controller.PageController.PageBoundsCalculator;
  AActiveViewPageClientBounds := CalculatePageClientBounds(AActiveViewPageCalculator, SectionProperties);
  ADeltaX := AActiveViewPageClientBounds.Left - APrintLayoutViewPageClientBounds.Left;
  Result := TRect.CreateSize(APrintLayoutViewPageClientBounds.Left + ADeltaX, ClientBounds.Top, APrintLayoutViewPageClientBounds.Width, ClientBounds.Height);
end;

function TdxHorizontalRulerViewInfo.GetRulerAreaCollection(ASectionProperties: TdxSectionProperties; const ABounds: TRect): TdxRectList;
var
  ACalculator: TdxColumnsBoundsCalculator;
begin
  ACalculator := TdxColumnsBoundsCalculator.Create(FControl.DocumentModel.ToDocumentLayoutUnitConverter);
  try
    Result := TdxRectList.Create;
    if ASectionProperties.EqualWidthColumns then
      ACalculator.PopulateEqualWidthColumnsBounds(Result, ABounds, ASectionProperties.ColumnCount, ACalculator.UnitConverter.ToLayoutUnits(ASectionProperties.Space))
    else
      ACalculator.PopulateColumnsBounds(Result, ABounds, ASectionProperties.ColumnInfoCollection);
  finally
    ACalculator.Free;
  end;
end;

procedure TdxHorizontalRulerViewInfo.CalculateSpaceAreaCollection;
var
  ALayoutUnitConverter: TdxDocumentLayoutUnitConverter;
  I: Integer;
  ABounds: TdxRectF;
begin
  if IsMainPieceTable then
  begin
    ALayoutUnitConverter := Control.DocumentModel.LayoutUnitConverter;
    for I := 1 to ActiveAreaCollection.Count - 1 do
    begin
      ABounds.InitSize(ActiveAreaCollection[I - 1].Right, ActiveAreaCollection[I].Top, ActiveAreaCollection[I].Left - ActiveAreaCollection[I - 1].Right, ActiveAreaCollection[I].Height);
      SpaceAreaCollection.Add(ABounds);
      DisplaySpaceAreaCollection.Add(ALayoutUnitConverter.LayoutUnitsToPixels(TRect.Round(ABounds), Control.DpiX, Control.DpiY).ToRectF);
    end;
  end;
end;

procedure TdxHorizontalRulerViewInfo.CalculateTableHotZone;
var
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ATableViewInfo: TdxTableViewInfo;
  ATableHotZone: TdxTableHotZone;
  ACell: TdxTableCell;
  ABounds: TRect;
  I, AStartColumnIndex, ABorderLayoutHorizontalPositionIndex: Integer;
begin
  if not DocumentModel.DocumentCapabilities.TablesAllowed then
    Exit;
  if TableCellViewInfo = nil then
    Exit;
  AddTableLeftBorderHotZone;
  ARow := TableCellViewInfo.Cell.Row;
  ACells := ARow.Cells;
  ATableViewInfo := TableCellViewInfo.TableViewInfo;
  for I := 0 to ACells.Count - 1 do
  begin
    ACell := ACells[I];
    AStartColumnIndex := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ACell, True);
    ABorderLayoutHorizontalPositionIndex := AStartColumnIndex + ACell.LayoutProperties.ColumnSpan;
    ABounds := TRect.CreateSize(GetTableHotZoneLeft(ABorderLayoutHorizontalPositionIndex), Trunc(ActiveAreaCollection[CurrentActiveAreaIndex].Top), 0, Trunc(ActiveAreaCollection[CurrentActiveAreaIndex].Height));
    ATableHotZone := TdxTableHotZone.Create(Control, ABounds, ABorderLayoutHorizontalPositionIndex, ATableViewInfo, ACell);
    HotZones.Add(ATableHotZone);
  end;
end;

procedure TdxHorizontalRulerViewInfo.AddTableLeftBorderHotZone;
var
  ATableViewInfo: TdxTableViewInfo;
  ABounds: TRect;
  ATableLeftBorderHotZone: TdxTableLeftBorderHotZone;
begin
  ATableViewInfo := TableCellViewInfo.TableViewInfo;
  ABounds := TRect.CreateSize(GetTableHotZoneLeft(0), Trunc(ActiveAreaCollection[CurrentActiveAreaIndex].Top), 0, Trunc(ActiveAreaCollection[CurrentActiveAreaIndex].Height));
  ATableLeftBorderHotZone := TdxTableLeftBorderHotZone.Create(Control, ABounds, 0, ATableViewInfo, TableCellViewInfo.Cell.Row.Cells[0]);
  HotZones.Add(ATableLeftBorderHotZone);
end;

function TdxHorizontalRulerViewInfo.GetTableHotZoneLeft(ATableAlignedPositionsIndex: Integer): Integer;
begin
  Result := Trunc((FTableAlignedPositions[ATableAlignedPositionsIndex] + GetAdditionalParentCellIndent(False)) * Control.ZoomFactor + ActiveAreaCollection[CurrentActiveAreaIndex].Left);
end;

function TdxHorizontalRulerViewInfo.ApplyZoomFactor(const AArea: TRect): TRect;
begin
  Result := AArea;
  Result.Left := Ceil(AArea.Left * Control.ZoomFactor);
  Result.Width := Ceil(AArea.Width * Control.ZoomFactor);
end;

procedure TdxHorizontalRulerViewInfo.AddTickNumber(X: Single; AValue: Integer);
var
  ANumber: string;
  ASize: TSize;
  Y: Single;
  ABounds: TdxRectF;
begin
  ANumber := IntToStr(AValue);
  ASize := MeasureString(ANumber);
  Y := ClientBounds.Top + Control.Painter.VerticalTextPaddingTop;
  ABounds.InitSize(X - ASize.Width / 2.0, Y, ASize.Width, ASize.Height);
  RulerTickmarks.Add(TdxRulerTickmarkNumber.Create(FControl, ABounds, ANumber));
end;

procedure TdxHorizontalRulerViewInfo.CalculateTabsBounds;
var
  ATabsController: TdxTabsController;
begin
  ATabsController := TdxTabsController.Create;
  try
    ATabsController.BeginParagraph(Paragraph);
    CalculateTabsBoundsCore(ATabsController);
  finally
    ATabsController.Free;
  end;
end;

procedure TdxHorizontalRulerViewInfo.CalculateTabsBoundsCore(ATabsController: TdxTabsController);
var
  AAdditionalCellIndent, ALayoutPosition, AModelPosition, APos: Integer;
  ATabInfo: TdxTabInfo;
begin
  AAdditionalCellIndent := GetAdditionalCellIndent(True);
  ALayoutPosition := Round(ClientBounds.Left - (ActiveAreaCollection[CurrentActiveAreaIndex].Left / Control.ZoomFactor));
  AModelPosition := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(ALayoutPosition);
  ATabInfo := ATabsController.GetNextTab(AModelPosition);
  APos := GetRulerLayoutPosition(ATabInfo.Position) + AAdditionalCellIndent;
  while (APos > ClientBounds.Left) and (APos < ClientBounds.Right) do
  begin
    if ATabInfo.IsDefault then
      AddDefaultTab(APos)
    else
      AddTabHotZone(ATabInfo, APos);
    ATabInfo := ATabsController.GetNextTab(ATabInfo.Position);
    APos := GetRulerLayoutPosition(ATabInfo.Position) + AAdditionalCellIndent;
  end;
end;

function TdxHorizontalRulerViewInfo.GetAdditionalCellIndent(ACellViewInfo: TdxTableCellViewInfo; AApplyZoomFactor: Boolean): Integer;
begin
  Result := 0;
  while ACellViewInfo <> nil do
  begin
    Inc(Result, ACellViewInfo.TextLeft);
    Dec(Result, ACellViewInfo.TextOffset);
    ACellViewInfo := ACellViewInfo.TableViewInfo.ParentTableCellViewInfo;
  end;
  if AApplyZoomFactor then
    Result := Round(Result * Control.ZoomFactor);
end;

function TdxHorizontalRulerViewInfo.GetAdditionalCellIndent(AApplyZoomFactor: Boolean): Integer;
begin
  Result := GetAdditionalCellIndent(TableCellViewInfo, AApplyZoomFactor);
end;

function TdxHorizontalRulerViewInfo.GetAdditionalParentCellIndent(AApplyZoomFactor: Boolean): Integer;
begin
  if TableCellViewInfo = nil then
    Exit(0);
  Result := GetAdditionalCellIndent(TableCellViewInfo.TableViewInfo.ParentTableCellViewInfo, AApplyZoomFactor);
end;

function TdxHorizontalRulerViewInfo.GetRightIndentBasePosition: Single;
begin
  if TableCellViewInfo = nil then
    Result := ActiveAreaCollection[CurrentActiveAreaIndex].Right
  else
    Result := ActiveAreaCollection[CurrentActiveAreaIndex].Left + GetCellTextRight(TableCellViewInfo, True);
end;

function TdxHorizontalRulerViewInfo.GetCellTextRight(ACellViewInfo: TdxTableCellViewInfo; AApplyZoomFactor: Boolean): Integer;
begin
  Result := GetAdditionalCellIndent(ACellViewInfo, False) + ACellViewInfo.TextWidth;
  if AApplyZoomFactor then
    Result := Trunc(Result * Control.ZoomFactor);
end;

procedure TdxHorizontalRulerViewInfo.AddTabHotZone(const ATabInfo: TdxTabInfo; APos: Integer);
begin
  FDefaultTabsCollection.Clear;
  FDisplayDefaultTabsCollection.Clear;
  if DocumentModel.DocumentCapabilities.ParagraphTabsAllowed and Control.RichEditControl.InnerControl.Options.HorizontalRuler.ShowTabs then
    AddTabHotZoneCore(ATabInfo, APos);
end;

function TdxHorizontalRulerViewInfo.AddTabHotZoneCore(const ATabInfo: TdxTabInfo; APos: Integer): TdxTabHotZone;
begin
  Result := CreateTabHotZone(ATabInfo, GetTabBounds(APos));
  HotZones.Add(Result);
end;

function TdxHorizontalRulerViewInfo.CreateTabHotZone(const ATabInfo: TdxTabInfo; const ABounds: TRect): TdxTabHotZone;
begin
  case ATabInfo.Alignment of
    TdxTabAlignmentType.Center:
      Exit(TdxCenterTabHotZone.Create(Control, ABounds, ATabInfo));
    TdxTabAlignmentType.Right:
      Exit(TdxRightTabHotZone.Create(Control, ABounds, ATabInfo));
    TdxTabAlignmentType.Decimal:
      Exit(TdxDecimalTabHotZone.Create(Control, ABounds, ATabInfo));
    else
      Exit(TdxLeftTabHotZone.Create(Control, ABounds, ATabInfo));
  end;
end;

function TdxHorizontalRulerViewInfo.GetTabBounds(APosition: Integer): TRect;
begin
  Result := TRect.CreateSize(APosition, ClientBounds.Top + ClientBounds.Height div 2, 10, ClientBounds.Height div 2);
end;

procedure TdxHorizontalRulerViewInfo.AddDefaultTab(APos: Integer);
var
  AAreaBounds: TdxRectF;
  ABounds: TRect;
begin
  AAreaBounds := ActiveAreaCollection[CurrentActiveAreaIndex];
  if (APos > AAreaBounds.Left) and (APos < AAreaBounds.Right) then
  begin
    ABounds := FDefaultTabBounds;
    ABounds.X := APos;
    FDefaultTabsCollection.Add(ABounds);
    FDisplayDefaultTabsCollection.Add(Control.DocumentModel.LayoutUnitConverter.LayoutUnitsToPixels(ABounds, Control.DpiX, Control.DpiY));
  end;
end;

function TdxHorizontalRulerViewInfo.GetRulerLayoutPosition(AModelPosition: Integer): Integer;
var
  ALayoutPosition: Integer;
begin
  ALayoutPosition := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AModelPosition);
  Result := Round(ALayoutPosition * Control.ZoomFactor + ActiveAreaCollection[CurrentActiveAreaIndex].Left);
end;

function TdxHorizontalRulerViewInfo.GetRulerModelPosition(ALayoutPosition: Integer): Integer;
var
  K: Integer;
begin
  K := Trunc((ALayoutPosition - ActiveAreaCollection[CurrentActiveAreaIndex].Left) / Control.ZoomFactor);
  Result := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(K);
end;

function TdxHorizontalRulerViewInfo.GetRulerModelPositionRelativeToTableCellViewInfo(ALayoutPosition: Integer): Integer;
var
  AAdditionalIndent, K: Integer;
begin
  AAdditionalIndent := GetAdditionalCellIndent(False);
  K := Trunc((ALayoutPosition - ActiveAreaCollection[CurrentActiveAreaIndex].Left) / Control.ZoomFactor) - AAdditionalIndent;
  Result := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(K);
end;

procedure TdxHorizontalRulerViewInfo.CalculateIndentElementsBounds;
var
  AAdditionalIndent, AFirstIndentPosition, ALeftIndentPosition, ARightIndentPosition, ATabWidth, ARightBorder, ALeftBorder: Integer;
begin
  if not DocumentModel.DocumentCapabilities.ParagraphFormattingAllowed then
    Exit;
  AAdditionalIndent := GetAdditionalCellIndent(True);
  AFirstIndentPosition := GetRulerLayoutPosition(GetIndentInModelUnits(Paragraph)) + AAdditionalIndent;
  ALeftIndentPosition := GetRulerLayoutPosition(Paragraph.LeftIndent) + AAdditionalIndent;
  ARightIndentPosition := GetRightIndentLayoutPosition(Paragraph.RightIndent);
  ATabWidth := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(FDefaultTabWidth);
  if ARightIndentPosition < ALeftIndentPosition then
    ARightIndentPosition := ALeftIndentPosition + Control.DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(MinDistanceBetweenIndents);
  ARightBorder := ARightIndentPosition - ATabWidth;
  ALeftBorder := Math.Max(ALeftIndentPosition, AFirstIndentPosition) + ATabWidth;
  if ALeftBorder > ARightIndentPosition then
    ALeftBorder := ARightIndentPosition;
  if Control.RichEditControl.InnerControl.Options.HorizontalRuler.ShowLeftIndent then
    AddLeftIndentHotZone(Paragraph, ALeftIndentPosition, ARightBorder, AFirstIndentPosition);
  if Control.RichEditControl.InnerControl.Options.HorizontalRuler.ShowRightIndent then
    AddRightIndentHotZone(Paragraph, ARightIndentPosition, ALeftBorder);
end;

function TdxHorizontalRulerViewInfo.GetRightIndentLayoutPosition(AModelPosition: Integer): Integer;
var
  ALayoutPosition: Integer;
  ARightPosition: Single;
begin
  ARightPosition := GetRightIndentBasePosition;
  ALayoutPosition := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AModelPosition);
  Result := Trunc(ARightPosition - ALayoutPosition * Control.ZoomFactor);
end;

function TdxHorizontalRulerViewInfo.GetRightIndentModelPosition(ALayoutPosition: Integer): Integer;
var
  ALayoutRightIndent: Integer;
  ARightPosition: Single;
begin
  ARightPosition := GetRightIndentBasePosition;
  ALayoutRightIndent := Trunc((ARightPosition - ALayoutPosition) / Control.ZoomFactor);
  Result := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(ALayoutRightIndent);
end;

function TdxHorizontalRulerViewInfo.GetIndentInModelUnits(AParagraph: TdxParagraph): Integer;
var
  AIndent: Integer;
begin
  case AParagraph.FirstLineIndentType of
    TdxParagraphFirstLineIndent.Indented:
      AIndent := AParagraph.FirstLineIndent;
    TdxParagraphFirstLineIndent.Hanging:
      AIndent := -AParagraph.FirstLineIndent;
    else
      AIndent := 0;
  end;
  Result := AIndent + AParagraph.LeftIndent;
end;

procedure TdxHorizontalRulerViewInfo.AddLeftIndentHotZone(AParagraph: TdxParagraph; ALeftIndent: Integer; ARightBorder: Integer; AFirstIndent: Integer);
var
  AHeight: Integer;
  AFirstLineHotZone: TdxFirstLineIndentHotZone;
  ABounds: TRect;
  ABottomHotZone: TdxLeftBottomHotZone;
begin
  AHeight := ClientBounds.Height div 2;
  AFirstLineHotZone := AddFirstLineHotZone(AParagraph, ARightBorder, AFirstIndent, AHeight);
  ABounds := TRect.CreateSize(ALeftIndent, ClientBounds.Top + AHeight, 0, AHeight);
  ABottomHotZone := TdxLeftBottomHotZone.Create(Control, ABounds, AParagraph.LeftIndent, ARightBorder, AFirstLineHotZone);
  ABottomHotZone.AddLeftHotZone(ABounds);
  HotZones.Add(ABottomHotZone);
end;

function TdxHorizontalRulerViewInfo.AddFirstLineHotZone(AParagraph: TdxParagraph; ARightBorder: Integer; AFirstIndent: Integer; AHeight: Integer): TdxFirstLineIndentHotZone;
var
  ABounds: TRect;
  AFirstLineHotZone: TdxFirstLineIndentHotZone;
begin
  ABounds := TRect.CreateSize(AFirstIndent, ClientBounds.Top, 0, AHeight);
  AFirstLineHotZone := TdxFirstLineIndentHotZone.Create(Control, ABounds, GetIndentInModelUnits(AParagraph), AParagraph.LeftIndent);
  AFirstLineHotZone.RightBorder := ARightBorder;
  HotZones.Add(AFirstLineHotZone);
  Result := AFirstLineHotZone;
end;

procedure TdxHorizontalRulerViewInfo.AddRightIndentHotZone(AParagraph: TdxParagraph; ARightIndent: Integer; ALeftBorder: Integer);
var
  AHeight: Integer;
  ABounds: TRect;
  AHotZone: TdxRightIndentHotZone;
begin
  AHeight := ClientBounds.Height div 2;
  ABounds := TRect.CreateSize(ARightIndent, ClientBounds.Top + AHeight, 0, AHeight);
  AHotZone := TdxRightIndentHotZone.Create(Control, ABounds, AParagraph.RightIndent);
  AHotZone.LeftBorder := ALeftBorder;
  HotZones.Add(AHotZone);
end;

procedure TdxHorizontalRulerViewInfo.CalculateSectionSeparator;
var
  AIsEqualWidthColumns: Boolean;
  I: Integer;
begin
  if not IsMainPieceTable then
    Exit;
  if not DocumentModel.DocumentCapabilities.SectionsAllowed then
    Exit;

  AIsEqualWidthColumns := SectionProperties.EqualWidthColumns;
  for I := 0 to ActiveAreaCollection.Count - 1 do
  begin
    if Control.RichEditControl.InnerControl.ActiveViewType <> TdxRichEditViewType.Draft then
    begin
      AddRightColumnResizer(I);
      AddLeftColumnResizer(I);
    end
    else
    begin
      if I <> ActiveAreaCollection.Count - 1 then
        AddRightColumnResizer(I);
      if I <> 0 then
        AddLeftColumnResizer(I);
    end;
    if not AIsEqualWidthColumns and (I < ActiveAreaCollection.Count - 1) then
      AddMiddleColumnResizer(I);
  end;
end;

procedure TdxHorizontalRulerViewInfo.AddMiddleColumnResizer(AIndex: Integer);
var
  X: Integer;
  AHotZone: TdxMiddleColumnResizerHotZone;
begin
  X := Trunc(ActiveAreaCollection[AIndex].Right + (ActiveAreaCollection[AIndex + 1].Left - ActiveAreaCollection[AIndex].Right) / 2);
  AHotZone := TdxMiddleColumnResizerHotZone.Create(Control, TRect.CreateSize(X, ClientBounds.Top, 0, 0), AIndex);
  HotZones.Add(AHotZone);
end;

procedure TdxHorizontalRulerViewInfo.AddLeftColumnResizer(AIndex: Integer);
var
  ABounds: TRect;
  AHotZone: TdxLeftColumnResizerHotZone;
begin
  ABounds := TRect.CreateSize(Trunc(ActiveAreaCollection[AIndex].Left), ClientBounds.Top, 0, 0);
  AHotZone := TdxLeftColumnResizerHotZone.Create(Control, ABounds, AIndex);
  if AIndex = 0 then
    AHotZone.Visible := False;
  HotZones.Add(AHotZone);
end;

procedure TdxHorizontalRulerViewInfo.AddRightColumnResizer(AIndex: Integer);
var
  ABounds: TRect;
  AHotZone: TdxRightColumnResizerHotZone;
begin
  ABounds := TRect.CreateSize(Trunc(ActiveAreaCollection[AIndex].Right), ClientBounds.Top, 0, 0);
  AHotZone := TdxRightColumnResizerHotZone.Create(Control, ABounds, AIndex);
  if AIndex = ActiveAreaCollection.Count - 1 then
    AHotZone.Visible := False;
  HotZones.Add(AHotZone);
end;

function TdxHorizontalRulerViewInfo.GetDefaultTabWidth: Integer;
begin
  Result := Trunc(Control.DocumentModel.DocumentProperties.DefaultTabWidth * Control.ZoomFactor);
end;

procedure TdxHorizontalRulerViewInfo.CalculateTabTypeToggle;
var
  ABounds: TRect;
  ASize: TSize;
begin
  ABounds := FControl.CalculateTabTypeToggleBackgroundBounds;
  FTabTypeToggleBackground := TdxTabTypeToggleBackgroundHotZone.Create(FControl, ABounds);
  ASize := FControl.GetTabTypeToggleActiveAreaSize;
  ABounds.InitSize(ABounds.Left + (ABounds.Width - ASize.Width) div 2, ABounds.Top + (ABounds.Height - ASize.Height) div 2, ASize.Width, ASize.Height);
  FTabTypeToggle := TdxTabTypeToggleHotZone.Create(FControl, ABounds);
end;

function TdxHorizontalRulerViewInfo.CalculateHalfTickBounds: TdxRectF;
var
  ASize: TdxSizeF;
  AHalfTickWidth: Integer;
  Y: Single;
begin
  ASize := TextSize.ToSizeF;
  ASize.Height := ASize.Height / 2.0;
  AHalfTickWidth := 1;
  Y := (ClientBounds.Top + inherited Control.Painter.VerticalTextPaddingTop + ASize.Height / 2.0);
  Result.InitSize(0, Y, AHalfTickWidth, ASize.Height);
end;

function TdxHorizontalRulerViewInfo.CalculateQuarterTickBounds: TdxRectF;
var
  AQuarterTickWidth: Integer;
begin
  AQuarterTickWidth := 1;
  Result.InitSize(0, HalfTickBounds.Top + HalfTickBounds.Height / 4.0, AQuarterTickWidth, HalfTickBounds.Height / 2.0);
end;

function TdxHorizontalRulerViewInfo.CalculateDefaultTabBounds: TRect;
var
  ATabWidth, APaddingBottom: Integer;
  AHeight, Y: Single;
begin
  ATabWidth := 1;
  APaddingBottom := Control.Painter.PaddingBottom;
  AHeight := APaddingBottom / 2.0;
  Y := ClientBounds.Bottom + (APaddingBottom - AHeight) / 2.0;
  Result.InitSize(0, Trunc(Y), ATabWidth, Trunc(AHeight));
end;

procedure TdxHorizontalRulerViewInfo.SortingHotZones;
var
  AComparer: TdxRulerHotZonesComparer;
begin
  AComparer := TdxRulerHotZonesComparer.Create;
  try
    HotZones.Sort(AComparer);
  finally
    AComparer.Free;
  end;
end;

{ TdxRulerHotZonesComparer }

function TdxRulerHotZonesComparer.Compare(const AHotZone, ANextHotZone: TdxRulerHotZone): Integer;
begin
  if AHotZone.Weight > ANextHotZone.Weight then
    Result := -1
  else
    if AHotZone.Weight < ANextHotZone.Weight then
      Result := 1
    else
      Result := 0;
end;

{ TdxVerticalRulerViewInfo }

constructor TdxVerticalRulerViewInfo.Create(const AControl: TdxVerticalRulerControl;
  ASectionProperties: TdxSectionProperties; AIsMainPieceTable: Boolean; ATableVerticalPositions: TdxSortedList<Integer>;
  ATableCellViewInfo: TdxTableCellViewInfo);
begin
  inherited Create(AControl, ASectionProperties, AIsMainPieceTable);
  FControl := AControl;
  FTableVerticalPositions := ATableVerticalPositions;
  FTableCellViewInfo := ATableCellViewInfo;
end;

destructor TdxVerticalRulerViewInfo.Destroy;
begin
  FreeAndNil(FTableVerticalPositions);
  inherited Destroy;
end;

procedure TdxVerticalRulerViewInfo.Initialize;
begin
  CalculateClientBounds(FControl);
  inherited Initialize;
  CalculateBounds;
  CalculateSectionSeparator;
  CalculateTableHotZone;
end;

function TdxVerticalRulerViewInfo.CalculateClientBounds(const AControl: TdxRulerControlBase): TRect;
var
  APageClientBounds: TRect;
  ACaretPosition: TdxCaretPosition;
  AClientWidth, X: Integer;
begin
  IsReady := False;
  ACaretPosition := AControl.RichEditControl.InnerControl.ActiveView.CaretPosition;
  if not ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Page) then
    Exit(TRect.CreateSize(0, 0, 100, 100));
  IsReady := True;
  APageClientBounds := ACaretPosition.PageViewInfo.ClientBounds;
  APageClientBounds.MoveToTop(0);
  if AControl.RichEditControl.InnerControl.ActiveViewType = TdxRichEditViewType.PrintLayout then
    APageClientBounds.Height := Round(ToDocumentLayoutUnitConverter.ToLayoutUnits(Math.Min(MaxRulerDimensionInModelUnits, SectionProperties.PageHeight)) * AControl.ZoomFactor)
  else
    APageClientBounds.Height := Round(ToDocumentLayoutUnitConverter.ToLayoutUnits(Math.Min(MaxRulerDimensionInModelUnits, SectionProperties.PageHeight - SectionProperties.TopMargin)) * AControl.ZoomFactor);
  AClientWidth := TextSize.Height + 2 * AControl.Painter.VerticalTextPaddingBottom;
  X := AControl.Painter.PaddingTop;
  Result.InitSize(X, APageClientBounds.Top, AClientWidth, APageClientBounds.Height);
end;

procedure TdxVerticalRulerViewInfo.CalculateBounds;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  AWidth: Integer;
begin
  AUnitConverter := FControl.DocumentModel.LayoutUnitConverter;
  AWidth := DisplayClientBounds.Width + AUnitConverter.LayoutUnitsToPixels(FControl.Painter.PaddingTop + FControl.Painter.PaddingBottom, Control.DpiX);
  Control.Width := AWidth;
end;

function TdxVerticalRulerViewInfo.GetRulerSize: Integer;
begin
  Result := Control.Painter.CalculateTotalRulerSize(TextSize.Height);
end;

function TdxVerticalRulerViewInfo.CalculateHalfTickBounds: TdxRectF;
var
  ASize: TdxSizeF;
  AHalfTickHeight: Integer;
  X: Single;
begin
  ASize := TextSize.ToSizeF;
  ASize.Height := ASize.Height / 2.0;
  AHalfTickHeight := 1;
  X := ClientBounds.Left + inherited Control.Painter.VerticalTextPaddingBottom + ASize.Height / 2.0;
  Result.InitSize(X, 0, ASize.Height, AHalfTickHeight);
end;

function TdxVerticalRulerViewInfo.CalculateQuarterTickBounds: TdxRectF;
var
  AQuarterTickHeight: Integer;
begin
  AQuarterTickHeight := 1;
  Result.InitSize(HalfTickBounds.Left + HalfTickBounds.Width / 4.0, 0, HalfTickBounds.Width / 2.0, AQuarterTickHeight);
end;

procedure TdxVerticalRulerViewInfo.CalculateActiveAreaCollection;
var
  AActiveBounds: TRect;
  ALayoutUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AActiveBounds := GetActiveBounds;
  AActiveBounds := TRect.CreateSize(ClientBounds.Left, AActiveBounds.Top, ClientBounds.Width, AActiveBounds.Height);
  AActiveBounds := ApplyZoomFactor(AActiveBounds);
  AActiveBounds.Offset(0, ClientBounds.Left);
  ActiveAreaCollection.Add(AActiveBounds.ToRectF);
  ALayoutUnitConverter := FControl.DocumentModel.LayoutUnitConverter;
  DisplayActiveAreaCollection.Add(ALayoutUnitConverter.LayoutUnitsToPixels(AActiveBounds, Control.DpiX, Control.DpiY).ToRectF);
end;

function TdxVerticalRulerViewInfo.GetActiveBounds: TRect;
var
  APage: TdxPage;
begin
  if not IsMainPieceTable and (Control.RichEditControl.InnerControl.ActiveView.CaretPosition.Update(TdxDocumentLayoutDetailsLevel.PageArea)) then
  begin
    APage := Control.RichEditControl.InnerControl.ActiveView.CaretPosition.LayoutPosition.Page;
    if (APage.Header <> nil) and (APage.Header.PieceTable = Control.DocumentModel.Selection.PieceTable) then
      Exit(APage.Header.Bounds)
    else
      if (APage.Footer <> nil) and (APage.Footer.PieceTable = Control.DocumentModel.Selection.PieceTable) then
        Exit(APage.Footer.Bounds);
  end;
  Result := GetPageBounds;
end;

function TdxVerticalRulerViewInfo.GetPageBounds: TRect;
var
  AControl: IdxRichEditControl;
  APageCalculator, AActiveViewPageCalculator: TdxPageBoundsCalculator;
  APrintLayoutViewPageClientBounds, AActiveViewPageClientBounds: TRect;
  ADeltaY, ADeltaHeight: Integer;
begin
  AControl := Control.RichEditControl;
  APageCalculator := TdxPageBoundsCalculator.Create(Control.DocumentModel.ToDocumentLayoutUnitConverter);
  try
    APrintLayoutViewPageClientBounds := CalculatePageClientBounds(APageCalculator, SectionProperties);
  finally
    APageCalculator.Free;
  end;
  AActiveViewPageCalculator := AControl.InnerControl.Formatter.DocumentFormatter.Controller.PageController.PageBoundsCalculator;
  AActiveViewPageClientBounds := CalculatePageClientBounds(AActiveViewPageCalculator, SectionProperties);
  ADeltaY := AActiveViewPageClientBounds.Top - APrintLayoutViewPageClientBounds.Top;
  ADeltaHeight := AActiveViewPageClientBounds.Height - APrintLayoutViewPageClientBounds.Height;
  APrintLayoutViewPageClientBounds.MoveToTop(APrintLayoutViewPageClientBounds.Top + ADeltaY);
  APrintLayoutViewPageClientBounds.Height := APrintLayoutViewPageClientBounds.Height + ADeltaHeight;
  Result := APrintLayoutViewPageClientBounds;
end;

procedure TdxVerticalRulerViewInfo.CalculateSpaceAreaCollection;
begin
end;

procedure TdxVerticalRulerViewInfo.CalculateTableHotZone;
var
  I: Integer;
  ABounds: TRect;
  AHotZone: TdxVerticalTableHotZone;
begin
  if not DocumentModel.DocumentCapabilities.TablesAllowed then
    Exit;
  for I := 0 to FTableVerticalPositions.Count - 1 do
  begin
    ABounds := TRect.CreateSize(Trunc(ActiveAreaCollection[0].Left), Trunc(FTableVerticalPositions[I] * Control.ZoomFactor), Trunc(ActiveAreaCollection[0].Width), Trunc(ActiveAreaCollection[0].Width * Control.ZoomFactor));
    AHotZone := TdxVerticalTableHotZone.Create(Control, ABounds, FTableCellViewInfo.TableViewInfo, I);
    HotZones.Add(AHotZone);
  end;
end;

function TdxVerticalRulerViewInfo.ApplyZoomFactor(const AArea: TRect): TRect;
begin
  Result := AArea;
  Result.Top := Ceil(AArea.Top * Control.ZoomFactor);
  Result.Height := Ceil(AArea.Height * Control.ZoomFactor);
end;

procedure TdxVerticalRulerViewInfo.AddTickNumber(Y: Single; AValue: Integer);
var
  ANumber: string;
  ASize: TdxSizeF;
  X: Single;
  ABounds: TdxRectF;
begin
  ANumber := IntToStr(AValue);
  ASize := MeasureString(ANumber).ToSizeF;
  X := ClientBounds.Left + inherited Control.Painter.VerticalTextPaddingBottom;
  ABounds.InitSize(X, Y - ASize.Height / 2.0, ASize.Height, ASize.Width);
  RulerTickmarks.Add(TdxRulerTickmarkNumber.Create(FControl, ABounds, ANumber));
end;

procedure TdxVerticalRulerViewInfo.CalculateSectionSeparator;
var
  I: Integer;
begin
  if not IsMainPieceTable then
    Exit;
  if not DocumentModel.DocumentCapabilities.SectionsAllowed then
    Exit;
  for I := 0 to ActiveAreaCollection.Count - 1 do
  begin
    AddTopSectionResizer(I);
    AddBottomSectionResizer(I);
  end;
end;

procedure TdxVerticalRulerViewInfo.AddBottomSectionResizer(AIndex: Integer);
var
  ABounds: TRect;
  AHotZone: TdxSectionBottomResizerHotZone;
begin
  ABounds := TRect.CreateSize(ClientBounds.Left, Trunc(ActiveAreaCollection[AIndex].Bottom), ClientBounds.Width, ClientBounds.Width);
  AHotZone := TdxSectionBottomResizerHotZone.Create(Control, ABounds, AIndex);
  HotZones.Add(AHotZone);
end;

procedure TdxVerticalRulerViewInfo.AddTopSectionResizer(AIndex: Integer);
var
  ABounds: TRect;
  AHotZone: TdxSectionTopResizerHotZone;
begin
  ABounds := TRect.CreateSize(ClientBounds.Left, Trunc(ActiveAreaCollection[AIndex].Top), ClientBounds.Width, ClientBounds.Width);
  AHotZone := TdxSectionTopResizerHotZone.Create(Control, ABounds, AIndex);
  HotZones.Add(AHotZone);
end;

function TdxVerticalRulerViewInfo.GetRulerLayoutPosition(AModelPosition: Integer): Integer;
var
  ALayoutPosition: Integer;
begin
  ALayoutPosition := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(AModelPosition);
  Result := Trunc(ALayoutPosition * Control.ZoomFactor + ActiveAreaCollection[CurrentActiveAreaIndex].Top);
end;

function TdxVerticalRulerViewInfo.GetRulerModelPosition(ALayoutPosition: Integer): Integer;
var
  K: Integer;
begin
  K := Trunc((ALayoutPosition - ActiveAreaCollection[CurrentActiveAreaIndex].Top) / Control.ZoomFactor);
  Result := Control.DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(K);
end;

{ TdxRulerControlBase }

constructor TdxRulerControlBase.Create(ARichEditControl: TdxCustomRichEditControl);
begin
  inherited Create(ARichEditControl);
  FMinWidth := 5;
  FMinHeight := 5;
  BoundsRect := TRect.CreateSize(0, 0, FMinWidth, FMinWidth);
  FPageIndex := -1;
  FColumnIndex := -1;
  FParagraphIndex := -1;
  TabStop := False;
  FRichEditControl := ARichEditControl;
  FDpiX := ARichEditControl.DpiX;
  FDpiY := ARichEditControl.DpiY;
  LookAndFeel.MasterLookAndFeel := ARichEditControl.LookAndFeel;
  UpdateSkinInfo;
  FTickMarkFont := CreateTickMarkFont;
  FOrientation  := CreateOrientation;
  FPainter      := CreatePainter;
  FDirtyViewInfos := TdxFastObjectList.Create;
  SetViewInfo(CreateViewInfo);
  InitializeMouseHandler;
end;

destructor TdxRulerControlBase.Destroy;
begin
  FreeAndNil(FMouseController);
  FreeAndNil(FPainter);
  CheckReleaseDirtyViewInfos;
  FreeAndNil(FDirtyViewInfos);
  FreeAndNil(FViewInfo);
  FreeAndNil(FTickMarkFont);
  FreeAndNil(FOrientation);
  inherited Destroy;
end;

procedure TdxRulerControlBase.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.style := {CS_VREDRAW + CS_HREDRAW +} CS_DBLCLKS;
end;

function TdxRulerControlBase.CreateTickMarkFont: TdxGPFont;
begin
  Result := TdxGPFont.Create('Arial', TickMarkFontSize * InternalScaleFactor.Numerator / InternalScaleFactor.Denominator, FontStyleRegular);
end;

procedure TdxRulerControlBase.DoCancelMode;
begin
  inherited DoCancelMode;
  MouseController.DoCancelMode;
end;

procedure TdxRulerControlBase.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;

function TdxRulerControlBase.GetZoomFactor: Single;
begin
  Result := RichEditControl.ActiveView.ZoomFactor * InternalScaleFactor.Numerator / InternalScaleFactor.Denominator;
end;

function TdxRulerControlBase.GetRichEditControl: IdxRichEditControl;
begin
  if not Supports(RichEditControl, IdxRichEditControl, Result) then
    Result := nil;
end;

function TdxRulerControlBase.GetIgnoreChildren: Boolean;
begin
  Result := False;
end;

function TdxRulerControlBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := RichEditControl.DocumentModel;
end;

procedure TdxRulerControlBase.Reset;
begin
  InvalidateRulerViewInfo;
end;

procedure TdxRulerControlBase.CheckReleaseDirtyViewInfos;
begin
  FDirtyViewInfos.Clear;
end;

procedure TdxRulerControlBase.SetViewInfo(ANewViewInfo: TdxRulerViewInfoBase);
begin
  if (FViewInfo <> nil) and (ANewViewInfo <> FViewInfo) then
    FDirtyViewInfos.Add(FViewInfo);
  FViewInfo := ANewViewInfo;
end;

procedure TdxRulerControlBase.InvalidateRulerViewInfo;
begin
  if not RichEditControl.ActiveView.CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column) then
    Exit;
  if IsVisible then
  begin
    SetViewInfo(CreateViewInfo);
    FViewInfo.Initialize;
  end;
  Repaint;
end;

procedure TdxRulerControlBase.DoPaint;
begin
  if (Painter <> nil) and IsVisible then
    DrawRuler(Graphics);
end;

procedure TdxRulerControlBase.ScaleFactorChanged;
begin
  inherited ScaleFactorChanged;
  RecreateTickMarkFont;
  InvalidateRulerViewInfo;
  RecreatePainter;
end;

procedure TdxRulerControlBase.DrawRuler(AGraphics: TdxGraphics);
begin
  Painter.DrawBackground(AGraphics);
end;

function TdxRulerControlBase.CalculateParagraphIndex: TdxParagraphIndex;
var
  ASelection: TdxSelection;
  ALogPosition: TdxDocumentLogPosition;
  AParagraphIndex: TdxParagraphIndex;
begin
  ASelection := DocumentModel.Selection;
  ALogPosition := ASelection.VirtualEnd;
  AParagraphIndex := DocumentModel.ActivePieceTable.FindParagraphIndex(ALogPosition);
  if ALogPosition > ASelection.Start then
  begin
    if ALogPosition = DocumentModel.ActivePieceTable.Paragraphs[AParagraphIndex].LogPosition then
      AParagraphIndex := Max(0, AParagraphIndex - 1);
  end;
  Result := AParagraphIndex;
end;

function TdxRulerControlBase.GetSection(APieceTable: TdxPieceTable): TdxSection;
var
  AParagraphIndex: TdxParagraphIndex;
  ASectionIndex: TdxSectionIndex;
  ATextBoxContent: TdxTextBoxContentType;
  ACaretPosition: TdxCaretPosition;
  ASection: TdxSection;
begin
  AParagraphIndex := CalculateParagraphIndex;
  if APieceTable.IsMain then
  begin
    ASectionIndex := DocumentModel.MainPieceTable.LookupSectionIndexByParagraphIndex(AParagraphIndex);
    Exit(DocumentModel.Sections[ASectionIndex]);
  end
  else
    if APieceTable.IsTextBox then
    begin
      ATextBoxContent := TdxTextBoxContentType(APieceTable.ContentType);
      if ATextBoxContent.AnchorRun.PieceTable.IsMain then
      begin
        ASectionIndex := DocumentModel.MainPieceTable.LookupSectionIndexByParagraphIndex(ATextBoxContent.AnchorRun.Paragraph.Index);
        Exit(DocumentModel.Sections[ASectionIndex]);
      end
      else
      begin
        ACaretPosition := RichEditControl.ActiveView.CaretPosition;
        Exit(ACaretPosition.LayoutPosition.PageArea.Section);
      end;
    end;
  ASection := DocumentModel.GetActiveSection;
  Assert(ASection <> nil);
  Result := ASection;
end;

function TdxRulerControlBase.LayoutUnitsToPixelsH(AValue: Integer): Integer;
begin
  Result := DocumentModel.LayoutUnitConverter.LayoutUnitsToPixels(AValue, RichEditControl.DpiX);
end;

function TdxRulerControlBase.LayoutUnitsToPixelsV(AValue: Integer): Integer;
begin
  Result := DocumentModel.LayoutUnitConverter.LayoutUnitsToPixels(AValue, RichEditControl.DpiY);
end;

function TdxRulerControlBase.LayoutUnitsToPixels(const AValue: TRect): TRect;
begin
  Result := DocumentModel.LayoutUnitConverter.LayoutUnitsToPixels(AValue, RichEditControl.DpiX, RichEditControl.DpiY);
end;

function TdxRulerControlBase.PixelsToLayoutUnitsH(AValue: Integer): Integer;
begin
  Result := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(AValue, RichEditControl.DpiX);
end;

function TdxRulerControlBase.PixelsToLayoutUnitsV(AValue: Integer): Integer;
begin
  Result := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(AValue, RichEditControl.DpiY);
end;

function TdxRulerControlBase.PixelsToLayoutUnits(const AValue: TRect): TRect;
begin
  Result := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(AValue, RichEditControl.DpiX, RichEditControl.DpiY);
end;

function TdxRulerControlBase.PixelsToLayoutUnits(const AValue: TSize): TSize;
begin
  Result := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(AValue, RichEditControl.DpiX, RichEditControl.DpiY);
end;

procedure TdxRulerControlBase.AddKeyboardService(const AService: IdxKeyboardHandlerService);
begin
  FRichEditControl.AddKeyboardService(AService);
end;

procedure TdxRulerControlBase.RemoveKeyboardService(const AService: IdxKeyboardHandlerService);
begin
  FRichEditControl.RemoveKeyboardService(AService);
end;

procedure TdxRulerControlBase.UpdateSkinInfo;
begin
  LookAndFeelPainter.GetPainterData(FSkinInfo);
end;

procedure TdxRulerControlBase.RecreatePainter;
begin
  FPainter.Free;
  FPainter := CreatePainter;
end;

procedure TdxRulerControlBase.RecreateTickMarkFont;
begin
  FTickMarkFont.Free;
  FTickMarkFont := CreateTickMarkFont;
end;

procedure TdxRulerControlBase.InitializeMouseHandler;
begin
  FMouseController := CreateMouseController;
  FMouseController.Initialize;
end;

function TdxRulerControlBase.CanFocusOnClick(X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TdxRulerControlBase.CanHandleMouseEvent: Boolean;
begin
  Result := not (RichEditControl.IsDesigning or
    not RichEditControl.ActiveView.CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column));
end;

procedure TdxRulerControlBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  try
    if not CanHandleMouseEvent or (Button = mbMiddle) then
      Exit;
    MouseController.MouseDown(Button, Shift, X, Y);
    inherited MouseDown(Button, Shift, X, Y);
  finally
    CheckReleaseDirtyViewInfos;
  end;
end;

procedure TdxRulerControlBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  try
    if not CanHandleMouseEvent then
    begin
      if (Button = mbLeft) and MouseButtonPressed then
      begin
        inherited MouseUp(Button, Shift, X, Y);
        MouseController.Cancel;
      end;
      Exit;
    end;
    MouseController.MouseUp(Button, Shift, X, Y);
    inherited MouseUp(Button, Shift, X, Y);
  finally
    CheckReleaseDirtyViewInfos;
  end;
end;

procedure TdxRulerControlBase.OnLookAndFeelChanged;
begin
  UpdateSkinInfo;
  RecreatePainter;
  InvalidateRulerViewInfo;
end;

procedure TdxRulerControlBase.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  if not CanHandleMouseEvent then
    Exit;
  MouseController.MouseMove(Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

function TdxRulerControlBase.UpdatePageIndex: Boolean;
var
  AIndex: Integer;
begin
  AIndex := FRichEditControl.ActiveView.CaretPosition.LayoutPosition.Page.PageIndex;
  if AIndex = FPageIndex then
    Exit(False);
  FPageIndex := AIndex;
  Result := True;
end;

function TdxRulerControlBase.UpdateColumnIndex: Boolean;
var
  AIndex: Integer;
begin
  AIndex := FRichEditControl.ActiveView.CaretPosition.LayoutPosition.PageArea.Columns.IndexOf(FRichEditControl.ActiveView.CaretPosition.LayoutPosition.Column);
  if AIndex = FColumnIndex then
    Exit(False);
  FColumnIndex := AIndex;
  Result := True;
end;

function TdxRulerControlBase.UpdateParagraphIndex: Boolean;
var
  ANewIndex: TdxParagraphIndex;
begin
  ANewIndex := FindEndParagraphIndex;
  if ANewIndex = FParagraphIndex then
    Exit(False);
  FParagraphIndex := ANewIndex;
  Result := True;
end;

function TdxRulerControlBase.FindEndParagraphIndex: TdxParagraphIndex;
var
  ASelection: TdxSelection;
  AActivePieceTable: TdxPieceTable;
begin
  ASelection := DocumentModel.Selection;
  AActivePieceTable := DocumentModel.ActivePieceTable;
  Result := AActivePieceTable.FindParagraphIndex(ASelection.&End);
  if (ASelection.Length > 0) and (ASelection.&End = AActivePieceTable.Paragraphs[Result].LogPosition) then
    Dec(Result);
end;

function TdxRulerControlBase.GetTableAlignedPositions(ATableViewInfo: TdxTableViewInfo): TdxSortedList<Integer>;
var
  I: Integer;
begin
  Result := TdxSortedList<Integer>.Create;
  for I := 0 to ATableViewInfo.VerticalBorderPositions.AlignedPosition.Count - 1 do
    Result.Add(ATableViewInfo.VerticalBorderPositions.AlignedPosition[I]);
end;

function TdxRulerControlBase.GetTableCellViewInfo(AIsMainPieceTable: Boolean): TdxTableCellViewInfo;
var
  ASelection: TdxSelection;
  ACaretPosition: TdxCaretPosition;
  ALogPosition: TdxDocumentLogPosition;
  ADocumentLayout: TdxDocumentLayout;
  ALayoutPosition: TdxDocumentLayoutPosition;
begin
  ASelection := DocumentModel.Selection;
  ACaretPosition := RichEditControl.ActiveView.CaretPosition;
  if (ASelection.Items.Count = 1) and (DocumentModel.Selection.Length = 0) then
  begin
    if ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.TableCell) and (ACaretPosition.LayoutPosition.TableCell <> nil) then
      Exit(ACaretPosition.LayoutPosition.TableCell);
    Exit(nil);
  end
  else
  begin
    ALogPosition := DocumentModel.Selection.ActiveSelection.Start;
    ADocumentLayout := ACaretPosition.LayoutPosition.DocumentLayout;
    if AIsMainPieceTable then
      ALayoutPosition := TdxDocumentLayoutPosition.Create(ADocumentLayout, ACaretPosition.LayoutPosition.PieceTable, ALogPosition)
    else
    begin
      ALayoutPosition := ACaretPosition.CreateCaretDocumentLayoutPosition;
      ALayoutPosition.SetLogPosition(ALogPosition);
    end;
    try
      if ALayoutPosition.Update(ADocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.TableCell) then
        Exit(ALayoutPosition.TableCell);
    finally
      ALayoutPosition.Free;
    end;
    Result := nil;
  end;
end;

function TdxRulerControlBase.GetTableVerticalPositions(AIsMainPieceTable: Boolean): TdxSortedList<Integer>;
var
  ACaretPosition: TdxCaretPosition;
  ATableViewInfo: TdxTableViewInfo;
  I: Integer;
begin
  ACaretPosition := RichEditControl.ActiveView.CaretPosition;
  Result := TdxSortedList<Integer>.Create;
  if AIsMainPieceTable and ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.TableCell) and
    (ACaretPosition.LayoutPosition.TableCell <> nil) then
  begin
    ATableViewInfo := ACaretPosition.LayoutPosition.TableCell.TableViewInfo;
    for I := 0 to ATableViewInfo.Anchors.Count - 1 do
      Result.Add(ATableViewInfo.Anchors[I].VerticalPosition);
  end;
end;

{ TdxHorizontalRulerControl }

constructor TdxHorizontalRulerControl.Create(ARichEditControl: TdxCustomRichEditControl);
begin
  inherited Create(ARichEditControl);
  FTabTypeIndex := 0;
  Painter.Initialize;
  ViewInfo.Initialize;
  inherited SetBounds(ARichEditControl.ClientBounds.Left, ARichEditControl.ClientBounds.Top,
    ARichEditControl.ClientBounds.Width, GetRulerHeightInPixels);
end;

function TdxHorizontalRulerControl.GetViewInfo: TdxHorizontalRulerViewInfo;
begin
  Result := TdxHorizontalRulerViewInfo(inherited ViewInfo);
end;

procedure TdxHorizontalRulerControl.RecreatePainter;
begin
  inherited RecreatePainter;
  Painter.Initialize;
end;

function TdxHorizontalRulerControl.GetIsVisible: Boolean;
begin
  Result := RichEditControl.CalculateHorizontalRulerVisibility;
end;

procedure TdxHorizontalRulerControl.SetViewInfo(ANewViewInfo: TdxHorizontalRulerViewInfo);
begin
  inherited SetViewInfo(ANewViewInfo);
  ViewInfo.Initialize;
end;

function TdxHorizontalRulerControl.GetPainter: TdxHorizontalRulerPainter;
begin
  Result := TdxHorizontalRulerPainter(inherited Painter);
end;

function TdxHorizontalRulerControl.GetParagraph: TdxParagraph;
var
  AParagraphIndex: TdxParagraphIndex;
begin
  AParagraphIndex := CalculateParagraphIndex;
  Result := DocumentModel.Selection.PieceTable.Paragraphs[AParagraphIndex];
end;

function TdxHorizontalRulerControl.GetColumnIndex(AIsMainPieceTable: Boolean): Integer;
var
  ACaretPosition: TdxCaretPosition;
begin
  ACaretPosition := RichEditControl.ActiveView.CaretPosition;
  if ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column) and AIsMainPieceTable then
    Exit(ACaretPosition.LayoutPosition.PageArea.Columns.IndexOf(ACaretPosition.LayoutPosition.Column));
  Result := 0;
end;

function TdxHorizontalRulerControl.CreateMouseController: TdxRulerMouseController;
begin
  Result := TdxHorizontalRulerMouseHandler.Create(Self);
end;

function TdxHorizontalRulerControl.CreateViewInfo: TdxRulerViewInfoBase;
var
  AIsMainPieceTable: Boolean;
  ASection: TdxSection;
  ASectionProperties: TdxSectionProperties;
begin
  AIsMainPieceTable := DocumentModel.Selection.PieceTable.IsMain;
  ASection := GetSection(DocumentModel.Selection.PieceTable);
  ASectionProperties := TdxSectionProperties.Create(ASection);
  Result := CreateViewInfoCore(ASectionProperties, AIsMainPieceTable);
end;

function TdxHorizontalRulerControl.CreateViewInfoCore(ASectionProperties: TdxSectionProperties;
  AIsMainPieceTable: Boolean): TdxHorizontalRulerViewInfo;
var
  AParagraph: TdxParagraph;
  AColumnIndex: Integer;
  ATableCellViewInfo: TdxTableCellViewInfo;
  ATableAlignedPositions: TdxSortedList<Integer>;
begin
  AParagraph := GetParagraph;
  AColumnIndex := GetColumnIndex(AIsMainPieceTable);
  ATableCellViewInfo := GetTableCellViewInfo(AIsMainPieceTable);
  if ATableCellViewInfo <> nil then
    ATableAlignedPositions := GetTableAlignedPositions(ATableCellViewInfo.TableViewInfo)
  else
    ATableAlignedPositions := TdxSortedList<Integer>.Create;
  Result := TdxHorizontalRulerViewInfo.Create(Self, ASectionProperties, AParagraph, AColumnIndex,
    AIsMainPieceTable, ATableCellViewInfo, ATableAlignedPositions);
end;

function TdxHorizontalRulerControl.GetRulerSizeInPixels: Integer;
begin
  Result := GetRulerHeightInPixels;
end;

function TdxHorizontalRulerControl.GetRulerHeightInPixels: Integer;
begin
  Result := ViewInfo.DisplayClientBounds.Height + LayoutUnitsToPixelsV(Painter.PaddingTop + Painter.PaddingBottom);
end;

procedure TdxHorizontalRulerControl.DrawRuler(AGraphics: TdxGraphics);
begin
  inherited DrawRuler(AGraphics);
  Painter.Draw(AGraphics);
  Painter.DrawTabTypeToggle(AGraphics);
end;

function TdxHorizontalRulerControl.CreateOrientation: TdxOrientation;
begin
  Result := TdxHorizontalOrientation.Create;
end;

function TdxHorizontalRulerControl.CreatePainter: TdxRulerPainterBase;
begin
  if LookAndFeel.ActiveStyle = lfsSkin then
    Result := TdxHorizontalRulerSkinPainter.Create(Self)
  else
    Result := TdxHorizontalRulerColorBasedPainter.Create(Self);
end;

function TdxHorizontalRulerControl.GetPhysicalPoint(const APoint: TPoint): TPoint;
var
  X, Y: Integer;
begin
  X := PixelsToLayoutUnitsH(APoint.X) - RichEditControl.ViewBounds.Left;
  Y := PixelsToLayoutUnitsV(APoint.Y);
  Result.Init(X, Y);
end;

function TdxHorizontalRulerControl.CalculateHotZoneSize(AHotZone: TdxHorizontalRulerHotZone): TSize;
begin
  Result := Painter.ElementPainter.CalculateHotZoneSize(AHotZone);
end;

function TdxHorizontalRulerControl.CanUpdate: Boolean;
begin
  if not RichEditControl.ActiveView.CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column) then
    Exit(False);
  Result := UpdatePageIndex or UpdateColumnIndex or UpdateParagraphIndex;
end;

function TdxHorizontalRulerControl.GetTabTypeToggleActiveAreaSize: TSize;
begin
  Result := Painter.GetTabTypeToggleActiveAreaSize;
end;

function TdxHorizontalRulerControl.CalculateTabTypeToggleBackgroundBounds: TRect;
begin
  Result.InitSize(0, 0, Height, Height);
end;

{ TdxVerticalRulerControl }

constructor TdxVerticalRulerControl.Create(ARichEditControl: TdxCustomRichEditControl);
begin
  inherited Create(ARichEditControl);
  Font.Handle := CreateRotatedFont(Font, vtdBottomToTop);
  Painter.Initialize;
  ViewInfo.Initialize;
end;

function TdxVerticalRulerControl.GetIsVisible: Boolean;
begin
  Result := RichEditControl.CalculateVerticalRulerVisibility;
end;

function TdxVerticalRulerControl.CreateViewInfo: TdxRulerViewInfoBase;
var
  AIsMainPieceTable: Boolean;
  ATableCellViewInfo: TdxTableCellViewInfo;
  AVerticalPositions: TdxSortedList<Integer>;
  AAnchors: TdxTableCellVerticalAnchorCollection;
  I: Integer;
  ASection: TdxSection;
begin
  AIsMainPieceTable := DocumentModel.Selection.PieceTable.IsMain;
  ATableCellViewInfo := GetTableCellViewInfo(AIsMainPieceTable);
  AVerticalPositions := TdxSortedList<Integer>.Create;
  if ATableCellViewInfo <> nil then
  begin
    AAnchors := ATableCellViewInfo.TableViewInfo.Anchors;
    for I := 1 to AAnchors.Count - 1 do
      AVerticalPositions.Add(AAnchors[I].VerticalPosition);
  end;
  ASection := GetSection(DocumentModel.Selection.PieceTable);
  Result := TdxVerticalRulerViewInfo.Create(Self, TdxSectionProperties.Create(ASection),
    AIsMainPieceTable, AVerticalPositions, ATableCellViewInfo);
end;

procedure TdxVerticalRulerControl.SetViewInfo(ANewViewInfo: TdxVerticalRulerViewInfo);
begin
  inherited SetViewInfo(ANewViewInfo);
  ViewInfo.Initialize;
end;

function TdxVerticalRulerControl.CreateMouseController: TdxRulerMouseController;
begin
  Result := TdxVerticalRulerMouseHandler.Create(Self);
end;

function TdxVerticalRulerControl.GetRulerSizeInPixels: Integer;
begin
  Result := GetRulerWidthInPixels;
end;

function TdxVerticalRulerControl.GetRulerWidthInPixels: Integer;
begin
  Result := LayoutUnitsToPixelsH(ViewInfo.GetRulerSize);
end;

procedure TdxVerticalRulerControl.RecreatePainter;
begin
  inherited RecreatePainter;
  Painter.Initialize;
end;

function TdxVerticalRulerControl.GetViewInfo: TdxVerticalRulerViewInfo;
begin
  Result := TdxVerticalRulerViewInfo(inherited ViewInfo);
end;

procedure TdxVerticalRulerControl.DrawRuler(AGraphics: TdxGraphics);
begin
  inherited DrawRuler(AGraphics);
  Painter.Draw(AGraphics);
end;

function TdxVerticalRulerControl.CreateOrientation: TdxOrientation;
begin
  Result := TdxVerticalOrientation.Create;
end;

function TdxVerticalRulerControl.CreatePainter: TdxRulerPainterBase;
begin
  if LookAndFeel.ActiveStyle = lfsSkin then
    Result := TdxVerticalRulerSkinPainter.Create(Self)
  else
    Result := TdxVerticalRulerColorBasedPainter.Create(Self);
end;

function TdxVerticalRulerControl.GetPainter: TdxVerticalRulerPainter;
begin
  Result := TdxVerticalRulerPainter(inherited Painter);
end;

function TdxVerticalRulerControl.GetPhysicalPoint(const APoint: TPoint): TPoint;
begin
  Result.X := PixelsToLayoutUnitsH(APoint.X);
  Result.Y := PixelsToLayoutUnitsV(APoint.Y) - RichEditControl.ViewBounds.Top;
end;

function TdxVerticalRulerControl.CanUpdate: Boolean;
begin
  if not RichEditControl.ActiveView.CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column) then
    Exit(False);
  Result := UpdatePageIndex;
end;

procedure Finalize;
begin
  TdxRulerViewInfoBase.FinalizeMeasureGraphics;
end;

initialization
  dxUnitsLoader.AddUnit(nil, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.
