{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridWinExplorerView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Forms, StdCtrls, Graphics, Classes, Controls, SysUtils, Windows, cxClasses, cxGeometry,
  cxControls, cxLookAndFeelPainters, cxCustomData, cxGridCustomView, cxGridCustomTableView,
  dxCoreClasses, cxGraphics, cxStyles, cxEdit, cxGridCommon, cxTextEdit, cxMemo, cxImage, ImgList;

const
  cxGridWinExplorerViewDefaultRecordWidth = -1;
  cxGridWinExplorerViewRecordItemIndent = 1;
  cxGridWinExplorerViewImageSmallSize = 16;
  cxGridWinExplorerViewImageMediumSize = 48;
  cxGridWinExplorerViewImageLargeSize = 96;
  cxGridWinExplorerViewImageExtraLargeSize = 256;
  cxGridWinExplorerViewContentModeIndentBetweenImageAndText = 10;

  //item index in ItemSet
  isiCheckBoxItem = 1;
  isiDescriptionItem = isiCheckBoxItem + 1;
  isiExtraLargeImageItem = isiCheckBoxItem + 2;
  isiExtraLargeImageIndexItem = isiCheckBoxItem + 3;
  isiGroupItem = isiCheckBoxItem + 4;
  isiLargeImageItem = isiCheckBoxItem + 5;
  isiLargeImageIndexItem = isiCheckBoxItem + 6;
  isiMediumImageItem = isiCheckBoxItem + 7;
  isiMediumImageIndexItem = isiCheckBoxItem + 8;
  isiSmallImageItem = isiCheckBoxItem + 9;
  isiSmallImageIndexItem = isiCheckBoxItem + 10;
  isiTextItem = isiCheckBoxItem + 11;

  isWinExplorerViewItemFirst = isCustomItemLast + 1;
  isWinExplorerViewItemTextItem = isWinExplorerViewItemFirst;
  isWinExplorerViewItemDescriptionItem = isWinExplorerViewItemFirst + 1;

  vsWinExplorerViewFirst = vsCustomTableLast + 1;
  vsWinExplorerGroup = vsWinExplorerViewFirst;
  vsWinExplorerTextItem = vsWinExplorerViewFirst + 1;
  vsWinExplorerDescriptionItem = vsWinExplorerViewFirst + 2;
  vsWinExplorerRecordHotTrack = vsWinExplorerViewFirst + 3;

type
  TcxGridWinExplorerViewExpandButtonViewInfo = class;
  TcxGridWinExplorerViewImageViewInfo = class;
  TcxGridWinExplorerViewCustomRecordViewInfo = class;
  TcxGridWinExplorerViewRecordViewInfo = class;
  TcxGridWinExplorerViewGroupRecordViewInfo = class;
  TcxGridWinExplorerViewContentModeRecordViewInfo = class;
  TcxGridWinExplorerViewListModeRecordsViewInfo = class;
  TcxGridWinExplorerViewRecordsViewInfo = class;
  TcxGridWinExplorerViewCustomRecord = class;
  TcxGridWinExplorerViewGroupRecord = class;
  TcxGridWinExplorerViewOptionsDisplayMode = class;
  TcxGridWinExplorerViewOptionsDisplayModes = class;
  TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode = class;
  TcxGridWinExplorerViewItem = class;
  TcxGridWinExplorerViewItemSet = class;
  TcxGridWinExplorerViewViewData = class;
  TcxGridWinExplorerViewController = class;
  TcxGridWinExplorerView = class;

  TcxGridWinExplorerViewImageType = (itSmall, itMedium, itLarge, itExtraLarge);
  TcxGridWinExplorerViewDisplayMode = (dmContent, dmExtraLargeImages, dmLargeImages, dmList,
    dmMediumImages, dmSmallImages, dmTiles);

  { TcxGridWinExplorerViewRecordExpandButtonPainter }

  TcxGridWinExplorerViewRecordExpandButtonPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridWinExplorerViewExpandButtonViewInfo;
  protected
    procedure Paint; override;

    property ViewInfo: TcxGridWinExplorerViewExpandButtonViewInfo read GetViewInfo;
  end;

  { TcxGridWinExplorerViewImageCellPainter }

  TcxGridWinExplorerViewImageCellPainter = class(TcxGridTableDataCellPainter)
  private
    function GetViewInfo: TcxGridWinExplorerViewImageViewInfo; inline;
  protected
    procedure DrawEditViewInfo; override;

    property ViewInfo: TcxGridWinExplorerViewImageViewInfo read GetViewInfo;
  end;

  { TcxGridWinExplorerViewCustomRecordPainter }

  TcxGridWinExplorerViewCustomRecordPainter = class(TcxCustomGridRecordPainter)
  private
    function GetViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo; inline;
  protected
    procedure AfterPaint; override;
    procedure BeforePaint; override;

    procedure DrawBackground; override;
    procedure DrawBackground(const R: TRect); override;
    procedure DrawContentBackground(const ABounds: TRect); virtual;

    property ViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo read GetViewInfo;
  end;

  { TcxGridWinExplorerViewGroupRecordPainter }

  TcxGridWinExplorerViewGroupRecordPainter = class(TcxGridWinExplorerViewCustomRecordPainter)
  private
    function GetViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo; inline;
  protected
    procedure DrawCaptionLine; virtual;
    procedure DrawContentBackground(const ABounds: TRect); override;
    procedure DrawExpandButton; override;
    procedure Paint; override;

    property ViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo read GetViewInfo;
  end;

  { TcxGridWinExplorerViewRecordPainter }

  TcxGridWinExplorerViewRecordPainter = class(TcxGridWinExplorerViewCustomRecordPainter)
  private
    function GetViewInfo: TcxGridWinExplorerViewRecordViewInfo; inline;
  protected
    function CanDrawCheckBox: Boolean; virtual;
    procedure DrawCheckBox; virtual;
    procedure DrawContentBackground(const ABounds: TRect); override;
    procedure DrawDescription; virtual;
    procedure DrawImage; virtual;
    procedure DrawTextBox; virtual;
    procedure Paint; override;

    property ViewInfo: TcxGridWinExplorerViewRecordViewInfo read GetViewInfo;
  end;

  { TcxGridWinExplorerViewContentModeRecordPainter }

  TcxGridWinExplorerViewContentModeRecordPainter = class(TcxGridWinExplorerViewRecordPainter)
  private
    function GetViewInfo: TcxGridWinExplorerViewContentModeRecordViewInfo; inline;
  protected
    function CanDrawCheckBox: Boolean; override;
    procedure DrawSeparator; virtual;
    procedure Paint; override;

    property ViewInfo: TcxGridWinExplorerViewContentModeRecordViewInfo read GetViewInfo;
  end;

  { TcxGridWinExplorerViewTilesModeRecordPainter }

  TcxGridWinExplorerViewTilesModeRecordPainter = class(TcxGridWinExplorerViewRecordPainter)
  protected
    function CanDrawCheckBox: Boolean; override;
  end;

  { TcxGridWinExplorerViewRecordsPainter }

  TcxGridWinExplorerViewRecordsPainter = class(TcxCustomGridRecordsPainter);

  { TcxGridWinExplorerViewListModeRecordsPainter }

  TcxGridWinExplorerViewListModeRecordsPainter = class(TcxGridWinExplorerViewRecordsPainter)
  private
    function GetViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo;
  protected
    procedure Paint; override;

    property ViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo read GetViewInfo;
  end;

  { TcxGridWinExplorerViewPainter }

  TcxGridWinExplorerViewPainter = class(TcxCustomGridTablePainter)
  private
    function GetGridView: TcxGridWinExplorerView; inline;
    function GetViewData: TcxGridWinExplorerViewViewData;
  protected
    function CanOffset(AItemsOffset: Integer; DX: Integer; DY: Integer): Boolean; override;
    procedure PaintContent; override;

    property ViewData: TcxGridWinExplorerViewViewData read GetViewData;
  public
    property GridView: TcxGridWinExplorerView read GetGridView;
  end;

  { TcxGridWinExplorerViewExpandButtonViewInfo }

  TcxGridWinExplorerViewExpandButtonViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FRecordViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo;

    function GetRecord: TcxGridWinExplorerViewGroupRecord;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CaptureMouseOnPress: Boolean; override;
    procedure Click; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    property GridRecord: TcxGridWinExplorerViewGroupRecord read GetRecord;
    property RecordViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo read FRecordViewInfo;
  public
    constructor Create(ARecordViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo); reintroduce; virtual;
  end;

  { TcxGridWinExplorerViewCustomCellViewInfo }

  TcxGridWinExplorerViewCustomCellViewInfo = class(TcxGridTableDataCellViewInfo)
  private
    FInternalProperties: TcxCustomEditProperties;

    function GetGridView: TcxGridWinExplorerView;
    function GetItem: TcxGridWinExplorerViewItem;
    function GetRecordViewInfo: TcxGridWinExplorerViewRecordViewInfo;
  protected
    function GetAlwaysSelected: Boolean; override;
    function GetBorders: TcxBorders; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetDesignSelectionBounds: TRect; override;
    function GetHotTrack: Boolean; override;
    function GetInternalPropertiesClass: TcxCustomEditPropertiesClass; virtual; abstract;
    function GetIsDesignSelected: Boolean; override;
    function GetItemProperties: TcxCustomEditProperties; override;
    function GetTransparent: Boolean; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure InitInternalProperties; virtual;
    function IsHotTracking: Boolean; virtual;
    function NeedLocalCopyOfEditViewData: Boolean; override;
    function NeedShowEditOnDblClick: Boolean; override;
    procedure StateChanged(APrevState: TcxGridCellState); override;
    function UseInternalProperties: Boolean; virtual;

    property InternalProperties: TcxCustomEditProperties read FInternalProperties;
  public
    constructor Create(ARecordViewInfo: TcxCustomGridRecordViewInfo; AItem: TcxCustomGridTableItem); override;
    destructor Destroy; override;

    procedure BeforeRecalculation; override;
    function CanDrawSelected: Boolean; override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState): Boolean; override;

    property GridView: TcxGridWinExplorerView read GetGridView;
    property Item: TcxGridWinExplorerViewItem read GetItem;
    property RecordViewInfo: TcxGridWinExplorerViewRecordViewInfo read GetRecordViewInfo;
  end;

  { TcxGridWinExplorerViewCheckBoxViewInfo }

  TcxGridWinExplorerViewCheckBoxViewInfo = class(TcxGridWinExplorerViewCustomCellViewInfo)
  protected
    function GetBorders: TcxBorders; override;
    function GetInternalPropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TcxGridWinExplorerViewTextViewInfo }

  TcxGridWinExplorerViewTextViewInfo = class(TcxGridWinExplorerViewCustomCellViewInfo)
  private
    function GetInternalProperties: TcxTextEditProperties;
  protected
    function GetInternalPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetRequiredHeight: Integer; virtual;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure InitInternalProperties; override;

    property InternalProperties: TcxTextEditProperties read GetInternalProperties;
  end;

  { TcxGridWinExplorerViewDescriptionViewInfo }

  TcxGridWinExplorerViewDescriptionViewInfo = class(TcxGridWinExplorerViewCustomCellViewInfo)
  private
    function GetInternalProperties: TcxMemoProperties;
  protected
    function GetInternalPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetRequiredHeight: Integer; virtual;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure InitInternalProperties; override;

    property InternalProperties: TcxMemoProperties read GetInternalProperties;
  end;

  { TcxGridWinExplorerViewImageViewInfo }

  TcxGridWinExplorerViewImageViewInfo = class(TcxGridWinExplorerViewCustomCellViewInfo)
  private
    FImageList: TCustomImageList;

    function GetInternalProperties: TcxImageProperties;
  protected
    function CanShowEdit: Boolean; override;
    function GetInternalPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure InitInternalProperties; override;
    function UseInternalProperties: Boolean; override;

    property InternalProperties: TcxImageProperties read GetInternalProperties;
    property ImageList: TCustomImageList read FImageList;
  public
    constructor Create(ARecordViewInfo: TcxCustomGridRecordViewInfo; AItem: TcxCustomGridTableItem;
      AImageList: TCustomImageList); reintroduce; virtual;
  end;

  { TcxGridWinExplorerViewCustomRecordViewInfo }

  TcxGridWinExplorerViewCustomRecordViewInfo = class(TcxCustomGridRecordViewInfo)
  private
    function GetController: TcxGridWinExplorerViewController;
    function GetGridRecord: TcxGridWinExplorerViewCustomRecord;
    function GetGridView: TcxGridWinExplorerView; inline;
    function GetOptionsDisplayMode: TcxGridWinExplorerViewOptionsDisplayMode;
    function GetRecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo;
  protected
    BackParams: TcxViewParams;

    function AllowChangeStateOnMouseDown: Boolean; override;
    procedure DoCalculateParams; override;
    procedure GetBackViewParams(var AParams: TcxViewParams); virtual;
    function GetContentBackgroundBounds: TRect; virtual;
    function GetFocusRectBounds: TRect; override;
    function GetHotTrack: Boolean; override;
    function GetPaintState: TcxButtonState; virtual;
    function GetVisible: Boolean; override;
    function HasFocusRect: Boolean; override;
    function InvalidateOnStateChange: Boolean; override;
    function IsData: Boolean; virtual;
    function IsHotTracking: Boolean; virtual;
    procedure StateChanged(APrevState: TcxGridCellState); override;

    property ContentBackgroundBounds: TRect read GetContentBackgroundBounds;
    property Controller: TcxGridWinExplorerViewController read GetController;
  public
    property GridRecord: TcxGridWinExplorerViewCustomRecord read GetGridRecord;
    property GridView: TcxGridWinExplorerView read GetGridView;
    property OptionsDisplayMode: TcxGridWinExplorerViewOptionsDisplayMode read GetOptionsDisplayMode;
    property RecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo read GetRecordsViewInfo;
  end;

  TcxGridWinExplorerViewGroupClickTimer = class(TcxTimer)
  public
    Button: TMouseButton;
    HitTest: TcxCustomGridHitTest;
    Shift: TShiftState
  end;

  { TcxGridWinExplorerViewGroupRecordViewInfo }

  TcxGridWinExplorerViewGroupRecordViewInfo = class(TcxGridWinExplorerViewCustomRecordViewInfo)
  private
    FClickTimer: TcxGridWinExplorerViewGroupClickTimer;
    FExpandButtonViewInfo: TcxGridWinExplorerViewExpandButtonViewInfo;

    //click timer
    procedure ClickTimerHandler(Sender: TObject);
    procedure CreateClickTimer;
    procedure DestroyClickTimer;
    function IsClickTimerActive: Boolean;
    procedure StartClickTimer(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState);
    procedure StopClickTimer;

    function GetGridRecord: TcxGridWinExplorerViewGroupRecord;
  protected
    procedure CalculateExpandButtonBounds(var ABounds: TRect); override;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetCaptionLineBounds: TRect; virtual;
    function GetExpandButtonAreaBounds: TRect; override;
    function GetExpandButtonSize: Integer; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRealExpandButtonAreaBounds: TRect; override;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetWidth: Integer; override;
    function HasExpandButton: Boolean; virtual;
    procedure Offset(DX: Integer; DY: Integer); override;
    function ShowExpandButton: Boolean; virtual;
    procedure PrepareCanvas(ACanvas: TcxCanvas); override;

    property CaptionLineBounds: TRect read GetCaptionLineBounds;
    property ExpandButtonViewInfo: TcxGridWinExplorerViewExpandButtonViewInfo read FExpandButtonViewInfo;
    property ExpandButtonSize: Integer read GetExpandButtonSize;
  public
    constructor Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo; ARecord: TcxCustomGridRecord); override;
    destructor Destroy; override;

    procedure AfterRecalculation; override;
    procedure BeforeRecalculation; override;

    function Click(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    property GridRecord: TcxGridWinExplorerViewGroupRecord read GetGridRecord;
  end;

  { TcxGridWinExplorerViewListModeGroupRecordViewInfo }

  TcxGridWinExplorerViewListModeGroupRecordViewInfo = class(TcxGridWinExplorerViewGroupRecordViewInfo)
  private
    function GetRecordsViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo;
  protected
    function CalculateWidth: Integer; override;
    function ShowExpandButton: Boolean; override;
  public
    property RecordsViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridWinExplorerViewRecordViewInfo }

  TcxGridWinExplorerViewRecordViewInfo = class(TcxGridWinExplorerViewCustomRecordViewInfo)
  private
    FCheckBoxBounds: TRect;
    FCheckBoxViewInfo: TcxGridWinExplorerViewCheckBoxViewInfo;
    FDescriptionBounds: TRect;
    FDescriptionViewInfo: TcxGridWinExplorerViewDescriptionViewInfo;
    FImageBounds: TRect;
    FImageViewInfo: TcxGridWinExplorerViewImageViewInfo;
    FTextBounds: TRect;
    FTextViewInfo: TcxGridWinExplorerViewTextViewInfo;

    function GetItemSet: TcxGridWinExplorerViewItemSet;
  protected
    procedure CalculateCellViewInfos; virtual;
    procedure CalculateCheckBox; virtual;
    function CalculateCheckBoxBounds: TRect; virtual; abstract;
    procedure CalculateDescription; virtual;
    function CalculateDescriptionBounds: TRect; virtual; abstract;
    procedure CalculateImage; virtual;
    function CalculateImageBounds: TRect; virtual; abstract;
    procedure CalculateText; virtual;
    function CalculateTextBounds: TRect; virtual; abstract;
    function CalculateWidth: Integer; override;
    function CanShowCheckBox: Boolean; virtual;
    function CanShowDescription: Boolean; virtual;
    function CanShowImage: Boolean; virtual;
    function CanShowText: Boolean; virtual;
    procedure CellHotTrackStateChanged; virtual;
    procedure CorrectTextCellViewParams(var AParams: TcxViewParams); virtual;
    function FindCell(AItem: TcxCustomGridTableItem): TcxGridWinExplorerViewCustomCellViewInfo;
    function GetCheckBoxSize: TSize; virtual;
    function GetDescriptionHeight: Integer; virtual;
    function GetDescriptionWidth: Integer; virtual; abstract;
    function GetImageItem: TcxGridWinExplorerViewItem; overload; virtual;
    function GetImageItem(out AImageList: TCustomImageList): TcxGridWinExplorerViewItem; overload; virtual;
    function GetImageSize: TcxSize; virtual;
    function GetImageType: TcxGridWinExplorerViewImageType; virtual;
    function GetIndentBetweenCheckBoxAndImage: Integer; virtual;
    function GetIndentBetweenImageAndText: Integer; virtual;
    function GetIndentBetweenItemAndBorders: Integer; virtual;
    function GetIndentBetweenTextAndDescription: Integer; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetTextHeight: Integer; virtual;
    function GetTextWidth: Integer; virtual; abstract;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetWidth: Integer; override;
    procedure InitDescriptionProperties(AProperties: TcxMemoProperties); virtual;
    procedure InitImageProperties(AProperties: TcxImageProperties); virtual;
    procedure InitTextProperties(AProperties: TcxTextEditProperties); virtual;
    function IsHotTracking: Boolean; override;
    procedure Offset(DX: Integer; DY: Integer); override;
    procedure OffsetCells(DX: Integer; DY: Integer); virtual;
    procedure OffsetRects(DX: Integer; DY: Integer); virtual;
  public
    constructor Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo; ARecord: TcxCustomGridRecord); override;
    destructor Destroy; override;

    procedure AfterRecalculation; override;
    procedure BeforeRecalculation; override;

    procedure Calculate(ALeftBound: Integer; ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetCellViewInfoByItem(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo; override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    function HasCheckBox: Boolean; virtual;
    function HasDescription: Boolean; virtual;
    function HasImage: Boolean; virtual;
    function HasText: Boolean; virtual;

    property CheckBoxBounds: TRect read FCheckBoxBounds;
    property CheckBoxViewInfo: TcxGridWinExplorerViewCheckBoxViewInfo read FCheckBoxViewInfo;
    property DescriptionBounds: TRect read FDescriptionBounds;
    property DescriptionViewInfo: TcxGridWinExplorerViewDescriptionViewInfo read FDescriptionViewInfo;
    property ImageBounds: TRect read FImageBounds;
    property ImageViewInfo: TcxGridWinExplorerViewImageViewInfo read FImageViewInfo;
    property ItemSet: TcxGridWinExplorerViewItemSet read GetItemSet;
    property TextBounds: TRect read FTextBounds;
    property TextViewInfo: TcxGridWinExplorerViewTextViewInfo read FTextViewInfo;
  end;

  { TcxGridWinExplorerViewContentModeRecordViewInfo }

  TcxGridWinExplorerViewContentModeRecordViewInfo = class(TcxGridWinExplorerViewRecordViewInfo)
  private
    FTextFont: TFont;
  protected
    function CalculateCheckBoxBounds: TRect; override;
    function CalculateDescriptionBounds: TRect; override;
    function CalculateHeight: Integer; override;
    function CalculateImageBounds: TRect; override;
    function CalculateTextBounds: TRect; override;
    function CalculateWidth: Integer; override;
    procedure CorrectTextCellViewParams(var AParams: TcxViewParams); override;
    function GetContentBackgroundBounds: TRect; override;
    function GetDescriptionHeight: Integer; override;
    function GetDescriptionWidth: Integer; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetSeparatorBounds: TRect; virtual;
    function GetSeparatorSize: Integer; virtual;
    function GetTextHeight: Integer; override;
    function GetTextWidth: Integer; override;

    property TextFont: TFont read FTextFont;
  public
    constructor Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo; ARecord: TcxCustomGridRecord); override;
    destructor Destroy; override;
  end;

  { TcxGridWinExplorerViewSmallModeRecordViewInfo }

  TcxGridWinExplorerViewSmallImagesModeRecordViewInfo = class(TcxGridWinExplorerViewRecordViewInfo)
  private
    function GetOptionsDisplayMode: TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode;
  protected
    function CalculateCheckBoxBounds: TRect; override;
    function CalculateDescriptionBounds: TRect; override;
    function CalculateHeight: Integer; override;
    function CalculateImageBounds: TRect; override;
    function CalculateTextBounds: TRect; override;
    function CalculateWidth: Integer; override;
    function GetDescriptionHeight: Integer; override;
    function GetDescriptionMinHeight: Integer; virtual;
    function GetDescriptionWidth: Integer; override;
    function GetImageType: TcxGridWinExplorerViewImageType; override;
    function GetIndentBetweenImageAndDescription: Integer; virtual;
    function GetTextHeight: Integer; override;
    function GetTextWidth: Integer; override;
  public
    property OptionsDisplayMode: TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode read GetOptionsDisplayMode;
  end;

  { TcxGridWinExplorerViewListModeRecordViewInfo }

  TcxGridWinExplorerViewListModeRecordViewInfo = class(TcxGridWinExplorerViewSmallImagesModeRecordViewInfo);

  { TcxGridWinExplorerViewTilesModeRecordViewInfo }

  TcxGridWinExplorerViewTilesModeRecordViewInfo = class(TcxGridWinExplorerViewSmallImagesModeRecordViewInfo)
  protected
    function CalculateCheckBoxBounds: TRect; override;
    function CalculateImageBounds: TRect; override;
    function CalculateWidth: Integer; override;
    function GetImageType: TcxGridWinExplorerViewImageType; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetTextWidth: Integer; override;
  end;

  { TcxGridWinExplorerViewMediumModeRecordViewInfo }

  TcxGridWinExplorerViewMediumImagesModeRecordViewInfo = class(TcxGridWinExplorerViewRecordViewInfo)
  protected
    function CalculateCheckBoxBounds: TRect; override;
    function CalculateDescriptionBounds: TRect; override;
    function CalculateHeight: Integer; override;
    function CalculateImageBounds: TRect; override;
    function CalculateTextBounds: TRect; override;
    function CalculateWidth: Integer; override;
    function GetDescriptionHeight: Integer; override;
    function GetDescriptionWidth: Integer; override;
    function GetTextHeight: Integer; override;
    function GetTextWidth: Integer; override;
    procedure InitDescriptionProperties(AProperties: TcxMemoProperties); override;
    procedure InitTextProperties(AProperties: TcxTextEditProperties); override;
  end;

  { TcxGridWinExplorerViewLargeModeRecordViewInfo }

  TcxGridWinExplorerViewLargeImagesModeRecordViewInfo = class(TcxGridWinExplorerViewMediumImagesModeRecordViewInfo)
  protected
    function GetImageType: TcxGridWinExplorerViewImageType; override;
  end;

  { TcxGridWinExplorerViewExtraLargeModeRecordViewInfo }

  TcxGridWinExplorerViewExtraLargeImagesModeRecordViewInfo = class(TcxGridWinExplorerViewLargeImagesModeRecordViewInfo)
  protected
    function GetImageType: TcxGridWinExplorerViewImageType; override;
  end;

  { TcxGridWinExplorerViewRecordsViewInfo }

  TcxGridWinExplorerViewRecordsViewInfo = class(TcxCustomGridRecordsViewInfo)
  strict private
    FCalculated: Boolean;

    function GetController: TcxGridWinExplorerViewController; inline;
    function GetGridView: TcxGridWinExplorerView; inline;
    function GetItem(Index: Integer): TcxGridWinExplorerViewCustomRecordViewInfo;
    function GetOptionsDisplayMode: TcxGridWinExplorerViewOptionsDisplayMode;
    function GetViewData: TcxGridWinExplorerViewViewData; inline;
    function GetDisplayModes: TcxGridWinExplorerViewOptionsDisplayModes;
  protected
    procedure AfterCalculate; override;
    procedure BeforeCalculate; override;
    procedure Calculate; override;
    procedure CalculateItems; virtual;
    procedure CalculatePartVisibleCount; virtual;
    procedure CalculateVisibleCount; override;
    function GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; virtual;
    function GetGroupRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; virtual;
    function GetHorizontalIndent: Integer; virtual;
    function GetVerticalIndent: Integer; virtual;
    function GetItemsOffset(AItemCountDelta: Integer): Integer; override;
    function GetIndentBetweenGroupAndItem: Integer; virtual;
    function GetIndentBetweenGroups: Integer; virtual;
    function GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode; virtual; abstract;
    function GetFirstRecordIndexInBand(ARecordIndexInBand: Integer): Integer; virtual;
    function GetItemCountInBand(ARecordIndexInBand: Integer): Integer; virtual;
    function GetLastRecordIndexInBand(ARecordIndexInBand: Integer): Integer;
    function GetPainterClass: TcxCustomGridRecordsPainterClass; override;
    function GetContentScrollSize: Integer; virtual; abstract;
    function IsHorizontalScrolling: Boolean; virtual;
    function IsRecordFullyVisibleAtBottom(ARecordIndex: Integer): Boolean; virtual;
    function NeedConsideredRecordOffset(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Boolean; virtual;
    procedure OffsetItem(AIndex: Integer; AOffset: Integer); override;
    procedure UpdateVisibleCount; virtual;

    property Calculated: Boolean read FCalculated;
    property Controller: TcxGridWinExplorerViewController read GetController;
    property OptionsDisplayMode: TcxGridWinExplorerViewOptionsDisplayMode read GetOptionsDisplayMode;
    property ViewData: TcxGridWinExplorerViewViewData read GetViewData;
    property DisplayModes: TcxGridWinExplorerViewOptionsDisplayModes read GetDisplayModes;
  public
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure Offset(DX: Integer; DY: Integer); override;

    property GridView: TcxGridWinExplorerView read GetGridView;
    property Items[Index: Integer]: TcxGridWinExplorerViewCustomRecordViewInfo read GetItem; default;
  end;

  { TcxGridWinExplorerViewContentModeRecordsViewInfo }

  TcxGridWinExplorerViewContentModeRecordsViewInfo = class(TcxGridWinExplorerViewRecordsViewInfo)
  protected
    function CalculateContentBounds: TRect; override;
    procedure CalculatePartVisibleCount; override;
    function GetItemLeftBound(AIndex: Integer): Integer; override;
    function GetItemTopBound(AIndex: Integer): Integer; override;
    function GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    function GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode; override;
    function GetContentScrollSize: Integer; override;
    procedure UpdateVisibleCount; override;
  end;

  { TcxGridWinExplorerViewCustomImagesModeRecordsViewInfo }

  TcxGridWinExplorerViewCustomImagesModeRecordsViewInfo = class(TcxGridWinExplorerViewRecordsViewInfo)
  protected
    function GetHorizontalIndent: Integer; override;
    function GetItemHorizontalIndent: Integer; virtual; abstract;
    function GetItemVerticalIndent: Integer; virtual; abstract;
  end;

  { TcxGridWinExplorerViewImagesModeRecordsViewInfo }

  TcxGridWinExplorerViewImagesModeRecordsViewInfo = class(TcxGridWinExplorerViewCustomImagesModeRecordsViewInfo)
  protected
    function CalculateBounds: TRect; override;
    function CalculateContentBounds: TRect; override;
    procedure CalculatePartVisibleCount; override;
    function GetColumnCountByItem(AItem: TcxCustomGridRecordViewInfo): Integer; virtual;
    function GetColumnCountByItemWidth(AValue: Integer): Integer; virtual;
    function GetColumnCountByRecordIndex(AValue: Integer): Integer; virtual;
    function GetDataWidth: Integer; virtual;
    function GetItemColumnIndex(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer; overload; virtual;
    function GetItemColumnIndex(ARecordIndex: Integer): Integer; overload; virtual;
    function GetItemHorizontalIndent: Integer; override;
    function GetItemLeftBound(AIndex: Integer): Integer; override;
    function GetItemTopBound(AIndex: Integer): Integer; override;
    function GetItemVerticalIndent: Integer; override;
    function GetRecordScrollSize(ARecordIndex: Integer): Integer; override;
    function GetFirstRecordIndexInBand(ARecordIndexInBand: Integer): Integer; override;
    function GetItemCountInBand(ARecordIndexInBand: Integer): Integer; override;
    function GetContentScrollSize: Integer; override;
    function NeedConsideredRecordOffset(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Boolean; override;
    procedure UpdateVisibleCount; override;
  end;

  { TcxGridWinExplorerViewExtraLargeModeRecordsViewInfo }

  TcxGridWinExplorerViewExtraLargeImagesModeRecordsViewInfo = class(TcxGridWinExplorerViewImagesModeRecordsViewInfo)
  protected
    function GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    function GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode; override;
  end;

  { TcxGridWinExplorerViewLargeModeRecordsViewInfo }

  TcxGridWinExplorerViewLargeImagesModeRecordsViewInfo = class(TcxGridWinExplorerViewImagesModeRecordsViewInfo)
  protected
    function GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    function GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode; override;
  end;

  { TcxGridWinExplorerViewMediumModeRecordsViewInfo }

  TcxGridWinExplorerViewMediumImagesModeRecordsViewInfo = class(TcxGridWinExplorerViewImagesModeRecordsViewInfo)
  protected
    function GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    function GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode; override;
  end;

  { TcxGridWinExplorerViewSmallModeRecordsViewInfo }

  TcxGridWinExplorerViewSmallImagesModeRecordsViewInfo = class(TcxGridWinExplorerViewImagesModeRecordsViewInfo)
  protected
    function GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    function GetItemHorizontalIndent: Integer; override;
    function GetItemVerticalIndent: Integer; override;
    function GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode; override;
  end;

  { TcxGridWinExplorerViewTilesModeRecordsViewInfo }

  TcxGridWinExplorerViewTilesModeRecordsViewInfo = class(TcxGridWinExplorerViewImagesModeRecordsViewInfo)
  protected
    function GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    function GetItemHorizontalIndent: Integer; override;
    function GetItemVerticalIndent: Integer; override;
    function GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode; override;
  end;

  { TcxGridWinExplorerViewListModeRecordsViewInfo }

  TcxGridWinExplorerViewListModeRecordsViewInfo = class(TcxGridWinExplorerViewCustomImagesModeRecordsViewInfo)
  private
    FFixedGroupItem: TcxCustomGridRecordViewInfo;

    procedure SetFixedGroupItem(AValue: TcxCustomGridRecordViewInfo);
  protected
    function CalculateBounds: TRect; override;
    function CalculateContentBounds: TRect; override;
    procedure CalculateItems; override;
    procedure CalculatePartVisibleCount; override;
    function GetDataHeight: Integer; virtual;
    function GetFirstRecordIndexInBand(ARecordIndexInBand: Integer): Integer; override;
    function GetFixedGroupTopLeft: TPoint; virtual;
    function GetGroupRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    function GetItemHorizontalIndent: Integer; override;
    function GetItemLeftBound(AIndex: Integer): Integer; override;
    function GetItemPixelScrollSize(AItem: TcxCustomGridRecordViewInfo): Integer; override;
    function GetItemRowIndex(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer; overload; virtual;
    function GetItemRowIndex(ARecordIndex: Integer): Integer; overload; virtual;
    function GetItemTopBound(AIndex: Integer): Integer; override;
    function GetItemVerticalIndent: Integer; override;
    function GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode; override;
    function GetPainterClass: TcxCustomGridRecordsPainterClass; override;
    function GetPixelScrollContentSize: Integer; override;
    function GetRecordScrollSize(ARecordIndex: Integer): Integer; override;
    function GetRowCountByItem(AItem: TcxCustomGridRecordViewInfo): Integer; virtual;
    function GetRowCountByItemHeight(AValue: Integer): Integer; virtual;
    function GetRowCountByRecordIndex(AValue: Integer): Integer; virtual;
    function GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    function GetItemCountInBand(ARecordIndexInBand: Integer): Integer; override;
    function GetContentScrollSize: Integer; override;
    function HasFixedGroupItem: Boolean; virtual;
    function IsHorizontalScrolling: Boolean; override;
    function IsRecordFullyVisibleAtBottom(ARecordIndex: Integer): Boolean; override;
    function NeedConsideredRecordOffset(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Boolean; override;
    procedure OffsetItem(AIndex: Integer; AOffset: Integer); override;
    procedure UpdateVisibleCount; override;

    property FixedGroupItem: TcxCustomGridRecordViewInfo read FFixedGroupItem write SetFixedGroupItem;
  public
    destructor Destroy; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;

    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure OffsetRecords(AItemCountDelta: Integer; APixelScrollRecordOffsetDelta: Integer); override;
  end;

  { TcxGridWinExplorerViewViewInfo }

  TcxGridWinExplorerViewViewInfo = class(TcxCustomGridTableViewInfo)
  private
    function GetGridView: TcxGridWinExplorerView;
    function GetRecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo;
    function GetScrollableAreaHeight: Integer;
  protected
    procedure BeforeCalculating; override;
    procedure Calculate; override;
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;
    function GetContentScrollSize: Integer; virtual;
  public
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    property GridView: TcxGridWinExplorerView read GetGridView;
    property RecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo read GetRecordsViewInfo;
    property ScrollableAreaHeight: Integer read GetScrollableAreaHeight;
  end;

  { TcxGridWinExplorerViewItemOptions }

  TcxGridWinExplorerViewItemOptions = class(TcxCustomGridTableItemOptions)
  published
    property Sorting;
  end;

  { TcxGridWinExplorerViewItemStyles }

  TcxGridWinExplorerViewItemStyles = class(TcxCustomGridTableItemStyles)
  private
    FOnGetTextItemStyle: TcxGridGetCellStyleEvent;
    FOnGetDescriptionItemStyle: TcxGridGetCellStyleEvent;

    function GetGridViewValue: TcxGridWinExplorerView;
    function GetItem: TcxGridWinExplorerViewItem;
    procedure SetOnGetTextItemStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetDescriptionItemStyle(Value: TcxGridGetCellStyleEvent);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure GetTextItemParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    procedure GetDescriptionItemParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;

    property GridView: TcxGridWinExplorerView read GetGridViewValue;
    property Item: TcxGridWinExplorerViewItem read GetItem;
  published
    property TextItem: TcxStyle index isWinExplorerViewItemTextItem read GetValue write SetValue;
    property DescriptionItem: TcxStyle index isWinExplorerViewItemDescriptionItem read GetValue write SetValue;
    property OnGetTextItemStyle: TcxGridGetCellStyleEvent read FOnGetTextItemStyle write SetOnGetTextItemStyle;
    property OnGetDescriptionItemStyle: TcxGridGetCellStyleEvent read FOnGetDescriptionItemStyle write SetOnGetDescriptionItemStyle;
  end;

  { TcxGridWinExplorerViewItem }

  TcxGridWinExplorerViewItem = class(TcxCustomGridTableItem)
  private
    function GetOptions: TcxGridWinExplorerViewItemOptions;
    function GetStyles: TcxGridWinExplorerViewItemStyles;
    procedure SetOptions(AValue: TcxGridWinExplorerViewItemOptions);
    procedure SetStyles(Value: TcxGridWinExplorerViewItemStyles);
  protected
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
    function GetStylesClass: TcxCustomGridTableItemStylesClass; override;
    function IsPropertiesDefault: Boolean; virtual;
  published
    property Options: TcxGridWinExplorerViewItemOptions read GetOptions write SetOptions;
    property SortIndex;
    property SortOrder;
    property Styles: TcxGridWinExplorerViewItemStyles read GetStyles write SetStyles;
  end;

    { TcxGridWinExplorerViewInfoCacheItem }

  TcxGridWinExplorerViewInfoCacheItem = class(TcxCustomGridTableViewInfoCacheItem);

  { TcxGridWinExplorerViewCustomRecord }

  TcxGridWinExplorerViewCustomRecord = class(TcxCustomGridRecord)
  private
    function GetGridView: TcxGridWinExplorerView;
    function GetViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo;
  protected
    function GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass; override;
  public
    property GridView: TcxGridWinExplorerView read GetGridView;
    property ViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo read GetViewInfo;
  end;

  { TcxGridWinExplorerViewGroupRecord }

  TcxGridWinExplorerViewGroupRecord = class(TcxGridWinExplorerViewCustomRecord)
  private
    function GetGroupItem: TcxGridWinExplorerViewItem;
  protected
    procedure DoCollapse(ARecurse: Boolean); override;
    procedure DoExpand(ARecurse: Boolean); override;
    function GetExpandable: Boolean; override;
    function GetExpanded: Boolean; override;
    function GetDisplayCaption: string; virtual;
    function GetDisplayText(Index: Integer): string; override;
    function GetIsData: Boolean; override;
    function GetIsParent: Boolean; override;
    function GetViewInfoClass: TcxCustomGridRecordViewInfoClass; override;

    property GroupItem: TcxGridWinExplorerViewItem read GetGroupItem;
  public
    property DisplayCaption: string read GetDisplayCaption;
  end;

  { TcxGridWinExplorerViewRecord }

  TcxGridWinExplorerViewDataRecord = class(TcxGridWinExplorerViewCustomRecord)
  protected
    function GetHasCells: Boolean; override;
    function GetViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
  end;

  { TcxGridWinExplorerViewData }

  TcxGridWinExplorerViewViewData = class(TcxCustomGridTableViewData)
  private
    function GetGridView: TcxGridWinExplorerView;
  protected
    function GetRecordChildCount(ARecord: TcxCustomGridRecord): Integer; virtual;
    function GetDataRecordClass: TcxCustomGridRecordClass; virtual;
    function GetGroupRecordClass: TcxCustomGridRecordClass; virtual;
    function GetLastChildRecordIndex(AParentRecord: TcxCustomGridRecord): Integer; virtual;
    function GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; override;
    function IsGroup(const ARecordInfo: TcxRowInfo): Boolean; virtual;
    function IsGrouped: Boolean; virtual;
    function IsLastChildRecord(ARecord: TcxCustomGridRecord): Boolean; virtual;
  public
    procedure Collapse(ARecurse: Boolean); override;
    procedure Expand(ARecurse: Boolean); override;

    property GridView: TcxGridWinExplorerView read GetGridView;
  end;

  { TcxGridWinExplorerViewControllerHelper }

  TcxGridWinExplorerViewControllerHelper = class
  private
    FController: TcxGridWinExplorerViewController;

    function GetRecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo;
    function GetSite: TcxGridSite;
    function GetViewData: TcxGridWinExplorerViewViewData;
    function GetViewInfo: TcxGridWinExplorerViewViewInfo;
  protected
    procedure GoDown; virtual;
    procedure GoLeft; virtual;
    procedure GoNext; virtual;
    procedure GoPrev; virtual;
    procedure GoRight; virtual;
    procedure GoUp; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;

    procedure AdjustContentScrollPos(var Value: Integer); virtual;
    function GetContentScrollPageSize: Integer; virtual;
    function GetContentScrollPosition: Integer; virtual;
    function GetContentScrollSize: Integer; virtual;
    function GetContentScrollStep: Integer; virtual;
    function GetDataScrollPageSize: Integer; virtual;
    function GetDataScrollPosition: Integer; virtual;
    function GetDataScrollSize: Integer; virtual;
    function GetDataScrollStep: Integer; virtual;
    function GetIsRecordsScrollHorizontal: Boolean; virtual;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; virtual;
    function GetScrollableAreaSize(AKind: TScrollBarKind): Integer; virtual;
    function HScrollBarMaxValue: Integer; virtual;
    function HScrollBarPageSize: Integer; virtual;
    function HScrollBarPosition: Integer; virtual;
    function HScrollBarStep: Integer; virtual;
    procedure InitHScrollBarsParameters; virtual;
    procedure InitScrollBarsParameters; virtual;
    procedure InitVScrollBarsParameters; virtual;
    function IsDataScrollBar(AKind: TScrollBarKind): Boolean; virtual;
    function NeedAdjustScrollPosOnDataScroll(AScrollCode: TScrollCode): Boolean; virtual;
    function ScrollBarMaxValue(AKind: TScrollBarKind): Integer; virtual;
    function ScrollBarPageSize(AKind: TScrollBarKind): Integer; virtual;
    function ScrollBarPosition(AKind: TScrollBarKind): Integer; virtual;
    function ScrollBarStep(AKind: TScrollBarKind): Integer; virtual;
    procedure ScrollData(ADirection: TcxDirection); virtual;
    function VScrollBarMaxValue: Integer; virtual;
    function VScrollBarPageSize: Integer; virtual;
    function VScrollBarPosition: Integer; virtual;
    function VScrollBarStep: Integer; virtual;

    property Controller: TcxGridWinExplorerViewController read FController;
    property RecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo read GetRecordsViewInfo;
    property Site: TcxGridSite read GetSite;
    property ViewData: TcxGridWinExplorerViewViewData read GetViewData;
    property ViewInfo: TcxGridWinExplorerViewViewInfo read GetViewInfo;
  public
    constructor Create(AController: TcxGridWinExplorerViewController); virtual;
  end;

  TcxGridWinExplorerViewControllerHelperClass = class of TcxGridWinExplorerViewControllerHelper;

  { TcxGridWinExplorerViewControllerContentModeHelper }

  TcxGridWinExplorerViewControllerContentModeHelper = class(TcxGridWinExplorerViewControllerHelper)
  protected
    procedure ScrollData(ADirection: TcxDirection); override;
  end;

  { TcxGridWinExplorerViewControllerImagesModeHelper }

  TcxGridWinExplorerViewControllerImagesModeHelper = class(TcxGridWinExplorerViewControllerHelper)
  private
    function GetRecordsViewInfo: TcxGridWinExplorerViewImagesModeRecordsViewInfo;
  protected
    procedure FocusRecordInNextRow; virtual;
    procedure FocusRecordInPrevRow; virtual;

    procedure GoDown; override;
    procedure GoUp; override;
    procedure ScrollData(ADirection: TcxDirection); override;

    property RecordsViewInfo: TcxGridWinExplorerViewImagesModeRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridWinExplorerViewControllerListModeHelper }

  TcxGridWinExplorerViewControllerListModeHelper = class(TcxGridWinExplorerViewControllerHelper)
  private
    function GetRecordsViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo;
  protected
    procedure FocusNextRecord; virtual;
    procedure FocusPrevRecord; virtual;
    procedure FocusRecordInNextColumn; virtual;
    procedure FocusRecordInPrevColumn; virtual;

    procedure GoDown; override;
    procedure GoNext; override;
    procedure GoPrev; override;
    procedure GoUp; override;
    function GetContentScrollPageSize: Integer; override;
    function GetContentScrollStep: Integer; override;
    function GetDataScrollPageSize: Integer; override;
    function GetDataScrollPosition: Integer; override;
    function GetDataScrollSize: Integer; override;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function IsDataScrollBar(AKind: TScrollBarKind): Boolean; override;
    function NeedAdjustScrollPosOnDataScroll(AScrollCode: TScrollCode): Boolean; override;
    function NeedDataScroll: Boolean; virtual;
    procedure ScrollData(ADirection: TcxDirection); override;

    property RecordsViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridWinExplorerViewController }

  TcxGridWinExplorerViewController = class(TcxCustomGridTableController)
  private
    FHelper: TcxGridWinExplorerViewControllerHelper;
    FContentScrollPos: Integer;

    procedure CreateHelper;
    procedure DestroyHelper;
    procedure RecreateHelper;

    function GetGridView: TcxGridWinExplorerView;
    function GetItemSet: TcxGridWinExplorerViewItemSet;
    function GetViewData: TcxGridWinExplorerViewViewData; inline;
    function GetViewInfo: TcxGridWinExplorerViewViewInfo;
    procedure SetContentScrollPos(Value: Integer);
  protected
    function CanProcessMultiSelect(AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean; override;
    procedure CheckCoordinates; override;
    procedure ContentScroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure DataScroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    function GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean; override;
    function GetDragScrollInterval: Integer; override;
    function GetFirstIndexInScrollBand(ARecordIndexInBand: Integer): Integer; override;
    function GetHelperClass: TcxGridWinExplorerViewControllerHelperClass; virtual;
    function GetIsRecordsScrollHorizontal: Boolean; override;
    function GetMaxTopRecordIndexValue: Integer; override;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetRecordCountInScrollBand(ARecordIndexInBand: Integer): Integer; override;
    function GetScrollBarLineDownDirection(AScrollBarKind: TScrollBarKind): TcxDirection; virtual;
    function GetScrollBarLineUpDirection(AScrollBarKind: TScrollBarKind): TcxDirection; virtual;
    function GetTopRecordIndexByBottomRecord(ABottomRecordIndex: Integer): Integer; override;
    function GetVisibleChildCountOnTop(AParentRecord: TcxCustomGridRecord): Integer; virtual;
    function IsRecordFullyVisibleAtBottom(ARecordIndex: Integer): Boolean; override;
    function NeedAdjustDataScrollBarPosOnDataScroll(AScrollCode: TScrollCode): Boolean; virtual;
    procedure ScrollData(ADirection: TcxDirection); override;
    procedure DisplayModeChanged; virtual;

    property ContentScrollPos: Integer read FContentScrollPos write SetContentScrollPos;
    property Helper: TcxGridWinExplorerViewControllerHelper read FHelper;
    property ItemSet: TcxGridWinExplorerViewItemSet read GetItemSet;
    property ViewData: TcxGridWinExplorerViewViewData read GetViewData;
    property ViewInfo: TcxGridWinExplorerViewViewInfo read GetViewInfo;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    procedure InitScrollBarsParameters; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function FindNextItem(AFocusedItemIndex: Integer; AGoForward: Boolean; AGoOnCycle: Boolean;
      AFollowVisualOrder: Boolean; out ACycleChanged: Boolean; ARecord: TcxCustomGridRecord): Integer; override;
    procedure MakeItemVisible(AItem: TcxCustomGridTableItem); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    property GridView: TcxGridWinExplorerView read GetGridView;
  end;

  { TcxGridWinExplorerViewDateTimeHandling }

  TcxGridWinExplorerViewDateTimeHandling = class(TcxCustomGridTableDateTimeHandling);

  { TcxGridWinExplorerViewFiltering }

  TcxGridWinExplorerViewFiltering = class(TcxCustomGridTableFiltering);

  { TcxGridWinExplorerViewOptionsBehavior }

  TcxGridWinExplorerViewOptionsBehavior = class(TcxCustomGridTableOptionsBehavior)
  private
    FHotTrack: Boolean;

    procedure SetHotTrack(const Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;

    procedure Assign(Source: TPersistent); override;
  published
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
  end;

  TcxGridWinExplorerViewItemSetItem = class
  private
    FGridItem: TcxGridWinExplorerViewItem;
    FIndex: Integer;
  public
    constructor Create(AGridItem: TcxGridWinExplorerViewItem; AIndex: Integer); virtual;

    property GridItem: TcxGridWinExplorerViewItem read FGridItem;
    property Index: Integer read FIndex;
  end;

  { TcxGridWinExplorerViewItemSet }

  TcxGridWinExplorerViewItemSet = class(TcxCustomGridOptions)
  private
    FItems: TdxFastObjectList;

    function GetGridView: TcxGridWinExplorerView;
    function GetItem(AIndex: Integer): TcxGridWinExplorerViewItemSetItem;
  protected
    function GetValue(AIndex: Integer): TcxGridWinExplorerViewItem; virtual;
    procedure SetValue(AIndex: Integer; AValue: TcxGridWinExplorerViewItem); virtual;

    function FindItemIndex(AGridItemIndex: Integer): Integer; virtual;
    procedure GridItemRemove(AGridItem: TcxCustomGridTableItem); virtual;
    function HasCheckBoxItem: Boolean; virtual;
    function HasDescriptionItem: Boolean; virtual;
    function HasTextItem: Boolean; virtual;

    property Items[AIndex: Integer]: TcxGridWinExplorerViewItemSetItem read GetItem;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property GridView: TcxGridWinExplorerView read GetGridView;
  published
    property CheckBoxItem: TcxGridWinExplorerViewItem index isiCheckBoxItem read GetValue write SetValue;
    property DescriptionItem: TcxGridWinExplorerViewItem index isiDescriptionItem read GetValue write SetValue;
    property ExtraLargeImageItem: TcxGridWinExplorerViewItem index isiExtraLargeImageItem read GetValue write SetValue;
    property ExtraLargeImageIndexItem: TcxGridWinExplorerViewItem index isiExtraLargeImageIndexItem read GetValue write SetValue;
    property GroupItem: TcxGridWinExplorerViewItem index isiGroupItem read GetValue write SetValue;
    property LargeImageItem: TcxGridWinExplorerViewItem index isiLargeImageItem read GetValue write SetValue;
    property LargeImageIndexItem: TcxGridWinExplorerViewItem index isiLargeImageIndexItem read GetValue write SetValue;
    property MediumImageItem: TcxGridWinExplorerViewItem index isiMediumImageItem read GetValue write SetValue;
    property MediumImageIndexItem: TcxGridWinExplorerViewItem index isiMediumImageIndexItem read GetValue write SetValue;
    property SmallImageItem: TcxGridWinExplorerViewItem index isiSmallImageItem read GetValue write SetValue;
    property SmallImageIndexItem: TcxGridWinExplorerViewItem index isiSmallImageIndexItem read GetValue write SetValue;
    property TextItem: TcxGridWinExplorerViewItem index isiTextItem read GetValue write SetValue;
  end;

  { TcxGridWinExplorerViewOptionsDisplayModeIndents }

  TcxGridWinExplorerViewOptionsDisplayModeIndentsClass = class of TcxGridWinExplorerViewOptionsDisplayModeIndents;
  TcxGridWinExplorerViewOptionsDisplayModeIndents = class(TcxCustomGridOptions)
  strict private
    FBetweenCheckBoxAndImage: Integer;
    FBetweenGroups: Integer;
    FBetweenGroupAndItem: Integer;
    FBetweenImageAndDescription: Integer;
    FBetweenImageAndText: Integer;
    FBetweenTextAndDescription: Integer;

    procedure SetIndentBetweenCheckBoxAndImage(AValue: Integer);
    procedure SetIndentBetweenGroupAndItem(AValue: Integer);
    procedure SetIndentBetweenGroups(AValue: Integer);
    procedure SetBetweenImageAndDescription(AValue: Integer);
    procedure SetIndentBetweenImageAndText(AValue: Integer);
    procedure SetIndentBetweenTextAndDescription(AValue: Integer);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function GetIndentBetweenImageAndTextDefaultValue: Integer; virtual;

    property BetweenCheckBoxAndImage: Integer read FBetweenCheckBoxAndImage write SetIndentBetweenCheckBoxAndImage default cxGridWinExplorerViewRecordItemIndent;
    property BetweenGroupAndItem: Integer read FBetweenGroupAndItem write SetIndentBetweenGroupAndItem default 0;
    property BetweenGroups: Integer read FBetweenGroups write SetIndentBetweenGroups default 0;
    property BetweenImageAndDescription: Integer read FBetweenImageAndDescription write SetBetweenImageAndDescription default cxGridWinExplorerViewRecordItemIndent;
    property BetweenImageAndText: Integer read FBetweenImageAndText write SetIndentBetweenImageAndText default cxGridWinExplorerViewRecordItemIndent;
    property BetweenTextAndDescription: Integer read FBetweenTextAndDescription write SetIndentBetweenTextAndDescription default cxGridWinExplorerViewRecordItemIndent;
  public
    constructor Create(AGridView: TcxCustomGridView); override;

    procedure Assign(Source: TPersistent); override;
  end;

  { TcxGridWinExplorerViewOptionsContentDisplayModeIndents }

  TcxGridWinExplorerViewOptionsContentDisplayModeIndents = class(TcxGridWinExplorerViewOptionsDisplayModeIndents)
  protected
    function GetIndentBetweenImageAndTextDefaultValue: Integer; override;
  published
    property BetweenCheckBoxAndImage;
    property BetweenImageAndText default cxGridWinExplorerViewContentModeIndentBetweenImageAndText;
    property BetweenTextAndDescription;
  end;

  { TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents }

  TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents = class(TcxGridWinExplorerViewOptionsDisplayModeIndents)
  published
    property BetweenCheckBoxAndImage;
    property BetweenGroupAndItem;
    property BetweenGroups;
    property BetweenImageAndText;
    property BetweenTextAndDescription;
  end;

  { TcxGridWinExplorerViewOptionsLargeDisplayModeIndents }

  TcxGridWinExplorerViewOptionsLargeDisplayModeIndents = class(TcxGridWinExplorerViewOptionsDisplayModeIndents)
  published
    property BetweenCheckBoxAndImage;
    property BetweenGroupAndItem;
    property BetweenGroups;
    property BetweenImageAndText;
    property BetweenTextAndDescription;
  end;

  { TcxGridWinExplorerViewOptionsMediumDisplayModeIndents }

  TcxGridWinExplorerViewOptionsMediumDisplayModeIndents = class(TcxGridWinExplorerViewOptionsDisplayModeIndents)
  published
    property BetweenCheckBoxAndImage;
    property BetweenGroupAndItem;
    property BetweenGroups;
    property BetweenImageAndText;
    property BetweenTextAndDescription;
  end;

  { TcxGridWinExplorerViewOptionsSmallDisplayModeIndents }

  TcxGridWinExplorerViewOptionsSmallDisplayModeIndents = class(TcxGridWinExplorerViewOptionsDisplayModeIndents)
  published
    property BetweenCheckBoxAndImage;
    property BetweenGroupAndItem;
    property BetweenGroups;
    property BetweenImageAndDescription;
    property BetweenImageAndText;
    property BetweenTextAndDescription;
  end;

  { TcxGridWinExplorerViewOptionsListDisplayModeIndents }

  TcxGridWinExplorerViewOptionsListDisplayModeIndents = class(TcxGridWinExplorerViewOptionsDisplayModeIndents)
  published
    property BetweenCheckBoxAndImage;
    property BetweenGroupAndItem;
    property BetweenGroups;
    property BetweenImageAndDescription;
    property BetweenImageAndText;
    property BetweenTextAndDescription;
  end;

  { TcxGridWinExplorerViewOptionsTilesDisplayModeIndents }

  TcxGridWinExplorerViewOptionsTilesDisplayModeIndents = class(TcxGridWinExplorerViewOptionsDisplayModeIndents)
  published
    property BetweenGroupAndItem;
    property BetweenGroups;
    property BetweenImageAndDescription;
    property BetweenImageAndText;
    property BetweenTextAndDescription;
  end;

  { TcxGridWinExplorerViewDisplayModeOptions }

  TcxGridWinExplorerViewOptionsDisplayMode = class(TcxCustomGridOptions)
  strict private
    FImageSize: TcxSize;
    FIndents: TcxGridWinExplorerViewOptionsDisplayModeIndents;
    FRecordWidth: Integer;
    FShowItemDescriptions: Boolean;

    function GetActive: Boolean;
    function GetGridView: TcxGridWinExplorerView;
    procedure ImageSizeChangeHandler(Sender: TObject);
    procedure SetActive(Value: Boolean);
    procedure SetImageSize(AValue: TcxSize);
    procedure SetIndents(AValue: TcxGridWinExplorerViewOptionsDisplayModeIndents);
    procedure SetRecordWidth(AValue: Integer);
    procedure SetShowItemDescriptions(AValue: Boolean);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    function GetDefaultDisplayModeKind: TcxGridWinExplorerViewDisplayMode; virtual;
    function GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode; virtual; abstract;
    function GetImageHeightDefaultValue: Integer; virtual;
    function GetImageWidthDefaultValue: Integer; virtual;
    function GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass; virtual;
    function GetShowItemDescriptionsDefaultValue: Boolean; virtual;

    property RecordWidth: Integer read FRecordWidth write SetRecordWidth default cxGridWinExplorerViewDefaultRecordWidth;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;

    property GridView: TcxGridWinExplorerView read GetGridView;
  published
    property Active: Boolean read GetActive write SetActive stored False;
    property ImageSize: TcxSize read FImageSize write SetImageSize;
    property Indents: TcxGridWinExplorerViewOptionsDisplayModeIndents read FIndents write SetIndents;
    property ShowItemDescriptions: Boolean read FShowItemDescriptions write SetShowItemDescriptions default False;
  end;

  { TcxGridWinExplorerViewOptionsContentDisplayMode }

  TcxGridWinExplorerViewOptionsContentDisplayMode = class(TcxGridWinExplorerViewOptionsDisplayMode)
  private
    function GetIndents: TcxGridWinExplorerViewOptionsContentDisplayModeIndents;
    procedure SetIndents(AValue: TcxGridWinExplorerViewOptionsContentDisplayModeIndents);
  protected
    function GetImageHeightDefaultValue: Integer; override;
    function GetImageWidthDefaultValue: Integer; override;
    function GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass; override;
    function GetShowItemDescriptionsDefaultValue: Boolean; override;
    function GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode; override;
  published
    property Indents: TcxGridWinExplorerViewOptionsContentDisplayModeIndents read GetIndents write SetIndents;
    property ShowItemDescriptions default True;
  end;

  { TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode }

  TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode = class(TcxGridWinExplorerViewOptionsDisplayMode)
  private
    function GetIndents: TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents;
    procedure SetIndents(AValue: TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents);
  protected
    function GetImageHeightDefaultValue: Integer; override;
    function GetImageWidthDefaultValue: Integer; override;
    function GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass; override;
    function GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode; override;
  published
    property Indents: TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents read GetIndents write SetIndents;
    property RecordWidth;
  end;

  { TcxGridWinExplorerViewOptionsLargeImagesDisplayMode }

  TcxGridWinExplorerViewOptionsLargeImagesDisplayMode = class(TcxGridWinExplorerViewOptionsDisplayMode)
  private
    function GetIndents: TcxGridWinExplorerViewOptionsLargeDisplayModeIndents;
    procedure SetIndents(AValue: TcxGridWinExplorerViewOptionsLargeDisplayModeIndents);
  protected
    function GetImageHeightDefaultValue: Integer; override;
    function GetImageWidthDefaultValue: Integer; override;
    function GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass; override;
    function GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode; override;
  published
    property Indents: TcxGridWinExplorerViewOptionsLargeDisplayModeIndents read GetIndents write SetIndents;
    property RecordWidth;
  end;

  { TcxGridWinExplorerViewOptionsMediumDisplayMode }

  TcxGridWinExplorerViewOptionsMediumDisplayMode = class(TcxGridWinExplorerViewOptionsDisplayMode)
  private
    function GetIndents: TcxGridWinExplorerViewOptionsMediumDisplayModeIndents;
    procedure SetIndents(AValue: TcxGridWinExplorerViewOptionsMediumDisplayModeIndents);
  protected
    function GetImageHeightDefaultValue: Integer; override;
    function GetImageWidthDefaultValue: Integer; override;
    function GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass; override;
    function GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode; override;
  published
    property Indents: TcxGridWinExplorerViewOptionsMediumDisplayModeIndents read GetIndents write SetIndents;
    property RecordWidth;
  end;

  { TcxGridWinExplorerViewOptionsSmallDisplayMode }

  TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode = class(TcxGridWinExplorerViewOptionsDisplayMode)
  protected
    function GetImageHeightDefaultValue: Integer; override;
    function GetImageWidthDefaultValue: Integer; override;
  published
    property RecordWidth;
  end;

  { TcxGridWinExplorerViewOptionsSmallImagesDisplayMode }

  TcxGridWinExplorerViewOptionsSmallImagesDisplayMode = class(TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode)
  private
    function GetIndents: TcxGridWinExplorerViewOptionsSmallDisplayModeIndents;
    procedure SetIndents(AValue: TcxGridWinExplorerViewOptionsSmallDisplayModeIndents);
  protected
    function GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass; override;
    function GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode; override;
  published
    property Indents: TcxGridWinExplorerViewOptionsSmallDisplayModeIndents read GetIndents write SetIndents;
  end;

    { TcxGridWinExplorerViewOptionsListDisplayMode }

  TcxGridWinExplorerViewOptionsListDisplayMode = class(TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode)
  private
    function GetIndents: TcxGridWinExplorerViewOptionsListDisplayModeIndents;
    procedure SetIndents(AValue: TcxGridWinExplorerViewOptionsListDisplayModeIndents);
  protected
    function GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass; override;
    function GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode; override;
  published
    property Indents: TcxGridWinExplorerViewOptionsListDisplayModeIndents read GetIndents write SetIndents;
  end;

  { TcxGridWinExplorerViewOptionsTilesDisplayMode }

  TcxGridWinExplorerViewOptionsTilesDisplayMode = class(TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode)
  private
    function GetIndents: TcxGridWinExplorerViewOptionsTilesDisplayModeIndents;
    procedure SetIndents(AValue: TcxGridWinExplorerViewOptionsTilesDisplayModeIndents);
  protected
    function GetImageHeightDefaultValue: Integer; override;
    function GetImageWidthDefaultValue: Integer; override;
    function GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass; override;
    function GetShowItemDescriptionsDefaultValue: Boolean; override;
    function GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode; override;
  published
    property Indents: TcxGridWinExplorerViewOptionsTilesDisplayModeIndents read GetIndents write SetIndents;
    property ShowItemDescriptions default True;
  end;

  { TcxGridWinExplorerViewOptionsDisplayModes }

  TcxGridWinExplorerViewOptionsDisplayModes = class(TcxCustomGridOptions)
  strict private
    FContent: TcxGridWinExplorerViewOptionsContentDisplayMode;
    FExtraLargeImages: TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode;
    FLargeImages: TcxGridWinExplorerViewOptionsLargeImagesDisplayMode;
    FList: TcxGridWinExplorerViewOptionsListDisplayMode;
    FMediumImages: TcxGridWinExplorerViewOptionsMediumDisplayMode;
    FSmallImages: TcxGridWinExplorerViewOptionsSmallImagesDisplayMode;
    FTiles: TcxGridWinExplorerViewOptionsTilesDisplayMode;

    procedure SetContent(AValue: TcxGridWinExplorerViewOptionsContentDisplayMode);
    procedure SetExtraLargeImages(AValue: TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode);
    procedure SetLargeImages(AValue: TcxGridWinExplorerViewOptionsLargeImagesDisplayMode);
    procedure SetList(AValue: TcxGridWinExplorerViewOptionsListDisplayMode);
    procedure SetMediumImages(AValue: TcxGridWinExplorerViewOptionsMediumDisplayMode);
    procedure SetSmallImages(AValue: TcxGridWinExplorerViewOptionsSmallImagesDisplayMode);
    procedure SetTiles(AValue: TcxGridWinExplorerViewOptionsTilesDisplayMode);
  protected
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Content: TcxGridWinExplorerViewOptionsContentDisplayMode read FContent write SetContent;
    property ExtraLargeImages: TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode read FExtraLargeImages write SetExtraLargeImages;
    property LargeImages: TcxGridWinExplorerViewOptionsLargeImagesDisplayMode read FLargeImages write SetLargeImages;
    property List: TcxGridWinExplorerViewOptionsListDisplayMode read FList write SetList;
    property MediumImages: TcxGridWinExplorerViewOptionsMediumDisplayMode read FMediumImages write SetMediumImages;
    property SmallImages: TcxGridWinExplorerViewOptionsSmallImagesDisplayMode read FSmallImages write SetSmallImages;
    property Tiles: TcxGridWinExplorerViewOptionsTilesDisplayMode read FTiles write SetTiles;
  end;

  { TcxGridWinExplorerViewOptionsData }

  TcxGridWinExplorerViewOptionsData = class(TcxCustomGridTableOptionsData);

  { TcxGridWinExplorerViewOptionsSelection }

  TcxGridWinExplorerViewOptionsSelection = class(TcxCustomGridTableOptionsSelection);

  { TcxGridWinExplorerViewOptionsView }

  TcxGridWinExplorerViewOptionsView = class(TcxCustomGridTableOptionsView)
  private
    FShowItemCheckBoxes: Boolean;
    FShowExpandButtons: Boolean;

    function GetGridView: TcxGridWinExplorerView; inline;
    procedure SetShowItemCheckBoxes(AValue: Boolean);
    procedure SetShowExpandButtons(AValue: Boolean);
  public
    procedure Assign(ASource: TPersistent); override;

    property GridView: TcxGridWinExplorerView read GetGridView;
  published
    property ShowItemCheckBoxes: Boolean read FShowItemCheckBoxes write SetShowItemCheckBoxes default False;
    property ShowExpandButtons: Boolean read FShowExpandButtons write SetShowExpandButtons default False;
  end;

  { TcxGridWinExplorerViewStyles }

  TcxGridWinExplorerViewStyles = class(TcxCustomGridTableViewStyles)
  private
    FIsGroupParams: Boolean;

    FOnGetTextItemStyle: TcxGridGetCellStyleEvent;
    FOnGetDescriptionItemStyle: TcxGridGetCellStyleEvent;
    FOnGetGroupStyle: TcxGridGetRecordStyleEvent;

    function GetState(AIndex: Integer): TcxButtonState;
    function IsGroupParams(AIndex: Integer): Boolean;
    procedure SetOnGetTextItemStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetDescriptionItemStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetGroupStyle(Value: TcxGridGetRecordStyleEvent);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    procedure GetDefaultTextItemParams(AData: TObject; out AParams: TcxViewParams); virtual;
    procedure GetTextItemParams(ARecord: TcxCustomGridRecord;
      AItem: TcxGridWinExplorerViewItem; out AParams: TcxViewParams); virtual;
    procedure GetDescriptionItemParams(ARecord: TcxCustomGridRecord;
      AItem: TcxGridWinExplorerViewItem; out AParams: TcxViewParams); virtual;
  public
    procedure Assign(Source: TPersistent); override;

    procedure GetDataCellParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams;
      AUseViewInfo: Boolean = False; ACellViewInfo: TcxGridTableCellViewInfo = nil; AIgnoreSelection: Boolean = False); override;
    procedure GetTextItemCellParams(ARecord: TcxCustomGridRecord;
      AItem: TcxGridWinExplorerViewItem; out AParams: TcxViewParams); virtual;
    procedure GetDescriptionItemCellParams(ARecord: TcxCustomGridRecord;
      AItem: TcxGridWinExplorerViewItem; out AParams: TcxViewParams); virtual;
    procedure GetGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
  published
    property TextItem: TcxStyle index vsWinExplorerTextItem read GetValue write SetValue;
    property DescriptionItem: TcxStyle index vsWinExplorerDescriptionItem read GetValue write SetValue;
    property Group: TcxStyle index vsWinExplorerGroup read GetValue write SetValue;
    property OnGetTextItemStyle: TcxGridGetCellStyleEvent read FOnGetTextItemStyle write SetOnGetTextItemStyle;
    property OnGetDescriptionItemStyle: TcxGridGetCellStyleEvent read FOnGetDescriptionItemStyle write SetOnGetDescriptionItemStyle;
    property OnGetGroupStyle: TcxGridGetRecordStyleEvent read FOnGetGroupStyle write SetOnGetGroupStyle;
  end;

  { TcxGridWinExplorerViewImageSet }

  TcxGridWinExplorerViewImageSet = class(TcxCustomGridOptions)
  private
    FExtraLargeImages: TCustomImageList;
    FExtraLargeImagesChangeLink: TChangeLink;
    FLargeImages: TCustomImageList;
    FLargeImagesChangeLink: TChangeLink;
    FMediumImages: TCustomImageList;
    FMediumImagesChangeLink: TChangeLink;
    FSmallImages: TCustomImageList;
    FSmallImagesChangeLink: TChangeLink;

    function GetGridView: TcxGridWinExplorerView; inline;
    procedure SetExtraLargeImages(Value: TCustomImageList);
    procedure SetLargeImages(Value: TCustomImageList);
    procedure SetMediumImages(Value: TCustomImageList);
    procedure SetSmallImages(Value: TCustomImageList);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property GridView: TcxGridWinExplorerView read GetGridView;
  published
    property ExtraLargeImages: TCustomImageList read FExtraLargeImages write SetExtraLargeImages;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property MediumImages: TCustomImageList read FMediumImages write SetMediumImages;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
  end;

  { TcxGridWinExplorerView }

  TcxGridWinExplorerView = class(TcxCustomGridTableView)
  strict private
    FActiveDisplayMode: TcxGridWinExplorerViewDisplayMode;
    FImageSet: TcxGridWinExplorerViewImageSet;
    FItemSet: TcxGridWinExplorerViewItemSet;
    FDisplayModes: TcxGridWinExplorerViewOptionsDisplayModes;

    function GetController: TcxGridWinExplorerViewController; inline;
    function GetDataController: TcxGridDataController;
    function GetDateTimeHandling: TcxGridWinExplorerViewDateTimeHandling;
    function GetFiltering: TcxGridWinExplorerViewFiltering;
    function GetItemSet: TcxGridWinExplorerViewItemSet;
    function GetOptionsBehavior: TcxGridWinExplorerViewOptionsBehavior;
    function GetOptionsData: TcxGridWinExplorerViewOptionsData;
    function GetImageSet: TcxGridWinExplorerViewImageSet;
    function GetOptionsSelection: TcxGridWinExplorerViewOptionsSelection;
    function GetOptionsView: TcxGridWinExplorerViewOptionsView;
    function GetStyles: TcxGridWinExplorerViewStyles;
    function GetViewData: TcxGridWinExplorerViewViewData;
    function GetViewInfo: TcxGridWinExplorerViewViewInfo; inline;
    procedure SetActiveDisplayMode(AValue: TcxGridWinExplorerViewDisplayMode);
    procedure SetDataController(AValue: TcxGridDataController);
    procedure SetDateTimeHandling(AValue: TcxGridWinExplorerViewDateTimeHandling);
    procedure SetFiltering(AValue: TcxGridWinExplorerViewFiltering);
    procedure SetItemSet(AValue: TcxGridWinExplorerViewItemSet);
    procedure SetOptionsBehavior(AValue: TcxGridWinExplorerViewOptionsBehavior);
    procedure SetOptionsData(AValue: TcxGridWinExplorerViewOptionsData);
    procedure SetImageSet(AValue: TcxGridWinExplorerViewImageSet);
    procedure SetOptionsSelection(AValue: TcxGridWinExplorerViewOptionsSelection);
    procedure SetOptionsView(AValue: TcxGridWinExplorerViewOptionsView);
    procedure SetStyles(Value: TcxGridWinExplorerViewStyles);
    procedure SetDisplayModes(AValue: TcxGridWinExplorerViewOptionsDisplayModes);
  protected
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    function HasLayoutCustomizationForm: Boolean; override;
    function IsLayoutChangeable: Boolean; override;

    procedure CreateOptions; override;
    procedure DestroyOptions; override;
    procedure DoAssign(ASource: TcxCustomGridView); override;

    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass; override;
    function GetFilteringClass: TcxCustomGridTableFilteringClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsDataClass: TcxCustomGridOptionsDataClass; override;
    function GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetPainterClass: TcxCustomGridPainterClass; override;
    function GetStylesClass: TcxCustomGridViewStylesClass; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;

    //scrolling
    function CanOffset(ARecordCountDelta: Integer; APixelScrollRecordOffsetDelta: Integer): Boolean; override;
    function IsRecordPixelScrolling: Boolean; override;
    procedure Offset(ARecordCountDelta: Integer; APixelScrollRecordOffsetDelta, DX, DY: Integer); override;

    procedure ChangeGroupIndex(AValue: Integer); virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure Grouping; virtual;
    procedure RemoveItem(AItem: TcxCustomGridTableItem); override;
    procedure ResetGrouping; virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GroupsAlwaysExpanded: Boolean; virtual;
    procedure SelectionChanged(AInfo: TcxSelectionChangedInfo); override;
  public
    property Controller: TcxGridWinExplorerViewController read GetController;
    property ViewData: TcxGridWinExplorerViewViewData read GetViewData;
    property ViewInfo: TcxGridWinExplorerViewViewInfo read GetViewInfo;
  published
    property ActiveDisplayMode: TcxGridWinExplorerViewDisplayMode read FActiveDisplayMode write SetActiveDisplayMode default dmContent;
    property DateTimeHandling: TcxGridWinExplorerViewDateTimeHandling read GetDateTimeHandling write SetDateTimeHandling;
    property DataController: TcxGridDataController read GetDataController write SetDataController;
    property Filtering: TcxGridWinExplorerViewFiltering read GetFiltering write SetFiltering;
    property ItemSet: TcxGridWinExplorerViewItemSet read GetItemSet write SetItemSet;
    property OptionsBehavior: TcxGridWinExplorerViewOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsData: TcxGridWinExplorerViewOptionsData read GetOptionsData write SetOptionsData;
    property ImageSet: TcxGridWinExplorerViewImageSet read GetImageSet write SetImageSet;
    property OptionsSelection: TcxGridWinExplorerViewOptionsSelection read GetOptionsSelection write SetOptionsSelection;
    property OptionsView: TcxGridWinExplorerViewOptionsView read GetOptionsView write SetOptionsView;
    property Styles: TcxGridWinExplorerViewStyles read GetStyles write SetStyles;
    property DisplayModes: TcxGridWinExplorerViewOptionsDisplayModes read FDisplayModes write SetDisplayModes;
  end;

implementation

uses
  Types, Math, Variants, RTLConsts, dxCore, cxCheckBox;

const
  HScrollDelta = 10;
  VScrollDelta = 10;

  cxGridWinExplorerViewContentBackgroundIndent = 1;
  cxGridWinExplorerViewGroupTextIndent = 5;
  cxGridWinExplorerViewGroupIndentBetweenExpandButtonAndText = 5;
  cxGridWinExplorerViewGroupExpandButtonIndent = 5;
  cxGridWinExplorerViewGroupCaptionLineIndent = 5;
  cxGridWinExplorerViewGroupRecordHeight = 27;
  cxGridWinExplorerViewIndentBetweenItemAndRecordBorder = 6;
  cxGridWinExplorerViewContentModeRecordMinHeight = 68;
  cxGridWinExplorerViewContentModeTextHeight = 25;
  cxGridWinExplorerViewContentModeTextWidth = 244;
  cxGridWinExplorerViewContentModeTextFontFactor = 1.5;
  cxGridWinExplorerViewSmallImagesModeTextWidth = 150;
  cxGridWinExplorerViewSmallImagesModeTextHeight = 19;
  cxGridWinExplorerViewSmallImagesModeDescriptionMinHeight = 35;
  cxGridWinExplorerViewMediumImagesModeTextHeight = 19;
  cxGridWinExplorerViewMediumImagesModeDescriptionHeight = 35;
  cxGridWinExplorerViewImagesModeHorizontalIndent = 23;
  cxGridWinExplorerViewImagesModeVerticalIndent = 23;
  cxGridWinExplorerViewSmallImagesModeHorizontalIndent = 0;
  cxGridWinExplorerViewSmallImagesModeVerticalIndent = 0;

type
  TcxCustomEditPropertiesAccess = class(TcxCustomEditProperties);
  TcxGridSiteAccess = class(TcxGridSite);

{ TcxGridWinExplorerViewRecordExpandButtonPainter }

function TcxGridWinExplorerViewRecordExpandButtonPainter.GetViewInfo: TcxGridWinExplorerViewExpandButtonViewInfo;
begin
  Result := TcxGridWinExplorerViewExpandButtonViewInfo(inherited ViewInfo);
end;

procedure TcxGridWinExplorerViewRecordExpandButtonPainter.Paint;
begin
  ViewInfo.LookAndFeelPainter.WinExplorerViewDrawScaledRecordExpandButton(
    Canvas, ViewInfo.Bounds, ViewInfo.ButtonState, ViewInfo.RecordViewInfo.Expanded, ScaleFactor);
end;

{ TcxGridWinExplorerViewImageCellPainter }

procedure TcxGridWinExplorerViewImageCellPainter.DrawEditViewInfo;
var
  ADisplayValue: Variant;
begin
  if ViewInfo.ImageList <> nil then
  begin
    ADisplayValue := ViewInfo.GetDisplayValue;
    if ADisplayValue <> '' then
      cxDrawImage(Canvas, ViewInfo.Bounds, nil, ViewInfo.ImageList, ADisplayValue, ifmProportionalStretch)
  end
  else
    inherited DrawEditViewInfo;
end;

function TcxGridWinExplorerViewImageCellPainter.GetViewInfo: TcxGridWinExplorerViewImageViewInfo;
begin
  Result := TcxGridWinExplorerViewImageViewInfo(inherited ViewInfo);
end;

{ TcxGridWinExplorerViewCustomRecordPainter }

procedure TcxGridWinExplorerViewCustomRecordPainter.AfterPaint;
begin
  inherited AfterPaint;
  Canvas.RestoreClipRegion;
end;

procedure TcxGridWinExplorerViewCustomRecordPainter.BeforePaint;
begin
  Canvas.SaveClipRegion;
  Canvas.IntersectClipRect(ViewInfo.Bounds);
  inherited BeforePaint;
end;

procedure TcxGridWinExplorerViewCustomRecordPainter.DrawBackground;
begin
  DrawBackground(ViewInfo.BackgroundBitmapBounds);
  DrawContentBackground(ViewInfo.ContentBackgroundBounds);
  if ViewInfo.Focused and not ViewInfo.Selected and not ViewInfo.HasFocusRect then
    Canvas.FrameRect(ViewInfo.ContentBackgroundBounds, ViewInfo.LookAndFeelPainter.DefaultGridLineColor, 1, cxBordersAll, True);
end;

procedure TcxGridWinExplorerViewCustomRecordPainter.DrawBackground(const R: TRect);
var
  AParams: TcxViewParams;
begin
  ViewInfo.SaveParams(AParams);
  try
    ViewInfo.Params := ViewInfo.BackParams;
    inherited DrawBackground(R);
  finally
    ViewInfo.RestoreParams(AParams);
  end;
end;

procedure TcxGridWinExplorerViewCustomRecordPainter.DrawContentBackground(const ABounds: TRect);
begin
//do nothing
end;

function TcxGridWinExplorerViewCustomRecordPainter.GetViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  Result := TcxGridWinExplorerViewCustomRecordViewInfo(inherited ViewInfo);
end;

{ TcxGridWinExplorerViewGroupRecordPainter }

procedure TcxGridWinExplorerViewGroupRecordPainter.DrawCaptionLine;
begin
  ViewInfo.LookAndFeelPainter.WinExplorerViewDrawGroupCaptionLine(Canvas, ViewInfo.CaptionLineBounds);
end;

procedure TcxGridWinExplorerViewGroupRecordPainter.DrawContentBackground(const ABounds: TRect);
var
  AState: TcxButtonState;
begin
  AState := ViewInfo.GetPaintState;
  if AState <> cxbsNormal then
    ViewInfo.LookAndFeelPainter.WinExplorerViewDrawGroup(Canvas, ABounds, AState, ViewInfo.Params.Color, ViewInfo.Params.Bitmap);
end;

procedure TcxGridWinExplorerViewGroupRecordPainter.DrawExpandButton;
begin
  if ViewInfo.HasExpandButton then
    ViewInfo.ExpandButtonViewInfo.Paint;
end;

procedure TcxGridWinExplorerViewGroupRecordPainter.Paint;
begin
  DrawCaptionLine;
  DrawText;
  inherited Paint;
end;

function TcxGridWinExplorerViewGroupRecordPainter.GetViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo;
begin
  Result := TcxGridWinExplorerViewGroupRecordViewInfo(inherited ViewInfo);
end;

{ TcxGridWinExplorerViewRecordPainter }

function TcxGridWinExplorerViewRecordPainter.CanDrawCheckBox: Boolean;
begin
  Result := ViewInfo.HasCheckBox;
end;

procedure TcxGridWinExplorerViewRecordPainter.DrawCheckBox;
begin
  if CanDrawCheckBox then
    ViewInfo.CheckBoxViewInfo.Paint;
end;

procedure TcxGridWinExplorerViewRecordPainter.DrawContentBackground(const ABounds: TRect);
var
  AState: TcxButtonState;
begin
  AState := ViewInfo.GetPaintState;
  if AState <> cxbsNormal then
    ViewInfo.LookAndFeelPainter.WinExplorerViewDrawRecord(Canvas, ABounds, AState, ViewInfo.Params.Color, ViewInfo.Params.Bitmap);
end;

procedure TcxGridWinExplorerViewRecordPainter.DrawDescription;
begin
  if ViewInfo.HasDescription then
    ViewInfo.DescriptionViewInfo.Paint;
end;

procedure TcxGridWinExplorerViewRecordPainter.DrawImage;
begin
  if ViewInfo.HasImage then
    ViewInfo.ImageViewInfo.Paint;
end;

procedure TcxGridWinExplorerViewRecordPainter.DrawTextBox;
begin
  if ViewInfo.HasText then
    ViewInfo.TextViewInfo.Paint;
end;

procedure TcxGridWinExplorerViewRecordPainter.Paint;
begin
  DrawImage;
  DrawTextBox;
  DrawDescription;
  DrawCheckBox;
  inherited Paint;
end;

function TcxGridWinExplorerViewRecordPainter.GetViewInfo: TcxGridWinExplorerViewRecordViewInfo;
begin
  Result := TcxGridWinExplorerViewRecordViewInfo(inherited ViewInfo);
end;

{ TcxGridWinExplorerViewContentModeRecordPainter }

function TcxGridWinExplorerViewContentModeRecordPainter.CanDrawCheckBox: Boolean;
begin
  Result := inherited CanDrawCheckBox and ((ViewInfo.GetPaintState <> cxbsNormal) or
    (ViewInfo.CheckBoxViewInfo.Value = True));
end;

procedure TcxGridWinExplorerViewContentModeRecordPainter.DrawSeparator;
begin
  ViewInfo.LookAndFeelPainter.DrawSeparator(Canvas, ViewInfo.GetSeparatorBounds, False);
end;

procedure TcxGridWinExplorerViewContentModeRecordPainter.Paint;
begin
  inherited Paint;
  DrawSeparator;
end;

function TcxGridWinExplorerViewContentModeRecordPainter.GetViewInfo: TcxGridWinExplorerViewContentModeRecordViewInfo;
begin
  Result := TcxGridWinExplorerViewContentModeRecordViewInfo(inherited ViewInfo);
end;

{ TcxGridWinExplorerViewTilesModeRecordPainter }

function TcxGridWinExplorerViewTilesModeRecordPainter.CanDrawCheckBox: Boolean;
begin
  Result := inherited CanDrawCheckBox and ((ViewInfo.GetPaintState <> cxbsNormal) or
    (ViewInfo.CheckBoxViewInfo.Value = True));
end;

{ TcxGridWinExplorerViewListModeRecordsPainter }

procedure TcxGridWinExplorerViewListModeRecordsPainter.Paint;
begin
  if ViewInfo.HasFixedGroupItem then
    ViewInfo.FixedGroupItem.Paint;
  inherited Paint;
end;

function TcxGridWinExplorerViewListModeRecordsPainter.GetViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo;
begin
  Result := TcxGridWinExplorerViewListModeRecordsViewInfo(inherited ViewInfo);
end;

{ TcxGridWinExplorerViewPainter }

function TcxGridWinExplorerViewPainter.CanOffset(AItemsOffset: Integer; DX: Integer; DY: Integer): Boolean;
begin
  Result := inherited CanOffset(AItemsOffset, DX, DY) and
    not (ViewData.IsGrouped and (GridView.ActiveDisplayMode = dmList));
end;

procedure TcxGridWinExplorerViewPainter.PaintContent;
begin
  DrawFindPanel;
  DrawFilterBar;
  inherited PaintContent;
end;

function TcxGridWinExplorerViewPainter.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewPainter.GetViewData: TcxGridWinExplorerViewViewData;
begin
  Result := TcxGridWinExplorerViewViewData(inherited ViewData);
end;

{ TcxGridWinExplorerViewExpandButtonViewInfo }

constructor TcxGridWinExplorerViewExpandButtonViewInfo.Create(
  ARecordViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo);
begin
  inherited Create(ARecordViewInfo.GridViewInfo);
  FRecordViewInfo := ARecordViewInfo;
end;

function TcxGridWinExplorerViewExpandButtonViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridWinExplorerViewExpandButtonViewInfo.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridWinExplorerViewExpandButtonViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := True;
end;

procedure TcxGridWinExplorerViewExpandButtonViewInfo.Click;
begin
  inherited Click;
  GridRecord.ToggleExpanded;
end;

function TcxGridWinExplorerViewExpandButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridExpandButtonHitTest;
end;

function TcxGridWinExplorerViewExpandButtonViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxGridWinExplorerViewExpandButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridWinExplorerViewRecordExpandButtonPainter;
end;

function TcxGridWinExplorerViewExpandButtonViewInfo.GetVisible: Boolean;
begin
  Result := GridRecord.Expandable;
end;

procedure TcxGridWinExplorerViewExpandButtonViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited InitHitTest(AHitTest);
  TcxGridExpandButtonHitTest(AHitTest).GridRecord := GridRecord;
end;

function TcxGridWinExplorerViewExpandButtonViewInfo.GetRecord: TcxGridWinExplorerViewGroupRecord;
begin
  Result := RecordViewInfo.GridRecord;
end;

{ TcxGridWinExplorerViewCustomCellViewInfo }

constructor TcxGridWinExplorerViewCustomCellViewInfo.Create(
  ARecordViewInfo: TcxCustomGridRecordViewInfo; AItem: TcxCustomGridTableItem);
begin
  FInternalProperties := GetInternalPropertiesClass.Create(nil);
  inherited Create(ARecordViewInfo, AItem);
  TcxCustomEditPropertiesAccess(FInternalProperties).ScaleFactor.Owner := nil;
  TcxCustomEditPropertiesAccess(FInternalProperties).ScaleFactor.Assign(ARecordViewInfo.ScaleFactor);
end;

destructor TcxGridWinExplorerViewCustomCellViewInfo.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FInternalProperties);
end;

procedure TcxGridWinExplorerViewCustomCellViewInfo.BeforeRecalculation;
begin
  inherited BeforeRecalculation;
  CalculateSelectedNeeded;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.CanDrawSelected: Boolean;
begin
  Result := not Editing;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.MouseDown(
  AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  ASelfLink: TcxObjectLink;
begin
  ASelfLink := cxAddObjectLink(Self);
  try
    Result := inherited MouseDown(AHitTest, AButton, AShift);
    if (ASelfLink.Ref <> nil) and (AButton = mbLeft) and GridView.IsDesigning then
    begin
      GridView.Controller.DesignController.SelectObject(Item, not (ssShift in AShift));
      Result := True;
    end;
  finally
    cxRemoveObjectLink(ASelfLink);
  end;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetAlwaysSelected: Boolean;
begin
  Result := CanDrawSelected;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetBorders: TcxBorders;
begin
  Result := inherited GetBorders;
  if Editing then
    Result := cxBordersAll;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := LookAndFeelPainter.DefaultGridLineColor;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := LookAndFeelPainter.BorderSize;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetDesignSelectionBounds: TRect;
begin
  Result := Bounds;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetHotTrack: Boolean;
begin
  Result := inherited GetHotTrack or RecordViewInfo.HotTrack;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and GridView.Controller.DesignController.IsObjectSelected(Item);
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetItemProperties: TcxCustomEditProperties;
begin
  Result := inherited GetItemProperties;
  if UseInternalProperties then
  begin
    if Item.IsPropertiesDefault then
      InitInternalProperties
    else
      InternalProperties.Assign(Result);
    Result := InternalProperties;
  end;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetTransparent: Boolean;
begin
  Result := inherited GetTransparent or (not Editing and (RecordViewInfo.GetPaintState <> cxbsNormal));
end;

procedure TcxGridWinExplorerViewCustomCellViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetDataCellParams(GridRecord, Item, AParams, True, Self);
end;

procedure TcxGridWinExplorerViewCustomCellViewInfo.InitInternalProperties;
begin
//do nothing
end;

function TcxGridWinExplorerViewCustomCellViewInfo.IsHotTracking: Boolean;
begin
  Result := ButtonState = cxbsHot;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.NeedLocalCopyOfEditViewData: Boolean;
begin
  Result := True;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.NeedShowEditOnDblClick: Boolean;
begin
  Result := True;
end;

procedure TcxGridWinExplorerViewCustomCellViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  inherited StateChanged(APrevState);
  if not IsDestroying and ((State = gcsSelected) or (APrevState = gcsSelected)) then
    RecordViewInfo.CellHotTrackStateChanged;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.UseInternalProperties: Boolean;
begin
  Result := True;
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetItem: TcxGridWinExplorerViewItem;
begin
  Result := TcxGridWinExplorerViewItem(inherited Item);
end;

function TcxGridWinExplorerViewCustomCellViewInfo.GetRecordViewInfo: TcxGridWinExplorerViewRecordViewInfo;
begin
  Result := TcxGridWinExplorerViewRecordViewInfo(inherited RecordViewInfo);
end;

{ TcxGridWinExplorerViewCheckBoxViewInfo }

function TcxGridWinExplorerViewCheckBoxViewInfo.GetBorders: TcxBorders;
begin
  Result := [];
end;

function TcxGridWinExplorerViewCheckBoxViewInfo.GetInternalPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckBoxProperties;
end;

{ TcxGridWinExplorerViewTextViewInfo }

function TcxGridWinExplorerViewTextViewInfo.GetInternalPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTextEditProperties;
end;

function TcxGridWinExplorerViewTextViewInfo.GetRequiredHeight: Integer;
begin
  CalculateParams;
  Result := cxTextHeight(Params.Font) + cxTextOffset * 2;
end;

procedure TcxGridWinExplorerViewTextViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetTextItemCellParams(GridRecord, Item, AParams);
  RecordViewInfo.CorrectTextCellViewParams(AParams);
end;

procedure TcxGridWinExplorerViewTextViewInfo.InitInternalProperties;
begin
  RecordViewInfo.InitTextProperties(InternalProperties);
end;

function TcxGridWinExplorerViewTextViewInfo.GetInternalProperties: TcxTextEditProperties;
begin
  Result := TcxTextEditProperties(inherited InternalProperties);
end;

{ TcxGridWinExplorerViewDescriptionViewInfo }

function TcxGridWinExplorerViewDescriptionViewInfo.GetInternalPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMemoProperties;
end;

function TcxGridWinExplorerViewDescriptionViewInfo.GetRequiredHeight: Integer;
begin
  CalculateParams;
  Result := cxTextHeight(Params.Font) + cxTextOffset * 2;
end;

procedure TcxGridWinExplorerViewDescriptionViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetDescriptionItemCellParams(GridRecord, Item, AParams);
end;

procedure TcxGridWinExplorerViewDescriptionViewInfo.InitInternalProperties;
begin
  RecordViewInfo.InitDescriptionProperties(InternalProperties);
end;

function TcxGridWinExplorerViewDescriptionViewInfo.GetInternalProperties: TcxMemoProperties;
begin
  Result := TcxMemoProperties(inherited InternalProperties);
end;

{ TcxGridWinExplorerViewImageViewInfo }

constructor TcxGridWinExplorerViewImageViewInfo.Create(ARecordViewInfo: TcxCustomGridRecordViewInfo;
  AItem: TcxCustomGridTableItem; AImageList: TCustomImageList);
begin
  FImageList := AImageList;
  inherited Create(ARecordViewInfo, AItem);
end;

function TcxGridWinExplorerViewImageViewInfo.CanShowEdit: Boolean;
begin
  Result := inherited CanShowEdit and (ImageList = nil);
end;

function TcxGridWinExplorerViewImageViewInfo.GetInternalPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageProperties;
end;

function TcxGridWinExplorerViewImageViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridWinExplorerViewImageCellPainter;
end;

procedure TcxGridWinExplorerViewImageViewInfo.InitInternalProperties;
begin
  RecordViewInfo.InitImageProperties(InternalProperties);
end;

function TcxGridWinExplorerViewImageViewInfo.UseInternalProperties: Boolean;
begin
  Result := inherited UseInternalProperties and (ImageList = nil);
end;

function TcxGridWinExplorerViewImageViewInfo.GetInternalProperties: TcxImageProperties;
begin
  Result := TcxImageProperties(inherited InternalProperties);
end;

{ TcxGridWinExplorerViewCustomRecordViewInfo }

function TcxGridWinExplorerViewCustomRecordViewInfo.AllowChangeStateOnMouseDown: Boolean;
begin
  Result := False;
end;

procedure TcxGridWinExplorerViewCustomRecordViewInfo.DoCalculateParams;
begin
  GetBackViewParams(BackParams);
  inherited DoCalculateParams;
end;

procedure TcxGridWinExplorerViewCustomRecordViewInfo.GetBackViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetDataCellParams(GridRecord, nil, AParams, False, nil, True);
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetContentBackgroundBounds: TRect;
begin
  Result := cxRectInflate(ContentBounds, -ScaleFactor.Apply(cxGridWinExplorerViewContentBackgroundIndent));
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetFocusRectBounds: TRect;
begin
  Result := cxRectInflate(ContentBackgroundBounds, -1, -1);
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetHotTrack: Boolean;
begin
  Result := inherited GetHotTrack or GridView.OptionsBehavior.HotTrack;
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetPaintState: TcxButtonState;
begin
  if Selected then
    if GridView.DrawRecordActive(GridRecord) then
      Result := cxbsPressed
    else
      Result := cxbsDisabled
  else
    if HotTrack and IsHotTracking then
      Result := cxbsHot
    else
      Result := cxbsNormal;
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetVisible: Boolean;
begin
  Result := Index < RecordsViewInfo.PartVisibleCount;
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.HasFocusRect: Boolean;
begin
  Result := inherited HasFocusRect and GridView.OptionsView.FocusRect;
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.InvalidateOnStateChange: Boolean;
begin
  Result := not IsDestroying and HotTrack and not Selected;
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.IsData: Boolean;
begin
  Result := GridRecord.IsData;
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.IsHotTracking: Boolean;
begin
  Result := ButtonState = cxbsHot;
end;

procedure TcxGridWinExplorerViewCustomRecordViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  if InvalidateOnStateChange then
    Recalculate;
  inherited StateChanged(APrevState);
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetController: TcxGridWinExplorerViewController;
begin
  Result := TcxGridWinExplorerViewController(inherited Controller);
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetGridRecord: TcxGridWinExplorerViewCustomRecord;
begin
  Result := TcxGridWinExplorerViewCustomRecord(inherited GridRecord);
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetOptionsDisplayMode: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := RecordsViewInfo.OptionsDisplayMode;
end;

function TcxGridWinExplorerViewCustomRecordViewInfo.GetRecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo;
begin
  Result := TcxGridWinExplorerViewRecordsViewInfo(inherited RecordsViewInfo);
end;

{ TcxGridWinExplorerViewGroupRecordViewInfo }

constructor TcxGridWinExplorerViewGroupRecordViewInfo.Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
  ARecord: TcxCustomGridRecord);
begin
  inherited Create(ARecordsViewInfo, ARecord);
  if ShowExpandButton then
    FExpandButtonViewInfo := TcxGridWinExplorerViewExpandButtonViewInfo.Create(Self);
  CreateClickTimer;
end;

destructor TcxGridWinExplorerViewGroupRecordViewInfo.Destroy;
begin
  DestroyClickTimer;
  FreeAndNil(FExpandButtonViewInfo);
  inherited Destroy;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.AfterRecalculation;
begin
  if HasExpandButton then
    ExpandButtonViewInfo.AfterRecalculation;
  inherited AfterRecalculation;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.BeforeRecalculation;
begin
  inherited BeforeRecalculation;
  if HasExpandButton then
    ExpandButtonViewInfo.BeforeRecalculation;
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.Click(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  AController: TcxGridWinExplorerViewController;
begin
  Result := True;
  if not Controller.CanProcessMultiSelect(False) or IsClickTimerActive then
  begin
    StopClickTimer;
    if ssDouble in AShift then
      GridRecord.ToggleExpanded
    else
    begin
      AController := Controller;
      Result := inherited Click(AHitTest, AButton, AShift);
      if Result and AController.CanProcessMultiSelect(False) then
        AController.MultiSelectMouseDown(AHitTest, AShift);
    end;
  end
  else
    StartClickTimer(AHitTest, AButton, AShift);
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  if HasExpandButton and not IsRectEmpty(ContentBounds) then
    ExpandButtonViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if HasExpandButton then
  begin
    AHitTest := ExpandButtonViewInfo.GetHitTest(P);
    if AHitTest <> nil then
      Result := AHitTest;
  end;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.CalculateExpandButtonBounds(var ABounds: TRect);
begin
  if not HasExpandButton or IsRectEmpty(ContentBounds) then
    ABounds := cxEmptyRect
  else
  begin
    ABounds := ContentBounds;
    ABounds := cxRectSetLeft(ABounds, ABounds.Left + ScaleFactor.Apply(cxGridWinExplorerViewGroupExpandButtonIndent), ExpandButtonSize);
    ABounds := cxRectCenterVertically(ABounds, ExpandButtonSize);
    ExpandButtonViewInfo.Calculate(ABounds);
  end;
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.CalculateHeight: Integer;
begin
  CalculateParams;
  Result := Max(
    cxTextHeight(Params.Font) + 2 * ScaleFactor.Apply(cxTextOffset),
    ScaleFactor.Apply(cxGridWinExplorerViewGroupRecordHeight));
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.CalculateWidth: Integer;
begin
  Result := cxRectWidth(RecordsViewInfo.ContentBounds);
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetCaptionLineBounds: TRect;
begin
  Result := cxRectCenterVertically(ContentBounds, LookAndFeelPainter.WinExplorerViewGroupCaptionLineHeight);
  if IsRightToLeftConverted then
    Result.Right := TextAreaBounds.Left
  else
    Result.Left := TextAreaBounds.Right;
  Result := cxRectInflate(Result, - ScaleFactor.Apply(cxGridWinExplorerViewGroupCaptionLineIndent), 0);
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetExpandButtonAreaBounds: TRect;
begin
  Result := ExpandButtonBounds;
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetExpandButtonSize: Integer;
begin
  Result := LookAndFeelPainter.WinExplorerViewScaledExpandButtonSize(ScaleFactor);
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridWinExplorerViewGroupRecordPainter;
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetText: string;
begin
  Result := GridRecord.DisplayCaption;
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetTextAreaBounds: TRect;
var
  AIndent: Integer;
  R: TRect;
begin
  Result := inherited GetTextAreaBounds;
  if HasExpandButton then
  begin
    R := ExpandButtonBounds;
    if IsRightToLeftConverted then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, ContentBounds);
    AIndent := R.Right + ScaleFactor.Apply(cxGridWinExplorerViewGroupIndentBetweenExpandButtonAndText);
    Result := cxRectSetLeft(Result, AIndent + ScaleFactor.Apply(cxGridCellTextOffset), TextWidth);
  end
  else
  begin
    AIndent := ScaleFactor.Apply(cxGridWinExplorerViewGroupTextIndent);
    Result := cxRectSetLeft(Result, Result.Left + AIndent, TextWidth);
  end;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetGroupParams(GridRecord, nil, AParams);
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetWidth: Integer;
begin
  Result := CalculateWidth;
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.HasExpandButton: Boolean;
begin
  Result := ExpandButtonViewInfo <> nil;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.Offset(DX: Integer; DY: Integer);
begin
  inherited Offset(DX, DY);
  if HasExpandButton then
    ExpandButtonViewInfo.Offset(Dx, DY);
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.ShowExpandButton: Boolean;
begin
  Result := GridView.OptionsView.ShowExpandButtons;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.PrepareCanvas(ACanvas: TcxCanvas);
begin
  inherited;
  if LookAndFeelPainter.WinExplorerViewGroupTextBold then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.ClickTimerHandler(Sender: TObject);
begin
  Click(FClickTimer.HitTest, FClickTimer.Button, FClickTimer.Shift);
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.CreateClickTimer;
begin
  FClickTimer := TcxGridWinExplorerViewGroupClickTimer.Create(nil);
  FClickTimer.Enabled := False;
  FClickTimer.Interval := GetDoubleClickTime;
  FClickTimer.OnTimer := ClickTimerHandler;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.DestroyClickTimer;
begin
  FreeAndNil(FClickTimer);
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.IsClickTimerActive: Boolean;
begin
  Result := FClickTimer.Enabled;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.StartClickTimer(
  AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState);
begin
  FClickTimer.Button := AButton;
  FClickTimer.HitTest := AHitTest;
  FClickTimer.Shift := AShift;
  FClickTimer.Enabled := True;
end;

procedure TcxGridWinExplorerViewGroupRecordViewInfo.StopClickTimer;
begin
  FClickTimer.Enabled := False;
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetGridRecord: TcxGridWinExplorerViewGroupRecord;
begin
  Result := TcxGridWinExplorerViewGroupRecord(inherited GridRecord);
end;

function TcxGridWinExplorerViewGroupRecordViewInfo.GetRealExpandButtonAreaBounds: TRect;
begin
  Result := GetExpandButtonAreaBounds;
end;

{ TcxGridWinExplorerViewListModeGroupRecordViewInfo }

function TcxGridWinExplorerViewListModeGroupRecordViewInfo.CalculateWidth: Integer;
var
  ARowCount, AColumnCount, AChildCount: Integer;
  ANewlyCreated: Boolean;
  AChildItem: TcxCustomGridRecordViewInfo;
begin
  Result := 0;
  if not GridRecord.Expanded then
    Exit;
  AChildItem := RecordsViewInfo.GetRecordViewInfo(GridRecord.Index + 1, ANewlyCreated);
  try
    AChildCount := Controller.GetVisibleChildCountOnTop(GridRecord);
    ARowCount := RecordsViewInfo.GetRowCountByItem(AChildItem);
    AColumnCount := (AChildCount - 1) div ARowCount + 1;
    Result := AColumnCount * (AChildItem.Width + RecordsViewInfo.GetItemHorizontalIndent);
  finally
    if ANewlyCreated then
      AChildItem.Free;
  end;
end;

function TcxGridWinExplorerViewListModeGroupRecordViewInfo.GetRecordsViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo;
begin
  Result := TcxGridWinExplorerViewListModeRecordsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridWinExplorerViewListModeGroupRecordViewInfo.ShowExpandButton: Boolean;
begin
  Result := False;
end;

{ TcxGridWinExplorerViewRecordViewInfo }

constructor TcxGridWinExplorerViewRecordViewInfo.Create(
  ARecordsViewInfo: TcxCustomGridRecordsViewInfo; ARecord: TcxCustomGridRecord);
var
  AImageList: TCustomImageList;
  AItem: TcxGridWinExplorerViewItem;
begin
  inherited Create(ARecordsViewInfo, ARecord);
  if CanShowCheckBox then
    FCheckBoxViewInfo := TcxGridWinExplorerViewCheckBoxViewInfo.Create(Self, ItemSet.CheckBoxItem);
  if CanShowDescription then
    FDescriptionViewInfo := TcxGridWinExplorerViewDescriptionViewInfo.Create(Self, ItemSet.DescriptionItem);
  if CanShowImage then
  begin
    AItem := GetImageItem(AImageList);
    FImageViewInfo := TcxGridWinExplorerViewImageViewInfo.Create(Self, AItem, AImageList);
  end;
  if CanShowText then
    FTextViewInfo := TcxGridWinExplorerViewTextViewInfo.Create(Self, ItemSet.TextItem);
end;

destructor TcxGridWinExplorerViewRecordViewInfo.Destroy;
begin
  FreeAndNil(FCheckBoxViewInfo);
  FreeAndNil(FDescriptionViewInfo);
  FreeAndNil(FImageViewInfo);
  FreeAndNil(FTextViewInfo);
  inherited Destroy;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.AfterRecalculation;
begin
  if HasCheckBox then
    CheckBoxViewInfo.AfterRecalculation;
  if HasDescription then
    DescriptionViewInfo.AfterRecalculation;
  if HasImage then
    ImageViewInfo.AfterRecalculation;
  if HasText then
    TextViewInfo.AfterRecalculation;
  inherited AfterRecalculation;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.BeforeRecalculation;
begin
  inherited BeforeRecalculation;
  if HasCheckBox then
    CheckBoxViewInfo.BeforeRecalculation;
  if HasDescription then
    DescriptionViewInfo.BeforeRecalculation;
  if HasImage then
    ImageViewInfo.BeforeRecalculation;
  if HasText then
    TextViewInfo.BeforeRecalculation;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.Calculate(ALeftBound: Integer;
  ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  if HasCheckBox then
    FCheckBoxBounds := CalculateCheckBoxBounds;
  FImageBounds := CalculateImageBounds;
  if HasText then
    FTextBounds := CalculateTextBounds;
  if HasDescription then
    FDescriptionBounds := CalculateDescriptionBounds;
  if not UseRightToLeftAlignment then
    CalculateCellViewInfos;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  if HasCheckBox then
    FCheckBoxBounds := TdxRightToLeftLayoutConverter.ConvertRect(FCheckBoxBounds, ABounds);
  FImageBounds := TdxRightToLeftLayoutConverter.ConvertRect(FImageBounds, ABounds);
  if HasText then
    FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, ABounds);
  if HasDescription then
    FDescriptionBounds := TdxRightToLeftLayoutConverter.ConvertRect(FDescriptionBounds, ABounds);
  CalculateCellViewInfos;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetCellViewInfoByItem(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo;
begin
  Result := inherited GetCellViewInfoByItem(AItem);
  if Result <> nil then
    Exit;
  if HasText and (TextViewInfo.Item = AItem) then
  begin
    Result := TextViewInfo;
    Exit;
  end;
  if HasDescription and (DescriptionViewInfo.Item = AItem) then
  begin
    Result := DescriptionViewInfo;
    Exit;
  end;
  if HasCheckBox and (CheckBoxViewInfo.Item = AItem) then
  begin
    Result := CheckBoxViewInfo;
    Exit;
  end;
  if HasImage and (ImageViewInfo.Item = AItem) then
  begin
    Result := ImageViewInfo;
    Exit;
  end;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if (Result <> nil) and (Result.ClassType = GetHitTestClass) then
  begin
    if HasCheckBox then
    begin
      AHitTest := CheckBoxViewInfo.GetHitTest(P);
      if AHitTest <> nil then
      begin
        Result := AHitTest;
        Exit;
      end;
    end;
    if HasDescription then
    begin
      AHitTest := DescriptionViewInfo.GetHitTest(P);
      if AHitTest <> nil then
      begin
        Result := AHitTest;
        Exit;
      end;
    end;
    if HasImage then
    begin
      AHitTest := ImageViewInfo.GetHitTest(P);
      if AHitTest <> nil then
      begin
        Result := AHitTest;
        Exit;
      end;
    end;
    if HasText then
    begin
      AHitTest := TextViewInfo.GetHitTest(P);
      if AHitTest <> nil then
      begin
        Result := AHitTest;
        Exit;
      end;
    end;
  end;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.CalculateCellViewInfos;
begin
  if HasCheckBox then
    CalculateCheckBox;
  if HasDescription then
    CalculateDescription;
  if HasImage then
    CalculateImage;
  if HasText then
    CalculateText;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.CalculateCheckBox;
begin
  CheckBoxViewInfo.Calculate(FCheckBoxBounds.Left, FCheckBoxBounds.Top,
    cxRectWidth(FCheckBoxBounds), cxRectHeight(FCheckBoxBounds));
end;

procedure TcxGridWinExplorerViewRecordViewInfo.CalculateDescription;
begin
  DescriptionViewInfo.Calculate(FDescriptionBounds.Left, FDescriptionBounds.Top,
    cxRectWidth(FDescriptionBounds), cxRectHeight(FDescriptionBounds));
end;

procedure TcxGridWinExplorerViewRecordViewInfo.CalculateImage;
begin
  ImageViewInfo.Calculate(FImageBounds.Left, FImageBounds.Top, cxRectWidth(FImageBounds), cxRectHeight(FImageBounds));
end;

procedure TcxGridWinExplorerViewRecordViewInfo.CalculateText;
begin
  TextViewInfo.Calculate(FTextBounds.Left, FTextBounds.Top, cxRectWidth(FTextBounds), cxRectHeight(FTextBounds));
end;

function TcxGridWinExplorerViewRecordViewInfo.CalculateWidth: Integer;
begin
  Result := OptionsDisplayMode.RecordWidth;
end;

function TcxGridWinExplorerViewRecordViewInfo.CanShowCheckBox: Boolean;
begin
  Result := GridView.OptionsView.ShowItemCheckBoxes and ItemSet.HasCheckBoxItem;
end;

function TcxGridWinExplorerViewRecordViewInfo.CanShowDescription: Boolean;
begin
  Result := CanShowText and OptionsDisplayMode.ShowItemDescriptions and ItemSet.HasDescriptionItem;
end;

function TcxGridWinExplorerViewRecordViewInfo.CanShowImage: Boolean;
begin
  Result := GetImageItem <> nil;
end;

function TcxGridWinExplorerViewRecordViewInfo.CanShowText: Boolean;
begin
  Result := ItemSet.HasTextItem;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.CellHotTrackStateChanged;
begin
  if InvalidateOnStateChange then
  begin
    Recalculate;
    Invalidate;
  end;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.CorrectTextCellViewParams(var AParams: TcxViewParams);
begin
//do nothing
end;

function TcxGridWinExplorerViewRecordViewInfo.FindCell(AItem: TcxCustomGridTableItem): TcxGridWinExplorerViewCustomCellViewInfo;
begin
  Result := nil;
  if HasCheckBox and (CheckBoxViewInfo.Item = AItem) then
    Exit(CheckBoxViewInfo);
  if HasDescription and (DescriptionViewInfo.Item = AItem) then
    Exit(DescriptionViewInfo);
  if HasImage and (ImageViewInfo.Item = AItem) then
    Exit(ImageViewInfo);
  if HasText and (TextViewInfo.Item = AItem) then
    Exit(TextViewInfo);
end;

function TcxGridWinExplorerViewRecordViewInfo.GetCheckBoxSize: TSize;
begin
  Result := LookAndFeelPainter.ScaledCheckButtonAreaSize(ScaleFactor);
  Inc(Result.cx, LookAndFeelPainter.CheckBorderSize);
  Inc(Result.cy, LookAndFeelPainter.CheckBorderSize);
end;

function TcxGridWinExplorerViewRecordViewInfo.GetDescriptionHeight: Integer;
begin
  Result := DescriptionViewInfo.GetRequiredHeight;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetImageItem: TcxGridWinExplorerViewItem;
var
  AImageList: TCustomImageList;
begin
  Result := GetImageItem(AImageList);
end;

function TcxGridWinExplorerViewRecordViewInfo.GetImageItem(out AImageList: TCustomImageList): TcxGridWinExplorerViewItem;

  function GetAssignedItem(AImageItem, AImageIndexItem: TcxGridWinExplorerViewItem;
    ACheckImageList: TCustomImageList; out AImageList: TCustomImageList): TcxGridWinExplorerViewItem;
  begin
    AImageList := nil;
    Result := AImageItem;
    if (Result = nil) and (ACheckImageList <> nil) then
    begin
      Result := AImageIndexItem;
      AImageList := ACheckImageList;
    end;
  end;

var
  AIndex, ACheckIndex: Integer;
  ACheckImageType: TcxGridWinExplorerViewImageType;
begin
  AImageList := nil;
  Result := nil;
  AIndex := Integer(GetImageType);
  ACheckIndex := AIndex;
  while (Result = nil) and (ACheckIndex <> -1) do
  begin
    ACheckImageType := TcxGridWinExplorerViewImageType(ACheckIndex);
    case ACheckImageType of
      itSmall:
        Result := GetAssignedItem(ItemSet.SmallImageItem, ItemSet.SmallImageIndexItem,
          GridView.ImageSet.SmallImages, AImageList);
      itMedium:
        Result := GetAssignedItem(ItemSet.MediumImageItem, ItemSet.MediumImageIndexItem,
          GridView.ImageSet.MediumImages, AImageList);
      itLarge:
        Result := GetAssignedItem(ItemSet.LargeImageItem, ItemSet.LargeImageIndexItem,
          GridView.ImageSet.LargeImages, AImageList);
      itExtraLarge:
        Result := GetAssignedItem(ItemSet.ExtraLargeImageItem, ItemSet.ExtraLargeImageIndexItem,
          GridView.ImageSet.ExtraLargeImages, AImageList);
    end;
    if ACheckImageType = High(ACheckImageType) then
      ACheckIndex := AIndex - 1
    else
      if ACheckIndex >= AIndex then
        Inc(ACheckIndex)
      else
        Dec(ACheckIndex);
  end;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetImageSize: TcxSize;
begin
  Result := OptionsDisplayMode.ImageSize;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetImageType: TcxGridWinExplorerViewImageType;
begin
  Result := itMedium;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetIndentBetweenCheckBoxAndImage: Integer;
begin
  Result := OptionsDisplayMode.Indents.BetweenCheckBoxAndImage;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetIndentBetweenImageAndText: Integer;
begin
  Result := OptionsDisplayMode.Indents.BetweenImageAndText;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetIndentBetweenItemAndBorders: Integer;
begin
  Result := ScaleFactor.Apply(cxGridWinExplorerViewIndentBetweenItemAndRecordBorder);
end;

function TcxGridWinExplorerViewRecordViewInfo.GetIndentBetweenTextAndDescription: Integer;
begin
  Result := OptionsDisplayMode.Indents.BetweenTextAndDescription;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridWinExplorerViewRecordPainter;
end;

function TcxGridWinExplorerViewRecordViewInfo.GetTextHeight: Integer;
begin
  Result := TextViewInfo.GetRequiredHeight;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetDataCellParams(GridRecord, nil, AParams);
end;

function TcxGridWinExplorerViewRecordViewInfo.GetWidth: Integer;
begin
  Result := CalculateWidth;
end;

function TcxGridWinExplorerViewRecordViewInfo.HasCheckBox: Boolean;
begin
  Result := (CheckBoxViewInfo <> nil) and CheckBoxViewInfo.Item.Visible;
end;

function TcxGridWinExplorerViewRecordViewInfo.HasDescription: Boolean;
begin
  Result := (DescriptionViewInfo <> nil) and DescriptionViewInfo.Item.Visible and HasText;
end;

function TcxGridWinExplorerViewRecordViewInfo.HasImage: Boolean;
begin
  Result := (ImageViewInfo <> nil) and ImageViewInfo.Item.Visible;
end;

function TcxGridWinExplorerViewRecordViewInfo.HasText: Boolean;
begin
  Result := (TextViewInfo <> nil) and TextViewInfo.Item.Visible;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.InitDescriptionProperties(AProperties: TcxMemoProperties);
begin
  AProperties.Alignment := taLeftJustify;
  AProperties.UseLeftAlignmentOnEditing := False;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.InitImageProperties(AProperties: TcxImageProperties);
begin
  AProperties.FitMode := ifmProportionalStretch;
end;

procedure TcxGridWinExplorerViewRecordViewInfo.InitTextProperties(AProperties: TcxTextEditProperties);
begin
  AProperties.Alignment.Horz := taLeftJustify;
  AProperties.Alignment.Vert := taTopJustify;
  AProperties.UseLeftAlignmentOnEditing := False;
end;

function TcxGridWinExplorerViewRecordViewInfo.IsHotTracking: Boolean;
begin
  Result := inherited IsHotTracking or
    (HasCheckBox and CheckBoxViewInfo.IsHotTracking) or
    (HasDescription and DescriptionViewInfo.IsHotTracking) or
    (HasText and TextViewInfo.IsHotTracking) or
    (HasImage and ImageViewInfo.IsHotTracking);
end;

procedure TcxGridWinExplorerViewRecordViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  OffsetRects(DX, DY);
  OffsetCells(DX, DY);
end;

procedure TcxGridWinExplorerViewRecordViewInfo.OffsetCells(DX: Integer; DY: Integer);
begin
  if HasCheckBox then
    CheckBoxViewInfo.Offset(DX, DY);
  if HasDescription then
    DescriptionViewInfo.Offset(DX, DY);
  if HasImage then
    ImageViewInfo.Offset(DX, DY);
  if HasText then
    TextViewInfo.Offset(DX, DY);
end;

procedure TcxGridWinExplorerViewRecordViewInfo.OffsetRects(DX: Integer; DY: Integer);
begin
  OffsetRect(FCheckBoxBounds, DX, DY);
  OffsetRect(FDescriptionBounds, DX, DY);
  OffsetRect(FImageBounds, DX, DY);
  OffsetRect(FTextBounds, DX, DY);
end;

function TcxGridWinExplorerViewRecordViewInfo.GetItemSet: TcxGridWinExplorerViewItemSet;
begin
  Result := GridView.ItemSet;
end;

{ TcxGridWinExplorerViewContentModeRecordViewInfo }

constructor TcxGridWinExplorerViewContentModeRecordViewInfo.Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
  ARecord: TcxCustomGridRecord);
begin
  inherited Create(ARecordsViewInfo, ARecord);
  FTextFont := TFont.Create;
end;

destructor TcxGridWinExplorerViewContentModeRecordViewInfo.Destroy;
begin
  FreeAndNil(FTextFont);
  inherited Destroy;
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.CalculateCheckBoxBounds: TRect;
begin
  Result := cxRectCenterVertically(Bounds, GetCheckBoxSize.cy);
  Result := cxRectSetLeft(Result, Bounds.Left + GetIndentBetweenItemAndBorders, GetCheckBoxSize.cx);
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.CalculateDescriptionBounds: TRect;
begin
  Result := cxRectSetLeft(cxNullRect, TextBounds.Right + GetIndentBetweenTextAndDescription, GetDescriptionWidth);
  Result := cxRectSetTop(Result, Bounds.Top + GetIndentBetweenItemAndBorders, GetDescriptionHeight);
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.CalculateHeight: Integer;
begin
  Result := GetImageSize.Height + 2 * GetIndentBetweenItemAndBorders;
  Result := Max(Result, ScaleFactor.Apply(cxGridWinExplorerViewContentModeRecordMinHeight));
  Inc(Result, GetSeparatorSize);
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.CalculateImageBounds: TRect;
begin
  Result := cxRectCenterVertically(Bounds, GetImageSize.Height);
  if HasCheckBox then
    Result := cxRectSetLeft(Result, CheckBoxBounds.Right + GetIndentBetweenCheckBoxAndImage, GetImageSize.Width)
  else
    Result := cxRectSetLeft(Result, Bounds.Left + GetIndentBetweenItemAndBorders, GetImageSize.Width);
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.CalculateTextBounds: TRect;
begin
  Result := cxRectSetLeft(cxNullRect, ImageBounds.Right + GetIndentBetweenImageAndText, GetTextWidth);
  Result := cxRectSetTop(Result, Bounds.Top + GetIndentBetweenItemAndBorders, GetTextHeight);
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.CalculateWidth: Integer;
begin
  Result := cxRectWidth(RecordsViewInfo.ContentBounds);
end;

procedure TcxGridWinExplorerViewContentModeRecordViewInfo.CorrectTextCellViewParams(var AParams: TcxViewParams);
begin
  if AParams.Font <> TextFont then
  begin
    TextFont.Assign(AParams.Font);
    TextFont.Size := Round(TextFont.Size * cxGridWinExplorerViewContentModeTextFontFactor);
    AParams.Font := TextFont;
  end;
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.GetContentBackgroundBounds: TRect;
begin
  Result := inherited GetContentBackgroundBounds;
  Result.Bottom := GetSeparatorBounds.Top - ScaleFactor.Apply(cxGridWinExplorerViewContentBackgroundIndent);
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.GetDescriptionHeight: Integer;
begin
  Result := Max(inherited GetDescriptionHeight, cxRectHeight(Bounds) - 2 * GetIndentBetweenItemAndBorders);
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.GetDescriptionWidth: Integer;
begin
  Result := Bounds.Right - GetIndentBetweenItemAndBorders - TextBounds.Right - GetIndentBetweenTextAndDescription;
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridWinExplorerViewContentModeRecordPainter;
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.GetSeparatorBounds: TRect;
begin
  Result := Bounds;
  Result.Top := Result.Bottom - GetSeparatorSize;
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.GetSeparatorSize: Integer;
begin
  Result := LookAndFeelPainter.SeparatorSize;
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.GetTextHeight: Integer;
begin
  Result := Max(inherited GetTextHeight, ScaleFactor.Apply(cxGridWinExplorerViewContentModeTextHeight));
end;

function TcxGridWinExplorerViewContentModeRecordViewInfo.GetTextWidth: Integer;
begin
  Result := ScaleFactor.Apply(cxGridWinExplorerViewContentModeTextWidth);
end;

{ TcxGridWinExplorerViewSmallModeRecordViewInfo }

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.CalculateCheckBoxBounds: TRect;
begin
  Result := cxRectCenterVertically(Bounds, GetCheckBoxSize.cy);
  Result := cxRectSetLeft(Result, Bounds.Left + GetIndentBetweenItemAndBorders, GetCheckBoxSize.cx);
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.CalculateDescriptionBounds: TRect;
begin
  Result := cxRectSetLeft(cxNullRect, ImageBounds.Right + GetIndentBetweenImageAndDescription, GetDescriptionWidth);
  Result.Top := TextBounds.Bottom + GetIndentBetweenTextAndDescription;
  Result.Bottom := Bounds.Bottom - GetIndentBetweenItemAndBorders;
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.CalculateHeight: Integer;
var
  AHeight: Integer;
begin
  AHeight := 0;
  if HasText then
    AHeight := GetTextHeight;
  if HasDescription then
    Inc(AHeight, GetIndentBetweenTextAndDescription + GetDescriptionMinHeight);
  Result := Max(GetImageSize.Height, AHeight);
  Inc(Result, 2 * GetIndentBetweenItemAndBorders);
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.CalculateImageBounds: TRect;
begin
  Result := cxRectCenterVertically(Bounds, GetImageSize.Height);
  if HasCheckBox then
    Result := cxRectSetLeft(Result, CheckBoxBounds.Right + GetIndentBetweenCheckBoxAndImage, GetImageSize.Width)
  else
    Result := cxRectSetLeft(Result, Bounds.Left + GetIndentBetweenItemAndBorders, GetImageSize.Width);
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.CalculateTextBounds: TRect;
begin
  Result := cxRectSetLeft(cxNullRect, ImageBounds.Right + GetIndentBetweenImageAndText, GetTextWidth);
  Result := cxRectSetTop(Result, Bounds.Top + GetIndentBetweenItemAndBorders, GetTextHeight);
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.CalculateWidth: Integer;
var
  AIndent: Integer;
begin
  if OptionsDisplayMode.RecordWidth = -1 then
  begin
    AIndent := 0;
    if HasText then
      AIndent := GetIndentBetweenImageAndText;
    if HasDescription then
      AIndent := Max(AIndent, GetIndentBetweenImageAndDescription);
    Result := GetImageSize.Width + AIndent + 2 * GetIndentBetweenItemAndBorders;
    if HasText then
      Inc(Result, GetTextWidth);
    if HasCheckBox then
      Inc(Result, GetCheckBoxSize.cx + GetIndentBetweenCheckBoxAndImage);
  end
  else
    Result := inherited CalculateWidth;
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.GetDescriptionHeight: Integer;
begin
  Result := Max(inherited GetDescriptionHeight, Height - GetIndentBetweenItemAndBorders -
    GetTextHeight - GetIndentBetweenTextAndDescription);
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.GetDescriptionMinHeight: Integer;
begin
  Result := Max(DescriptionViewInfo.GetRequiredHeight,
    ScaleFactor.Apply(cxGridWinExplorerViewSmallImagesModeDescriptionMinHeight));
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.GetDescriptionWidth: Integer;
begin
  Result := GetTextWidth;
  if OptionsDisplayMode.RecordWidth <> -1 then
    Result := Result + GetIndentBetweenImageAndText - GetIndentBetweenImageAndDescription;
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.GetImageType: TcxGridWinExplorerViewImageType;
begin
  Result := itSmall;
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.GetIndentBetweenImageAndDescription: Integer;
begin
  Result := OptionsDisplayMode.Indents.BetweenImageAndDescription;
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.GetTextHeight: Integer;
begin
  Result := Max(inherited GetTextHeight, ScaleFactor.Apply(cxGridWinExplorerViewSmallImagesModeTextHeight));
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.GetTextWidth: Integer;
begin
  if OptionsDisplayMode.RecordWidth = -1 then
    Result := ScaleFactor.Apply(cxGridWinExplorerViewSmallImagesModeTextWidth)
  else
  begin
    Result := Width - GetImageSize.Width - GetIndentBetweenImageAndText - 2 * GetIndentBetweenItemAndBorders;
    if HasCheckBox then
      Dec(Result, GetCheckBoxSize.cx + GetIndentBetweenCheckBoxAndImage);
  end;
end;

function TcxGridWinExplorerViewSmallImagesModeRecordViewInfo.GetOptionsDisplayMode: TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode;
begin
  Result := TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode(inherited OptionsDisplayMode);
end;

{ TcxGridWinExplorerViewTilesModeRecordViewInfo }

function TcxGridWinExplorerViewTilesModeRecordViewInfo.CalculateCheckBoxBounds: TRect;
var
  AImageRect: TRect;
begin
  Result := inherited CalculateCheckBoxBounds;
  AImageRect := cxRectCenterVertically(Bounds, GetImageSize.Height);
  Result := cxRectSetTop(Result, AImageRect.Top, GetCheckBoxSize.cy);
end;

function TcxGridWinExplorerViewTilesModeRecordViewInfo.CalculateImageBounds: TRect;
begin
  Result := cxRectCenterVertically(Bounds, GetImageSize.Height);
  Result := cxRectSetLeft(Result, Bounds.Left + GetIndentBetweenItemAndBorders, GetImageSize.Width);
end;

function TcxGridWinExplorerViewTilesModeRecordViewInfo.CalculateWidth: Integer;
var
  AIndent: Integer;
begin
  if OptionsDisplayMode.RecordWidth = -1 then
  begin
    AIndent := 0;
    if HasText then
      AIndent := GetIndentBetweenImageAndText;
    if HasDescription then
      AIndent := Max(AIndent, GetIndentBetweenImageAndDescription);
    Result := GetImageSize.Width + AIndent + 2 * GetIndentBetweenItemAndBorders;
    if HasText then
      Inc(Result, GetTextWidth);
  end
  else
    Result := inherited CalculateWidth;
end;

function TcxGridWinExplorerViewTilesModeRecordViewInfo.GetImageType: TcxGridWinExplorerViewImageType;
begin
  Result := itMedium;
end;

function TcxGridWinExplorerViewTilesModeRecordViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridWinExplorerViewTilesModeRecordPainter;
end;

function TcxGridWinExplorerViewTilesModeRecordViewInfo.GetTextWidth: Integer;
begin
  if OptionsDisplayMode.RecordWidth = -1 then
    Result := inherited GetTextWidth
  else
    Result := Width - GetImageSize.Width - GetIndentBetweenImageAndText - 2 * GetIndentBetweenItemAndBorders;
end;

{ TcxGridWinExplorerViewExtraLargeModeRecordViewInfo }

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.CalculateCheckBoxBounds: TRect;
var
  ATop: Integer;
begin
  Result := cxRectSetLeft(cxNullRect, Bounds.Left + GetIndentBetweenItemAndBorders, GetCheckBoxSize.cx);
  ATop := Bounds.Top + Round(GetImageSize.Height / 2) - Round(GetCheckBoxSize.cy / 2) +
    GetIndentBetweenItemAndBorders;
  Result := cxRectSetTop(Result, ATop, GetCheckBoxSize.cy);
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.CalculateDescriptionBounds: TRect;
begin
  Result := cxRectSetLeft(cxNullRect, Bounds.Left + GetIndentBetweenItemAndBorders, GetDescriptionWidth);
  Result := cxRectSetTop(Result, TextBounds.Bottom + GetIndentBetweenTextAndDescription, GetDescriptionHeight);
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.CalculateHeight: Integer;
begin
  Result := GetImageSize.Height + 2 * GetIndentBetweenItemAndBorders;
  if HasText then
    Inc(Result, GetTextHeight + GetIndentBetweenImageAndText);
  if HasDescription then
    Inc(Result, GetDescriptionHeight + GetIndentBetweenTextAndDescription);
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.CalculateImageBounds: TRect;
begin
  Result := cxRectSetTop(Bounds, Bounds.Top + GetIndentBetweenItemAndBorders, GetImageSize.Height);
  if HasCheckBox then
    Result.Left := CheckBoxBounds.Right + GetIndentBetweenCheckBoxAndImage
  else
    Inc(Result.Left, GetIndentBetweenItemAndBorders);
  Dec(Result.Right, GetIndentBetweenItemAndBorders);
  Result := cxRectCenterHorizontally(Result, GetImageSize.Width);
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.CalculateTextBounds: TRect;
begin
  Result := cxRectSetLeft(cxNullRect, Bounds.Left + GetIndentBetweenItemAndBorders, GetTextWidth);
  Result := cxRectSetTop(Result, ImageBounds.Bottom + GetIndentBetweenImageAndText, GetTextHeight);
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.CalculateWidth: Integer;
begin
  if OptionsDisplayMode.RecordWidth = -1 then
  begin
    Result := GetImageSize.Width + 2 * GetIndentBetweenItemAndBorders;
    if HasCheckBox then
      Inc(Result, GetCheckBoxSize.cx + GetIndentBetweenCheckBoxAndImage);
  end
  else
    Result := inherited CalculateWidth;
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.GetDescriptionHeight: Integer;
begin
  Result := Max(inherited GetDescriptionHeight, ScaleFactor.Apply(cxGridWinExplorerViewMediumImagesModeDescriptionHeight));
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.GetDescriptionWidth: Integer;
begin
  Result := cxRectWidth(Bounds) - 2 * GetIndentBetweenItemAndBorders;
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.GetTextHeight: Integer;
begin
  Result := Max(inherited GetTextHeight, ScaleFactor.Apply(cxGridWinExplorerViewMediumImagesModeTextHeight));
end;

function TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.GetTextWidth: Integer;
begin
  Result := cxRectWidth(Bounds) - 2 * GetIndentBetweenItemAndBorders;
end;

procedure TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.InitDescriptionProperties(AProperties: TcxMemoProperties);
begin
  inherited InitDescriptionProperties(AProperties);
  AProperties.Alignment := taCenter;
end;

procedure TcxGridWinExplorerViewMediumImagesModeRecordViewInfo.InitTextProperties(AProperties: TcxTextEditProperties);
begin
  inherited InitTextProperties(AProperties);
  AProperties.Alignment.Horz := taCenter;
  AProperties.Alignment.Vert := taVCenter;
end;

{ TcxGridWinExplorerViewLargeImagesModeRecordViewInfo }

function TcxGridWinExplorerViewLargeImagesModeRecordViewInfo.GetImageType: TcxGridWinExplorerViewImageType;
begin
  Result := itLarge;
end;

{ TcxGridWinExplorerViewExtraLargeImagesModeRecordViewInfo }

function TcxGridWinExplorerViewExtraLargeImagesModeRecordViewInfo.GetImageType: TcxGridWinExplorerViewImageType;
begin
  Result := itExtraLarge;
end;

{ TcxGridWinExplorerViewRecordsViewInfo }

procedure TcxGridWinExplorerViewRecordsViewInfo.Offset(DX: Integer; DY: Integer);
var
  I: Integer;
begin
  inherited Offset(DX, DY);
  for I := 0 to Count - 1 do
    Items[I].DoOffset(DX, DY);
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.AfterCalculate;
begin
  FCalculated := True;
  UpdateVisibleCount;
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.BeforeCalculate;
begin
  FCalculated := False;
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.Calculate;
begin
  inherited Calculate;
  CalculateVisibleCount;
  CalculateItems;
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.CalculateItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].MainCalculate(GetItemLeftBound(I), GetItemTopBound(I));
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.CalculatePartVisibleCount;
begin
//do nothing
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.CalculateVisibleCount;
begin
  inherited CalculateVisibleCount;
  CalculatePartVisibleCount;
  UpdateVisibleCount;
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to FPartVisibleCount - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewRecordViewInfo;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetGroupRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewGroupRecordViewInfo;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetHorizontalIndent: Integer;
begin
  Result := 0;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetVerticalIndent: Integer;
begin
  Result := 0;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetItemsOffset(AItemCountDelta: Integer): Integer;
var
  I: Integer;
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  Result := 0;
  for I := 0 to Abs(AItemCountDelta) - 1 do
  begin
    AItem := Items[I];
    if NeedConsideredRecordOffset(AItem) then
      Inc(Result, GetRecordScrollSize(AItem.GridRecord.Index));
  end;
  if (AItemCountDelta > 0) xor (IsRightToLeftConverted and IsHorizontalScrolling) then
    Result := -Result;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetIndentBetweenGroupAndItem: Integer;
begin
  Result := OptionsDisplayMode.Indents.BetweenGroupAndItem;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetIndentBetweenGroups: Integer;
begin
  Result := OptionsDisplayMode.Indents.BetweenGroups;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetFirstRecordIndexInBand(ARecordIndexInBand: Integer): Integer;
begin
  Result := ARecordIndexInBand;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetItemCountInBand(ARecordIndexInBand: Integer): Integer;
begin
  Result := 1;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetLastRecordIndexInBand(ARecordIndexInBand: Integer): Integer;
begin
  Result := Controller.GetLastIndexInScrollBand(ARecordIndexInBand);
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetPainterClass: TcxCustomGridRecordsPainterClass;
begin
  Result := TcxGridWinExplorerViewRecordsPainter;
end;

function TcxGridWinExplorerViewRecordsViewInfo.IsHorizontalScrolling: Boolean;
begin
  Result := False;
end;

function TcxGridWinExplorerViewRecordsViewInfo.IsRecordFullyVisibleAtBottom(ARecordIndex: Integer): Boolean;
var
  AItem: TcxCustomGridRecordViewInfo;
begin
  Result := True;
  AItem := GetRealItem(ViewData.Records[ARecordIndex]);
  if AItem <> nil then
    Result := AItem.Bounds.Bottom <= ContentBounds.Bottom;
end;

function TcxGridWinExplorerViewRecordsViewInfo.NeedConsideredRecordOffset(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Boolean;
begin
  Result := True;
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.OffsetItem(AIndex: Integer; AOffset: Integer);
begin
  Items[AIndex].DoOffset(0, AOffset);
end;

procedure TcxGridWinExplorerViewRecordsViewInfo.UpdateVisibleCount;
begin
  FVisibleCount := FPartVisibleCount;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetController: TcxGridWinExplorerViewController;
begin
  Result := TcxGridWinExplorerViewController(inherited Controller);
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetItem(Index: Integer): TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  Result := TcxGridWinExplorerViewCustomRecordViewInfo(inherited Items[Index]);
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetOptionsDisplayMode: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := GetOptionsDisplayModeInstance;
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetViewData: TcxGridWinExplorerViewViewData;
begin
  Result := TcxGridWinExplorerViewViewData(inherited ViewData);
end;

function TcxGridWinExplorerViewRecordsViewInfo.GetDisplayModes: TcxGridWinExplorerViewOptionsDisplayModes;
begin
  Result := GridView.DisplayModes;
end;

{ TcxGridWinExplorerViewContentModeRecordsViewInfo }

function TcxGridWinExplorerViewContentModeRecordsViewInfo.CalculateContentBounds: TRect;
begin
  Result := cxRectInflate(inherited CalculateContentBounds, 0, - GetVerticalIndent, 0, 0);
end;

procedure TcxGridWinExplorerViewContentModeRecordsViewInfo.CalculatePartVisibleCount;
var
  I, ATopPos: Integer;
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  inherited CalculatePartVisibleCount;
  ATopPos := ContentBounds.Top + GridViewInfo.PixelScrollRecordOffset;
  for I := 0 to MaxCount - 1 do
  begin
    if ATopPos >= GridViewInfo.ClientBounds.Bottom then
      Break;
    AItem := Items[I];
    ATopPos := ATopPos + AItem.Height;
    Inc(FPartVisibleCount);
  end;
end;

function TcxGridWinExplorerViewContentModeRecordsViewInfo.GetItemLeftBound(AIndex: Integer): Integer;
begin
  Result := ContentBounds.Left + GetHorizontalIndent;
end;

function TcxGridWinExplorerViewContentModeRecordsViewInfo.GetItemTopBound(AIndex: Integer): Integer;
begin
  if AIndex = 0 then
    Result := ContentBounds.Top + GridViewInfo.PixelScrollRecordOffset
  else
    Result := Items[AIndex - 1].Bounds.Bottom;
end;

function TcxGridWinExplorerViewContentModeRecordsViewInfo.GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewContentModeRecordViewInfo;
end;

function TcxGridWinExplorerViewContentModeRecordsViewInfo.GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := DisplayModes.Content;
end;

function TcxGridWinExplorerViewContentModeRecordsViewInfo.GetContentScrollSize: Integer;
begin
  Result := cxRectWidth(ContentBounds);
end;

procedure TcxGridWinExplorerViewContentModeRecordsViewInfo.UpdateVisibleCount;
var
  I: Integer;
begin
  if Calculated then
  begin
    FVisibleCount := 0;
    for I := 0 to FPartVisibleCount - 1 do
      if Items[I].Bounds.Bottom <= ContentBounds.Bottom then
        Inc(FVisibleCount);
  end
  else
    inherited UpdateVisibleCount;
end;

{ TcxGridWinExplorerViewCustomImagesModeRecordsViewInfo }

function TcxGridWinExplorerViewCustomImagesModeRecordsViewInfo.GetHorizontalIndent: Integer;
begin
  Result := 10;
end;

{ TcxGridWinExplorerViewImagesModeRecordsViewInfo }

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.CalculateBounds: TRect;
begin
  Result := inherited CalculateBounds;
  Result := cxRectSetLeft(Result, Result.Left - Controller.ContentScrollPos, GetDataWidth);
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.CalculateContentBounds: TRect;
begin
  Result := cxRectInflate(inherited CalculateContentBounds, 0, - GetVerticalIndent, 0, 0);
end;

procedure TcxGridWinExplorerViewImagesModeRecordsViewInfo.CalculatePartVisibleCount;

  function GetDataItemIndent(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
  begin
    if ViewData.IsGrouped and ViewData.IsLastChildRecord(AItem.GridRecord) then
      Result := GetIndentBetweenGroupAndItem
    else
      Result := GetItemVerticalIndent;
  end;

  function GetGroupItemIndent(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
  begin
    if AItem.GridRecord.Expanded then
      Result := GetIndentBetweenGroupAndItem
    else
      Result := GetIndentBetweenGroups;
  end;

  function GetIndent(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
  begin
    if AItem.IsData then
      Result := GetDataItemIndent(AItem)
    else
      Result := GetGroupItemIndent(AItem);
  end;

var
  I, ATopPos: Integer;
  ANeedIncTop: Boolean;
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  inherited CalculatePartVisibleCount;
  ATopPos := ContentBounds.Top + GridViewInfo.PixelScrollRecordOffset;
  for I := 0 to MaxCount - 1 do
  begin
    if ATopPos >= GridViewInfo.ClientBounds.Bottom then
      Break;
    AItem := Items[I];
    ANeedIncTop := not AItem.IsData or (GetItemColumnIndex(AItem) = GetColumnCountByItem(AItem) - 1) or
      (ViewData.IsGrouped and ViewData.IsLastChildRecord(AItem.GridRecord));
    if ANeedIncTop then
      ATopPos := ATopPos + AItem.Height + GetIndent(AItem);
    Inc(FPartVisibleCount);
  end;
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetColumnCountByItem(AItem: TcxCustomGridRecordViewInfo): Integer;
begin
  Result := GetColumnCountByItemWidth(AItem.Width);
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetColumnCountByItemWidth(AValue: Integer): Integer;
const
  AMinColumnCount = 1;
var
  AAvailableWidth, AAllowedColumnCount: Integer;
begin
  AAvailableWidth := GridViewInfo.ClientWidth - 2 * GetHorizontalIndent;
  AAllowedColumnCount := (AAvailableWidth + GetItemHorizontalIndent) div (AValue + GetItemHorizontalIndent);
  Result := Max(AAllowedColumnCount, AMinColumnCount);
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetColumnCountByRecordIndex(AValue: Integer): Integer;
var
  ANewlyCreated: Boolean;
  AItem: TcxCustomGridRecordViewInfo;
begin
  AItem := GetRecordViewInfo(AValue, ANewlyCreated);
  try
    Result := GetColumnCountByItem(AItem);
  finally
    if ANewlyCreated then
      AItem.Free;
  end;
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetDataWidth: Integer;
var
  AFirstItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  if ViewData.RecordCount = 0 then
    Result := 0
  else
  begin
    AFirstItem := Items[0];
    if ViewData.IsGrouped or (GetColumnCountByRecordIndex(0) > 1) then
      Result := GridViewInfo.ClientWidth
    else
      Result := AFirstItem.Width + 2 * GetHorizontalIndent;
  end;
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetItemColumnIndex(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
var
  ARecordIndex: Integer;
  ARecord: TcxCustomGridRecord;
begin
  Result := 0;
  if not AItem.IsData then
    Exit;
  ARecordIndex := AItem.GridRecord.Index;
  ARecord := ViewData.Records[ARecordIndex];
  if ViewData.IsGrouped then
    ARecordIndex := ARecordIndex - ARecord.ParentRecord.Index - 1;
  Result := ARecordIndex mod GetColumnCountByItem(AItem);
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetItemColumnIndex(ARecordIndex: Integer): Integer;
var
  ANewlyCreated: Boolean;
  AItem: TcxCustomGridRecordViewInfo;
begin
  AItem := GetRecordViewInfo(ARecordIndex, ANewlyCreated);
  try
    Result := GetItemColumnIndex(AItem as TcxGridWinExplorerViewCustomRecordViewInfo);
  finally
    if ANewlyCreated then
      AItem.Free;
  end;
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetItemHorizontalIndent: Integer;
begin
  Result := ScaleFactor.Apply(cxGridWinExplorerViewImagesModeHorizontalIndent);
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetItemLeftBound(AIndex: Integer): Integer;
var
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  AItem := Items[AIndex];
  if AItem.IsData then
    Result := ContentBounds.Left + GetHorizontalIndent +
      GetItemColumnIndex(AItem) * (AItem.Width + GetItemHorizontalIndent)
  else
    Result := ContentBounds.Left;
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetItemTopBound(AIndex: Integer): Integer;

  function GetDataItemTopBound(AItem, APrevItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
  begin
    if not APrevItem.IsData or (GetItemColumnIndex(AItem) = 0) then
      if APrevItem.IsData then
        Result := APrevItem.Bounds.Bottom + GetItemVerticalIndent
      else
        Result := APrevItem.Bounds.Bottom + GetIndentBetweenGroupAndItem
    else
      Result := APrevItem.Bounds.Top
  end;

  function GetGroupItemTopBound(APrevItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
  begin
    if APrevItem.IsData then
      Result := APrevItem.Bounds.Bottom + GetIndentBetweenGroupAndItem
    else
      Result := APrevItem.Bounds.Bottom + GetIndentBetweenGroups;
  end;

var
  AItem, APrevItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  Result := ContentBounds.Top + GridViewInfo.PixelScrollRecordOffset;
  AItem := Items[AIndex];
  if AIndex = 0 then
    Exit;
  APrevItem := Items[AIndex - 1];
  if AItem.IsData then
    Result := GetDataItemTopBound(AItem, APrevItem)
  else
    Result := GetGroupItemTopBound(APrevItem);
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetItemVerticalIndent: Integer;
begin
  Result := ScaleFactor.Apply(cxGridWinExplorerViewImagesModeVerticalIndent);
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetRecordScrollSize(ARecordIndex: Integer): Integer;

  function GetDataIndent(ARecord: TcxCustomGridRecord): Integer;
  begin
    if ViewData.IsGrouped and ViewData.IsLastChildRecord(ARecord) then
      Result := GetIndentBetweenGroupAndItem
    else
      Result := GetItemVerticalIndent;
  end;

  function GetGroupIndent(ARecord: TcxCustomGridRecord): Integer;
  begin
    if ARecord.Expanded then
      Result := GetIndentBetweenGroupAndItem
    else
      Result := GetIndentBetweenGroups;
   end;

var
  ARecord: TcxCustomGridRecord;
  ALastRecordIndexInBand: Integer;
begin
  Result := inherited GetRecordScrollSize(ARecordIndex);
  ARecord := ViewData.Records[ARecordIndex];
  if ARecord.IsLast then
    Exit;
  if not ARecord.IsData then
    Inc(Result, GetGroupIndent(ARecord))
  else
  begin
    ALastRecordIndexInBand := GetLastRecordIndexInBand(ARecordIndex);
    if ARecordIndex = ALastRecordIndexInBand then
      Inc(Result, GetDataIndent(ARecord))
    else
      Result := GetRecordScrollSize(ALastRecordIndexInBand);
  end;
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetFirstRecordIndexInBand(ARecordIndexInBand: Integer): Integer;
var
  ARecord: TcxCustomGridRecord;
begin
  ARecord := ViewData.Records[ARecordIndexInBand];
  if not ARecord.IsData then
    Result := inherited GetFirstRecordIndexInBand(ARecordIndexInBand)
  else
    Result := ARecordIndexInBand - GetItemColumnIndex(ARecordIndexInBand);
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetItemCountInBand(ARecordIndexInBand: Integer): Integer;
var
  ARecord: TcxCustomGridRecord;
  AFirstRecordIndexInBand, ALastIndexInBand, ALastRecordIndex: Integer;
begin
  AFirstRecordIndexInBand := GetFirstRecordIndexInBand(ARecordIndexInBand);
  ARecord := ViewData.Records[AFirstRecordIndexInBand];
  if not ARecord.IsData then
    Result := inherited GetItemCountInBand(ARecordIndexInBand)
  else
  begin
    Result := GetColumnCountByRecordIndex(AFirstRecordIndexInBand);
    ALastIndexInBand := AFirstRecordIndexInBand + Result - 1;
    if ViewData.IsGrouped then
      ALastRecordIndex := ViewData.GetLastChildRecordIndex(ARecord.ParentRecord)
    else
      ALastRecordIndex := ViewData.RecordCount - 1;
    if ALastIndexInBand > ALastRecordIndex then
      Result := ALastRecordIndex - AFirstRecordIndexInBand + 1
  end;
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.GetContentScrollSize: Integer;
begin
  Result := GetDataWidth;
end;

function TcxGridWinExplorerViewImagesModeRecordsViewInfo.NeedConsideredRecordOffset(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Boolean;
begin
  Result := not AItem.IsData or (GetItemColumnIndex(AItem) = GetColumnCountByItem(AItem) - 1) or
    ViewData.IsLastChildRecord(AItem.GridRecord) or AItem.GridRecord.IsLast;
end;

procedure TcxGridWinExplorerViewImagesModeRecordsViewInfo.UpdateVisibleCount;
var
  I: Integer;
begin
  if Calculated then
  begin
    FVisibleCount := 0;
    for I := 0 to FPartVisibleCount - 1 do
      if Items[I].Bounds.Bottom <= ContentBounds.Bottom then
        Inc(FVisibleCount);
  end
  else
    inherited UpdateVisibleCount;
end;

{ TcxGridWinExplorerViewExtraLargeModeRecordsViewInfo }

function TcxGridWinExplorerViewExtraLargeImagesModeRecordsViewInfo.GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewExtraLargeImagesModeRecordViewInfo;
end;

function TcxGridWinExplorerViewExtraLargeImagesModeRecordsViewInfo.GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := DisplayModes.ExtraLargeImages;
end;

{ TcxGridWinExplorerViewLargeModeRecordsViewInfo }

function TcxGridWinExplorerViewLargeImagesModeRecordsViewInfo.GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewLargeImagesModeRecordViewInfo;
end;

function TcxGridWinExplorerViewLargeImagesModeRecordsViewInfo.GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := DisplayModes.LargeImages;
end;

{ TcxGridWinExplorerViewMediumModeRecordsViewInfo }

function TcxGridWinExplorerViewMediumImagesModeRecordsViewInfo.GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewMediumImagesModeRecordViewInfo;
end;

function TcxGridWinExplorerViewMediumImagesModeRecordsViewInfo.GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := DisplayModes.MediumImages;
end;

{ TcxGridWinExplorerViewSmallModeRecordsViewInfo }

function TcxGridWinExplorerViewSmallImagesModeRecordsViewInfo.GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewSmallImagesModeRecordViewInfo;
end;

function TcxGridWinExplorerViewSmallImagesModeRecordsViewInfo.GetItemHorizontalIndent: Integer;
begin
  Result := ScaleFactor.Apply(cxGridWinExplorerViewSmallImagesModeHorizontalIndent);
end;

function TcxGridWinExplorerViewSmallImagesModeRecordsViewInfo.GetItemVerticalIndent: Integer;
begin
  Result := ScaleFactor.Apply(cxGridWinExplorerViewSmallImagesModeVerticalIndent);
end;

function TcxGridWinExplorerViewSmallImagesModeRecordsViewInfo.GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := DisplayModes.SmallImages;
end;

{ TcxGridWinExplorerViewTilesModeRecordsViewInfo }

function TcxGridWinExplorerViewTilesModeRecordsViewInfo.GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewTilesModeRecordViewInfo;
end;

function TcxGridWinExplorerViewTilesModeRecordsViewInfo.GetItemHorizontalIndent: Integer;
begin
  Result := ScaleFactor.Apply(cxGridWinExplorerViewSmallImagesModeHorizontalIndent);
end;

function TcxGridWinExplorerViewTilesModeRecordsViewInfo.GetItemVerticalIndent: Integer;
begin
  Result := ScaleFactor.Apply(cxGridWinExplorerViewSmallImagesModeVerticalIndent);
end;

function TcxGridWinExplorerViewTilesModeRecordsViewInfo.GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := DisplayModes.Tiles;
end;

{ TcxGridWinExplorerViewListModeRecordsViewInfo }

destructor TcxGridWinExplorerViewListModeRecordsViewInfo.Destroy;
begin
  FixedGroupItem := nil;
  inherited Destroy;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if (Result = nil) and HasFixedGroupItem then
  begin
    AHitTest := FixedGroupItem.GetHitTest(P);
    if AHitTest <> nil then
    begin
      Result := AHitTest;
      Exit;
    end;
  end;
end;

procedure TcxGridWinExplorerViewListModeRecordsViewInfo.OffsetRecords(AItemCountDelta: Integer;
  APixelScrollRecordOffsetDelta: Integer);
begin
  inherited OffsetRecords(AItemCountDelta, APixelScrollRecordOffsetDelta);
  UpdateVisibleCount;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.CalculateBounds: TRect;
begin
  Result := inherited CalculateBounds;
  Result := cxRectSetTop(Result, Result.Top - Controller.ContentScrollPos, GetDataHeight);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.CalculateContentBounds: TRect;
begin
  Result := cxRectInflate(inherited CalculateContentBounds, - GetHorizontalIndent, 0, 0, 0);
end;

procedure TcxGridWinExplorerViewListModeRecordsViewInfo.CalculateItems;
begin
  if HasFixedGroupItem then
    FFixedGroupItem.MainCalculate(GetFixedGroupTopLeft.X, GetFixedGroupTopLeft.Y);
  inherited CalculateItems;
end;

procedure TcxGridWinExplorerViewListModeRecordsViewInfo.CalculatePartVisibleCount;
var
  I, ALeftPos: Integer;
  ANeedIncLeft: Boolean;
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  inherited CalculatePartVisibleCount;
  FixedGroupItem := nil;
  ALeftPos := ContentBounds.Left + GridViewInfo.PixelScrollRecordOffset;
  for I := 0 to MaxCount - 1 do
  begin
    if ALeftPos >= GridViewInfo.ClientBounds.Right then
      Break;
    AItem := Items[I];
    if ViewData.IsGrouped and (I = 0) and AItem.IsData then
      FixedGroupItem := CreateRecordViewInfo(AItem.GridRecord.ParentRecord);
    ANeedIncLeft := AItem.IsData and ((GetItemRowIndex(AItem) = GetRowCountByItem(AItem) - 1) or
      (ViewData.IsGrouped and ViewData.IsLastChildRecord(AItem.GridRecord)));
    if ANeedIncLeft then
    begin
      ALeftPos := ALeftPos + AItem.Width + GetItemHorizontalIndent;
      if ViewData.IsGrouped and ViewData.IsLastChildRecord(AItem.GridRecord) then
        ALeftPos := ALeftPos + GetIndentBetweenGroups;
    end;
    Inc(FPartVisibleCount);
  end;
end;

procedure TcxGridWinExplorerViewListModeRecordsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  if HasFixedGroupItem then
    FixedGroupItem.RightToLeftConversion(ABounds);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetDataHeight: Integer;
var
  AFirstItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  if ViewData.RecordCount = 0 then
    Result := 0
  else
  begin
    AFirstItem := Items[0];
    if ViewData.IsGrouped or (GetRowCountByItem(AFirstItem) > 1) then
      Result := GridViewInfo.ClientHeight
    else
      Result := AFirstItem.Height + 2 * GetVerticalIndent;
  end;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetFirstRecordIndexInBand(ARecordIndexInBand: Integer): Integer;
var
  ARecord: TcxCustomGridRecord;
begin
  ARecord := ViewData.Records[ARecordIndexInBand];
  if not ARecord.IsData then
    Result := inherited GetFirstRecordIndexInBand(ARecordIndexInBand)
  else
  begin
    Result := ARecordIndexInBand - GetItemRowIndex(ARecordIndexInBand);
    if ViewData.IsGrouped and (Result = ARecord.ParentRecord.Index + 1) then
      Result := ARecord.ParentRecord.Index;
  end;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetFixedGroupTopLeft: TPoint;
begin
  Result := Point(GetHorizontalIndent, GetVerticalIndent);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetGroupRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewListModeGroupRecordViewInfo;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetItemHorizontalIndent: Integer;
begin
  Result := 20;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetItemLeftBound(AIndex: Integer): Integer;

  function GetDataItemLeftBound(AItem, APrevItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
  var
    R: TRect;
  begin
    R := APrevItem.Bounds;
    if GridViewInfo.IsRightToLeftConverted then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, Bounds);
    if APrevItem.IsData and (GetItemRowIndex(AItem) = 0) then
      Result := R.Right + GetItemHorizontalIndent
    else
      Result := R.Left;
  end;

  function GetGroupItemLeftBound(APrevItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
  begin
    Result := APrevItem.Bounds.Right + GetItemHorizontalIndent + GetIndentBetweenGroups;
  end;

var
  AItem, APrevItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  Result := ContentBounds.Left + GridViewInfo.PixelScrollRecordOffset;
  if AIndex = 0 then
    Exit;
  AItem := Items[AIndex];
  APrevItem := Items[AIndex - 1];
  if AItem.IsData then
    Result := GetDataItemLeftBound(AItem, APrevItem)
  else
    Result := GetGroupItemLeftBound(APrevItem);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetItemPixelScrollSize(AItem: TcxCustomGridRecordViewInfo): Integer;
begin
  Result := AItem.Width;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetItemRowIndex(
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Integer;
var
  ARecordIndex: Integer;
  ARecord: TcxCustomGridRecord;
begin
  Result := 0;
  if not AItem.IsData then
    Exit;
  ARecordIndex := AItem.GridRecord.Index;
  ARecord := ViewData.Records[ARecordIndex];
  if ViewData.IsGrouped then
    ARecordIndex := ARecordIndex - ARecord.ParentRecord.Index - 1;
  Result := ARecordIndex mod GetRowCountByItem(AItem);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetItemRowIndex(ARecordIndex: Integer): Integer;
var
  ANewlyCreated: Boolean;
  AItem: TcxCustomGridRecordViewInfo;
begin
  AItem := GetRecordViewInfo(ARecordIndex, ANewlyCreated);
  try
    Result := GetItemRowIndex(AItem as TcxGridWinExplorerViewCustomRecordViewInfo);
  finally
    if ANewlyCreated then
      AItem.Free;
  end;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetItemTopBound(AIndex: Integer): Integer;
var
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo;
  AParentItem: TcxCustomGridRecordViewInfo;
  ARecord: TcxCustomGridRecord;
begin
  Result := 0;
  AItem := Items[AIndex];
  if not AItem.IsData then
    Exit;
  if ViewData.IsGrouped then
  begin
    ARecord := AItem.GridRecord;
    AParentItem := GetRealItem(ARecord.ParentRecord);
    Result := AParentItem.Bounds.Bottom  + GetIndentBetweenGroupAndItem;
  end
  else
    Result := ContentBounds.Top + GetVerticalIndent;
  Result := Result + GetItemRowIndex(AItem) * (AItem.Height + GetItemVerticalIndent);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetItemVerticalIndent: Integer;
begin
  Result := 3;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetOptionsDisplayModeInstance: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  Result := DisplayModes.List;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetPainterClass: TcxCustomGridRecordsPainterClass;
begin
  Result := TcxGridWinExplorerViewListModeRecordsPainter;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetPixelScrollContentSize: Integer;
begin
  Result := cxRectWidth(ContentBounds);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetRecordScrollSize(ARecordIndex: Integer): Integer;
var
  ARecord: TcxCustomGridRecord;
  ALastRecordIndexInBand: Integer;
begin
  Result := inherited GetRecordScrollSize(ARecordIndex);
  ARecord := ViewData.Records[ARecordIndex];
  if not ARecord.IsData then
    Result := 0
  else
  begin
    ALastRecordIndexInBand := GetLastRecordIndexInBand(ARecordIndex);
    if ARecordIndex = ALastRecordIndexInBand then
    begin
      Inc(Result, GetItemHorizontalIndent);
      if ViewData.IsGrouped then
        Inc(Result, GetIndentBetweenGroups);
    end
    else
      Result := GetRecordScrollSize(ALastRecordIndexInBand);
  end;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetRowCountByItem(AItem: TcxCustomGridRecordViewInfo): Integer;
begin
  Result := GetRowCountByItemHeight(AItem.Height);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetRowCountByItemHeight(AValue: Integer): Integer;
const
  AMinRowCount = 1;
var
  AAvailableHeight: Integer;
  AParentItem: TcxCustomGridRecordViewInfo;
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  AAvailableHeight := GridViewInfo.ClientHeight - 2 * GetVerticalIndent;
  if ViewData.IsGrouped then
  begin
    AItem := Items[0];
    if not AItem.IsData then
      AAvailableHeight := AAvailableHeight - AItem.Height
    else
    begin
      AParentItem := GetRealItem(AItem.GridRecord.ParentRecord);
      AAvailableHeight := AAvailableHeight - AParentItem.Height;
    end;
    AAvailableHeight := AAvailableHeight - GetIndentBetweenGroupAndItem;
  end;
  Result := (AAvailableHeight + GetItemVerticalIndent) div (AValue + GetItemVerticalIndent);
  Result := Max(Result, AMinRowCount);
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetRowCountByRecordIndex(AValue: Integer): Integer;
var
  ANewlyCreated: Boolean;
  AItem: TcxCustomGridRecordViewInfo;
begin
  AItem := GetRecordViewInfo(AValue, ANewlyCreated);
  try
    Result := GetRowCountByItem(AItem);
  finally
    if ANewlyCreated then
       AItem.Free;
  end;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetDataRecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridWinExplorerViewListModeRecordViewInfo;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetItemCountInBand(ARecordIndexInBand: Integer): Integer;
var
  ARecord: TcxCustomGridRecord;
  ADataRecordIndex, AFirstRecordIndexInBand, ALastRecordIndex, ALastIndexInBand: Integer;
begin
  AFirstRecordIndexInBand := GetFirstRecordIndexInBand(ARecordIndexInBand);
  ADataRecordIndex := AFirstRecordIndexInBand;
  ARecord := ViewData.Records[AFirstRecordIndexInBand];
  if not ARecord.IsData then
    Inc(ADataRecordIndex);
  Result := GetRowCountByRecordIndex(ADataRecordIndex);
  if not ARecord.IsData then
    Inc(Result);
  ALastIndexInBand := AFirstRecordIndexInBand + Result - 1;
  if ViewData.IsGrouped then
    if ARecord.IsData then
      ALastRecordIndex := ViewData.GetLastChildRecordIndex(ARecord.ParentRecord)
    else
      ALastRecordIndex := ViewData.GetLastChildRecordIndex(ARecord)
  else
    ALastRecordIndex := ViewData.RecordCount - 1;
  if ALastIndexInBand > ALastRecordIndex then
    Result := ALastRecordIndex - AFirstRecordIndexInBand + 1;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.GetContentScrollSize: Integer;
begin
  Result := GetDataHeight;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.HasFixedGroupItem: Boolean;
begin
  Result := FixedGroupItem <> nil;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.IsHorizontalScrolling: Boolean;
begin
  Result := True;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.IsRecordFullyVisibleAtBottom(ARecordIndex: Integer): Boolean;
var
  AItem: TcxCustomGridRecordViewInfo;
  ARecord: TcxCustomGridRecord;
begin
  Result := True;
  ARecord := ViewData.Records[ARecordIndex];
  if ARecord.IsData then
  begin
    AItem := GetRealItem(ARecord);
    if AItem <> nil then
      Result := AItem.Bounds.Right <= ContentBounds.Right;
  end;
end;

function TcxGridWinExplorerViewListModeRecordsViewInfo.NeedConsideredRecordOffset(AItem: TcxGridWinExplorerViewCustomRecordViewInfo): Boolean;
begin
  Result := AItem.IsData and ((GetItemRowIndex(AItem) = GetRowCountByItem(AItem) - 1) or
    ViewData.IsLastChildRecord(AItem.GridRecord) or AItem.GridRecord.IsLast);
end;

procedure TcxGridWinExplorerViewListModeRecordsViewInfo.OffsetItem(AIndex: Integer; AOffset: Integer);
begin
  Items[AIndex].DoOffset(AOffset, 0);
end;

procedure TcxGridWinExplorerViewListModeRecordsViewInfo.UpdateVisibleCount;
var
  I: Integer;
  AItem: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  if Calculated then
  begin
    FVisibleCount := 0;
    for I := 0 to FPartVisibleCount - 1 do
    begin
      AItem := Items[I];
      if not AItem.IsData or not IsRightToLeftConverted and (AItem.Bounds.Right <= ContentBounds.Right) or
        IsRightToLeftConverted and (AItem.Bounds.Left >= ContentBounds.Left) then
        Inc(FVisibleCount);
    end;
  end
  else
    inherited UpdateVisibleCount;
end;

procedure TcxGridWinExplorerViewListModeRecordsViewInfo.SetFixedGroupItem(AValue: TcxCustomGridRecordViewInfo);
begin
  if FFixedGroupItem <> AValue then
  begin
    FreeAndNil(FFixedGroupItem);
    FFixedGroupItem := AValue;
  end;
end;

{ TcxGridWinExplorerViewViewInfo }

procedure TcxGridWinExplorerViewViewInfo.BeforeCalculating;
begin
  RecreateViewInfos;
  inherited BeforeCalculating;
end;

procedure TcxGridWinExplorerViewViewInfo.Calculate;
begin
  FindPanelViewInfo.MainCalculate;
  FilterViewInfo.MainCalculate;
  inherited Calculate;
end;

procedure TcxGridWinExplorerViewViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FindPanelViewInfo.RightToLeftConversion(ABounds);
  FilterViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridWinExplorerViewViewInfo.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  case GridView.ActiveDisplayMode of
    dmContent:
      Result := TcxGridWinExplorerViewContentModeRecordsViewInfo;
    dmExtraLargeImages:
      Result := TcxGridWinExplorerViewExtraLargeImagesModeRecordsViewInfo;
    dmLargeImages:
      Result := TcxGridWinExplorerViewLargeImagesModeRecordsViewInfo;
    dmList:
      Result := TcxGridWinExplorerViewListModeRecordsViewInfo;
    dmMediumImages:
      Result := TcxGridWinExplorerViewMediumImagesModeRecordsViewInfo;
    dmSmallImages:
      Result := TcxGridWinExplorerViewSmallImagesModeRecordsViewInfo;
    else //dmTiles
      Result := TcxGridWinExplorerViewTilesModeRecordsViewInfo;
  end;
end;

function TcxGridWinExplorerViewViewInfo.GetContentScrollSize: Integer;
begin
  Result := RecordsViewInfo.GetContentScrollSize;
end;

function TcxGridWinExplorerViewViewInfo.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewViewInfo.GetRecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo;
begin
  Result := TcxGridWinExplorerViewRecordsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridWinExplorerViewViewInfo.GetScrollableAreaHeight: Integer;
begin
  Result := ScrollableAreaBoundsVert.Right - ScrollableAreaBoundsVert.Left;
end;

{ TcxGridWinExplorerViewItemStyles }

procedure TcxGridWinExplorerViewItemStyles.Assign(Source: TPersistent);
var
  ASource: TcxGridWinExplorerViewItemStyles;
begin
  inherited Assign(Source);
  if Source is TcxGridWinExplorerViewItemStyles then
  begin
    ASource := TcxGridWinExplorerViewItemStyles(Source);
    TextItem := ASource.TextItem;
    DescriptionItem := ASource.DescriptionItem;
    OnGetTextItemStyle := ASource.OnGetTextItemStyle;
    OnGetDescriptionItemStyle := ASource.OnGetDescriptionItemStyle;
  end;
end;

procedure TcxGridWinExplorerViewItemStyles.GetTextItemParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetTextItemStyle) then
    FOnGetTextItemStyle(GridView, ARecord, Item, AStyle);
  GetViewParams(isWinExplorerViewItemTextItem, ARecord, AStyle, AParams);
end;

procedure TcxGridWinExplorerViewItemStyles.GetDescriptionItemParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetDescriptionItemStyle) then
    FOnGetDescriptionItemStyle(GridView, ARecord, Item, AStyle);
  GetViewParams(isWinExplorerViewItemDescriptionItem, ARecord, AStyle, AParams);
end;

procedure TcxGridWinExplorerViewItemStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);
begin
  case Index of
    isWinExplorerViewItemTextItem:
      GridView.Styles.GetTextItemParams(TcxCustomGridRecord(AData), Item, AParams);
    isWinExplorerViewItemDescriptionItem:
      GridView.Styles.GetDescriptionItemParams(TcxCustomGridRecord(AData), Item, AParams);
  else
    inherited;
  end;
end;

function TcxGridWinExplorerViewItemStyles.GetGridViewValue: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewItemStyles.GetItem: TcxGridWinExplorerViewItem;
begin
  Result := TcxGridWinExplorerViewItem(inherited Item);
end;

procedure TcxGridWinExplorerViewItemStyles.SetOnGetTextItemStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetTextItemStyle, Value) then
  begin
    FOnGetTextItemStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridWinExplorerViewItemStyles.SetOnGetDescriptionItemStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetDescriptionItemStyle, Value) then
  begin
    FOnGetDescriptionItemStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

{ TcxGridWinExplorerViewItem }

function TcxGridWinExplorerViewItem.GetOptions: TcxGridWinExplorerViewItemOptions;
begin
  Result := TcxGridWinExplorerViewItemOptions(inherited Options);
end;

function TcxGridWinExplorerViewItem.GetStyles: TcxGridWinExplorerViewItemStyles;
begin
  Result := TcxGridWinExplorerViewItemStyles(inherited Styles);
end;

procedure TcxGridWinExplorerViewItem.SetOptions(AValue: TcxGridWinExplorerViewItemOptions);
begin
  inherited Options := AValue;
end;

procedure TcxGridWinExplorerViewItem.SetStyles(Value: TcxGridWinExplorerViewItemStyles);
begin
  inherited Styles := Value;
end;

function TcxGridWinExplorerViewItem.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxGridWinExplorerViewItemOptions;
end;

function TcxGridWinExplorerViewItem.GetStylesClass: TcxCustomGridTableItemStylesClass;
begin
  Result := TcxGridWinExplorerViewItemStyles;
end;

function TcxGridWinExplorerViewItem.IsPropertiesDefault: Boolean;
begin
  Result := (RepositoryItem = nil) and (Properties = nil) and not HasCustomPropertiesHandler;
end;

{ TcxGridWinExplorerViewCustomRecord }

function TcxGridWinExplorerViewCustomRecord.GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass;
begin
  Result := TcxGridWinExplorerViewInfoCacheItem;
end;

function TcxGridWinExplorerViewCustomRecord.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewCustomRecord.GetViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo;
begin
  Result := TcxGridWinExplorerViewCustomRecordViewInfo(inherited ViewInfo);
end;

{ TcxGridWinExplorerViewGroupRecord }

procedure TcxGridWinExplorerViewGroupRecord.DoCollapse(ARecurse: Boolean);
begin
  if ARecurse or Expanded then
    DataController.Groups.ChangeExpanding(Index, False, ARecurse);
end;

procedure TcxGridWinExplorerViewGroupRecord.DoExpand(ARecurse: Boolean);
begin
  if ARecurse or not Expanded then
    DataController.Groups.ChangeExpanding(Index, True, ARecurse);
end;

function TcxGridWinExplorerViewGroupRecord.GetExpandable: Boolean;
begin
  Result := not ((dcoGroupsAlwaysExpanded in DataController.Options) or GridView.GroupsAlwaysExpanded);
end;

function TcxGridWinExplorerViewGroupRecord.GetExpanded: Boolean;
begin
  Result := RecordInfo.Expanded;
end;

function TcxGridWinExplorerViewGroupRecord.GetDisplayCaption: string;
begin
  Result := DisplayTexts[-1];
end;

function TcxGridWinExplorerViewGroupRecord.GetDisplayText(Index: Integer): string;
begin
  if ViewData.HasCustomDataHandling(GroupItem, doGrouping) then
    Result := ViewData.GetCustomDataDisplayText(RecordIndex, GroupItem.Index, doGrouping)
  else
    Result := inherited GetDisplayText(Index);
end;

function TcxGridWinExplorerViewGroupRecord.GetIsData: Boolean;
begin
  Result := False;
end;

function TcxGridWinExplorerViewGroupRecord.GetIsParent: Boolean;
begin
  Result := RecordInfo.Level < DataController.Groups.GroupingItemCount;
end;

function TcxGridWinExplorerViewGroupRecord.GetViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := GridView.ViewInfo.RecordsViewInfo.GetGroupRecordViewInfoClass;
end;

function TcxGridWinExplorerViewGroupRecord.GetGroupItem: TcxGridWinExplorerViewItem;
begin
  Result := GridView.ItemSet.GroupItem;
end;

{ TcxGridWinExplorerViewRecord }

function TcxGridWinExplorerViewDataRecord.GetHasCells: Boolean;
begin
  Result := True;
end;

function TcxGridWinExplorerViewDataRecord.GetViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := GridView.ViewInfo.RecordsViewInfo.GetDataRecordViewInfoClass;
end;

{ TcxGridWinExplorerViewViewData }

procedure TcxGridWinExplorerViewViewData.Collapse(ARecurse: Boolean);
begin
  if not GridView.GroupsAlwaysExpanded then
    DataController.Groups.FullCollapse;
  inherited Collapse(ARecurse);
end;

procedure TcxGridWinExplorerViewViewData.Expand(ARecurse: Boolean);
begin
  DataController.Groups.FullExpand;
  inherited Expand(ARecurse);
end;

function TcxGridWinExplorerViewViewData.GetRecordChildCount(ARecord: TcxCustomGridRecord): Integer;
var
  AGroupIndex: Integer;
begin
  AGroupIndex := GridView.DataController.Groups.DataGroupIndexByRowIndex[ARecord.Index];
  Result := GridView.DataController.Groups.ChildCount[AGroupIndex];
end;

function TcxGridWinExplorerViewViewData.GetDataRecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridWinExplorerViewDataRecord;
end;

function TcxGridWinExplorerViewViewData.GetGroupRecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridWinExplorerViewGroupRecord;
end;

function TcxGridWinExplorerViewViewData.GetLastChildRecordIndex(AParentRecord: TcxCustomGridRecord): Integer;
begin
  Result := AParentRecord.Index + GetRecordChildCount(AParentRecord);
end;

function TcxGridWinExplorerViewViewData.GetRecordClass(
  const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  if IsGroup(ARecordInfo) then
    Result := GetGroupRecordClass
  else
    Result := GetDataRecordClass;
end;

function TcxGridWinExplorerViewViewData.IsGroup(const ARecordInfo: TcxRowInfo): Boolean;
begin
  Result := ARecordInfo.Level < GridView.GroupedItemCount;
end;

function TcxGridWinExplorerViewViewData.IsGrouped: Boolean;
begin
  Result := GridView.GroupedItemCount > 0;
end;

function TcxGridWinExplorerViewViewData.IsLastChildRecord(ARecord: TcxCustomGridRecord): Boolean;
var
  AParentRecord: TcxCustomGridRecord;
begin
  AParentRecord := ARecord.ParentRecord;
  Result := (AParentRecord <> nil) and (ARecord.Index = GetLastChildRecordIndex(AParentRecord));
end;

function TcxGridWinExplorerViewViewData.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

{ TcxGridWinExplorerViewControllerHelper }

constructor TcxGridWinExplorerViewControllerHelper.Create(AController: TcxGridWinExplorerViewController);
begin
  inherited Create;
  FController := AController;
end;

procedure TcxGridWinExplorerViewControllerHelper.AdjustContentScrollPos(var Value: Integer);
begin
  if Value > GetContentScrollSize - GetContentScrollPageSize then
    Value := GetContentScrollSize - GetContentScrollPageSize;
  if Value < 0 then
    Value := 0;
end;

function TcxGridWinExplorerViewControllerHelper.GetContentScrollPageSize: Integer;
begin
  Result := ViewInfo.ClientWidth;
end;

function TcxGridWinExplorerViewControllerHelper.GetContentScrollPosition: Integer;
begin
  Result := Controller.ContentScrollPos;
end;

function TcxGridWinExplorerViewControllerHelper.GetContentScrollSize: Integer;
begin
  Result := ViewInfo.GetContentScrollSize;
end;

function TcxGridWinExplorerViewControllerHelper.GetContentScrollStep: Integer;
begin
  Result := HScrollDelta;
end;

function TcxGridWinExplorerViewControllerHelper.GetDataScrollPageSize: Integer;
begin
  Result := Controller.VisibleDataScrollSize;
end;

function TcxGridWinExplorerViewControllerHelper.GetDataScrollPosition: Integer;
begin
  Result := Controller.ScrollBarPos;
end;

function TcxGridWinExplorerViewControllerHelper.GetDataScrollSize: Integer;
begin
  Result := Controller.DataScrollSize;
end;

function TcxGridWinExplorerViewControllerHelper.GetDataScrollStep: Integer;
begin
  Result := 1;
end;

function TcxGridWinExplorerViewControllerHelper.GetIsRecordsScrollHorizontal: Boolean;
begin
  Result := IsDataScrollBar(sbHorizontal);
end;

function TcxGridWinExplorerViewControllerHelper.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := mwskVertical;
end;

function TcxGridWinExplorerViewControllerHelper.GetScrollableAreaSize(AKind: TScrollBarKind): Integer;
begin
  if AKind = sbHorizontal then
    Result := ViewInfo.ScrollableAreaWidth
  else
    Result := ViewInfo.ScrollableAreaHeight;
end;

function TcxGridWinExplorerViewControllerHelper.HScrollBarMaxValue: Integer;
begin
  Result := ScrollBarMaxValue(sbHorizontal);
end;

function TcxGridWinExplorerViewControllerHelper.HScrollBarPageSize: Integer;
begin
  Result := ScrollBarPageSize(sbHorizontal);
end;

function TcxGridWinExplorerViewControllerHelper.HScrollBarPosition: Integer;
begin
  Result := ScrollBarPosition(sbHorizontal);
end;

function TcxGridWinExplorerViewControllerHelper.HScrollBarStep: Integer;
begin
  Result := ScrollBarStep(sbHorizontal);
end;

procedure TcxGridWinExplorerViewControllerHelper.InitHScrollBarsParameters;
begin
  Controller.SetScrollBarInfo(sbHorizontal, 0, HScrollBarMaxValue,
    HScrollBarStep, HScrollBarPageSize, HScrollBarPosition, True, Controller.CanHScrollBarHide);
end;

procedure TcxGridWinExplorerViewControllerHelper.InitScrollBarsParameters;
begin
  InitHScrollBarsParameters;
  InitVScrollBarsParameters;
end;

procedure TcxGridWinExplorerViewControllerHelper.InitVScrollBarsParameters;
begin
  Controller.SetScrollBarInfo(sbVertical, 0, VScrollBarMaxValue,
    VScrollBarStep, VScrollBarPageSize, VScrollBarPosition, True, True);
end;

function TcxGridWinExplorerViewControllerHelper.IsDataScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := AKind = sbVertical;
end;

procedure TcxGridWinExplorerViewControllerHelper.GoDown;
begin
  Controller.GoToNext(False, False);
end;

procedure TcxGridWinExplorerViewControllerHelper.GoLeft;
begin
  if ViewInfo.UseRightToLeftAlignment then
    GoNext
  else
    GoPrev;
end;

procedure TcxGridWinExplorerViewControllerHelper.GoNext;
begin
  Controller.GoToNext(False, False);
end;

procedure TcxGridWinExplorerViewControllerHelper.GoPrev;
begin
  Controller.GoToPrev(False, False);
end;

procedure TcxGridWinExplorerViewControllerHelper.GoRight;
begin
  if ViewInfo.UseRightToLeftAlignment then
    GoPrev
  else
    GoNext;
end;

procedure TcxGridWinExplorerViewControllerHelper.GoUp;
begin
  Controller.GoToPrev(False, False);
end;

procedure TcxGridWinExplorerViewControllerHelper.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT:
      GoLeft;
    VK_RIGHT:
      GoRight;
    VK_UP:
      GoUp;
    VK_DOWN:
      GoDown;
  end;
end;

function TcxGridWinExplorerViewControllerHelper.NeedAdjustScrollPosOnDataScroll(AScrollCode: TScrollCode): Boolean;
begin
  Result := True;
end;

function TcxGridWinExplorerViewControllerHelper.ScrollBarMaxValue(AKind: TScrollBarKind): Integer;
begin
  if IsDataScrollBar(AKind) then
    Result := GetDataScrollSize - 1
  else
    Result := GetContentScrollSize - 1;
end;

function TcxGridWinExplorerViewControllerHelper.ScrollBarPageSize(AKind: TScrollBarKind): Integer;
begin
  if IsDataScrollBar(AKind) then
    Result := GetDataScrollPageSize
  else
    Result := GetContentScrollPageSize;
end;

function TcxGridWinExplorerViewControllerHelper.ScrollBarPosition(AKind: TScrollBarKind): Integer;
begin
  if IsDataScrollBar(AKind) then
    Result := GetDataScrollPosition
  else
    if GetScrollableAreaSize(AKind) > 0 then
      Result := GetContentScrollPosition
    else
      Result := -1;
end;

function TcxGridWinExplorerViewControllerHelper.ScrollBarStep(AKind: TScrollBarKind): Integer;
begin
  if IsDataScrollBar(AKind) then
    Result := GetDataScrollStep
  else
    Result := GetContentScrollStep;
end;

procedure TcxGridWinExplorerViewControllerHelper.ScrollData(ADirection: TcxDirection);
begin
//do nothing
end;

function TcxGridWinExplorerViewControllerHelper.VScrollBarMaxValue: Integer;
begin
  Result := ScrollBarMaxValue(sbVertical);
end;

function TcxGridWinExplorerViewControllerHelper.VScrollBarPageSize: Integer;
begin
  Result := ScrollBarPageSize(sbVertical);
end;

function TcxGridWinExplorerViewControllerHelper.VScrollBarPosition: Integer;
begin
  Result := ScrollBarPosition(sbVertical);
end;

function TcxGridWinExplorerViewControllerHelper.VScrollBarStep: Integer;
begin
  Result := ScrollBarStep(sbVertical);
end;

function TcxGridWinExplorerViewControllerHelper.GetRecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo;
begin
  Result := ViewInfo.RecordsViewInfo;
end;

function TcxGridWinExplorerViewControllerHelper.GetSite: TcxGridSite;
begin
  Result := Controller.Site;
end;

function TcxGridWinExplorerViewControllerHelper.GetViewData: TcxGridWinExplorerViewViewData;
begin
  Result := Controller.ViewData;
end;

function TcxGridWinExplorerViewControllerHelper.GetViewInfo: TcxGridWinExplorerViewViewInfo;
begin
  Result := Controller.ViewInfo;
end;

procedure TcxGridWinExplorerViewControllerContentModeHelper.ScrollData(ADirection: TcxDirection);
begin
  case ADirection of
    dirUp:
      Controller.ScrollRecords(False, 1);
    dirDown:
      Controller.ScrollRecords(True, 1);
  end;
end;

{ TcxGridWinExplorerViewControllerImagesModeHelper }

procedure TcxGridWinExplorerViewControllerImagesModeHelper.FocusRecordInNextRow;
var
  AFocusedRecord: TcxCustomGridRecord;
  ANextIndex, ALastRecordIndex: Integer;
begin
  AFocusedRecord := Controller.FocusedRecord;
  if (AFocusedRecord = nil) then
    Exit;
  if not AFocusedRecord.IsData then
    ANextIndex := AFocusedRecord.Index + 1
  else
  begin
    ANextIndex := AFocusedRecord.Index + RecordsViewInfo.GetColumnCountByRecordIndex(AFocusedRecord.Index);
    if ViewData.IsGrouped then
      ALastRecordIndex := ViewData.GetLastChildRecordIndex(AFocusedRecord.ParentRecord)
    else
      ALastRecordIndex := ViewData.RecordCount - 1;
    if ANextIndex > ALastRecordIndex then
      if RecordsViewInfo.GetItemColumnIndex(AFocusedRecord.Index) > RecordsViewInfo.GetItemColumnIndex(ALastRecordIndex) then
        ANextIndex := ALastRecordIndex
      else
        if ViewData.IsGrouped and ViewData.IsRecordIndexValid(ALastRecordIndex + 1) then
          ANextIndex := ALastRecordIndex + 1
        else
          ANextIndex := -1;
  end;
  if ViewData.IsRecordIndexValid(ANextIndex) then
    Controller.FocusedRecordIndex := ANextIndex;
end;

procedure TcxGridWinExplorerViewControllerImagesModeHelper.FocusRecordInPrevRow;
var
  AFocusedRecord: TcxCustomGridRecord;
  APrevIndex, AColumnCount: Integer;
begin
  AFocusedRecord := Controller.FocusedRecord;
  if AFocusedRecord = nil then
    Exit;
  if not AFocusedRecord.IsData then
    APrevIndex := AFocusedRecord.Index - 1
  else
  begin
    AColumnCount := RecordsViewInfo.GetColumnCountByRecordIndex(AFocusedRecord.Index);
    if ViewData.IsGrouped and (AFocusedRecord.Index - AFocusedRecord.ParentRecord.Index < AColumnCount) then
      APrevIndex := AFocusedRecord.ParentRecord.Index
    else
      APrevIndex := AFocusedRecord.Index - AColumnCount;
  end;
  if ViewData.IsRecordIndexValid(APrevIndex) then
    Controller.FocusedRecordIndex := APrevIndex;
end;

procedure TcxGridWinExplorerViewControllerImagesModeHelper.GoDown;
begin
  FocusRecordInNextRow;
end;

procedure TcxGridWinExplorerViewControllerImagesModeHelper.GoUp;
begin
  FocusRecordInPrevRow;
end;

procedure TcxGridWinExplorerViewControllerImagesModeHelper.ScrollData(ADirection: TcxDirection);
begin
  case ADirection of
    dirLeft:
      Controller.ContentScrollPos := Controller.ContentScrollPos - GetContentScrollStep;
    dirRight:
      Controller.ContentScrollPos := Controller.ContentScrollPos + GetContentScrollStep;
    dirUp:
      Controller.ScrollRecords(False, 1);
    dirDown:
      Controller.ScrollRecords(True, Controller.GetRecordCountInScrollBand(Controller.TopRecordIndex));
  end;
end;

function TcxGridWinExplorerViewControllerImagesModeHelper.GetRecordsViewInfo: TcxGridWinExplorerViewImagesModeRecordsViewInfo;
begin
  Result := TcxGridWinExplorerViewImagesModeRecordsViewInfo(inherited RecordsViewInfo);
end;

{ TcxGridWinExplorerViewControllerListModeHelper }

procedure TcxGridWinExplorerViewControllerListModeHelper.FocusNextRecord;
var
  ARecord: TcxCustomGridRecord;
begin
  ARecord := Controller.FocusedRecord;
  if ViewData.IsGrouped and (ARecord <> nil) and ViewData.IsLastChildRecord(ARecord) and
    ViewData.IsRecordIndexValid(ARecord.Index + 1) then
    Controller.FocusNextRecordWithSelection(ARecord.Index + 1, True, False, False, False)
  else
    Controller.GoToNext(False, False);
end;

procedure TcxGridWinExplorerViewControllerListModeHelper.FocusPrevRecord;
var
  ARecord, APrevRecord: TcxCustomGridRecord;
begin
  APrevRecord := Controller.FocusedRecord;
  Controller.GoToPrev(False, False);
  ARecord := Controller.FocusedRecord;
  if ARecord = nil then
    Exit;
  if ViewData.IsGrouped and not ARecord.IsData then
    if ARecord.IsFirst then
      Controller.FocusedRecord := APrevRecord
    else
      Controller.GoToPrev(False, False);
end;

procedure TcxGridWinExplorerViewControllerListModeHelper.FocusRecordInNextColumn;
var
  AFocusedRecord, ANextGroup: TcxCustomGridRecord;
  AFocusedRecordRowIndex, ANextIndex, ANextGroupIndex, ALastRecordIndex: Integer;
begin
  AFocusedRecord := Controller.FocusedRecord;
  if (AFocusedRecord = nil) then
    Exit;
  if not AFocusedRecord.IsData then
    ANextIndex := AFocusedRecord.Index + 1
  else
  begin
    ANextIndex := AFocusedRecord.Index + RecordsViewInfo.GetRowCountByRecordIndex(AFocusedRecord.Index);
    if ViewData.IsGrouped then
      ALastRecordIndex := ViewData.GetLastChildRecordIndex(AFocusedRecord.ParentRecord)
    else
      ALastRecordIndex := ViewData.RecordCount - 1;
    if ANextIndex > ALastRecordIndex then
    begin
      AFocusedRecordRowIndex := RecordsViewInfo.GetItemRowIndex(AFocusedRecord.Index);
      if AFocusedRecordRowIndex > RecordsViewInfo.GetItemRowIndex(ALastRecordIndex) then
        ANextIndex := ALastRecordIndex
      else
      begin
        ANextIndex := -1;
        if ViewData.IsGrouped then
        begin
          ANextGroupIndex := ViewData.GetLastChildRecordIndex(AFocusedRecord.ParentRecord) + 1;
          if ViewData.IsRecordIndexValid(ANextGroupIndex) then
          begin
            ANextGroup := ViewData.Records[ANextGroupIndex];
            if AFocusedRecordRowIndex > ViewData.GetRecordChildCount(ANextGroup) - 1 then
              ANextIndex := ViewData.GetLastChildRecordIndex(ANextGroup)
            else
              ANextIndex := ANextGroupIndex + AFocusedRecordRowIndex + 1;
          end
        end;
      end;
    end;
  end;
  if ViewData.IsRecordIndexValid(ANextIndex) then
    Controller.FocusedRecordIndex := ANextIndex;
end;

procedure TcxGridWinExplorerViewControllerListModeHelper.FocusRecordInPrevColumn;
var
  AFocusedRecord: TcxCustomGridRecord;
  APrevIndex, AFocusedRecordRowIndex, AFirstRecordIndex: Integer;
  APrevGroupLastChildRecordIndex, APrevGroupLastChildRecordRowIndex: Integer;
begin
  AFocusedRecord := Controller.FocusedRecord;
  if AFocusedRecord = nil then
    Exit;
  if not AFocusedRecord.IsData then
    APrevIndex := AFocusedRecord.Index - 1
  else
  begin
    APrevIndex := AFocusedRecord.Index - RecordsViewInfo.GetRowCountByRecordIndex(AFocusedRecord.Index);
    if ViewData.IsGrouped then
      AFirstRecordIndex := AFocusedRecord.ParentRecord.Index + 1
    else
      AFirstRecordIndex := 0;
    if APrevIndex < AFirstRecordIndex then
    begin
      APrevIndex := -1;
      AFocusedRecordRowIndex := RecordsViewInfo.GetItemRowIndex(AFocusedRecord.Index);
      if ViewData.IsGrouped then
      begin
        APrevGroupLastChildRecordIndex := AFocusedRecord.ParentRecord.Index - 1;
        if ViewData.IsRecordIndexValid(APrevGroupLastChildRecordIndex) then
        begin
          APrevGroupLastChildRecordRowIndex := RecordsViewInfo.GetItemRowIndex(APrevGroupLastChildRecordIndex);
          if AFocusedRecordRowIndex > APrevGroupLastChildRecordRowIndex then
            APrevIndex := APrevGroupLastChildRecordIndex
          else
            APrevIndex := APrevGroupLastChildRecordIndex + AFocusedRecordRowIndex - APrevGroupLastChildRecordRowIndex;
        end
      end;
    end;
  end;
  if ViewData.IsRecordIndexValid(APrevIndex) then
    Controller.FocusedRecordIndex := APrevIndex;
end;

procedure TcxGridWinExplorerViewControllerListModeHelper.GoDown;
begin
  FocusNextRecord;
end;

procedure TcxGridWinExplorerViewControllerListModeHelper.GoNext;
begin
  FocusRecordInNextColumn;
end;

procedure TcxGridWinExplorerViewControllerListModeHelper.GoPrev;
begin
  FocusRecordInPrevColumn;
end;

procedure TcxGridWinExplorerViewControllerListModeHelper.GoUp;
begin
  FocusPrevRecord;
end;

function TcxGridWinExplorerViewControllerListModeHelper.GetContentScrollPageSize: Integer;
begin
  Result := ViewInfo.ClientHeight;
end;

function TcxGridWinExplorerViewControllerListModeHelper.GetContentScrollStep: Integer;
begin
  Result := VScrollDelta;
end;

function TcxGridWinExplorerViewControllerListModeHelper.GetDataScrollPageSize: Integer;
var
  ARecord: TcxCustomGridRecord;
  AIndex: Integer;
begin
  Result := inherited GetDataScrollPageSize;
  if NeedDataScroll and ViewData.IsRecordIndexValid(Controller.TopRecordIndex) then
  begin
    AIndex := Controller.TopRecordIndex;
    ARecord := ViewData.Records[Controller.TopRecordIndex];
    if not ARecord.IsData then
      Inc(AIndex);
    Result := RecordsViewInfo.GetRowCountByRecordIndex(AIndex);
  end;
end;

function TcxGridWinExplorerViewControllerListModeHelper.GetDataScrollPosition: Integer;
begin
  if (Site <> nil) and TcxGridSiteAccess(Site).IsScrollBarsCapture then
    Result := Site.HScrollBar.Position
  else
    Result := inherited GetDataScrollPosition;
end;

function TcxGridWinExplorerViewControllerListModeHelper.GetDataScrollSize: Integer;
var
  AItemCountInLastBand, ARowCount, ALastRecordIndex: Integer;
begin
  Result := inherited GetDataScrollSize;
  if NeedDataScroll and (ViewData.RecordCount > 0) then
  begin
    ALastRecordIndex := ViewData.RecordCount - 1;
    AItemCountInLastBand := RecordsViewInfo.GetItemCountInBand(ALastRecordIndex);
    ARowCount := RecordsViewInfo.GetRowCountByRecordIndex(ALastRecordIndex);
    Result := Result - AItemCountInLastBand + ARowCount;
  end;
end;

function TcxGridWinExplorerViewControllerListModeHelper.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := mwskHorizontal;
end;

function TcxGridWinExplorerViewControllerListModeHelper.IsDataScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := AKind = sbHorizontal;
end;

function TcxGridWinExplorerViewControllerListModeHelper.NeedAdjustScrollPosOnDataScroll(AScrollCode: TScrollCode): Boolean;
begin
  Result := AScrollCode in [scLineUp, scLineDown, scLineDown, scPageDown];
end;

function TcxGridWinExplorerViewControllerListModeHelper.NeedDataScroll: Boolean;
begin
  Result := ViewInfo.VisibleRecordCount <> ViewData.RecordCount;
end;

procedure TcxGridWinExplorerViewControllerListModeHelper.ScrollData(ADirection: TcxDirection);
begin
  case ADirection of
    dirLeft:
      Controller.ScrollRecords(False, 1);
    dirRight:
      Controller.ScrollRecords(True, Controller.GetRecordCountInScrollBand(Controller.TopRecordIndex));
    dirUp:
      Controller.ContentScrollPos := Controller.ContentScrollPos - GetContentScrollStep;
    dirDown:
      Controller.ContentScrollPos := Controller.ContentScrollPos + GetContentScrollStep;
  end
end;

function TcxGridWinExplorerViewControllerListModeHelper.GetRecordsViewInfo: TcxGridWinExplorerViewListModeRecordsViewInfo;
begin
  Result := TcxGridWinExplorerViewListModeRecordsViewInfo(inherited RecordsViewInfo);
end;

{ TcxGridWinExplorerViewController }

constructor TcxGridWinExplorerViewController.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  CreateHelper;
end;

destructor TcxGridWinExplorerViewController.Destroy;
begin
  DestroyHelper;
  inherited Destroy;
end;

procedure TcxGridWinExplorerViewController.InitScrollBarsParameters;
begin
  Helper.InitScrollBarsParameters;
end;

procedure TcxGridWinExplorerViewController.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_HOME:
      GoToFirst(False);
    VK_END:
      GoToLast(False, False);
  end;
  Helper.KeyDown(Key, Shift);
end;

function TcxGridWinExplorerViewController.FindNextItem(AFocusedItemIndex: Integer; AGoForward: Boolean;
  AGoOnCycle: Boolean; AFollowVisualOrder: Boolean; out ACycleChanged: Boolean; ARecord: TcxCustomGridRecord): Integer;

  procedure PopulateItems(AList: TList);
  begin
    if GridView.IsDestroying then
      Exit;
    if ItemSet.HasTextItem then
      AList.Add(ItemSet.TextItem);
    if ItemSet.HasDescriptionItem then
      AList.Add(ItemSet.DescriptionItem);
  end;

  function CheckIndex(AList: TList; var AIndex: Integer): Boolean;
  begin
    Result := True;
    if AGoForward then
      if AIndex > AList.Count - 1 then
        if AGoOnCycle then
        begin
          AIndex := 0;
          ACycleChanged := True;
        end
        else
          Result := False
      else
    else
      if AIndex < 0 then
        if AGoOnCycle then
        begin
          AIndex := AList.Count - 1;
          ACycleChanged := True;
        end
        else
          Result := False;
  end;

var
  AList: TList;

begin
  AList := TList.Create;
  try
    PopulateItems(AList);
    if AList.Count = 0 then
      Result := -1
    else
    begin
      Result := AFocusedItemIndex;
      if Result = -1 then
      begin
        if AGoForward then
          Result := 0
        else
          if AGoOnCycle then
            Result := AList.Count - 1
          else
            Result := -1;
      end
      else
      begin
        Result := AList.IndexOf(GridView.VisibleItems[Result]);
        if AGoForward then
          Inc(Result)
        else
          Dec(Result);
        if not CheckIndex(AList, Result) then
          Result := -1;
      end;
      if Result <> -1 then
        Result := TcxGridWinExplorerViewItem(AList[Result]).VisibleIndex;
    end;
  finally
    AList.Free;
  end;
end;

procedure TcxGridWinExplorerViewController.MakeItemVisible(AItem: TcxCustomGridTableItem);
begin
  MakeFocusedRecordVisible;
end;

procedure TcxGridWinExplorerViewController.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  inherited MouseUp(Button, Shift, X, Y);
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if (GetActiveCellViewInfo(AHitTest) = nil) and (Shift * [ssCtrl, ssShift] = []) then
    if CanProcessMultiSelect(False) and (FocusedRecord <> nil) and
      not FocusedRecord.IsData and FocusedRecord.Expanded then
      ClearSelection
    else
      DoNormalSelection;
end;

procedure TcxGridWinExplorerViewController.DataScroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  case AScrollCode of
    scLineUp:
      ScrollData(GetScrollBarLineUpDirection(AScrollBarKind));
    scLineDown:
      ScrollData(GetScrollBarLineDownDirection(AScrollBarKind));
    scPageUp:
      ScrollPage(False);
    scPageDown:
      ScrollPage(True);
    scTrack:
      ScrollBarPos := AScrollPos
  end;
  if NeedAdjustDataScrollBarPosOnDataScroll(AScrollCode) then
    AScrollPos := ScrollBarPos;
end;

procedure TcxGridWinExplorerViewController.DoScroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited DoScroll(AScrollBarKind, AScrollCode, AScrollPos);
  if ViewData.RecordCount = 0  then
    Exit;
  if Helper.IsDataScrollBar(AScrollBarKind) then
    DataScroll(AScrollBarKind, AScrollCode, AScrollPos)
  else
    ContentScroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

function TcxGridWinExplorerViewController.GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited GetDesignHitTest(AHitTest);
  if not Result then
    Result := AHitTest.HitTestCode in [htCell];
end;

function TcxGridWinExplorerViewController.GetDragScrollInterval: Integer;
begin
  Result := ViewInfo.RecordsViewInfo.GetItemCountInBand(0) * inherited GetDragScrollInterval;
end;

function TcxGridWinExplorerViewController.GetFirstIndexInScrollBand(ARecordIndexInBand: Integer): Integer;
begin
  Result := ViewInfo.RecordsViewInfo.GetFirstRecordIndexInBand(ARecordIndexInBand);
end;

function TcxGridWinExplorerViewController.GetHelperClass: TcxGridWinExplorerViewControllerHelperClass;
begin
  case GridView.ActiveDisplayMode of
    dmContent:
      Result := TcxGridWinExplorerViewControllerContentModeHelper;
    dmList:
      Result := TcxGridWinExplorerViewControllerListModeHelper;
  else
    Result := TcxGridWinExplorerViewControllerImagesModeHelper;
  end;
end;

function TcxGridWinExplorerViewController.GetIsRecordsScrollHorizontal: Boolean;
begin
  Result := Helper.GetIsRecordsScrollHorizontal;
end;

function TcxGridWinExplorerViewController.GetMaxTopRecordIndexValue: Integer;
begin
  if (GridView.ActiveDisplayMode = dmList) and (ViewData.RecordCount > 0) then
    Result := GetFirstIndexInScrollBand(ViewData.RecordCount - 1)
  else
    Result := inherited GetMaxTopRecordIndexValue;
end;

function TcxGridWinExplorerViewController.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := Helper.GetMouseWheelScrollingKind;
end;

function TcxGridWinExplorerViewController.GetRecordCountInScrollBand(ARecordIndexInBand: Integer): Integer;
begin
  Result := ViewInfo.RecordsViewInfo.GetItemCountInBand(ARecordIndexInBand);
end;

function TcxGridWinExplorerViewController.GetScrollBarLineDownDirection(AScrollBarKind: TScrollBarKind): TcxDirection;
begin
  if AScrollBarKind = sbHorizontal then
    Result := dirRight
  else
    Result := dirDown;
end;

function TcxGridWinExplorerViewController.GetScrollBarLineUpDirection(AScrollBarKind: TScrollBarKind): TcxDirection;
begin
  if AScrollBarKind = sbHorizontal then
    Result := dirLeft
  else
    Result := dirUp;
end;

function TcxGridWinExplorerViewController.GetTopRecordIndexByBottomRecord(ABottomRecordIndex: Integer): Integer;
var
  ATopIndex: Integer;
begin
  if (GridView.ActiveDisplayMode = dmList) and (ViewData.RecordCount > 0) then
  begin
    ATopIndex := ABottomRecordIndex - GetPageVisibleRecordCount(TopRecordIndex);
    if ViewData.IsRecordIndexValid(ATopIndex) then
      Result := GetLastIndexInScrollBand(ATopIndex) + 1
    else
    begin
      CheckTopRecordIndex(ATopIndex);
      Result := ATopIndex;
    end;
  end
  else
    Result := inherited GetTopRecordIndexByBottomRecord(ABottomRecordIndex);
end;

function TcxGridWinExplorerViewController.GetVisibleChildCountOnTop(AParentRecord: TcxCustomGridRecord): Integer;

  function IsParentLocatedOnTop(AParentRecord: TcxCustomGridRecord): Boolean;
  begin
    Result := ViewData.IsRecordIndexValid(TopRecordIndex) and (AParentRecord = ViewData.Records[TopRecordIndex].ParentRecord);
  end;

begin
  if IsParentLocatedOnTop(AParentRecord) then
    Result := ViewData.GetLastChildRecordIndex(AParentRecord) - GetFirstIndexInScrollBand(TopRecordIndex) + 1
  else
    Result := ViewData.GetRecordChildCount(AParentRecord);
end;

function TcxGridWinExplorerViewController.IsRecordFullyVisibleAtBottom(ARecordIndex: Integer): Boolean;
begin
  Result := inherited IsRecordFullyVisibleAtBottom(ARecordIndex) and
    ViewInfo.RecordsViewInfo.IsRecordFullyVisibleAtBottom(ARecordIndex);
end;

function TcxGridWinExplorerViewController.NeedAdjustDataScrollBarPosOnDataScroll(AScrollCode: TScrollCode): Boolean;
begin
  Result := Helper.NeedAdjustScrollPosOnDataScroll(AScrollCode);
end;

procedure TcxGridWinExplorerViewController.ScrollData(ADirection: TcxDirection);
begin
  Helper.ScrollData(ADirection);
end;

procedure TcxGridWinExplorerViewController.DisplayModeChanged;
begin
  BeginUpdate;
  try
    RecreateHelper;
    ContentScrollPos := 0;
    ScrollBarPos := 0;
    ViewData.Expand(True);
    GridView.Changed(vcSize);
  finally
    EndUpdate;
  end;
end;

function TcxGridWinExplorerViewController.CanProcessMultiSelect(AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean;
begin
  Result := inherited CanProcessMultiSelect(AHitTest, AShift) and TcxGridRecordHitTest(AHitTest).GridRecord.IsData;
end;

procedure TcxGridWinExplorerViewController.CheckCoordinates;
begin
  inherited CheckCoordinates;
  ContentScrollPos := ContentScrollPos;
end;

procedure TcxGridWinExplorerViewController.ContentScroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  case AScrollCode of
    scLineUp:
      ScrollData(GetScrollBarLineUpDirection(AScrollBarKind));
    scLineDown:
      ScrollData(GetScrollBarLineDownDirection(AScrollBarKind));
    scPageUp:
      ContentScrollPos := ContentScrollPos - Helper.GetScrollableAreaSize(AScrollBarKind);
    scPageDown:
      ContentScrollPos := ContentScrollPos + Helper.GetScrollableAreaSize(AScrollBarKind);
    scTrack:
      ContentScrollPos := AScrollPos;
  end;
  AScrollPos := ContentScrollPos;
end;

procedure TcxGridWinExplorerViewController.CreateHelper;
begin
  FHelper := GetHelperClass.Create(Self);
end;

procedure TcxGridWinExplorerViewController.DestroyHelper;
begin
  FreeAndNil(FHelper);
end;

procedure TcxGridWinExplorerViewController.RecreateHelper;
begin
  DestroyHelper;
  CreateHelper;
end;

function TcxGridWinExplorerViewController.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewController.GetItemSet: TcxGridWinExplorerViewItemSet;
begin
  Result := GridView.ItemSet;
end;

function TcxGridWinExplorerViewController.GetViewData: TcxGridWinExplorerViewViewData;
begin
  Result := TcxGridWinExplorerViewViewData(inherited ViewData);
end;

function TcxGridWinExplorerViewController.GetViewInfo: TcxGridWinExplorerViewViewInfo;
begin
  Result := TcxGridWinExplorerViewViewInfo(inherited ViewInfo);
end;

procedure TcxGridWinExplorerViewController.SetContentScrollPos(Value: Integer);
var
  ADelta: Integer;
begin
  Helper.AdjustContentScrollPos(Value);
  if FContentScrollPos <> Value then
  begin
    ADelta := FContentScrollPos - Value;
    FContentScrollPos := Value;
    if Helper.IsDataScrollBar(sbVertical) then
      GridView.Offset(0, 0, ADelta, 0)
    else
      GridView.Offset(0, 0, 0, ADelta);
  end;
end;

{ TcxGridWinExplorerViewOptionsBehavior }

constructor TcxGridWinExplorerViewOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FHotTrack := True;
end;

procedure TcxGridWinExplorerViewOptionsBehavior.Assign(Source: TPersistent);
var
  ASource: TcxGridWinExplorerViewOptionsBehavior;
begin
  inherited Assign(Source);
  if Source is TcxGridWinExplorerViewOptionsBehavior then
  begin
    ASource := TcxGridWinExplorerViewOptionsBehavior(Source);
    HotTrack := ASource.HotTrack;
  end;
end;

procedure TcxGridWinExplorerViewOptionsBehavior.SetHotTrack(
  const Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    Changed(vcLayout);
  end;
end;

{ TcxGridWinExplorerViewItemSetItem }

constructor TcxGridWinExplorerViewItemSetItem.Create(
  AGridItem: TcxGridWinExplorerViewItem; AIndex: Integer);
begin
  inherited Create;
  FGridItem := AGridItem;
  FIndex := AIndex;
end;

{ TcxGridWinExplorerViewItemSet }

constructor TcxGridWinExplorerViewItemSet.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FItems := TdxFastObjectList.Create;
end;

destructor TcxGridWinExplorerViewItemSet.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TcxGridWinExplorerViewItemSet.Assign(Source: TPersistent);

  function GetOwnSimilarItem(ASourceItem: TcxGridWinExplorerViewItem): TcxGridWinExplorerViewItem;
  begin
    if ASourceItem = nil then
      Result := nil
    else
      Result := GridView.FindItemByID(ASourceItem.ID) as TcxGridWinExplorerViewItem;
  end;

var
  ASource: TcxGridWinExplorerViewItemSet;
begin
  inherited Assign(Source);
  if Source is TcxGridWinExplorerViewItemSet then
  begin
    ASource := TcxGridWinExplorerViewItemSet(Source);
    CheckBoxItem := GetOwnSimilarItem(ASource.CheckBoxItem);
    DescriptionItem := GetOwnSimilarItem(ASource.DescriptionItem);
    ExtraLargeImageItem := GetOwnSimilarItem(ASource.ExtraLargeImageItem);
    GroupItem := GetOwnSimilarItem(ASource.GroupItem);
    LargeImageItem := GetOwnSimilarItem(ASource.LargeImageItem);
    MediumImageItem := GetOwnSimilarItem(ASource.MediumImageItem);
    SmallImageItem := GetOwnSimilarItem(ASource.SmallImageItem);
    TextItem := GetOwnSimilarItem(ASource.TextItem);
    ExtraLargeImageIndexItem := GetOwnSimilarItem(ASource.ExtraLargeImageIndexItem);
    LargeImageIndexItem := GetOwnSimilarItem(ASource.LargeImageIndexItem);
    MediumImageItem := GetOwnSimilarItem(ASource.MediumImageItem);
    SmallImageIndexItem := GetOwnSimilarItem(ASource.SmallImageIndexItem);
  end;
end;

function TcxGridWinExplorerViewItemSet.GetValue(AIndex: Integer): TcxGridWinExplorerViewItem;
var
  AItemIndex: Integer;
begin
  Result := nil;
  AItemIndex := FindItemIndex(AIndex);
  if AItemIndex <> -1 then
    Result := Items[AItemIndex].GridItem;
end;

procedure TcxGridWinExplorerViewItemSet.SetValue(AIndex: Integer; AValue: TcxGridWinExplorerViewItem);

  function IsGroupUpdate(AIndex: Integer): Boolean;
  begin
    Result := AIndex = isiGroupItem;
  end;

  procedure BeginItemUpdate(AIndex: Integer);
  begin
    if IsGroupUpdate(AIndex) then
      GridView.BeginGroupingUpdate;
  end;

  procedure EndItemUpdate(AIndex: Integer);
  begin
    if IsGroupUpdate(AIndex) then
      GridView.EndGroupingUpdate
    else
      Changed(vcSize);
  end;

var
  AItemIndex: Integer;
  AItem: TcxGridWinExplorerViewItemSetItem;
begin
  if GetValue(AIndex) = AValue then
    Exit;
  BeginItemUpdate(AIndex);
  try
    if IsGroupUpdate(AIndex) then
      GridView.ResetGrouping;
    AItemIndex := FindItemIndex(AIndex);
    if AItemIndex <> -1 then
      FItems.Delete(AItemIndex);
    if AValue <> nil then
    begin
      AItem := TcxGridWinExplorerViewItemSetItem.Create(AValue, AIndex);
      FItems.Add(AItem);
    end;
    if IsGroupUpdate(AIndex) then
      GridView.Grouping;
  finally
    EndItemUpdate(AIndex);
  end;
end;

function TcxGridWinExplorerViewItemSet.FindItemIndex(AGridItemIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if Items[I].Index = AGridItemIndex then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TcxGridWinExplorerViewItemSet.GridItemRemove(AGridItem: TcxCustomGridTableItem);
var
  I: Integer;
  AItem: TcxGridWinExplorerViewItemSetItem;
begin
  for I := FItems.Count - 1 downto 0 do
  begin
    AItem := Items[I];
    if AItem.GridItem = AGridItem then
      FItems.Remove(AItem);
  end;
end;

function TcxGridWinExplorerViewItemSet.HasCheckBoxItem: Boolean;
begin
  Result := CheckBoxItem <> nil;
end;

function TcxGridWinExplorerViewItemSet.HasDescriptionItem: Boolean;
begin
  Result := DescriptionItem <> nil;
end;

function TcxGridWinExplorerViewItemSet.HasTextItem: Boolean;
begin
  Result := TextItem <> nil;
end;

function TcxGridWinExplorerViewItemSet.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewItemSet.GetItem(AIndex: Integer): TcxGridWinExplorerViewItemSetItem;
begin
  Result := TcxGridWinExplorerViewItemSetItem(FItems[AIndex]);
end;

{ TcxGridWinExplorerViewOptionsDisplayModeIndents }

constructor TcxGridWinExplorerViewOptionsDisplayModeIndents.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FBetweenCheckBoxAndImage := cxGridWinExplorerViewRecordItemIndent;
  FBetweenImageAndDescription := cxGridWinExplorerViewRecordItemIndent;
  FBetweenImageAndText := GetIndentBetweenImageAndTextDefaultValue;
  FBetweenTextAndDescription := cxGridWinExplorerViewRecordItemIndent;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModeIndents.Assign(Source: TPersistent);
var
  ASource: TcxGridWinExplorerViewOptionsDisplayModeIndents;
begin
  inherited Assign(Source);
  if Source is TcxGridWinExplorerViewOptionsDisplayModeIndents then
  begin
    ASource := TcxGridWinExplorerViewOptionsDisplayModeIndents(Source);
    BetweenCheckBoxAndImage := ASource.BetweenCheckBoxAndImage;
    BetweenGroupAndItem := ASource.BetweenGroupAndItem;
    BetweenGroups := ASource.BetweenGroups;
    BetweenImageAndDescription := ASource.BetweenImageAndDescription;
    BetweenImageAndText := ASource.BetweenImageAndText;
    BetweenTextAndDescription := ASource.BetweenTextAndDescription;
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModeIndents.ChangeScale(M, D: Integer);
begin
  inherited;
  BetweenCheckBoxAndImage := MulDiv(BetweenCheckBoxAndImage, M, D);
  BetweenGroupAndItem := MulDiv(BetweenGroupAndItem, M, D);
  BetweenGroups := MulDiv(BetweenGroups, M, D);
  BetweenImageAndDescription := MulDiv(BetweenImageAndDescription, M, D);
  BetweenImageAndText := MulDiv(BetweenImageAndText, M, D);
  BetweenTextAndDescription := MulDiv(BetweenTextAndDescription, M, D);
end;

function TcxGridWinExplorerViewOptionsDisplayModeIndents.GetIndentBetweenImageAndTextDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewRecordItemIndent;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModeIndents.SetIndentBetweenCheckBoxAndImage(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue <> FBetweenCheckBoxAndImage then
  begin
    FBetweenCheckBoxAndImage := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModeIndents.SetIndentBetweenGroupAndItem(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue <> FBetweenGroupAndItem then
  begin
    FBetweenGroupAndItem := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModeIndents.SetIndentBetweenGroups(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue <> FBetweenGroups then
  begin
    FBetweenGroups := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModeIndents.SetBetweenImageAndDescription(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue <> FBetweenImageAndDescription then
  begin
    FBetweenImageAndDescription := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModeIndents.SetIndentBetweenImageAndText(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue <> FBetweenImageAndText then
  begin
    FBetweenImageAndText := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModeIndents.SetIndentBetweenTextAndDescription(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue <> FBetweenTextAndDescription then
  begin
    FBetweenTextAndDescription := AValue;
    Changed(vcSize);
  end;
end;

{ TcxGridWinExplorerViewOptionsContentDisplayModeIndents }

function TcxGridWinExplorerViewOptionsContentDisplayModeIndents.GetIndentBetweenImageAndTextDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewContentModeIndentBetweenImageAndText;
end;

{ TcxGridWinExplorerViewDisplayModeOptions }

constructor TcxGridWinExplorerViewOptionsDisplayMode.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FImageSize := TcxSize.Create(nil, GetImageWidthDefaultValue, GetImageHeightDefaultValue);
  FImageSize.OnChange := ImageSizeChangeHandler;
  FIndents := GetIndentsClass.Create(GridView);
  FRecordWidth := cxGridWinExplorerViewDefaultRecordWidth;
  FShowItemDescriptions := GetShowItemDescriptionsDefaultValue;
end;

destructor TcxGridWinExplorerViewOptionsDisplayMode.Destroy;
begin
  FreeAndNil(FIndents);
  FreeAndNil(FImageSize);
  inherited Destroy;
end;

procedure TcxGridWinExplorerViewOptionsDisplayMode.Assign(ASource: TPersistent);
var
  ASourceOptions: TcxGridWinExplorerViewOptionsDisplayMode;
begin
  inherited Assign(ASource);
  if ASource is TcxGridWinExplorerViewOptionsDisplayMode then
  begin
    ASourceOptions := TcxGridWinExplorerViewOptionsDisplayMode(ASource);
    ImageSize := ASourceOptions.ImageSize;
    Indents := ASourceOptions.Indents;
    RecordWidth := ASourceOptions.RecordWidth;
    ShowItemDescriptions := ASourceOptions.ShowItemDescriptions;
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayMode.ChangeScale(M, D: Integer);
begin
  inherited;
  ImageSize.ChangeScale(M, D);
  Indents.ChangeScale(M, D);
  if RecordWidth > 0 then
    RecordWidth := Max(MulDiv(RecordWidth, M, D), 1);
end;

function TcxGridWinExplorerViewOptionsDisplayMode.GetDefaultDisplayModeKind: TcxGridWinExplorerViewDisplayMode;
begin
  Result := dmContent;
end;

function TcxGridWinExplorerViewOptionsDisplayMode.GetImageHeightDefaultValue: Integer;
begin
  Result := 0;
end;

function TcxGridWinExplorerViewOptionsDisplayMode.GetImageWidthDefaultValue: Integer;
begin
  Result := 0;
end;

function TcxGridWinExplorerViewOptionsDisplayMode.GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass;
begin
  Result := TcxGridWinExplorerViewOptionsDisplayModeIndents;
end;

function TcxGridWinExplorerViewOptionsDisplayMode.GetShowItemDescriptionsDefaultValue: Boolean;
begin
  Result := False;
end;

function TcxGridWinExplorerViewOptionsDisplayMode.GetActive: Boolean;
begin
  Result := GridView.ActiveDisplayMode = GetDisplayModeKind;
end;

function TcxGridWinExplorerViewOptionsDisplayMode.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

procedure TcxGridWinExplorerViewOptionsDisplayMode.ImageSizeChangeHandler(Sender: TObject);
begin
  Changed(vcSize);
end;

procedure TcxGridWinExplorerViewOptionsDisplayMode.SetActive(Value: Boolean);
begin
  if Value then
    GridView.ActiveDisplayMode := GetDisplayModeKind
  else
    GridView.ActiveDisplayMode := GetDefaultDisplayModeKind;
end;

procedure TcxGridWinExplorerViewOptionsDisplayMode.SetImageSize(AValue: TcxSize);
begin
  FImageSize.Assign(AValue);
end;

procedure TcxGridWinExplorerViewOptionsDisplayMode.SetIndents(AValue: TcxGridWinExplorerViewOptionsDisplayModeIndents);
begin
  FIndents.Assign(AValue);
end;

procedure TcxGridWinExplorerViewOptionsDisplayMode.SetRecordWidth(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if AValue <> FRecordWidth then
  begin
    FRecordWidth := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayMode.SetShowItemDescriptions(AValue: Boolean);
begin
  if AValue <> FShowItemDescriptions then
  begin
    FShowItemDescriptions := AValue;
    Changed(vcSize);
  end;
end;

{ TcxGridWinExplorerViewOptionsContentDisplayMode }

function TcxGridWinExplorerViewOptionsContentDisplayMode.GetImageHeightDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageMediumSize;
end;

function TcxGridWinExplorerViewOptionsContentDisplayMode.GetImageWidthDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageMediumSize;
end;

function TcxGridWinExplorerViewOptionsContentDisplayMode.GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass;
begin
  Result := TcxGridWinExplorerViewOptionsContentDisplayModeIndents;
end;

function TcxGridWinExplorerViewOptionsContentDisplayMode.GetShowItemDescriptionsDefaultValue: Boolean;
begin
  Result := True;
end;

function TcxGridWinExplorerViewOptionsContentDisplayMode.GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode;
begin
  Result := dmContent;
end;

function TcxGridWinExplorerViewOptionsContentDisplayMode.GetIndents: TcxGridWinExplorerViewOptionsContentDisplayModeIndents;
begin
  Result := TcxGridWinExplorerViewOptionsContentDisplayModeIndents(inherited Indents);
end;

procedure TcxGridWinExplorerViewOptionsContentDisplayMode.SetIndents(AValue: TcxGridWinExplorerViewOptionsContentDisplayModeIndents);
begin
  inherited Indents := AValue;
end;

{ TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode }

function TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode.GetImageHeightDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageExtraLargeSize;
end;

function TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode.GetImageWidthDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageExtraLargeSize;
end;

function TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode.GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass;
begin
  Result := TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents;
end;

function TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode.GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode;
begin
  Result := dmExtraLargeImages;
end;

function TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode.GetIndents: TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents;
begin
  Result := TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents(inherited Indents);
end;

procedure TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode.SetIndents(AValue: TcxGridWinExplorerViewOptionsExtraLargeDisplayModeIndents);
begin
  inherited Indents := AValue;
end;

{ TcxGridWinExplorerViewOptionsLargeImagesDisplayMode }

function TcxGridWinExplorerViewOptionsLargeImagesDisplayMode.GetImageHeightDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageLargeSize;
end;

function TcxGridWinExplorerViewOptionsLargeImagesDisplayMode.GetImageWidthDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageLargeSize;
end;

function TcxGridWinExplorerViewOptionsLargeImagesDisplayMode.GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass;
begin
  Result := TcxGridWinExplorerViewOptionsLargeDisplayModeIndents;
end;

function TcxGridWinExplorerViewOptionsLargeImagesDisplayMode.GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode;
begin
  Result := dmLargeImages;
end;

function TcxGridWinExplorerViewOptionsLargeImagesDisplayMode.GetIndents: TcxGridWinExplorerViewOptionsLargeDisplayModeIndents;
begin
  Result := TcxGridWinExplorerViewOptionsLargeDisplayModeIndents(inherited Indents);
end;

procedure TcxGridWinExplorerViewOptionsLargeImagesDisplayMode.SetIndents(AValue: TcxGridWinExplorerViewOptionsLargeDisplayModeIndents);
begin
  inherited Indents := AValue;
end;

{ TcxGridWinExplorerViewOptionsMediumDisplayMode }

function TcxGridWinExplorerViewOptionsMediumDisplayMode.GetImageHeightDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageMediumSize;
end;

function TcxGridWinExplorerViewOptionsMediumDisplayMode.GetImageWidthDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageMediumSize;
end;

function TcxGridWinExplorerViewOptionsMediumDisplayMode.GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass;
begin
  Result := TcxGridWinExplorerViewOptionsMediumDisplayModeIndents;
end;

function TcxGridWinExplorerViewOptionsMediumDisplayMode.GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode;
begin
  Result := dmMediumImages;
end;

function TcxGridWinExplorerViewOptionsMediumDisplayMode.GetIndents: TcxGridWinExplorerViewOptionsMediumDisplayModeIndents;
begin
  Result := TcxGridWinExplorerViewOptionsMediumDisplayModeIndents(inherited Indents);
end;

procedure TcxGridWinExplorerViewOptionsMediumDisplayMode.SetIndents(AValue: TcxGridWinExplorerViewOptionsMediumDisplayModeIndents);
begin
  inherited Indents := AValue;
end;

{ TcxGridWinExplorerViewOptionsSmallDisplayMode }

function TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode.GetImageHeightDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageSmallSize;
end;

function TcxGridWinExplorerViewOptionsCustomSmallImagesDisplayMode.GetImageWidthDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageSmallSize;
end;

{ TcxGridWinExplorerViewOptionsSmallImagesDisplayMode }

function TcxGridWinExplorerViewOptionsSmallImagesDisplayMode.GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass;
begin
  Result := TcxGridWinExplorerViewOptionsSmallDisplayModeIndents;
end;

function TcxGridWinExplorerViewOptionsSmallImagesDisplayMode.GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode;
begin
  Result := dmSmallImages;
end;

function TcxGridWinExplorerViewOptionsSmallImagesDisplayMode.GetIndents: TcxGridWinExplorerViewOptionsSmallDisplayModeIndents;
begin
  Result := TcxGridWinExplorerViewOptionsSmallDisplayModeIndents(inherited Indents);
end;

procedure TcxGridWinExplorerViewOptionsSmallImagesDisplayMode.SetIndents(AValue: TcxGridWinExplorerViewOptionsSmallDisplayModeIndents);
begin
  inherited Indents := AValue;
end;

{ TcxGridWinExplorerViewOptionsListDisplayMode }

function TcxGridWinExplorerViewOptionsListDisplayMode.GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass;
begin
  Result := TcxGridWinExplorerViewOptionsListDisplayModeIndents;
end;

function TcxGridWinExplorerViewOptionsListDisplayMode.GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode;
begin
  Result := dmList;
end;

function TcxGridWinExplorerViewOptionsListDisplayMode.GetIndents: TcxGridWinExplorerViewOptionsListDisplayModeIndents;
begin
  Result := TcxGridWinExplorerViewOptionsListDisplayModeIndents(inherited Indents);
end;

procedure TcxGridWinExplorerViewOptionsListDisplayMode.SetIndents(AValue: TcxGridWinExplorerViewOptionsListDisplayModeIndents);
begin
  inherited Indents := AValue;
end;

{ TcxGridWinExplorerViewOptionsTilesDisplayMode }

function TcxGridWinExplorerViewOptionsTilesDisplayMode.GetImageHeightDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageMediumSize;
end;

function TcxGridWinExplorerViewOptionsTilesDisplayMode.GetImageWidthDefaultValue: Integer;
begin
  Result := cxGridWinExplorerViewImageMediumSize;
end;

function TcxGridWinExplorerViewOptionsTilesDisplayMode.GetIndentsClass: TcxGridWinExplorerViewOptionsDisplayModeIndentsClass;
begin
  Result := TcxGridWinExplorerViewOptionsTilesDisplayModeIndents;
end;

function TcxGridWinExplorerViewOptionsTilesDisplayMode.GetShowItemDescriptionsDefaultValue: Boolean;
begin
  Result := True;
end;

function TcxGridWinExplorerViewOptionsTilesDisplayMode.GetDisplayModeKind: TcxGridWinExplorerViewDisplayMode;
begin
  Result := dmTiles;
end;

function TcxGridWinExplorerViewOptionsTilesDisplayMode.GetIndents: TcxGridWinExplorerViewOptionsTilesDisplayModeIndents;
begin
  Result := TcxGridWinExplorerViewOptionsTilesDisplayModeIndents(inherited Indents);
end;

procedure TcxGridWinExplorerViewOptionsTilesDisplayMode.SetIndents(AValue: TcxGridWinExplorerViewOptionsTilesDisplayModeIndents);
begin
  inherited Indents := AValue;
end;

{ TcxGridWinExplorerViewOptionsDisplayModes }

constructor TcxGridWinExplorerViewOptionsDisplayModes.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FContent := TcxGridWinExplorerViewOptionsContentDisplayMode.Create(GridView);
  FExtraLargeImages := TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode.Create(GridView);
  FLargeImages := TcxGridWinExplorerViewOptionsLargeImagesDisplayMode.Create(GridView);
  FList := TcxGridWinExplorerViewOptionsListDisplayMode.Create(GridView);
  FMediumImages := TcxGridWinExplorerViewOptionsMediumDisplayMode.Create(GridView);
  FSmallImages := TcxGridWinExplorerViewOptionsSmallImagesDisplayMode.Create(GridView);
  FTiles := TcxGridWinExplorerViewOptionsTilesDisplayMode.Create(GridView);
end;

destructor TcxGridWinExplorerViewOptionsDisplayModes.Destroy;
begin
  FreeAndNil(FTiles);
  FreeAndNil(FSmallImages);
  FreeAndNil(FMediumImages);
  FreeAndNil(FList);
  FreeAndNil(FLargeImages);
  FreeAndNil(FExtraLargeImages);
  FreeAndNil(FContent);
  inherited Destroy;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.Assign(Source: TPersistent);
var
  ASource: TcxGridWinExplorerViewOptionsDisplayModes;
begin
  inherited Assign(Source);
  if Source is TcxGridWinExplorerViewOptionsDisplayModes then
  begin
    ASource := TcxGridWinExplorerViewOptionsDisplayModes(Source);
    Content := ASource.Content;
    ExtraLargeImages := ASource.ExtraLargeImages;
    LargeImages := ASource.LargeImages;
    List := ASource.List;
    MediumImages := ASource.MediumImages;
    SmallImages := ASource.SmallImages;
    Tiles := ASource.Tiles;
  end;
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.ChangeScale(M, D: Integer);
begin
  inherited;
  Content.ChangeScale(M, D);
  ExtraLargeImages.ChangeScale(M, D);
  LargeImages.ChangeScale(M, D);
  List.ChangeScale(M, D);
  MediumImages.ChangeScale(M, D);
  SmallImages.ChangeScale(M, D);
  Tiles.ChangeScale(M, D);
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.SetContent(AValue: TcxGridWinExplorerViewOptionsContentDisplayMode);
begin
  FContent.Assign(AValue);
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.SetExtraLargeImages(AValue: TcxGridWinExplorerViewOptionsExtraLargeImagesDisplayMode);
begin
  FExtraLargeImages.Assign(AValue);
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.SetLargeImages(AValue: TcxGridWinExplorerViewOptionsLargeImagesDisplayMode);
begin
  FLargeImages.Assign(AValue);
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.SetList(AValue: TcxGridWinExplorerViewOptionsListDisplayMode);
begin
  FList.Assign(AValue);
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.SetMediumImages(AValue: TcxGridWinExplorerViewOptionsMediumDisplayMode);
begin
  FMediumImages.Assign(AValue);
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.SetSmallImages(AValue: TcxGridWinExplorerViewOptionsSmallImagesDisplayMode);
begin
  FSmallImages.Assign(AValue);
end;

procedure TcxGridWinExplorerViewOptionsDisplayModes.SetTiles(AValue: TcxGridWinExplorerViewOptionsTilesDisplayMode);
begin
  FTiles.Assign(AValue);
end;

{ TcxGridWinExplorerViewOptionsView }

procedure TcxGridWinExplorerViewOptionsView.Assign(ASource: TPersistent);
var
  ASourceOptions: TcxGridWinExplorerViewOptionsView;
begin
  inherited Assign(ASource);
  if ASource is TcxGridWinExplorerViewOptionsView then
  begin
    ASourceOptions := TcxGridWinExplorerViewOptionsView(ASource);
    ShowItemCheckBoxes := ASourceOptions.ShowItemCheckBoxes;
    ShowExpandButtons := ASourceOptions.ShowExpandButtons;
  end;
end;

function TcxGridWinExplorerViewOptionsView.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

procedure TcxGridWinExplorerViewOptionsView.SetShowItemCheckBoxes(AValue: Boolean);
begin
  if AValue <> FShowItemCheckBoxes then
  begin
    FShowItemCheckBoxes := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridWinExplorerViewOptionsView.SetShowExpandButtons(AValue: Boolean);
begin
  if AValue <> FShowExpandButtons then
  begin
    FShowExpandButtons := AValue;
    Changed(vcSize);
  end;
end;

{ TcxGridWinExplorerViewStyles }

procedure TcxGridWinExplorerViewStyles.Assign(Source: TPersistent);
var
  ASource: TcxGridWinExplorerViewStyles;
begin
  inherited Assign(Source);
  if Source is TcxGridWinExplorerViewStyles then
  begin
    ASource := TcxGridWinExplorerViewStyles(Source);
    Group := ASource.Group;
    TextItem := ASource.TextItem;
    DescriptionItem := ASource.DescriptionItem;
    OnGetTextItemStyle := ASource.OnGetTextItemStyle;
    OnGetDescriptionItemStyle := ASource.OnGetDescriptionItemStyle;
    OnGetGroupStyle := ASource.OnGetGroupStyle;
  end;
end;

procedure TcxGridWinExplorerViewStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);
begin
  inherited GetDefaultViewParams(Index, AData, AParams);
  case Index of
    vsContent, vsWinExplorerGroup, vsWinExplorerRecordHotTrack, vsSelection, vsInactive:
      begin
        AParams.Color := LookAndFeelPainter.WinExplorerViewDefaultRecordColor(GetState(Index));
        if IsGroupParams(Index) then
          AParams.TextColor := LookAndFeelPainter.WinExplorerViewGroupTextColor(GetState(Index))
        else
          AParams.TextColor := LookAndFeelPainter.WinExplorerViewRecordTextColor(GetState(Index));
      end;
    vsWinExplorerTextItem, vsWinExplorerDescriptionItem:
      GetDefaultTextItemParams(AData, AParams);
  end;
end;

procedure TcxGridWinExplorerViewStyles.GetDefaultTextItemParams(AData: TObject; out AParams: TcxViewParams);
var
  ACell: TcxGridWinExplorerViewCustomCellViewInfo;
  ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem;
begin
  if AData <> nil then
  begin
    ARecord := TcxGridDataCellPos(AData).GridRecord;
    AItem := TcxGridDataCellPos(AData).Item;
    ACell := TcxGridWinExplorerViewRecordViewInfo(ARecord.ViewInfo).FindCell(AItem);
  end
  else
  begin
    ACell := nil;
    ARecord := nil;
    AItem := nil;
  end;
  GetDataCellParams(ARecord, AItem, AParams, ACell <> nil, ACell);
end;

procedure TcxGridWinExplorerViewStyles.GetTextItemParams(ARecord: TcxCustomGridRecord;
  AItem: TcxGridWinExplorerViewItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetTextItemStyle) then
    FOnGetTextItemStyle(GridView, ARecord, AItem, AStyle);
  ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
  try
    GetViewParams(vsWinExplorerTextItem, ADataCellPos, nil, AParams);
  finally
    ADataCellPos.Free;
  end;
end;

procedure TcxGridWinExplorerViewStyles.GetDescriptionItemParams(ARecord: TcxCustomGridRecord;
  AItem: TcxGridWinExplorerViewItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetDescriptionItemStyle) then
    FOnGetDescriptionItemStyle(GridView, ARecord, AItem, AStyle);
  ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
  try
    GetViewParams(vsWinExplorerDescriptionItem, ADataCellPos, nil, AParams);
  finally
    ADataCellPos.Free;
  end;
end;

procedure TcxGridWinExplorerViewStyles.GetDataCellParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
  out AParams: TcxViewParams; AUseViewInfo: Boolean = False; ACellViewInfo: TcxGridTableCellViewInfo = nil;
  AIgnoreSelection: Boolean = False);
var
  AWinRecord: TcxGridWinExplorerViewCustomRecord absolute ARecord;
begin
  if not AIgnoreSelection and (AWinRecord <> nil) and (AWinRecord.ViewInfo.GetPaintState = cxbsHot) then
    GetViewParams(vsWinExplorerRecordHotTrack, ARecord, nil, AParams)
  else
    inherited GetDataCellParams(ARecord, AItem, AParams, AUseViewInfo, ACellViewInfo, AIgnoreSelection);
end;

procedure TcxGridWinExplorerViewStyles.GetTextItemCellParams(ARecord: TcxCustomGridRecord;
  AItem: TcxGridWinExplorerViewItem; out AParams: TcxViewParams);
begin
  if AItem = nil then
    GetTextItemParams(ARecord, AItem, AParams)
  else
    AItem.Styles.GetTextItemParams(ARecord, AParams);
end;

procedure TcxGridWinExplorerViewStyles.GetDescriptionItemCellParams(ARecord: TcxCustomGridRecord;
  AItem: TcxGridWinExplorerViewItem; out AParams: TcxViewParams);
begin
  if AItem = nil then
    GetDescriptionItemParams(ARecord, AItem, AParams)
  else
    AItem.Styles.GetDescriptionItemParams(ARecord, AParams);
end;

procedure TcxGridWinExplorerViewStyles.GetGroupParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetGroupStyle) then
    FOnGetGroupStyle(GridView, ARecord, AStyle);
  FIsGroupParams := True;
  try
    if TcxGridWinExplorerViewGroupRecord(ARecord).ViewInfo.GetPaintState <> cxbsNormal then
      GetDataCellParams(ARecord, AItem, AParams)
    else
      GetViewParams(vsWinExplorerGroup, ARecord, nil, AParams);
  finally
    FIsGroupParams := False;
  end;
end;

function TcxGridWinExplorerViewStyles.GetState(AIndex: Integer): TcxButtonState;
begin
  case AIndex of
    vsContent, vsWinExplorerGroup:
      Result := cxbsNormal;
    vsWinExplorerRecordHotTrack:
      Result := cxbsHot;
    vsSelection:
      Result := cxbsPressed;
    else
      Result := cxbsDisabled;
  end
end;

function TcxGridWinExplorerViewStyles.IsGroupParams(AIndex: Integer): Boolean;
begin
  Result := FIsGroupParams or (AIndex = vsWinExplorerGroup);
end;

procedure TcxGridWinExplorerViewStyles.SetOnGetTextItemStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetTextItemStyle, Value) then
  begin
    FOnGetTextItemStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridWinExplorerViewStyles.SetOnGetDescriptionItemStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetDescriptionItemStyle, Value) then
  begin
    FOnGetDescriptionItemStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridWinExplorerViewStyles.SetOnGetGroupStyle(Value: TcxGridGetRecordStyleEvent);
begin
  if not dxSameMethods(FOnGetGroupStyle, Value) then
  begin
    FOnGetGroupStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

{ TcxGridWinExplorerViewImageSet }

constructor TcxGridWinExplorerViewImageSet.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FExtraLargeImagesChangeLink := TChangeLink.Create;
  FLargeImagesChangeLink := TChangeLink.Create;
  FMediumImagesChangeLink := TChangeLink.Create;
  FSmallImagesChangeLink := TChangeLink.Create;

  FExtraLargeImagesChangeLink.OnChange := GridView.ImageListChange;
  FLargeImagesChangeLink.OnChange := GridView.ImageListChange;
  FMediumImagesChangeLink.OnChange := GridView.ImageListChange;
  FSmallImagesChangeLink.OnChange := GridView.ImageListChange;
end;

destructor TcxGridWinExplorerViewImageSet.Destroy;
begin
  FreeAndNil(FSmallImagesChangeLink);
  FreeAndNil(FMediumImagesChangeLink);
  FreeAndNil(FLargeImagesChangeLink);
  FreeAndNil(FExtraLargeImagesChangeLink);
  inherited Destroy;
end;

procedure TcxGridWinExplorerViewImageSet.Assign(Source: TPersistent);
var
  ASource: TcxGridWinExplorerViewImageSet;
begin
  inherited Assign(Source);
  if Source is TcxGridWinExplorerViewImageSet then
  begin
    ASource := TcxGridWinExplorerViewImageSet(Source);
    ExtraLargeImages := ASource.ExtraLargeImages;
    LargeImages := ASource.LargeImages;
    MediumImages := ASource.MediumImages;
    SmallImages := ASource.SmallImages;
  end;
end;

function TcxGridWinExplorerViewImageSet.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

procedure TcxGridWinExplorerViewImageSet.SetExtraLargeImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FExtraLargeImages, FExtraLargeImagesChangeLink, GridView);
end;

procedure TcxGridWinExplorerViewImageSet.SetLargeImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FLargeImages, FLargeImagesChangeLink, GridView);
end;

procedure TcxGridWinExplorerViewImageSet.SetMediumImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FMediumImages, FMediumImagesChangeLink, GridView);
end;

procedure TcxGridWinExplorerViewImageSet.SetSmallImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FSmallImages, FSmallImagesChangeLink, GridView);
end;

{ TcxGridWinExplorerView }

function TcxGridWinExplorerView.HasLayoutCustomizationForm: Boolean;
begin
  Result := False;
end;

function TcxGridWinExplorerView.IsLayoutChangeable: Boolean;
begin
  Result := False;
end;

procedure TcxGridWinExplorerView.CreateOptions;
begin
  inherited CreateOptions;
  FActiveDisplayMode := dmContent;
  FItemSet := TcxGridWinExplorerViewItemSet.Create(Self);
  FImageSet := TcxGridWinExplorerViewImageSet.Create(Self);
  FDisplayModes := TcxGridWinExplorerViewOptionsDisplayModes.Create(Self);
end;

procedure TcxGridWinExplorerView.DestroyOptions;
begin
  FreeAndNil(FDisplayModes);
  FreeAndNil(FImageSet);
  FreeAndNil(FItemSet);
  inherited DestroyOptions;
end;

procedure TcxGridWinExplorerView.DoAssign(ASource: TcxCustomGridView);
var
  AView: TcxGridWinExplorerView;
begin
  inherited DoAssign(ASource);
  if ASource is TcxGridWinExplorerView then
  begin
    AView := TcxGridWinExplorerView(ASource);
    ActiveDisplayMode := AView.ActiveDisplayMode;
    ImageSet := AView.ImageSet;
    if not AssigningSettings then
      ItemSet := AView.ItemSet;
    DisplayModes := AView.DisplayModes;
  end;
end;

function TcxGridWinExplorerView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxGridWinExplorerViewController;
end;

function TcxGridWinExplorerView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDataController;
end;

function TcxGridWinExplorerView.GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass;
begin
  Result := TcxGridWinExplorerViewDateTimeHandling;
end;

function TcxGridWinExplorerView.GetFilteringClass: TcxCustomGridTableFilteringClass;
begin
  Result := TcxGridWinExplorerViewFiltering;
end;

function TcxGridWinExplorerView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridWinExplorerViewItem;
end;

function TcxGridWinExplorerView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxGridWinExplorerViewOptionsBehavior;
end;

function TcxGridWinExplorerView.GetOptionsDataClass: TcxCustomGridOptionsDataClass;
begin
  Result := TcxGridWinExplorerViewOptionsData;
end;

function TcxGridWinExplorerView.GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass;
begin
  Result := TcxGridWinExplorerViewOptionsSelection;
end;

function TcxGridWinExplorerView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxGridWinExplorerViewOptionsView;
end;

function TcxGridWinExplorerView.GetPainterClass: TcxCustomGridPainterClass;
begin
  Result := TcxGridWinExplorerViewPainter;
end;

function TcxGridWinExplorerView.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxGridWinExplorerViewStyles;
end;

function TcxGridWinExplorerView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridWinExplorerViewViewData;
end;

function TcxGridWinExplorerView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridWinExplorerViewViewInfo;
end;

function TcxGridWinExplorerView.CanOffset(ARecordCountDelta: Integer; APixelScrollRecordOffsetDelta: Integer): Boolean;
begin
  Result := inherited CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta) and
    not (ViewData.IsGrouped and (ActiveDisplayMode = dmList))
end;

function TcxGridWinExplorerView.IsRecordPixelScrolling: Boolean;
begin
  Result := ActiveDisplayMode <> dmList;
end;

procedure TcxGridWinExplorerView.Offset(ARecordCountDelta: Integer;
  APixelScrollRecordOffsetDelta: Integer; DX: Integer; DY: Integer);
begin
  if IsUpdateLocked then
    LayoutChanged
  else
    inherited Offset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY);
end;

procedure TcxGridWinExplorerView.RemoveItem(AItem: TcxCustomGridTableItem);
begin
  ItemSet.GridItemRemove(AItem);
  inherited RemoveItem(AItem);
end;

procedure TcxGridWinExplorerView.ChangeGroupIndex(AValue: Integer);
begin
  if ItemSet.GroupItem <> nil then
  begin
    ItemSet.GroupItem.ChangeGroupIndex(AValue);
    if AValue <> -1 then
      ViewData.Expand(True);
  end;
end;

procedure TcxGridWinExplorerView.ChangeScale(M, D: Integer);
begin
  inherited;
  DisplayModes.ChangeScale(M, D);
end;

procedure TcxGridWinExplorerView.Grouping;
begin
  ChangeGroupIndex(0);
end;

procedure TcxGridWinExplorerView.ResetGrouping;
begin
  ChangeGroupIndex(-1);
end;

procedure TcxGridWinExplorerView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (ImageSet <> nil) then
    if AComponent = ImageSet.SmallImages then
      ImageSet.SmallImages := nil
    else if AComponent = ImageSet.MediumImages then
      ImageSet.MediumImages := nil
    else if AComponent = ImageSet.LargeImages then
      ImageSet.LargeImages := nil
    else if AComponent = ImageSet.ExtraLargeImages then
      ImageSet.ExtraLargeImages := nil;
end;

function TcxGridWinExplorerView.GroupsAlwaysExpanded: Boolean;
begin
  Result := ActiveDisplayMode = dmList;
end;

procedure TcxGridWinExplorerView.SelectionChanged(AInfo: TcxSelectionChangedInfo);

  procedure SelectChildRecords(AParentRecord: TcxCustomGridRecord);
  var
    I, AStartIndex: Integer;
  begin
    AStartIndex := AParentRecord.Index + 1;
    for I := 0 to ViewData.GetRecordChildCount(AParentRecord) - 1 do
      ViewData.Records[AStartIndex + I].Selected := True;
  end;

var
  I, ARecordIndex: Integer;
  ARecord: TcxCustomGridRecord;
begin
  inherited SelectionChanged(AInfo);
  if ViewData.IsGrouped and Controller.CanProcessMultiSelect(False) then
  begin
    BeginUpdate;
    try
      for I := 0 to DataController.GetSelectedCount - 1 do
      begin
        ARecordIndex := DataController.GetSelectedRowIndex(I);
        ARecord := ViewData.Records[ARecordIndex];
        if not ARecord.IsData and ARecord.Focused and ARecord.Selected and ARecord.Expanded then
          SelectChildRecords(ARecord);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxGridWinExplorerView.GetController: TcxGridWinExplorerViewController;
begin
  Result := TcxGridWinExplorerViewController(inherited Controller);
end;

function TcxGridWinExplorerView.GetDataController: TcxGridDataController;
begin
  Result := TcxGridDataController(FDataController);
end;

function TcxGridWinExplorerView.GetDateTimeHandling: TcxGridWinExplorerViewDateTimeHandling;
begin
  Result := TcxGridWinExplorerViewDateTimeHandling(inherited DateTimeHandling);
end;

function TcxGridWinExplorerView.GetFiltering: TcxGridWinExplorerViewFiltering;
begin
  Result := TcxGridWinExplorerViewFiltering(inherited Filtering);
end;

function TcxGridWinExplorerView.GetItemSet: TcxGridWinExplorerViewItemSet;
begin
  Result := FItemSet;
end;

function TcxGridWinExplorerView.GetOptionsBehavior: TcxGridWinExplorerViewOptionsBehavior;
begin
  Result := TcxGridWinExplorerViewOptionsBehavior(inherited OptionsBehavior);
end;

function TcxGridWinExplorerView.GetOptionsData: TcxGridWinExplorerViewOptionsData;
begin
  Result := TcxGridWinExplorerViewOptionsData(inherited OptionsData);
end;

function TcxGridWinExplorerView.GetImageSet: TcxGridWinExplorerViewImageSet;
begin
  Result := FImageSet;
end;

function TcxGridWinExplorerView.GetOptionsSelection: TcxGridWinExplorerViewOptionsSelection;
begin
  Result := TcxGridWinExplorerViewOptionsSelection(inherited OptionsSelection);
end;

function TcxGridWinExplorerView.GetViewInfo: TcxGridWinExplorerViewViewInfo;
begin
  Result := TcxGridWinExplorerViewViewInfo(inherited ViewInfo);
end;

procedure TcxGridWinExplorerView.SetActiveDisplayMode(AValue: TcxGridWinExplorerViewDisplayMode);
begin
  if FActiveDisplayMode <> AValue then
  begin
    FActiveDisplayMode := AValue;
    Controller.DisplayModeChanged;
  end;
end;

function TcxGridWinExplorerView.GetOptionsView: TcxGridWinExplorerViewOptionsView;
begin
  Result := TcxGridWinExplorerViewOptionsView(inherited OptionsView);
end;

function TcxGridWinExplorerView.GetStyles: TcxGridWinExplorerViewStyles;
begin
  Result := TcxGridWinExplorerViewStyles(inherited Styles);
end;

function TcxGridWinExplorerView.GetViewData: TcxGridWinExplorerViewViewData;
begin
  Result := TcxGridWinExplorerViewViewData(inherited ViewData);
end;

procedure TcxGridWinExplorerView.SetDataController(AValue: TcxGridDataController);
begin
  FDataController.Assign(AValue);
end;

procedure TcxGridWinExplorerView.SetDateTimeHandling(AValue: TcxGridWinExplorerViewDateTimeHandling);
begin
  inherited DateTimeHandling := AValue;
end;

procedure TcxGridWinExplorerView.SetFiltering(AValue: TcxGridWinExplorerViewFiltering);
begin
  inherited Filtering := AValue;
end;

procedure TcxGridWinExplorerView.SetItemSet(AValue: TcxGridWinExplorerViewItemSet);
begin
  FItemSet.Assign(AValue);
end;

procedure TcxGridWinExplorerView.SetOptionsBehavior(AValue: TcxGridWinExplorerViewOptionsBehavior);
begin
  inherited OptionsBehavior := AValue;
end;

procedure TcxGridWinExplorerView.SetOptionsData(AValue: TcxGridWinExplorerViewOptionsData);
begin
  inherited OptionsData := AValue;
end;

procedure TcxGridWinExplorerView.SetImageSet(AValue: TcxGridWinExplorerViewImageSet);
begin
  FImageSet.Assign(AValue);
end;

procedure TcxGridWinExplorerView.SetOptionsSelection(AValue: TcxGridWinExplorerViewOptionsSelection);
begin
  inherited OptionsSelection := AValue;
end;

procedure TcxGridWinExplorerView.SetOptionsView(AValue: TcxGridWinExplorerViewOptionsView);
begin
  inherited OptionsView := AValue;
end;

procedure TcxGridWinExplorerView.SetStyles(Value: TcxGridWinExplorerViewStyles);
begin
  inherited Styles := Value;
end;

procedure TcxGridWinExplorerView.SetDisplayModes(AValue: TcxGridWinExplorerViewOptionsDisplayModes);
begin
  FDisplayModes.Assign(AValue);
end;

initialization
  RegisterClasses([TcxGridWinExplorerViewItem]);
  cxGridRegisteredViews.Register(TcxGridWinExplorerView, 'WinExplorer');
  RegisterClasses([TcxGridWinExplorerView]);

finalization
  cxGridRegisteredViews.Unregister(TcxGridWinExplorerView);

end.
