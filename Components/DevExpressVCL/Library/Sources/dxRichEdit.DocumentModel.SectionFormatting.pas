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

unit dxRichEdit.DocumentModel.SectionFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Contnrs, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxGenerics;

type
  TdxPaperKind = TdxRichEditPaperKind;
  TdxPaperKindList = class(TdxOrdinalList<TdxPaperKind>);
  TdxSectionStartType = TdxRichEditSectionStartType;
  TdxLineNumberingRestart = TdxRichEditLineNumberingRestart;

  TdxSectionGutterAlignment = (Left = 0, Right = 1, Top = 2, Bottom = 3);

  { TdxPaperSizeCalculator }

  TdxPaperSizeCalculator = class
  strict private const
{$REGION 'PaperSizeTable'}
    PaperSizeTable: array[TdxPaperKind] of TSize = (
      (cx: 0; cy: 0),
      (cx: 12240; cy: 15840),
      (cx: 12240; cy: 15840),
      (cx: 15840; cy: 24480),
      (cx: 24480; cy: 15840),
      (cx: 12240; cy: 20160),
      (cx: 7920; cy: 12240),
      (cx: 10440; cy: 15120),
      (cx: 16839; cy: 23814),
      (cx: 11907; cy: 16839),
      (cx: 11907; cy: 16839),
      (cx: 8391; cy: 11907),
      (cx: 14572; cy: 20639),
      (cx: 10319; cy: 14571),
      (cx: 12240; cy: 18720),
      (cx: 12189; cy: 15591),
      (cx: 14400; cy: 20160),
      (cx: 15840; cy: 24480),
      (cx: 12240; cy: 15840),
      (cx: 5580; cy: 12780),
      (cx: 5940; cy: 13680),
      (cx: 6480; cy: 14940),
      (cx: 6840; cy: 15840),
      (cx: 7200; cy: 16560),
      (cx: 24480; cy: 31680),
      (cx: 31680; cy: 48960),
      (cx: 48960; cy: 63360),
      (cx: 6237; cy: 12474),
      (cx: 9185; cy: 12984),
      (cx: 18369; cy: 25965),
      (cx: 12983; cy: 18369),
      (cx: 6463; cy: 9184),
      (cx: 6463; cy: 12983),
      (cx: 14173; cy: 20013),
      (cx: 9978; cy: 14173),
      (cx: 9978; cy: 7087),
      (cx: 6236; cy: 13039),
      (cx: 5580; cy: 10800),
      (cx: 5220; cy: 9360),
      (cx: 21420; cy: 15840),
      (cx: 12240; cy: 17280),
      (cx: 12240; cy: 18720),
      (cx: 14173; cy: 20013),
      (cx: 5669; cy: 8391),
      (cx: 12960; cy: 15840),
      (cx: 14400; cy: 15840),
      (cx: 21600; cy: 15840),
      (cx: 12472; cy: 12472),
      (cx: -1; cy: -1),
      (cx: -1; cy: -1),
      (cx: 13680; cy: 17280),
      (cx: 13680; cy: 21600),
      (cx: 16834; cy: 25920),
      (cx: 13349; cy: 18274),
      (cx: 12240; cy: 15840),
      (cx: 11907; cy: 16839),
      (cx: 13680; cy: 17280),
      (cx: 12869; cy: 20183),
      (cx: 17291; cy: 27609),
      (cx: 12240; cy: 18274),
      (cx: 11907; cy: 18709),
      (cx: 8391; cy: 11907),
      (cx: 10319; cy: 14571),
      (cx: 18255; cy: 25228),
      (cx: 9865; cy: 13323),
      (cx: 11395; cy: 15647),
      (cx: 23811; cy: 33676),
      (cx: 16839; cy: 23814),
      (cx: 18255; cy: 25228),
      (cx: 11339; cy: 8391),
      (cx: 5953; cy: 8391),
      (cx: 12240; cy: 15840),
      (cx: 12240; cy: 15840),
      (cx: 12240; cy: 15840),
      (cx: 12240; cy: 15840),
      (cx: 15840; cy: 12240),
      (cx: 23814; cy: 16839),
      (cx: 16839; cy: 11907),
      (cx: 11907; cy: 8391),
      (cx: 20636; cy: 14570),
      (cx: 14570; cy: 10318),
      (cx: 8391; cy: 5669),
      (cx: 8391; cy: 11339),
      (cx: 8391; cy: 5953),
      (cx: 12240; cy: 15840),
      (cx: 12240; cy: 15840),
      (cx: 12240; cy: 15840),
      (cx: 12240; cy: 15840),
      (cx: 7257; cy: 10318),
      (cx: 10318; cy: 7257),
      (cx: 17280; cy: 15840),
      (cx: 12240; cy: 15840),
      (cx: 15840; cy: 12240),
      (cx: 8277; cy: 12189),
      (cx: 5499; cy: 8561),
      (cx: 5499; cy: 8561),
      (cx: 5783; cy: 9354),
      (cx: 5783; cy: 9978),
      (cx: 7087; cy: 9978),
      (cx: 6236; cy: 11792),
      (cx: 6236; cy: 12472),
      (cx: 6803; cy: 13039),
      (cx: 9071; cy: 13039),
      (cx: 6803; cy: 17518),
      (cx: 12983; cy: 18369),
      (cx: 18369; cy: 25965),
      (cx: 12189; cy: 8277),
      (cx: 8561; cy: 5499),
      (cx: 8561; cy: 5499),
      (cx: 9354; cy: 5783),
      (cx: 9978; cy: 5783),
      (cx: 9978; cy: 7087),
      (cx: 11792; cy: 6236),
      (cx: 12472; cy: 6236),
      (cx: 13039; cy: 6803),
      (cx: 13039; cy: 9071),
      (cx: 17518; cy: 6803),
      (cx: 18369; cy: 12983),
      (cx: 25965; cy: 18369)
    );
{$ENDREGION}
  strict protected
    class function TryGetValue(AKind: TdxPaperKind; out ASize: TSize): Boolean; static;
  public
    class function CalculatePaperKind(const ASize: TSize; const ADefaultValue: TdxPaperKind): TdxPaperKind; overload;
    class function CalculatePaperKind(const ASize: TSize; const ADefaultValue: TdxPaperKind;
      ATolerance: Integer; const ABadSizeDefaultValue: TdxPaperKind): TdxPaperKind; overload;
    class function CalculatePaperSize(const APaperKind: TdxPaperKind): TSize;
  end;

  { TdxMarginsInfo }

  TdxMarginsInfo = class(TdxCloneable)
  private
    FBottom: Integer;
    FFooterOffset: Integer;
    FGutter: Integer;
    FGutterAlignment: TdxSectionGutterAlignment;
    FHeaderOffset: Integer;
    FLeft: Integer;
    FRight: Integer;
    FTop: Integer;
  public
    function Clone: TdxMarginsInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    property Bottom: Integer read FBottom write FBottom;
    property FooterOffset: Integer read FFooterOffset write FFooterOffset;
    property Gutter: Integer read FGutter write FGutter;
    property GutterAlignment: TdxSectionGutterAlignment read FGutterAlignment write FGutterAlignment;
    property HeaderOffset: Integer read FHeaderOffset write FHeaderOffset;
    property Left: Integer read FLeft write FLeft;
    property Right: Integer read FRight write FRight;
    property Top: Integer read FTop write FTop;
  end;

  { TdxMarginsInfoCache }

  TdxMarginsInfoCache = class(TdxUniqueItemsCache<TdxMarginsInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxMarginsInfo; override;
  end;

  { TdxPageInfo }

  TdxPageInfo = class(TdxCloneable)
  private
    FHeight: Integer;
    FLandscape: Boolean;
    FPaperKind: TdxPaperKind;
    FWidth: Integer;
  public
    function Clone: TdxPageInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    procedure ValidatePaperKind(AUnitConverter: TdxDocumentModelUnitConverter);

    property Height: Integer read FHeight write FHeight;
    property Landscape: Boolean read FLandscape write FLandscape;
    property PaperKind: TdxPaperKind read FPaperKind write FPaperKind;
    property Width: Integer read FWidth write FWidth;
  end;
  TdxPageInfoClass = class of TdxPageInfo;

  { TdxPageInfoCache }

  TdxPageInfoCache = class(TdxUniqueItemsCache<TdxPageInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxPageInfo; override;
  end;

  { TdxGeneralSectionInfo }

  TdxGeneralSectionInfo = class(TdxCloneable)
  private
    FDifferentFirstPage: Boolean;
    FFirstPagePaperSource: Integer;
    FOnlyAllowEditingOfFormFields: Boolean;
    FOtherPagePaperSource: Integer;
    FStartType: TdxSectionStartType;
    FTextDirection: TdxTextDirection;
    FVerticalTextAlignment: TdxVerticalAlignment;
  public
    constructor Create; override;
    function Clone: TdxGeneralSectionInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    property DifferentFirstPage: Boolean read FDifferentFirstPage write FDifferentFirstPage;
    property FirstPagePaperSource: Integer read FFirstPagePaperSource write FFirstPagePaperSource;
    property OnlyAllowEditingOfFormFields: Boolean read FOnlyAllowEditingOfFormFields write FOnlyAllowEditingOfFormFields;
    property OtherPagePaperSource: Integer read FOtherPagePaperSource write FOtherPagePaperSource;
    property StartType: TdxSectionStartType read FStartType write FStartType;
    property TextDirection: TdxTextDirection read FTextDirection write FTextDirection;
    property VerticalTextAlignment: TdxVerticalAlignment read FVerticalTextAlignment write FVerticalTextAlignment;
  end;
  TdxGeneralSectionInfoClass = class of TdxGeneralSectionInfo;

  { TdxGeneralSectionInfoCache }

  TdxGeneralSectionInfoCache = class(TdxUniqueItemsCache<TdxGeneralSectionInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxGeneralSectionInfo; override;
  end;

  { TdxPageNumberingInfo }

  TdxPageNumberingInfo = class(TdxCloneable)
  private
    FChapterSeparator: Char;
    FChapterHeaderStyle: Integer;
    FNumberingFormat: TdxRichEditNumberingFormat;
    FFirstPageNumber: Integer;
    FContinueNumbering: Boolean;
    procedure SetContinueNumbering(const Value: Boolean);
  public
    constructor Create; override;
    function Clone: TdxPageNumberingInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    property ChapterSeparator: Char read FChapterSeparator write FChapterSeparator;
    property ChapterHeaderStyle: Integer read FChapterHeaderStyle write FChapterHeaderStyle;
    property NumberingFormat: TdxRichEditNumberingFormat read FNumberingFormat write FNumberingFormat;
    property FirstPageNumber: Integer read FFirstPageNumber write FFirstPageNumber;
    property ContinueNumbering: Boolean read FContinueNumbering write SetContinueNumbering default True;
  end;

  { TdxPageNumberingInfoCache }

  TdxPageNumberingInfoCache = class(TdxUniqueItemsCache<TdxPageNumberingInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxPageNumberingInfo; override;
  end;

  { TdxSectionMarginsChangeActionsCalculator }

  TdxSectionMarginsChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxSectionMarginsChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxSectionMargins }

  TdxSectionMargins = class(TdxRichEditIndexBasedObject<TdxMarginsInfo>)
  private
    function GetLeft: Integer;
    function GetRight: Integer;
    function GetTop: Integer;
    function GetBottom: Integer;
    function GetGutter: Integer;
    function GetGutterAlignment: TdxSectionGutterAlignment;
    function GetHeaderOffset: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetBottom(const Value: Integer);
    procedure SetGutter(const Value: Integer);
    procedure SetGutterAlignment(const Value: TdxSectionGutterAlignment);
    procedure SetHeaderOffset(const Value: Integer);
    function GetFooterOffset: Integer;
    procedure SetFooterOffset(const Value: Integer);
  strict protected
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxMarginsInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;

    function SetLeftCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetRightCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetTopCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetBottomCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetGutterCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetGutterAlignmentCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHeaderOffsetCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetFooterOffsetCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
  public
    constructor Create(const ADocumentModel: TdxCustomDocumentModel); reintroduce;

    property Left: Integer read GetLeft write SetLeft;
    property Right: Integer read GetRight write SetRight;
    property Top: Integer read GetTop write SetTop;
    property Bottom: Integer read GetBottom write SetBottom;
    property Gutter: Integer read GetGutter write SetGutter;
    property GutterAlignment: TdxSectionGutterAlignment read GetGutterAlignment write SetGutterAlignment;
    property HeaderOffset: Integer read GetHeaderOffset write SetHeaderOffset;
    property FooterOffset: Integer read GetFooterOffset write SetFooterOffset;
  end;

  { TdxColumnInfo }

  TdxColumnInfo = class(TdxCloneable)
  strict private
    FWidth: Integer;
    FSpace: Integer;
  public
    function Clone: TdxColumnInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(Obj: TObject): Boolean; override;
    property Width: Integer read FWidth write FWidth;
    property Space: Integer read FSpace write FSpace;
  end;
  TdxColumnInfoCollection = class(TdxList<TdxColumnInfo>);

  { TdxColumnsInfo }

  TdxColumnsInfo = class(TdxCloneable)
  strict private
    FColumnCount: Integer;
    FDrawVerticalSeparator: Boolean;
    FEqualWidthColumns: Boolean;
    FColumns: TdxList<TdxColumnInfo>;
    FSpace: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Clone: TdxColumnsInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(Obj: TObject): Boolean; override;

    property DrawVerticalSeparator: Boolean read FDrawVerticalSeparator write FDrawVerticalSeparator;
    property EqualWidthColumns: Boolean read FEqualWidthColumns write FEqualWidthColumns;
    property Space: Integer read FSpace write FSpace;
    property ColumnCount: Integer read FColumnCount write FColumnCount;
    property Columns: TdxList<TdxColumnInfo> read FColumns;
  end;

  { TdxColumnsInfoCache }

  TdxColumnsInfoCache = class(TdxUniqueItemsCache<TdxColumnsInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxColumnsInfo; override;
  end;

  { TdxLineNumberingInfo }

  TdxLineNumberingInfo = class(TdxCloneable)
  private
    FDistance: Integer;
    FNumberingRestartType: TdxLineNumberingRestart;
    FStartingLineNumber: Integer;
    FStep: Integer;
  public
    function Clone: TdxLineNumberingInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    property Distance: Integer read FDistance write FDistance;
    property NumberingRestartType: TdxLineNumberingRestart read FNumberingRestartType write FNumberingRestartType;
    property StartingLineNumber: Integer read FStartingLineNumber write FStartingLineNumber;
    property Step: Integer read FStep write FStep;
  end;

  { TdxLineNumberingInfoCache }

  TdxLineNumberingInfoCache = class(TdxUniqueItemsCache<TdxLineNumberingInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxLineNumberingInfo; override;
  end;

  { TdxSectionLineNumbering }

  TdxSectionLineNumbering = class(TdxRichEditIndexBasedObject<TdxLineNumberingInfo>)
  strict protected
    function SetDistanceCore(const AInfo: TdxLineNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetStartingLineNumberCore(const AInfo: TdxLineNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetStepCore(const AInfo: TdxLineNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetNumberingRestartTypeCore(const AInfo: TdxLineNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxLineNumberingInfo>; override;
    function GetDistance: Integer;
    procedure SetDistance(const AValue: Integer);
    function GetStartingLineNumber: Integer;
    procedure SetStartingLineNumber(const AValue: Integer);
    function GetStep: Integer;
    procedure SetStep(const AValue: Integer);
    function GetNumberingRestartType: TdxLineNumberingRestart;
    procedure SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;

    property Distance: Integer read GetDistance write SetDistance;
    property StartingLineNumber: Integer read GetStartingLineNumber write SetStartingLineNumber;
    property Step: Integer read GetStep write SetStep;
    property NumberingRestartType: TdxLineNumberingRestart read GetNumberingRestartType write SetNumberingRestartType;
  end;

  { TdxFootNoteInfo }

  TdxFootNoteInfo = class(TdxCloneable)
  strict private
    FPosition: TdxFootNotePosition;
    FNumberingFormat: TdxRichEditNumberingFormat;
    FNumberingRestartType: TdxLineNumberingRestart;
    FStartingNumber: Integer;
    FCustomMark: string;
  public
    function Clone: TdxFootNoteInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(AObj: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property Position: TdxFootNotePosition read FPosition write FPosition;
    property NumberingFormat: TdxRichEditNumberingFormat read FNumberingFormat write FNumberingFormat;
    property NumberingRestartType: TdxLineNumberingRestart read FNumberingRestartType write FNumberingRestartType;
    property StartingNumber: Integer read FStartingNumber write FStartingNumber;
    property CustomMark: string read FCustomMark write FCustomMark;
  end;

  { TdxFootNoteInfoCache }

  TdxFootNoteInfoCache = class(TdxUniqueItemsCache<TdxFootNoteInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxFootNoteInfo; override;
    procedure InitItems(const AUnitConverter: IdxDocumentModelUnitConverter); override;
  public
    class function DefaultFootNoteItemIndex: Integer;
    class function DefaultEndNoteItemIndex: Integer;
  end;

  { TdxSectionPage }

  TdxSectionPage = class(TdxRichEditIndexBasedObject<TdxPageInfo>)
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetLandscape: Boolean;
    function GetPaperKind: TdxPaperKind;
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetLandscape(const Value: Boolean);
    procedure SetPaperKind(const Value: TdxPaperKind);
  strict protected
    function SetWidthCore(const AInfo: TdxPageInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHeightCore(const AInfo: TdxPageInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetLandscapeCore(const AInfo: TdxPageInfo; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetPaperKindCore(const AInfo: TdxPageInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;

    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxPageInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(const ADocumentModel: TdxCustomDocumentModel); reintroduce;

    property Height: Integer read GetHeight write SetHeight;
    property Landscape: Boolean read GetLandscape write SetLandscape;
    property PaperKind: TdxPaperKind read GetPaperKind write SetPaperKind;
    property Width: Integer read GetWidth write SetWidth;
  end;

  { TdxSectionColumns }

  TdxSectionColumns = class(TdxRichEditIndexBasedObject<TdxColumnsInfo>)
  strict private
    function GetEqualWidthColumns: Boolean;
    procedure SetEqualWidthColumns(const Value: Boolean);
    function GetDrawVerticalSeparator: Boolean;
    procedure SetDrawVerticalSeparator(const Value: Boolean);
    function GetSpace: Integer;
    procedure SetSpace(const Value: Integer);
    function GetColumnCount: Integer;
    procedure SetColumnCount(const Value: Integer);
  strict protected
    function SetEqualWidthColumnsCore(const AInfo: TdxColumnsInfo; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetDrawVerticalSeparatorCore(const AInfo: TdxColumnsInfo; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetSpaceCore(const AInfo: TdxColumnsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetColumnCountCore(const AInfo: TdxColumnsInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxColumnsInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;

    property EqualWidthColumns: Boolean read GetEqualWidthColumns write SetEqualWidthColumns;
    property DrawVerticalSeparator: Boolean read GetDrawVerticalSeparator write SetDrawVerticalSeparator;
    property Space: Integer read GetSpace write SetSpace;
    property ColumnCount: Integer read GetColumnCount write SetColumnCount;
    function GetColumns: TdxColumnInfoCollection;
    procedure SetColumns(const AValue: TdxColumnInfoCollection);
  end;

  { TdxSectionGeneralSettings }

  TdxSectionGeneralSettings = class(TdxRichEditIndexBasedObject<TdxGeneralSectionInfo>)
  strict private
    function GetOnlyAllowEditingOfFormFields: Boolean;
    procedure SetOnlyAllowEditingOfFormFields(const AValue: Boolean);
    function GetDifferentFirstPage: Boolean;
    procedure SetDifferentFirstPage(const AValue: Boolean);
    function GetFirstPagePaperSource: Integer;
    procedure SetFirstPagePaperSource(const AValue: Integer);
    function GetOtherPagePaperSource: Integer;
    procedure SetOtherPagePaperSource(const AValue: Integer);
    function GetTextDirection: TdxTextDirection;
    procedure SetTextDirection(const AValue: TdxTextDirection);
    function GetVerticalTextAlignment: TdxVerticalAlignment;
    procedure SetVerticalTextAlignment(const AValue: TdxVerticalAlignment);
    function GetStartType: TdxSectionStartType;
    procedure SetStartType(const AValue: TdxSectionStartType);
  strict protected
    function SetOnlyAllowEditingOfFormFieldsCore(const AInfo: TdxGeneralSectionInfo; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetDifferentFirstPageCore(const AInfo: TdxGeneralSectionInfo; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetFirstPagePaperSourceCore(const AInfo: TdxGeneralSectionInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetOtherPagePaperSourceCore(const AInfo: TdxGeneralSectionInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetTextDirectionCore(const AInfo: TdxGeneralSectionInfo; const AValue: TdxTextDirection): TdxDocumentModelChangeActions; virtual;
    function SetVerticalTextAlignmentCore(const AInfo: TdxGeneralSectionInfo; const AValue: TdxVerticalAlignment): TdxDocumentModelChangeActions; virtual;
    function SetStartTypeCore(const AInfo: TdxGeneralSectionInfo; const AValue: TdxSectionStartType): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxGeneralSectionInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;

    property OnlyAllowEditingOfFormFields: Boolean read GetOnlyAllowEditingOfFormFields write SetOnlyAllowEditingOfFormFields;
    property DifferentFirstPage: Boolean read GetDifferentFirstPage write SetDifferentFirstPage;
    property FirstPagePaperSource: Integer read GetFirstPagePaperSource write SetFirstPagePaperSource;
    property OtherPagePaperSource: Integer read GetOtherPagePaperSource write SetOtherPagePaperSource;
    property TextDirection: TdxTextDirection read GetTextDirection write SetTextDirection;
    property VerticalTextAlignment: TdxVerticalAlignment read GetVerticalTextAlignment write SetVerticalTextAlignment;
    property StartType: TdxSectionStartType read GetStartType write SetStartType;
  end;

  { TdxSectionPageNumbering }

  TdxSectionPageNumbering = class(TdxRichEditIndexBasedObject<TdxPageNumberingInfo>)
  strict protected
    function SetChapterHeaderStyleCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetNumberingFormatCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetChapterSeparatorCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetFirstPageNumberCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetContinueNumberingCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxPageNumberingInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;

    function GetChapterSeparator: Char;
    function GetFirstPageNumber: Integer;
    function GetChapterHeaderStyle: Integer;
    function GetNumberingFormat: TdxRichEditNumberingFormat;
    function GetContinueNumbering: Boolean;
    procedure SetChapterSeparator(const AValue: Char);
    procedure SetFirstPageNumber(const AValue: Integer);
    procedure SetChapterHeaderStyle(const AValue: Integer);
    procedure SetNumberingFormat(const AValue: TdxRichEditNumberingFormat);
    procedure SetContinueNumbering(const AValue: Boolean);
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;

    property ChapterSeparator: Char read GetChapterSeparator write SetChapterSeparator;
    property ChapterHeaderStyle: Integer read GetChapterHeaderStyle write SetChapterHeaderStyle;
    property NumberingFormat: TdxRichEditNumberingFormat read GetNumberingFormat write SetNumberingFormat;
    property FirstPageNumber: Integer read GetFirstPageNumber write SetFirstPageNumber;
    property ContinueNumbering: Boolean read GetContinueNumbering write SetContinueNumbering;
  end;

  { TdxSectionFootNote }

  TdxSectionFootNote = class(TdxRichEditIndexBasedObject<TdxFootNoteInfo>)
  strict private
    function GetPosition: TdxFootNotePosition;
    procedure SetPosition(const AValue: TdxFootNotePosition);
    function GetNumberingFormat: TdxRichEditNumberingFormat;
    procedure SetNumberingFormat(const AValue: TdxRichEditNumberingFormat);
    function GetNumberingRestartType: TdxLineNumberingRestart;
    procedure SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
    function GetStartingNumber: Integer;
    procedure SetStartingNumber(const AValue: Integer);
    function GetCustomMark: string;
    procedure SetCustomMark(const AValue: string);
  strict protected
    function SetPositionCore(const AInfo: TdxFootNoteInfo; const AValue: TdxFootNotePosition): TdxDocumentModelChangeActions; virtual;
    function SetNumberingFormatCore(const AInfo: TdxFootNoteInfo; const AValue: TdxRichEditNumberingFormat): TdxDocumentModelChangeActions; virtual;
    function SetNumberingRestartTypeCore(const AInfo: TdxFootNoteInfo; const AValue: TdxLineNumberingRestart): TdxDocumentModelChangeActions; virtual;
    function SetStartingNumberCore(const AInfo: TdxFootNoteInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetCustomMarkCore(const AInfo: TdxFootNoteInfo; const AValue: string): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxFootNoteInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;
    function FormatCounterValue(AValue: Integer): string;

    property Position: TdxFootNotePosition read GetPosition write SetPosition;
    property NumberingFormat: TdxRichEditNumberingFormat read GetNumberingFormat write SetNumberingFormat;
    property NumberingRestartType: TdxLineNumberingRestart read GetNumberingRestartType write SetNumberingRestartType;
    property StartingNumber: Integer read GetStartingNumber write SetStartingNumber;
    property CustomMark: string read GetCustomMark write SetCustomMark;
  end;

implementation

uses
  RTLConsts,
  Math, cxGeometry, dxCore, cxClasses, dxHash, dxHashUtils,

  dxRichEdit.Strs,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Utils.Exceptions;

type
  { TdxSectionFootNoteChangeActionsCalculator }

  TdxSectionFootNoteChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxSectionFootNoteChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxSectionLineNumberingChangeActionsCalculator }

  TdxSectionLineNumberingChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxSectionLineNumberingChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxSectionPageChangeActionsCalculator }

  TdxSectionPageChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxSectionPageChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxSectionColumnsChangeActionsCalculator }

  TdxSectionColumnsChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxSectionColumnsChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxSectionGeneralSettingsChangeActionsCalculator }

  TdxSectionGeneralSettingsChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxSectionGeneralSettingsChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxSectionPageNumberingChangeActionsCalculator }

  TdxSectionPageNumberingChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxSectionPageNumberingChangeType): TdxDocumentModelChangeActions; static;
  end;

{ TdxSectionFootNoteChangeActionsCalculator }

class function TdxSectionFootNoteChangeActionsCalculator.CalculateChangeActions(AChange: TdxSectionFootNoteChangeType): TdxDocumentModelChangeActions;
const
  SectionFootNoteChangeActionsMap: array[TdxSectionFootNoteChangeType] of TdxDocumentModelChangeActions = (
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout]
  );
begin
  Result := SectionFootNoteChangeActionsMap[AChange];
end;

{ TdxSectionGeneralSettingsChangeActionsCalculator }

class function TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(AChange: TdxSectionGeneralSettingsChangeType): TdxDocumentModelChangeActions;
const
  SectionGeneralSettingsChangeActionsMap: array[TdxSectionGeneralSettingsChangeType] of TdxDocumentModelChangeActions = (
    [],
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [],
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout]
  );
begin
  Result := SectionGeneralSettingsChangeActionsMap[AChange];
end;

{ TdxSectionPageNumberingChangeActionsCalculator }

class function TdxSectionPageNumberingChangeActionsCalculator.CalculateChangeActions(AChange: TdxSectionPageNumberingChangeType): TdxDocumentModelChangeActions;
const
  SectionPageNumberingChangeActionsMap: array[TdxSectionPageNumberingChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout]
  );
begin
  Result := SectionPageNumberingChangeActionsMap[AChange];
end;

{ TdxSectionColumnsChangeActionsCalculator }

class function TdxSectionColumnsChangeActionsCalculator.CalculateChangeActions(AChange: TdxSectionColumnsChangeType): TdxDocumentModelChangeActions;
const
  SectionColumnsChangeActionsMap: array[TdxSectionColumnsChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler]
  );
begin
  Result := SectionColumnsChangeActionsMap[AChange];
end;

{ TdxSectionLineNumberingChangeActionsCalculator }

class function TdxSectionLineNumberingChangeActionsCalculator.CalculateChangeActions(AChange: TdxSectionLineNumberingChangeType): TdxDocumentModelChangeActions;
const
  SectionLineNumberingChangeActionsMap: array[TdxSectionLineNumberingChangeType] of TdxDocumentModelChangeActions = (
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout]
  );
begin
  Result := SectionLineNumberingChangeActionsMap[AChange];
end;

{ TdxSectionPageChangeActionsCalculator }

class function TdxSectionPageChangeActionsCalculator.CalculateChangeActions(AChange: TdxSectionPageChangeType): TdxDocumentModelChangeActions;
const
  SectionPageChangeActionsMap: array[TdxSectionPageChangeType] of TdxDocumentModelChangeActions = (
     [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler]
  );
begin
  Result := SectionPageChangeActionsMap[AChange];
end;

{ TdxPaperSizeCalculator }

class function TdxPaperSizeCalculator.CalculatePaperKind(
  const ASize: TSize; const ADefaultValue: TdxPaperKind): TdxPaperKind;
begin
  Result := CalculatePaperKind(ASize, ADefaultValue, 0, ADefaultValue);
end;

class function TdxPaperSizeCalculator.CalculatePaperKind(const ASize: TSize;
  const ADefaultValue: TdxPaperKind; ATolerance: Integer; const ABadSizeDefaultValue: TdxPaperKind): TdxPaperKind;
var
  ACalculator: TdxPaperSizeCalculator;
  AItemSize: TSize;
  I: TdxPaperKind;
begin
  ACalculator := TdxPaperSizeCalculator.Create;
  try
    if (ASize.cx = 0) or (ASize.cy = 0) then
      Result := ABadSizeDefaultValue
    else
    begin
      for I := TdxPaperKind.Letter to High(TdxPaperKind) do
      begin
        AItemSize := ACalculator.PaperSizeTable[I];
        if (Abs(ASize.cx - AItemSize.cx) <= ATolerance) and
            (Abs(ASize.cy - AItemSize.cy) <= ATolerance) then
          Exit(I);
      end;
      Result := ADefaultValue;
    end;
  finally
    ACalculator.Free;
  end;
end;

class function TdxPaperSizeCalculator.CalculatePaperSize(
  const APaperKind: TdxPaperKind): TSize;
begin
  if not TryGetValue(APaperKind, Result) then
    Result := cxSize(12240, 15840);
end;

class function TdxPaperSizeCalculator.TryGetValue(AKind: TdxPaperKind; out ASize: TSize): Boolean;
begin
  Result := not (AKind in [TdxPaperKind.Custom, TdxPaperKind.Reserved_48, TdxPaperKind.Reserved_49]);
  if Result then
    ASize := PaperSizeTable[AKind];
end;

{ TdxMarginsInfo }

function TdxMarginsInfo.Clone: TdxMarginsInfo;
begin
  Result := TdxMarginsInfo(inherited Clone);
end;

procedure TdxMarginsInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxMarginsInfo absolute Source;
begin
  FBottom := ASource.Bottom;
  FFooterOffset := ASource.FooterOffset;
  FGutter := ASource.Gutter;
  FGutterAlignment := ASource.GutterAlignment;
  FHeaderOffset := ASource.HeaderOffset;
  FLeft := ASource.Left;
  FRight := ASource.Right;
  FTop := ASource.Top;
end;

{ TdxMarginsInfoCache }

function TdxMarginsInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxMarginsInfo;
begin
  Result := TdxMarginsInfo.Create;
  Result.Left := AUnitConverter.HundredthsOfMillimeterToModelUnits(3000);
  Result.Top := AUnitConverter.HundredthsOfMillimeterToModelUnits(2000);
  Result.Bottom := AUnitConverter.HundredthsOfMillimeterToModelUnits(2000);
  Result.Right := AUnitConverter.HundredthsOfMillimeterToModelUnits(1500);
  Result.HeaderOffset := AUnitConverter.HundredthsOfMillimeterToModelUnits(1250);
  Result.FooterOffset := AUnitConverter.HundredthsOfMillimeterToModelUnits(1250);
end;

{ TdxPageInfo }

function TdxPageInfo.Clone: TdxPageInfo;
begin
  Result := TdxPageInfo(inherited Clone);
end;

procedure TdxPageInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxPageInfo absolute Source;
begin
  Height := ASource.Height;
  Landscape := ASource.Landscape;
  PaperKind := ASource.PaperKind;
  Width := ASource.Width;
end;

procedure TdxPageInfo.ValidatePaperKind(AUnitConverter: TdxDocumentModelUnitConverter);
const
  MSO2010_PageSizeTolerance = 240;
var
  APaperKindSize, ASize: TSize;
begin
  APaperKindSize := TdxPaperSizeCalculator.CalculatePaperSize(PaperKind);
  if Width <= 0 then
  begin
    if Landscape then
      Width := AUnitConverter.TwipsToModelUnits(APaperKindSize.cy)
    else
      Width := AUnitConverter.TwipsToModelUnits(APaperKindSize.cx);
  end;

  if Height <= 0 then
  begin
    if Landscape then
      Height := AUnitConverter.TwipsToModelUnits(APaperKindSize.cx)
    else
      Height := AUnitConverter.TwipsToModelUnits(APaperKindSize.cy);
  end;
  if Landscape then
    ASize := cxSize(Height, Width)
  else
    ASize := cxSize(Width, Height);
  ASize := AUnitConverter.ModelUnitsToTwips(ASize);

  PaperKind := TdxPaperSizeCalculator.CalculatePaperKind(ASize, TdxPaperKind.Custom, MSO2010_PageSizeTolerance, TdxPaperKind.Letter);
end;

{ TdxPageInfoCache }

function TdxPageInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxPageInfo;
begin
  Result := TdxPageInfo.Create;
  Result.PaperKind := TdxPaperKind.Letter;
  Result.Landscape := False;
  Result.Width := AUnitConverter.TwipsToModelUnits(12240);
  Result.Height := AUnitConverter.TwipsToModelUnits(15840);
end;

{ TdxGeneralSectionInfo }

constructor TdxGeneralSectionInfo.Create;
begin
  inherited Create;
  FStartType := TdxSectionStartType.NextPage;
end;

function TdxGeneralSectionInfo.Clone: TdxGeneralSectionInfo;
begin
  Result := TdxGeneralSectionInfo(inherited Clone);
end;

procedure TdxGeneralSectionInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxGeneralSectionInfo absolute Source;
begin
  DifferentFirstPage := ASource.DifferentFirstPage;
  FirstPagePaperSource := ASource.FirstPagePaperSource;
  OnlyAllowEditingOfFormFields := ASource.OnlyAllowEditingOfFormFields;
  OtherPagePaperSource := ASource.OtherPagePaperSource;
  StartType := ASource.StartType;
  TextDirection := ASource.TextDirection;
  VerticalTextAlignment := ASource.VerticalTextAlignment;
end;

{ TdxGeneralSectionInfoCache }

function TdxGeneralSectionInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxGeneralSectionInfo;
begin
  Result := TdxGeneralSectionInfo.Create;
end;

{ TdxPageNumberingInfo }

constructor TdxPageNumberingInfo.Create;
begin
  inherited Create;
  FContinueNumbering := True;
end;

function TdxPageNumberingInfo.Clone: TdxPageNumberingInfo;
begin
  Result := TdxPageNumberingInfo(inherited Clone);
end;

procedure TdxPageNumberingInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxPageNumberingInfo absolute Source;
begin
  FChapterSeparator := ASource.ChapterSeparator;
  FChapterHeaderStyle := ASource.ChapterHeaderStyle;
  FNumberingFormat :=  ASource.NumberingFormat;
  FFirstPageNumber := ASource.FFirstPageNumber;
  FContinueNumbering := ASource.FContinueNumbering;
end;

procedure TdxPageNumberingInfo.SetContinueNumbering(const Value: Boolean);
begin
  if FContinueNumbering <> Value then
  begin
    FContinueNumbering := Value;
    if not FContinueNumbering and (FFirstPageNumber = -1) then
      FFirstPageNumber := 1;
  end;
end;

{ TdxPageNumberingInfoCache }

function TdxPageNumberingInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxPageNumberingInfo;
begin
  Result := TdxPageNumberingInfo.Create;
  Result.FirstPageNumber := -1;
  Result.ContinueNumbering := True;
end;

{ TdxSectionMarginsChangeActionsCalculator }

class function TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(AChange: TdxSectionMarginsChangeType): TdxDocumentModelChangeActions;
const
  SectionMarginsChangeActionsMap: array[TdxSectionMarginsChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResize,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler]
  );
begin
  Result := SectionMarginsChangeActionsMap[AChange];
end;

{ TdxSectionMargins }

constructor TdxSectionMargins.Create(const ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

function TdxSectionMargins.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.BatchUpdate);
end;

function TdxSectionMargins.GetBottom: Integer;
begin
  Result := Info.Bottom;
end;

function TdxSectionMargins.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxMarginsInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).MarginsInfoCache;
end;

function TdxSectionMargins.GetFooterOffset: Integer;
begin
  Result := Info.FooterOffset;
end;

function TdxSectionMargins.GetGutter: Integer;
begin
  Result := Info.Gutter;
end;

function TdxSectionMargins.GetGutterAlignment: TdxSectionGutterAlignment;
begin
  Result := Info.GutterAlignment;
end;

function TdxSectionMargins.GetHeaderOffset: Integer;
begin
  Result := Info.HeaderOffset;
end;

function TdxSectionMargins.GetLeft: Integer;
begin
  Result := Info.Left;
end;

function TdxSectionMargins.GetRight: Integer;
begin
  Result := Info.Right;
end;

function TdxSectionMargins.GetTop: Integer;
begin
  Result := Info.Top;
end;

procedure TdxSectionMargins.SetBottom(const Value: Integer);
begin
  if Bottom <> Value then
    SetPropertyValue<Integer>(SetBottomCore, Value);
end;

function TdxSectionMargins.SetBottomCore(const AMargins: TdxMarginsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AMargins.Bottom := AValue;
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.Bottom);
end;

procedure TdxSectionMargins.SetFooterOffset(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('FooterOffset', Value);
  if FooterOffset <> Value then
    SetPropertyValue<Integer>(SetFooterOffsetCore, Value);
end;

function TdxSectionMargins.SetFooterOffsetCore(const AMargins: TdxMarginsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AMargins.FooterOffset := AValue;
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.FooterOffset);
end;

procedure TdxSectionMargins.SetGutter(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Gutter', Value);
  if Gutter <> Value then
    SetPropertyValue<Integer>(SetGutterCore, Value);
end;

procedure TdxSectionMargins.SetGutterAlignment(const Value: TdxSectionGutterAlignment);
begin
  if GutterAlignment <> Value then
    SetPropertyValue<Integer>(SetGutterAlignmentCore, Ord(Value));
end;

function TdxSectionMargins.SetGutterAlignmentCore(const AMargins: TdxMarginsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AMargins.GutterAlignment := TdxSectionGutterAlignment(AValue);
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.GutterAlignment);
end;

function TdxSectionMargins.SetGutterCore(const AMargins: TdxMarginsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AMargins.Gutter := AValue;
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.Gutter);
end;

procedure TdxSectionMargins.SetHeaderOffset(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('HeaderOffset', Value);
  if HeaderOffset <> Value then
    SetPropertyValue<Integer>(SetHeaderOffsetCore, Value);
end;

function TdxSectionMargins.SetHeaderOffsetCore(const AMargins: TdxMarginsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AMargins.HeaderOffset := AValue;
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.HeaderOffset);
end;

procedure TdxSectionMargins.SetLeft(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Left', Value);
  if Left <> Value then
    SetPropertyValue<Integer>(SetLeftCore, Value);
end;

function TdxSectionMargins.SetLeftCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AMargins.Left := AValue;
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.Left);
end;

procedure TdxSectionMargins.SetRight(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Right', Value);
  if Right <> Value then
    SetPropertyValue<Integer>(SetRightCore, Value);
end;

function TdxSectionMargins.SetRightCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AMargins.Right := AValue;
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.Right);
end;

procedure TdxSectionMargins.SetTop(const Value: Integer);
begin
  if Top <> Value then
    SetPropertyValue<Integer>(SetTopCore, Value);
end;

function TdxSectionMargins.SetTopCore(const AMargins: TdxMarginsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AMargins.Top := AValue;
  Result := TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.Top);
end;

{ TdxColumnInfo }

function TdxColumnInfo.Clone: TdxColumnInfo;
begin
  Result := TdxColumnInfo(inherited Clone);
end;

procedure TdxColumnInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxColumnInfo absolute Source;
begin
  Width := ASource.Width;
  Space := ASource.Space;
end;

function TdxColumnInfo.Equals(Obj: TObject): Boolean;
var
  AOther: TdxColumnInfo absolute Obj;
begin
  Result := (FWidth = AOther.Width) and (FSpace = AOther.Space);
end;

{ TdxColumnsInfo }

function TdxColumnsInfo.Clone: TdxColumnsInfo;
begin
  Result := TdxColumnsInfo(inherited Clone);
end;

procedure TdxColumnsInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxColumnsInfo absolute Source;
var
  I: Integer;
begin
  DrawVerticalSeparator := AInfo.DrawVerticalSeparator;
  EqualWidthColumns := AInfo.EqualWidthColumns;
  Space := AInfo.Space;
  ColumnCount := AInfo.ColumnCount;
  Columns.Clear;
  for I := 0 to AInfo.Columns.Count - 1 do
    Columns.Add(AInfo.Columns[I].Clone);
end;

function TdxColumnsInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxColumnsInfo;
  ACount, I: Integer;
begin
  AInfo := TdxColumnsInfo(Obj);
  if (AInfo.EqualWidthColumns <> EqualWidthColumns) or (AInfo.DrawVerticalSeparator <> DrawVerticalSeparator) or
    (AInfo.Space <> Space) or (AInfo.ColumnCount <> ColumnCount) then
    Exit(False);

  ACount := FColumns.Count;
  if ACount <> AInfo.Columns.Count then
    Exit(False);
  for I := 0 to ACount - 1 do
    if not Columns[I].Equals(AInfo.Columns[I]) then
      Exit(False);
  Result := True;
end;

constructor TdxColumnsInfo.Create;
begin
  inherited Create;
  FColumns := TdxObjectList<TdxColumnInfo>.Create;
end;

destructor TdxColumnsInfo.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

{ TdxColumnsInfoCache }

function TdxColumnsInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxColumnsInfo;
begin
  Result := TdxColumnsInfo.Create;
  Result.ColumnCount := 1;
  Result.EqualWidthColumns := True;
  Result.Space := AUnitConverter.HundredthsOfMillimeterToModelUnits(1250);
end;

{ TdxLineNumberingInfo }

function TdxLineNumberingInfo.Clone: TdxLineNumberingInfo;
begin
  Result := TdxLineNumberingInfo(inherited Clone);
end;

procedure TdxLineNumberingInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxLineNumberingInfo absolute Source;
begin
  FDistance := ASource.Distance;
  FNumberingRestartType := ASource.NumberingRestartType;
  FStartingLineNumber := ASource.StartingLineNumber;
  FStep := ASource.Step;
end;

{ TdxLineNumberingInfoCache }

function TdxLineNumberingInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxLineNumberingInfo;
begin
  Result := TdxLineNumberingInfo.Create;
  Result.Distance := 0;
  Result.StartingLineNumber := 1;
  Result.Step := 0;
end;

{ TdxSectionLineNumbering }

constructor TdxSectionLineNumbering.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

function TdxSectionLineNumbering.SetDistanceCore(const AInfo: TdxLineNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Distance := AValue;
  Result := TdxSectionLineNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionLineNumberingChangeType.Distance);
end;

function TdxSectionLineNumbering.SetStartingLineNumberCore(const AInfo: TdxLineNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.StartingLineNumber := AValue;
  Result := TdxSectionLineNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionLineNumberingChangeType.StartingLineNumber);
end;

function TdxSectionLineNumbering.SetStepCore(const AInfo: TdxLineNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Step := AValue;
  Result := TdxSectionLineNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionLineNumberingChangeType.Step);
end;

function TdxSectionLineNumbering.SetNumberingRestartTypeCore(const AInfo: TdxLineNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.NumberingRestartType := TdxLineNumberingRestart(AValue);
  Result := TdxSectionLineNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionLineNumberingChangeType.NumberingRestartType);
end;

function TdxSectionLineNumbering.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxLineNumberingInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).LineNumberingInfoCache;
end;

function TdxSectionLineNumbering.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxSectionLineNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionLineNumberingChangeType.BatchUpdate);
end;

function TdxSectionLineNumbering.GetDistance: Integer;
begin
  Result := Info.Distance;
end;

procedure TdxSectionLineNumbering.SetDistance(const AValue: Integer);
begin
  if AValue < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Distance', AValue);

  if Distance = AValue then
    Exit;
  SetPropertyValue<Integer>(SetDistanceCore, AValue);
end;

function TdxSectionLineNumbering.GetStartingLineNumber: Integer;
begin
  Result := Info.StartingLineNumber;
end;

procedure TdxSectionLineNumbering.SetStartingLineNumber(const AValue: Integer);
begin
  if AValue < 0 then
    TdxRichEditExceptions.ThrowArgumentException('StartingLineNumber', AValue);

  if StartingLineNumber = AValue then
    Exit;
  SetPropertyValue<Integer>(SetStartingLineNumberCore, AValue);
end;

function TdxSectionLineNumbering.GetStep: Integer;
begin
  Result := Info.Step;
end;

procedure TdxSectionLineNumbering.SetStep(const AValue: Integer);
begin
  if AValue < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Step', AValue);

  if Step = AValue then
    Exit;
  SetPropertyValue<Integer>(SetStepCore, AValue);
end;

function TdxSectionLineNumbering.GetNumberingRestartType: TdxLineNumberingRestart;
begin
  Result := Info.NumberingRestartType;
end;

procedure TdxSectionLineNumbering.SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
begin
  if NumberingRestartType = AValue then
    Exit;
  SetPropertyValue<Integer>(SetNumberingRestartTypeCore, Ord(AValue));
end;

{ TdxFootNoteInfo }

function TdxFootNoteInfo.Clone: TdxFootNoteInfo;
begin
  Result := TdxFootNoteInfo(inherited Clone);
end;

procedure TdxFootNoteInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxFootNoteInfo absolute Source;
begin
  Position := AInfo.Position;
  NumberingFormat := AInfo.NumberingFormat;
  NumberingRestartType := AInfo.NumberingRestartType;
  StartingNumber := AInfo.StartingNumber;
  CustomMark := AInfo.CustomMark;
end;

function TdxFootNoteInfo.Equals(AObj: TObject): Boolean;
var
  AInfo: TdxFootNoteInfo absolute AObj;
begin
  Result := (Position = AInfo.Position) and (NumberingFormat = AInfo.NumberingFormat) and
    (NumberingRestartType = AInfo.NumberingRestartType) and (StartingNumber = AInfo.StartingNumber) and
    (CustomMark = AInfo.CustomMark);
end;

function TdxFootNoteInfo.GetHashCode: Integer;
begin
  Result := Ord(Position) xor Ord(NumberingFormat) xor Ord(NumberingRestartType) xor StartingNumber;
  if FCustomMark <> '' then
    Result := Result xor dxElfHash(CustomMark);
end;

{ TdxFootNoteInfoCache }

class function TdxFootNoteInfoCache.DefaultFootNoteItemIndex: Integer;
begin
  Result := 0;
end;

class function TdxFootNoteInfoCache.DefaultEndNoteItemIndex: Integer;
begin
  Result := 1;
end;

function TdxFootNoteInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxFootNoteInfo;
begin
  Result := TdxFootNoteInfo.Create;
  Result.Position := TdxFootNotePosition.BottomOfPage;
  Result.NumberingFormat := TdxNumberingFormat.Decimal;
  Result.NumberingRestartType := TdxLineNumberingRestart.Continuous;
  Result.StartingNumber := 1;
  Result.CustomMark := '';
end;

procedure TdxFootNoteInfoCache.InitItems(const AUnitConverter: IdxDocumentModelUnitConverter);
var
  AInfo: TdxFootNoteInfo;
begin
  inherited InitItems(AUnitConverter);

  AInfo := TdxFootNoteInfo.Create;
  AInfo.Position := TdxFootNotePosition.EndOfDocument;
  AInfo.NumberingFormat := TdxNumberingFormat.LowerRoman;
  AInfo.NumberingRestartType := TdxLineNumberingRestart.Continuous;
  AInfo.StartingNumber := 1;
  AInfo.CustomMark := '';
  AppendItem(AInfo);
end;

{ TdxSectionPage }

constructor TdxSectionPage.Create(const ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

function TdxSectionPage.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxSectionPageChangeActionsCalculator.CalculateChangeActions(TdxSectionPageChangeType.BatchUpdate);
end;

function TdxSectionPage.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxPageInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).PageInfoCache;
end;

function TdxSectionPage.GetHeight: Integer;
begin
  Result := Info.Height;
end;

function TdxSectionPage.GetLandscape: Boolean;
begin
  Result := Info.Landscape;
end;

function TdxSectionPage.GetPaperKind: TdxPaperKind;
begin
  Result := Info.PaperKind;
end;

function TdxSectionPage.GetWidth: Integer;
begin
  Result := Info.Width;
end;

procedure TdxSectionPage.SetHeight(const Value: Integer);
begin
  if Value <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('Height', Value);

  if Height <> value then
    SetPropertyValue<Integer>(SetHeightCore, Value);
end;

procedure TdxSectionPage.SetWidth(const Value: Integer);
begin
  if Value <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('Width', Value);

  if Width <> Value then
    SetPropertyValue<Integer>(SetWidthCore, Value);
end;

function TdxSectionPage.SetWidthCore(const AInfo: TdxPageInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Width := AValue;
  Result := TdxSectionPageChangeActionsCalculator.CalculateChangeActions(TdxSectionPageChangeType.Width);
end;

function TdxSectionPage.SetHeightCore(const AInfo: TdxPageInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Height := AValue;
  Result := TdxSectionPageChangeActionsCalculator.CalculateChangeActions(TdxSectionPageChangeType.Height);
end;

procedure TdxSectionPage.SetLandscape(const Value: Boolean);
begin
  if Landscape <> Value then
    SetPropertyValue<Boolean>(SetLandscapeCore, Value);
end;

function TdxSectionPage.SetLandscapeCore(const AInfo: TdxPageInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.Landscape := AValue;
  Result := TdxSectionPageChangeActionsCalculator.CalculateChangeActions(TdxSectionPageChangeType.Landscape);
end;

procedure TdxSectionPage.SetPaperKind(const Value: TdxPaperKind);
begin
  if PaperKind <> Value then
    SetPropertyValue<Integer>(SetPaperKindCore, Ord(Value));
end;

function TdxSectionPage.SetPaperKindCore(const AInfo: TdxPageInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.PaperKind := TdxPaperKind(AValue);
  Result := TdxSectionPageChangeActionsCalculator.CalculateChangeActions(TdxSectionPageChangeType.PaperKind);
end;

{ TdxSectionColumns }

constructor TdxSectionColumns.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

function TdxSectionColumns.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxSectionColumnsChangeActionsCalculator.CalculateChangeActions(TdxSectionColumnsChangeType.BatchUpdate);
end;

function TdxSectionColumns.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxColumnsInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).ColumnsInfoCache;
end;

function TdxSectionColumns.GetColumnCount: Integer;
begin
  Result := Info.ColumnCount;
end;

function TdxSectionColumns.GetColumns: TdxColumnInfoCollection;
begin
  Result := TdxColumnInfoCollection.Create;
  Result.AddRange(Info.Columns);
end;

function TdxSectionColumns.GetDrawVerticalSeparator: Boolean;
begin
  Result := Info.DrawVerticalSeparator;
end;

function TdxSectionColumns.GetEqualWidthColumns: Boolean;
begin
  Result := Info.EqualWidthColumns;
end;

function TdxSectionColumns.GetSpace: Integer;
begin
  Result := Info.Space;
end;

procedure TdxSectionColumns.SetColumnCount(const Value: Integer);
begin
  Assert(Value > 0);
  if ColumnCount = Value then
    Exit;
  SetPropertyValue<Integer>(SetColumnCountCore, Value);
end;

function TdxSectionColumns.SetColumnCountCore(const AInfo: TdxColumnsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.ColumnCount := AValue;
  Result := TdxSectionColumnsChangeActionsCalculator.CalculateChangeActions(TdxSectionColumnsChangeType.ColumnCount);
end;

procedure TdxSectionColumns.SetColumns(const AValue: TdxColumnInfoCollection);
var
  AColumns: TdxColumnsInfo;
  AIsDeferred: Boolean;
begin
  Assert(AValue <> nil);
  AColumns := GetInfoForModification(AIsDeferred);
  AColumns.Columns.Clear;
  AColumns.Columns.AddRange(AValue);
  ReplaceInfo(AColumns, BatchUpdateChangeActions);
  if not AIsDeferred then AColumns.Free;
end;

procedure TdxSectionColumns.SetDrawVerticalSeparator(const Value: Boolean);
begin
  if DrawVerticalSeparator = Value then
    Exit;
  SetPropertyValue<Boolean>(SetDrawVerticalSeparatorCore, Value);
end;

function TdxSectionColumns.SetDrawVerticalSeparatorCore(const AInfo: TdxColumnsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.DrawVerticalSeparator := AValue;
  Result := TdxSectionColumnsChangeActionsCalculator.CalculateChangeActions(TdxSectionColumnsChangeType.DrawVerticalSeparator);
end;

procedure TdxSectionColumns.SetEqualWidthColumns(const Value: Boolean);
begin
  if EqualWidthColumns = Value then
    Exit;
  SetPropertyValue<Boolean>(SetEqualWidthColumnsCore, Value);
end;

function TdxSectionColumns.SetEqualWidthColumnsCore(const AInfo: TdxColumnsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.EqualWidthColumns := AValue;
  Result := TdxSectionColumnsChangeActionsCalculator.CalculateChangeActions(TdxSectionColumnsChangeType.EqualWidthColumns);
end;

procedure TdxSectionColumns.SetSpace(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Space', Value);

  if Space = Value then
    Exit;
  SetPropertyValue<Integer>(SetSpaceCore, Value);
end;

function TdxSectionColumns.SetSpaceCore(const AInfo: TdxColumnsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Space := AValue;
  Result := TdxSectionColumnsChangeActionsCalculator.CalculateChangeActions(TdxSectionColumnsChangeType.Space);
end;

{ TdxSectionGeneralSettings }

constructor TdxSectionGeneralSettings.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

function TdxSectionGeneralSettings.GetStartType: TdxSectionStartType;
begin
  Result := Info.StartType;
end;

procedure TdxSectionGeneralSettings.SetStartType(const AValue: TdxSectionStartType);
begin
  if StartType = AValue then
    Exit;
  SetPropertyValue<TdxSectionStartType>(SetStartTypeCore, AValue);
end;

function TdxSectionGeneralSettings.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxSectionGeneralSettingsChangeType.BatchUpdate);
end;

function TdxSectionGeneralSettings.SetStartTypeCore(const AInfo: TdxGeneralSectionInfo;
  const AValue: TdxSectionStartType): TdxDocumentModelChangeActions;
begin
  AInfo.StartType := AValue;
  Result := TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxSectionGeneralSettingsChangeType.StartType);
end;

function TdxSectionGeneralSettings.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxGeneralSectionInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).GeneralSectionInfoCache;
end;

function TdxSectionGeneralSettings.GetOnlyAllowEditingOfFormFields: Boolean;
begin
  Result := Info.OnlyAllowEditingOfFormFields;
end;

procedure TdxSectionGeneralSettings.SetOnlyAllowEditingOfFormFields(const AValue: Boolean);
begin
  if OnlyAllowEditingOfFormFields = AValue then
    Exit;
  SetPropertyValue<Boolean>(SetOnlyAllowEditingOfFormFieldsCore, AValue);
end;

function TdxSectionGeneralSettings.SetOnlyAllowEditingOfFormFieldsCore(const AInfo: TdxGeneralSectionInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.OnlyAllowEditingOfFormFields := AValue;
  Result := TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxSectionGeneralSettingsChangeType.OnlyAllowEditingOfFormFields);
end;

function TdxSectionGeneralSettings.GetDifferentFirstPage: Boolean;
begin
  Result := Info.DifferentFirstPage;
end;

procedure TdxSectionGeneralSettings.SetDifferentFirstPage(const AValue: Boolean);
begin
  if DifferentFirstPage = AValue then
    Exit;
  SetPropertyValue<Boolean>(SetDifferentFirstPageCore, AValue);
end;

function TdxSectionGeneralSettings.SetDifferentFirstPageCore(const AInfo: TdxGeneralSectionInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.DifferentFirstPage := AValue;
  Result := TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxSectionGeneralSettingsChangeType.DifferentFirstPage);
end;

function TdxSectionGeneralSettings.GetFirstPagePaperSource: Integer;
begin
  Result := Info.FirstPagePaperSource;
end;

procedure TdxSectionGeneralSettings.SetFirstPagePaperSource(const AValue: Integer);
begin
  if FirstPagePaperSource = AValue then
    Exit;
  SetPropertyValue<Integer>(SetFirstPagePaperSourceCore, AValue);
end;

function TdxSectionGeneralSettings.SetFirstPagePaperSourceCore(const AInfo: TdxGeneralSectionInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.FirstPagePaperSource := AValue;
  Result := TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxSectionGeneralSettingsChangeType.FirstPagePaperSource);
end;

function TdxSectionGeneralSettings.GetOtherPagePaperSource: Integer;
begin
  Result := Info.OtherPagePaperSource;
end;

procedure TdxSectionGeneralSettings.SetOtherPagePaperSource(const AValue: Integer);
begin
  if OtherPagePaperSource = AValue then
    Exit;
  SetPropertyValue<Integer>(SetOtherPagePaperSourceCore, AValue);
end;

function TdxSectionGeneralSettings.SetOtherPagePaperSourceCore(const AInfo: TdxGeneralSectionInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.OtherPagePaperSource := AValue;
  Result := TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxSectionGeneralSettingsChangeType.OtherPagePaperSource);
end;

function TdxSectionGeneralSettings.GetTextDirection: TdxTextDirection;
begin
  Result := Info.TextDirection;
end;

procedure TdxSectionGeneralSettings.SetTextDirection(const AValue: TdxTextDirection);
begin
  if TextDirection = AValue then
    Exit;
  SetPropertyValue<TdxTextDirection>(SetTextDirectionCore, AValue);
end;

function TdxSectionGeneralSettings.SetTextDirectionCore(const AInfo: TdxGeneralSectionInfo; const AValue: TdxTextDirection): TdxDocumentModelChangeActions;
begin
  AInfo.TextDirection := AValue;
  Result := TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxSectionGeneralSettingsChangeType.TextDirection);
end;

function TdxSectionGeneralSettings.GetVerticalTextAlignment: TdxVerticalAlignment;
begin
  Result := Info.VerticalTextAlignment;
end;

procedure TdxSectionGeneralSettings.SetVerticalTextAlignment(const AValue: TdxVerticalAlignment);
begin
  if VerticalTextAlignment = AValue then
    Exit;
  SetPropertyValue<TdxVerticalAlignment>(SetVerticalTextAlignmentCore, AValue);
end;

function TdxSectionGeneralSettings.SetVerticalTextAlignmentCore(const AInfo: TdxGeneralSectionInfo; const AValue: TdxVerticalAlignment): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalTextAlignment := AValue;
  Result := TdxSectionGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxSectionGeneralSettingsChangeType.VerticalTextAlignment);
end;

{ TdxSectionPageNumbering }

constructor TdxSectionPageNumbering.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

function TdxSectionPageNumbering.SetChapterHeaderStyleCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.ChapterHeaderStyle := AValue;
  Result := TdxSectionPageNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionPageNumberingChangeType.ChapterHeaderStyle);
end;

function TdxSectionPageNumbering.SetNumberingFormatCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.NumberingFormat := TdxRichEditNumberingFormat(AValue);
  Result := TdxSectionPageNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionPageNumberingChangeType.NumberingFormat);
end;

function TdxSectionPageNumbering.SetChapterSeparatorCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.ChapterSeparator := Char(AValue);
  Result := TdxSectionPageNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionPageNumberingChangeType.ChapterSeparator);
end;

procedure TdxSectionPageNumbering.SetContinueNumbering(const AValue: Boolean);
begin
  if ContinueNumbering = AValue then
    Exit;
  SetPropertyValue<Integer>(SetContinueNumberingCore, Ord(AValue));
end;

function TdxSectionPageNumbering.SetContinueNumberingCore(const AInfo: TdxPageNumberingInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.ContinueNumbering := Boolean(AValue);
  Result := TdxSectionPageNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionPageNumberingChangeType.StartingPageNumber);
end;

function TdxSectionPageNumbering.SetFirstPageNumberCore(const AInfo: TdxPageNumberingInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.FirstPageNumber := AValue;
  Result := TdxSectionPageNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionPageNumberingChangeType.StartingPageNumber);
end;

function TdxSectionPageNumbering.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxPageNumberingInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).PageNumberingInfoCache;
end;

function TdxSectionPageNumbering.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxSectionPageNumberingChangeActionsCalculator.CalculateChangeActions(TdxSectionPageNumberingChangeType.BatchUpdate);
end;

function TdxSectionPageNumbering.GetChapterSeparator: Char;
begin
  Result := Info.ChapterSeparator;
end;

function TdxSectionPageNumbering.GetContinueNumbering: Boolean;
begin
  Result := Info.ContinueNumbering;
end;

procedure TdxSectionPageNumbering.SetChapterSeparator(const AValue: Char);
begin
  if ChapterSeparator = AValue then
    Exit;
  SetPropertyValue<Integer>(SetChapterSeparatorCore, Ord(AValue));
end;

function TdxSectionPageNumbering.GetFirstPageNumber: Integer;
begin
  Result := Info.FirstPageNumber;
end;

procedure TdxSectionPageNumbering.SetFirstPageNumber(const AValue: Integer);
begin
  if FirstPageNumber = AValue then
    Exit;
  SetPropertyValue<Integer>(SetFirstPageNumberCore, AValue);
end;

function TdxSectionPageNumbering.GetChapterHeaderStyle: Integer;
begin
  Result := Info.ChapterHeaderStyle;
end;

procedure TdxSectionPageNumbering.SetChapterHeaderStyle(const AValue: Integer);
begin
  if AValue < 0 then
    TdxRichEditExceptions.ThrowArgumentException('ChapterHeaderStyle', AValue);

  if ChapterHeaderStyle = AValue then
    Exit;
  SetPropertyValue<Integer>(SetChapterHeaderStyleCore, AValue);
end;

function TdxSectionPageNumbering.GetNumberingFormat: TdxRichEditNumberingFormat;
begin
  Result := Info.NumberingFormat;
end;

procedure TdxSectionPageNumbering.SetNumberingFormat(const AValue: TdxRichEditNumberingFormat);
begin
  if NumberingFormat = AValue then
    Exit;
  SetPropertyValue<Integer>(SetNumberingFormatCore, Ord(AValue));
end;

{ TdxSectionFootNote }

constructor TdxSectionFootNote.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

function TdxSectionFootNote.GetPosition: TdxFootNotePosition;
begin
  Result := Info.Position;
end;

procedure TdxSectionFootNote.SetPosition(const AValue: TdxFootNotePosition);
begin
  if Position = AValue then
    Exit;
  SetPropertyValue<TdxFootNotePosition>(SetPositionCore, AValue);
end;

function TdxSectionFootNote.SetPositionCore(const AInfo: TdxFootNoteInfo; const AValue: TdxFootNotePosition): TdxDocumentModelChangeActions;
begin
  AInfo.Position := AValue;
  Result := TdxSectionFootNoteChangeActionsCalculator.CalculateChangeActions(TdxSectionFootNoteChangeType.Position);
end;

function TdxSectionFootNote.GetNumberingFormat: TdxNumberingFormat;
begin
  Result := Info.NumberingFormat;
end;

procedure TdxSectionFootNote.SetNumberingFormat(const AValue: TdxNumberingFormat);
begin
  if NumberingFormat = AValue then
    Exit;
  SetPropertyValue<TdxNumberingFormat>(SetNumberingFormatCore, AValue);
end;

function TdxSectionFootNote.SetNumberingFormatCore(const AInfo: TdxFootNoteInfo; const AValue: TdxNumberingFormat): TdxDocumentModelChangeActions;
begin
  AInfo.NumberingFormat := AValue;
  Result := TdxSectionFootNoteChangeActionsCalculator.CalculateChangeActions(TdxSectionFootNoteChangeType.NumberingFormat);
end;

function TdxSectionFootNote.GetNumberingRestartType: TdxLineNumberingRestart;
begin
  Result := Info.NumberingRestartType;
end;

procedure TdxSectionFootNote.SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
begin
  if NumberingRestartType = AValue then
    Exit;
  SetPropertyValue<TdxLineNumberingRestart>(SetNumberingRestartTypeCore, AValue);
end;

function TdxSectionFootNote.SetNumberingRestartTypeCore(const AInfo: TdxFootNoteInfo; const AValue: TdxLineNumberingRestart): TdxDocumentModelChangeActions;
begin
  AInfo.NumberingRestartType := AValue;
  Result := TdxSectionFootNoteChangeActionsCalculator.CalculateChangeActions(TdxSectionFootNoteChangeType.NumberingRestartType);
end;

function TdxSectionFootNote.GetStartingNumber: Integer;
begin
  Result := Info.StartingNumber;
end;

procedure TdxSectionFootNote.SetStartingNumber(const AValue: Integer);
begin
  if AValue < 0 then
    TdxRichEditExceptions.ThrowArgumentException('StartingNumber', AValue);

  if StartingNumber = AValue then
    Exit;
  SetPropertyValue<Integer>(SetStartingNumberCore, AValue);
end;

function TdxSectionFootNote.SetStartingNumberCore(const AInfo: TdxFootNoteInfo; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.StartingNumber := AValue;
  Result := TdxSectionFootNoteChangeActionsCalculator.CalculateChangeActions(TdxSectionFootNoteChangeType.StartingNumber);
end;

function TdxSectionFootNote.GetCustomMark: string;
begin
  Result := Info.CustomMark;
end;

procedure TdxSectionFootNote.SetCustomMark(const AValue: string);
begin
  if CustomMark = AValue then
    Exit;
  SetPropertyValue<string>(SetCustomMarkCore, AValue);
end;

function TdxSectionFootNote.SetCustomMarkCore(const AInfo: TdxFootNoteInfo; const AValue: string): TdxDocumentModelChangeActions;
begin
  AInfo.CustomMark := AValue;
  Result := TdxSectionFootNoteChangeActionsCalculator.CalculateChangeActions(TdxSectionFootNoteChangeType.CustomMark);
end;

function TdxSectionFootNote.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxFootNoteInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).FootNoteInfoCache;
end;

function TdxSectionFootNote.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxSectionFootNoteChangeActionsCalculator.CalculateChangeActions(TdxSectionFootNoteChangeType.BatchUpdate);
end;

function TdxSectionFootNote.FormatCounterValue(AValue: Integer): string;
var
  AConverter: TdxOrdinalBasedNumberConverter;
begin
  AConverter := TdxOrdinalBasedNumberConverter.CreateConverter(NumberingFormat, TdxLanguageId.English);
  try
    Result := AConverter.ConvertNumber(AValue);
  finally
    AConverter.Free;
  end;
end;

end.
