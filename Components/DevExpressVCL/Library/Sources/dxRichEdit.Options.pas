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

unit dxRichEdit.Options;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Math, SysUtils, Classes, Windows, Generics.Defaults, Generics.Collections, DB,
  dxCoreClasses, dxCoreGraphics,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Options.Core,
  dxRichEdit.Options.Simple,
  dxRichEdit.Utils.Types,
  dxCharacters,
  dxRichEdit.ImportExportHelper,
  dxRichEdit.Export.Core,
  dxRichEdit.Import.Core;

type
  TdxNumberingOptions = class;
  TdxCharacterFormattingDetailedOptions = class;

  { TdxDocumentCapabilitiesOptions }

  TdxDocumentCapabilitiesOptions = class(TdxSimpleDocumentCapabilitiesOptions)
  public type
    TAction = class sealed
    public const
      Bookmarks           = TdxRichEditOptionsAction.Bookmarks;
      CharacterStyle      = TdxRichEditOptionsAction.CharacterStyle;
      EndNotes            = TdxRichEditOptionsAction.EndNotes;
      FloatingObjects     = TdxRichEditOptionsAction.FloatingObjects;
      FootNotes           = TdxRichEditOptionsAction.FootNotes;
      HeadersFooters      = TdxRichEditOptionsAction.HeadersFooters;
      ParagraphStyle      = TdxRichEditOptionsAction.ParagraphStyle;
      ParagraphTabs       = TdxRichEditOptionsAction.ParagraphTabs;
      Sections            = TdxRichEditOptionsAction.Sections;
      TableCellStyle      = TdxRichEditOptionsAction.TableCellStyle;
      Tables              = TdxRichEditOptionsAction.Tables;
      TableStyle          = TdxRichEditOptionsAction.TableStyle;
      TabSymbol           = TdxRichEditOptionsAction.TabSymbol;
      ParagraphFrames     = TdxRichEditOptionsAction.ParagraphFrames;
      Fields              = TdxRichEditOptionsAction.Fields;
    end;
  private
    FBookmarks: TdxDocumentCapability;
    FCharacterStyle: TdxDocumentCapability;
    FEndNotes: TdxDocumentCapability;
    FFields: TdxDocumentCapability;
    FFloatingObjects: TdxDocumentCapability;
    FFootNotes: TdxDocumentCapability;
    FHeadersFooters: TdxDocumentCapability;
    FParagraphFrames: TdxDocumentCapability;
    FParagraphStyle: TdxDocumentCapability;
    FParagraphTabs: TdxDocumentCapability;
    FSections: TdxDocumentCapability;
    FTableCellStyle: TdxDocumentCapability;
    FTables: TdxDocumentCapability;
    FTableStyle: TdxDocumentCapability;
    FTabSymbol: TdxDocumentCapability;
    FNumbering: TdxNumberingOptions;
    FCharacterFormattingDetailed: TdxCharacterFormattingDetailedOptions;
    procedure SetBookmarks(const Value: TdxDocumentCapability);
    procedure SetCharacterStyle(const Value: TdxDocumentCapability);
    procedure SetEndNotes(const Value: TdxDocumentCapability);
    procedure SetFields(const Value: TdxDocumentCapability);
    procedure SetFloatingObjects(const Value: TdxDocumentCapability);
    procedure SetFootNotes(const Value: TdxDocumentCapability);
    procedure SetHeadersFooters(const Value: TdxDocumentCapability);
    procedure SetParagraphFrames(const Value: TdxDocumentCapability);
    procedure SetParagraphStyle(const Value: TdxDocumentCapability);
    procedure SetParagraphTabs(const Value: TdxDocumentCapability);
    procedure SetSections(const Value: TdxDocumentCapability);
    procedure SetTableCellStyle(const Value: TdxDocumentCapability);
    procedure SetTables(const Value: TdxDocumentCapability);
    procedure SetTableStyle(const Value: TdxDocumentCapability);
    procedure SetTabSymbol(const Value: TdxDocumentCapability);
    procedure SetNumbering(const Value: TdxNumberingOptions);
  protected
    procedure CreateInnerOptions; override;
    procedure DoReset; override;
    procedure SubscribeInnerOptions; override;
  public
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function BookmarksAllowed: Boolean; override;
    function CharacterStyleAllowed: Boolean; override;
    function EndNotesAllowed: Boolean; override;
    function FieldsAllowed: Boolean; override;
    function FloatingObjectsAllowed: Boolean; override;
    function FootNotesAllowed: Boolean; override;
    function HeadersFootersAllowed: Boolean; override;
    function ParagraphFramesAllowed: Boolean; override;
    function ParagraphStyleAllowed: Boolean; override;
    function ParagraphTabsAllowed: Boolean; override;
    function SectionsAllowed: Boolean; override;
    function TableCellStyleAllowed: Boolean; override;
    function TablesAllowed: Boolean; override;
    function TableStyleAllowed: Boolean; override;
    function TabSymbolAllowed: Boolean; override;
    property CharacterFormattingDetailed: TdxCharacterFormattingDetailedOptions read FCharacterFormattingDetailed;
  published
    property Bookmarks: TdxDocumentCapability read FBookmarks write SetBookmarks default TdxDocumentCapability.Default;
    property CharacterStyle: TdxDocumentCapability read FCharacterStyle write SetCharacterStyle default TdxDocumentCapability.Default;
    property EndNotes: TdxDocumentCapability read FEndNotes write SetEndNotes default TdxDocumentCapability.Default;
    property Fields: TdxDocumentCapability read FFields write SetFields default TdxDocumentCapability.Default;
    property FloatingObjects: TdxDocumentCapability read FFloatingObjects write SetFloatingObjects default TdxDocumentCapability.Default;
    property FootNotes: TdxDocumentCapability read FFootNotes write SetFootNotes default TdxDocumentCapability.Default;
    property HeadersFooters: TdxDocumentCapability read FHeadersFooters write SetHeadersFooters default TdxDocumentCapability.Default;
    property ParagraphFrames: TdxDocumentCapability read FParagraphFrames write SetParagraphFrames default TdxDocumentCapability.Default;
    property ParagraphStyle: TdxDocumentCapability read FParagraphStyle write SetParagraphStyle default TdxDocumentCapability.Default;
    property ParagraphTabs: TdxDocumentCapability read FParagraphTabs write SetParagraphTabs default TdxDocumentCapability.Default;
    property Sections: TdxDocumentCapability read FSections write SetSections default TdxDocumentCapability.Default;
    property TableCellStyle: TdxDocumentCapability read FTableCellStyle write SetTableCellStyle default TdxDocumentCapability.Default;
    property Tables: TdxDocumentCapability read FTables write SetTables default TdxDocumentCapability.Default;
    property TableStyle: TdxDocumentCapability read FTableStyle write SetTableStyle default TdxDocumentCapability.Default;
    property TabSymbol: TdxDocumentCapability read FTabSymbol write SetTabSymbol default TdxDocumentCapability.Default;
    property Numbering: TdxNumberingOptions read FNumbering write SetNumbering;
  end;

  TdxDocumentCapabilitiesOptionsClass = class of TdxDocumentCapabilitiesOptions;

  { TdxRichEditBehaviorOptions }

  TdxRichEditBehaviorOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      AllowCopy                   = TdxRichEditOptionsAction.AllowCopy;
      AllowCreateNew              = TdxRichEditOptionsAction.AllowCreateNew;
      AllowCut                    = TdxRichEditOptionsAction.AllowCut;
      AllowDrag                   = TdxRichEditOptionsAction.AllowDrag;
      AllowDrop                   = TdxRichEditOptionsAction.AllowDrop;
      AllowOfficeScrolling        = TdxRichEditOptionsAction.AllowOfficeScrolling;
      AllowOpen                   = TdxRichEditOptionsAction.AllowOpen;
      AllowPaste                  = TdxRichEditOptionsAction.AllowPaste;
      AllowPrinting               = TdxRichEditOptionsAction.AllowPrinting;
      AllowSave                   = TdxRichEditOptionsAction.AllowSave;
      AllowSaveAs                 = TdxRichEditOptionsAction.AllowSaveAs;
      AllowShowPopupMenu          = TdxRichEditOptionsAction.AllowShowPopupMenu;
      AllowTouch                  = TdxRichEditOptionsAction.AllowTouch;
      AllowZooming                = TdxRichEditOptionsAction.AllowZooming;
      FontSource                  = TdxRichEditOptionsAction.FontSource;
      ForeColorSource             = TdxRichEditOptionsAction.ForeColorSource;
      MaxZoomFactor               = TdxRichEditOptionsAction.MaxZoomFactor;
      MinZoomFactor               = TdxRichEditOptionsAction.MinZoomFactor;
      OvertypeAllowed             = TdxRichEditOptionsAction.OvertypeAllowed;
      PageBreakInsertMode         = TdxRichEditOptionsAction.PageBreakInsertMode;
      PasteLineBreakSubstitution  = TdxRichEditOptionsAction.PasteLineBreakSubstitution;
      PasteSingleCellAsText       = TdxRichEditOptionsAction.PasteSingleCellAsText;
      TabMarker                   = TdxRichEditOptionsAction.TabMarker;
      UseFontSubstitution         = TdxRichEditOptionsAction.UseFontSubstitution;
    end;
  public const
    DefaultMaxZoomFactor = 5.00;
    DefaultMinZoomFactor = 0.10;
  strict private
    FAllowCopy: TdxDocumentCapability;
    FAllowPaste: TdxDocumentCapability;
    FAllowDrag: TdxDocumentCapability;
    FAllowDrop: TdxDocumentCapability;
    FAllowCut: TdxDocumentCapability;
    FAllowPrinting: TdxDocumentCapability;
    FAllowZooming: TdxDocumentCapability;
    FAllowSaveAs: TdxDocumentCapability;
    FAllowSave: TdxDocumentCapability;
    FAllowCreateNew: TdxDocumentCapability;
    FAllowOpen: TdxDocumentCapability;
    FAllowShowPopupMenu: TdxDocumentCapability;
    FAllowOfficeScrolling: TdxDocumentCapability;
    FAllowTouch: TdxDocumentCapability;
    FPasteSingleCellAsText: Boolean;
    FPasteLineBreakSubstitution: TdxLineBreakSubstitute;
    FMinZoomFactor: Single;
    FMaxZoomFactor: Single;
    FFontSource: TdxRichEditBaseValueSource;
    FForeColorSource: TdxRichEditBaseValueSource;
    FTabMarker: string;
    FUseFontSubstitution: Boolean;
    FOvertypeAllowed: Boolean;
    FPageBreakInsertMode: TdxPageBreakInsertMode;
  private
    function GetDragAllowed: Boolean;
    function GetDropAllowed: Boolean;
    function GetCopyAllowed: Boolean;
    function GetPasteAllowed: Boolean;
    function GetCutAllowed: Boolean;
    function GetPrintingAllowed: Boolean;
    function GetSaveAllowed: Boolean;
    function GetSaveAsAllowed: Boolean;
    function GetCreateNewAllowed: Boolean;
    function GetOpenAllowed: Boolean;
    function GetZoomingAllowed: Boolean;
    function GetShowPopupMenuAllowed: Boolean;
    function GetOfficeScrollingAllowed: Boolean;
    function GetTouchAllowed: Boolean;
    function IsMinZoomFactorStored: Boolean;
    function IsMaxZoomFactorStored: Boolean;
    function IsTabMarkerStored: Boolean;
    procedure SetCopy(const Value: TdxDocumentCapability); inline;
    procedure SetPaste(const Value: TdxDocumentCapability); inline;
    procedure SetCreateNew(const Value: TdxDocumentCapability); inline;
    procedure SetCut(const Value: TdxDocumentCapability); inline;
    procedure SetDrag(const Value: TdxDocumentCapability); inline;
    procedure SetDrop(const Value: TdxDocumentCapability); inline;
    procedure SetFontSource(const Value: TdxRichEditBaseValueSource); inline;
    procedure SetForeColorSource(const Value: TdxRichEditBaseValueSource); inline;
    procedure SetMaxZoomFactor(Value: Single);
    procedure SetMinZoomFactor(Value: Single);
    procedure SetOfficeScrolling(const Value: TdxDocumentCapability); inline;
    procedure SetOpen(const Value: TdxDocumentCapability); inline;
    procedure SetOvertypeAllowed(const Value: Boolean); inline;
    procedure SetPageBreakInsertMode(const Value: TdxPageBreakInsertMode); inline;
    procedure SetPasteSingleCellAsText(const Value: Boolean); inline;
    procedure SetPasteLineBreakSubstitution(const Value: TdxLineBreakSubstitute); inline;
    procedure SetPrinting(const Value: TdxDocumentCapability); inline;
    procedure SetSave(const Value: TdxDocumentCapability); inline;
    procedure SetSaveAs(const Value: TdxDocumentCapability); inline;
    procedure SetShowPopupMenu(const Value: TdxDocumentCapability); inline;
    procedure SetTabMarker(const Value: string); inline;
    procedure SetTouch(const Value: TdxDocumentCapability); inline;
    procedure SetUseFontSubstitution(const Value: Boolean); inline;
    procedure SetZooming(const Value: TdxDocumentCapability); inline;
  protected
    function IsAllowed(AOption: TdxDocumentCapability): Boolean;
    procedure DoReset; override;

    function ShouldSerializePrinting: Boolean;
    procedure ResetPrinting;

    function ShouldSerializeSaveAs: Boolean;
    procedure ResetSaveAs;

    function ShouldSerializeSave: Boolean;
    procedure ResetSave;

    function ShouldSerializeZooming: Boolean;
    procedure ResetZooming;

    function ShouldSerializeCreateNew: Boolean;
    procedure ResetCreateNew;

    function ShouldSerializeOpen: Boolean;
    procedure ResetOpen;
  public
    procedure Assign(Source: TPersistent); override;

    property DragAllowed: Boolean read GetDragAllowed;
    property DropAllowed: Boolean read GetDropAllowed;

    property CopyAllowed: Boolean read GetCopyAllowed;
    property PasteAllowed: Boolean read GetPasteAllowed;
    property CutAllowed: Boolean read GetCutAllowed;
    property PrintingAllowed: Boolean read GetPrintingAllowed;
    property SaveAllowed: Boolean read GetSaveAllowed;
    property SaveAsAllowed: Boolean read GetSaveAsAllowed;
    property CreateNewAllowed: Boolean read GetCreateNewAllowed;
    property OpenAllowed: Boolean read GetOpenAllowed;

    property ZoomingAllowed: Boolean read GetZoomingAllowed;
    property ShowPopupMenuAllowed: Boolean read GetShowPopupMenuAllowed;
    property OfficeScrollingAllowed: Boolean read GetOfficeScrollingAllowed;
    property TouchAllowed: Boolean read GetTouchAllowed;
  published
    property Copy: TdxDocumentCapability read FAllowCopy write SetCopy default TdxDocumentCapability.Default;
    property CreateNew: TdxDocumentCapability read FAllowCreateNew write SetCreateNew default TdxDocumentCapability.Default;
    property Cut: TdxDocumentCapability read FAllowCut write SetCut default TdxDocumentCapability.Default;
    property Drag: TdxDocumentCapability read FAllowDrag write SetDrag default TdxDocumentCapability.Default;
    property Drop: TdxDocumentCapability read FAllowDrop write SetDrop default TdxDocumentCapability.Default;
    property FontSource: TdxRichEditBaseValueSource read FFontSource write SetFontSource default TdxRichEditBaseValueSource.Auto;
    property ForeColorSource: TdxRichEditBaseValueSource read FForeColorSource write SetForeColorSource default TdxRichEditBaseValueSource.Auto;
    property MaxZoomFactor: Single read FMaxZoomFactor write SetMaxZoomFactor stored IsMaxZoomFactorStored;
    property MinZoomFactor: Single read FMinZoomFactor write SetMinZoomFactor stored IsMinZoomFactorStored;
    property OfficeScrolling: TdxDocumentCapability read FAllowOfficeScrolling write SetOfficeScrolling default TdxDocumentCapability.Default;
    property Open: TdxDocumentCapability read FAllowOpen write SetOpen default TdxDocumentCapability.Default;
    property OvertypeAllowed: Boolean read FOvertypeAllowed write SetOvertypeAllowed default True;
    property PageBreakInsertMode: TdxPageBreakInsertMode read FPageBreakInsertMode write SetPageBreakInsertMode default TdxPageBreakInsertMode.NewLine;
    property Paste: TdxDocumentCapability read FAllowPaste write SetPaste default TdxDocumentCapability.Default;
    property PasteLineBreakSubstitution: TdxLineBreakSubstitute read FPasteLineBreakSubstitution write SetPasteLineBreakSubstitution default TdxLineBreakSubstitute.None;
    property PasteSingleCellAsText: Boolean read FPasteSingleCellAsText write SetPasteSingleCellAsText default False;
    property Printing: TdxDocumentCapability read FAllowPrinting write SetPrinting default TdxDocumentCapability.Default;
    property Save: TdxDocumentCapability read FAllowSave write SetSave default TdxDocumentCapability.Default;
    property SaveAs: TdxDocumentCapability read FAllowSaveAs write SetSaveAs default TdxDocumentCapability.Default;
    property ShowPopupMenu: TdxDocumentCapability read FAllowShowPopupMenu write SetShowPopupMenu default TdxDocumentCapability.Default;
    property TabMarker: string read FTabMarker write SetTabMarker stored IsTabMarkerStored;
    property Touch: TdxDocumentCapability read FAllowTouch write SetTouch default TdxDocumentCapability.Default;
    property UseFontSubstitution: Boolean read FUseFontSubstitution write SetUseFontSubstitution default True;
    property Zooming: TdxDocumentCapability read FAllowZooming write SetZooming default TdxDocumentCapability.Default;
  end;

  { TdxRichEditEditingOptions }

  TdxRichEditEditingOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      MergeParagraphsContent      = TdxRichEditOptionsAction.MergeParagraphsContent;
      MergeUseFirstParagraphStyle = TdxRichEditOptionsAction.MergeUseFirstParagraphStyle;
    end;
  strict private
    FMergeParagraphsContent: Boolean;
    FMergeUseFirstParagraphStyle: Boolean;
  private
    procedure SetMergeParagraphsContent(const Value: Boolean);
    procedure SetMergeUseFirstParagraphStyle(const Value: Boolean);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property MergeParagraphsContent: Boolean read FMergeParagraphsContent write SetMergeParagraphsContent default True;
    property MergeUseFirstParagraphStyle: Boolean read FMergeUseFirstParagraphStyle write SetMergeUseFirstParagraphStyle default True;
  end;

  { TdxMailMergeCustomSeparators }

  TdxMailMergeCustomSeparators = class(TPersistent)
  strict private
    FFieldResultDecimalSeparator: string;
    FFieldResultGroupSeparator: string;
    FMaskDecimalSeparator: string;
    FMaskGroupSeparator: string;
  public
    constructor Create; overload;
    constructor Create(const AMaskGroupSeparator, AMaskDecimalSeparator,
      AFieldResultGroupSeparator, AFieldResultDecimalSeparator: string); overload;
    procedure Assign(Source: TPersistent); override;
    procedure ResetFieldResultGroupSeparator;
    procedure ResetFieldResultDecimalSeparator;
    procedure ResetMaskDecimalSeparator;
    procedure ResetMaskGroupSeparator;
    function ShouldSerializeFieldResultDecimalSeparator: Boolean;
    function ShouldSerializeFieldResultGroupSeparator: Boolean;
    function ShouldSerializeMaskDecimalSeparator: Boolean;
    function ShouldSerializeMaskGroupSeparator: Boolean;
    procedure Clear;
    function IsEmpty: Boolean;
  published
    property FieldResultDecimalSeparator: string read FFieldResultDecimalSeparator write FFieldResultDecimalSeparator;
    property FieldResultGroupSeparator: string read FFieldResultGroupSeparator write FFieldResultGroupSeparator;
    property MaskDecimalSeparator: string read FMaskDecimalSeparator write FMaskDecimalSeparator;
    property MaskGroupSeparator: string read FMaskGroupSeparator write FMaskGroupSeparator;
  end;

  { TdxRichEditMailMergeOptions }

  TdxRichEditMailMergeOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      DataSource        = TdxRichEditOptionsAction.DataSource;
      ViewMergedData    = TdxRichEditOptionsAction.ViewMergedData;
      KeepLastParagraph = TdxRichEditOptionsAction.KeepLastParagraph;
    end;
  private
    FCustomSeparators: TdxMailMergeCustomSeparators;
    FDataSource: TDataSource;
    FFreeNotificator: TcxFreeNotificator;
    FKeepLastParagraph: Boolean;
    FViewMergedData: Boolean;

    procedure FreeNotificationHandler(Sender: TComponent);

    procedure SetCustomSeparators(const Value: TdxMailMergeCustomSeparators);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetViewMergedData(const Value: Boolean);
  protected
    procedure CreateInnerOptions; override;
    procedure DoReset; override;
    procedure Initialize; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CustomSeparators: TdxMailMergeCustomSeparators read FCustomSeparators write SetCustomSeparators;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property KeepLastParagraph: Boolean read FKeepLastParagraph write FKeepLastParagraph default False;
    property ViewMergedData: Boolean read FViewMergedData write SetViewMergedData default False;
  end;

  { TdxUpdateFieldOptions }

  TdxUpdateFieldOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      Date = TdxRichEditOptionsAction.Date;
      Time = TdxRichEditOptionsAction.Time;
    end;
  protected const
    DefaultDate = True;
    DefaultTime = True;
  private
    FDate: Boolean;
    FTime: Boolean;
    procedure SetDate(const Value: Boolean);
    procedure SetTime(const Value: Boolean);
  protected
    procedure DoReset; override;
  public
    procedure CopyFrom(ASource: TdxUpdateFieldOptions);
    function GetNativeOptions: TdxFieldUpdateOnLoadOptions;
  published
    property Date: Boolean read FDate write SetDate default DefaultDate;
    property Time: Boolean read FTime write SetTime default DefaultTime;
  end;

  { TdxDocumentImporterOptions }

  TdxDocumentImporterOptions = class abstract(TdxRichEditNotificationOptions, IdxImporterOptions)
  public type
    TAction = class sealed
    public const
      Encoding = TdxRichEditOptionsAction.Encoding;
    end;
  strict private
    FActualEncoding: TEncoding;
    FSourceUri: string;
    FLineBreakSubstitute: TdxLineBreakSubstitute;
    FUpdateField: TdxUpdateFieldOptions;
    procedure CreateUpdateFieldOptions;
    procedure SetUpdateField(const Value: TdxUpdateFieldOptions);
  protected
    function GetActualEncoding: TEncoding; virtual;
    procedure SetActualEncoding(const Value: TEncoding); virtual;

    //IdxImporterOptions
    function GetSourceUri: string; virtual;
    procedure SetSourceUri(const Value: string); virtual;

    function ShouldSerializeSourceUri: Boolean; virtual;
    procedure DoReset; override;
    procedure CreateInnerOptions; override;
    procedure SubscribeInnerOptions; override;
  public
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    //IdxImporterOptions
    procedure CopyFrom(const AValue: TObject); virtual;
    function Format: TdxRichEditDocumentFormat; virtual; abstract;
    function IsDefaultEncoding: Boolean; virtual;

    property LineBreakSubstitute: TdxLineBreakSubstitute read FLineBreakSubstitute write FLineBreakSubstitute;
    property SourceUri: string read GetSourceUri write SetSourceUri;
    property UpdateField: TdxUpdateFieldOptions read FUpdateField write SetUpdateField;

    // for internal use
    property ActualEncoding: TEncoding read GetActualEncoding write SetActualEncoding;
  end;

  { TdxRtfDocumentImporterOptions }

  TdxRtfDocumentImporterOptions = class(TdxDocumentImporterOptions)
  public type
    TAction = class sealed
    public const
      IgnoreDeletedText = TdxRichEditOptionsAction.IgnoreDeletedText;
    end;
  strict private
    class var
      FDefaultEncoding: TEncoding;
    class constructor Initialize;
  private
    FCopySingleCellAsText: Boolean;
    FIgnoreDeletedText: Boolean;
    FPasteFromIe: Boolean;
    FSuppressLastParagraphDelete: Boolean;
    procedure SetIgnoreDeletedText(const Value: Boolean);
  protected
    procedure SetActualEncoding(const Value: TEncoding); override;
    procedure DoReset; override;
  public
    procedure CopyFrom(const AValue: TObject); override;
    function Format: TdxRichEditDocumentFormat; override;

    class property DefaultEncoding: TEncoding read FDefaultEncoding;
    property CopySingleCellAsText: Boolean read FCopySingleCellAsText write FCopySingleCellAsText default False;
    property PasteFromIE: Boolean read FPasteFromIe write FPasteFromIe default False;
    property SuppressLastParagraphDelete: Boolean read FSuppressLastParagraphDelete write FSuppressLastParagraphDelete default False;
  published
    property IgnoreDeletedText: Boolean read FIgnoreDeletedText write SetIgnoreDeletedText default False;
    property UpdateField;
  end;

  { TdxPlainTextDocumentImporterOptions }

  TdxPlainTextDocumentImporterOptions = class(TdxDocumentImporterOptions)
  public type
    TAction = class sealed
    public const
      AutoDetectEncoding = TdxRichEditOptionsAction.AutoDetectEncoding;
    end;
  strict private
    FAutoDetectEncoding: Boolean;
    procedure SetAutoDetectEncoding(const AValue: Boolean);
    procedure SetEncoding(const AValue: Word);
    function GetEncoding: Word;
  protected
    procedure DoReset; override;
  public
    procedure CopyFrom(const AValue: TObject); override;
    function Format: TdxRichEditDocumentFormat; override;
    function IsDefaultEncoding: Boolean; override;
  published
    property AutoDetectEncoding: Boolean read FAutoDetectEncoding write SetAutoDetectEncoding default True;
    property Encoding: Word read GetEncoding write SetEncoding default CP_ACP;
  end;

  { TdxXmlBasedDocumentImporterOptions }

  TdxXmlBasedDocumentImporterOptions = class abstract(TdxDocumentImporterOptions)
  public type
    TAction = class sealed
    public const
      IgnoreParseErrors = TdxRichEditOptionsAction.IgnoreParseErrors;
    end;
  strict private
    FIgnoreParseErrors: Boolean;
    procedure SetIgnoreParseErrors(const AValue: Boolean);
  protected
    procedure DoReset; override;
  public
    procedure CopyFrom(const AValue: TObject); override;

    property IgnoreParseErrors: Boolean read FIgnoreParseErrors write SetIgnoreParseErrors default False;
  end;

  { TdxOpenXmlDocumentImporterOptions }

  TdxOpenXmlDocumentImporterOptions = class(TdxXmlBasedDocumentImporterOptions)
  public type
    TAction = class sealed
    public const
      IgnoreDeletedText = TdxRichEditOptionsAction.IgnoreDeletedText;
      IgnoreInsertedText = TdxRichEditOptionsAction.IgnoreInsertedText;
    end;
  strict private
    FIgnoreDeletedText: Boolean;
    FIgnoreInsertedText: Boolean;
  private
    procedure SetIgnoreDeletedText(const Value: Boolean);
    procedure SetIgnoreInsertedText(const Value: Boolean);
  protected
    procedure DoReset; override;
  public
    function Format: TdxRichEditDocumentFormat; override;
  published
    property IgnoreParseErrors;
    property IgnoreDeletedText: Boolean read FIgnoreDeletedText write SetIgnoreDeletedText default True;
    property IgnoreInsertedText: Boolean read FIgnoreInsertedText write SetIgnoreInsertedText default True;
    property UpdateField;
  end;

  { TdxHtmlDocumentImporterOptions }

  TdxHtmlDocumentImporterOptions = class(TdxDocumentImporterOptions)
  public const
    DefaultTableCellSpacingDefaultValue = 15;
    DefaultTableCellMargingDefaultValue = 15;
  public type
    TAction = class sealed
    public const
      AsyncImageLoading = TdxRichEditOptionsAction.AsyncImageLoading;
      AutoDetectEncoding = TdxRichEditOptionsAction.AutoDetectEncoding;
      DefaultTableCellMargin = TdxRichEditOptionsAction.DefaultTableCellMargin;
      DefaultTableCellSpacing = TdxRichEditOptionsAction.DefaultTableCellSpacing;
      IgnoreFloatProperty = TdxRichEditOptionsAction.IgnoreFloatProperty;
      IgnoreMetaCharset = TdxRichEditOptionsAction.IgnoreMetaCharset;
      ReplaceSpaceWithNonBreakingSpaceInsidePre = TdxRichEditOptionsAction.ReplaceSpaceWithNonBreakingSpaceInsidePre;
    end;
  strict private
    FAsyncImageLoading: Boolean;
    FAutoDetectEncoding: Boolean;
    FDefaultAsyncImageLoading: Boolean;
    FDefaultTableCellMargin: Integer;
    FDefaultTableCellSpacing: Integer;
    FIgnoreFloatProperty: Boolean;
    FIgnoreMetaCharset: Boolean;
    FReplaceSpaceWithNonBreakingSpaceInsidePre: Boolean;
    procedure SetAsyncImageLoading(const AValue: Boolean);
    procedure SetAutoDetectEncoding(const AValue: Boolean);
    procedure SetDefaultTableCellMargin(const AValue: Integer);
    procedure SetDefaultTableCellSpacing(const AValue: Integer);
    procedure SetIgnoreFloatProperty(const AValue: Boolean);
    procedure SetIgnoreMetaCharset(const AValue: Boolean);
    procedure SetReplaceSpaceWithNonBreakingSpaceInsidePre(const AValue: Boolean);
    function GetEncoding: Word;
    procedure SetEncoding(const AValue: Word);
  protected
    procedure DoReset; override;

  public
    function Format: TdxRichEditDocumentFormat; override;
    procedure CopyFrom(const AValue: TObject); override;
    function IsDefaultEncoding: Boolean; override;

    property DefaultAsyncImageLoading: Boolean read FDefaultAsyncImageLoading write FDefaultAsyncImageLoading;
    property DefaultTableCellSpacing: Integer read FDefaultTableCellSpacing write SetDefaultTableCellSpacing;
    property DefaultTableCellMarging: Integer read FDefaultTableCellMargin write SetDefaultTableCellMargin;
  published
    property AsyncImageLoading: Boolean read FAsyncImageLoading write SetAsyncImageLoading default True;
    property AutoDetectEncoding: Boolean read FAutoDetectEncoding write SetAutoDetectEncoding default True;
    property Encoding: Word read GetEncoding write SetEncoding default CP_UTF8;
    property IgnoreFloatProperty: Boolean read FIgnoreFloatProperty write SetIgnoreFloatProperty default False;
    property IgnoreMetaCharset: Boolean read FIgnoreMetaCharset write SetIgnoreMetaCharset default False;
    property ReplaceSpaceWithNonBreakingSpaceInsidePre: Boolean
      read FReplaceSpaceWithNonBreakingSpaceInsidePre write SetReplaceSpaceWithNonBreakingSpaceInsidePre default False;
  end;


  { TdxMhtDocumentImporterOptions }

  TdxMhtDocumentImporterOptions = class(TdxHtmlDocumentImporterOptions)
  public
    function Format: TdxRichEditDocumentFormat; override;
  end;

  { TdxOpenDocumentImporterOptions }

  TdxOpenDocumentImporterOptions = class(TdxDocumentImporterOptions)
  public
    function Format: TdxRichEditDocumentFormat; override;
  end;

  { TdxWordMLDocumentImporterOptions }

  TdxWordMLDocumentImporterOptions = class(TdxXmlBasedDocumentImporterOptions)
  protected
    procedure DoReset; override;
  public
    function Format: TdxRichEditDocumentFormat; override;
  end;

  { TdxDocDocumentImporterOptions }

  TdxDocDocumentImporterOptions = class(TdxDocumentImporterOptions)
  public const
    DefaultIgnoreDeletedText               = True;
    DefaultKeepBookmarksForRemovedRanges   = True;
    DefaultKeepPermissionsForRemovedRanges = True;
  public type
    TAction = class sealed
    public const
      IgnoreDeletedText = TdxRichEditOptionsAction.IgnoreDeletedText;
      KeepBookmarksForRemovedRanges = TdxRichEditOptionsAction.KeepBookmarksForRemovedRanges;
      KeepPermissionsForRemovedRanges = TdxRichEditOptionsAction.KeepPermissionsForRemovedRanges;
    end;
  strict private
    FIgnoreDeletedText: Boolean;
    FKeepBookmarksForRemovedRanges: Boolean;
    FKeepPermissionsForRemovedRanges: Boolean;
    procedure SetIgnoreDeletedText(const AValue: Boolean);
    procedure SetKeepBookmarksForRemovedRanges(const AValue: Boolean);
    procedure SetKeepPermissionsForRemovedRanges(const AValue: Boolean);
  protected
    procedure DoReset; override;
  public
    procedure CopyFrom(const AValue: TObject); override;
    function Format: TdxRichEditDocumentFormat; override;
  published
    property IgnoreDeletedText: Boolean read FIgnoreDeletedText write SetIgnoreDeletedText default DefaultIgnoreDeletedText;
    property KeepBookmarksForRemovedRanges: Boolean read FKeepBookmarksForRemovedRanges write SetKeepBookmarksForRemovedRanges default DefaultKeepBookmarksForRemovedRanges;
    property KeepPermissionsForRemovedRanges: Boolean read FKeepPermissionsForRemovedRanges write SetKeepPermissionsForRemovedRanges default DefaultKeepPermissionsForRemovedRanges;
  end;

  { TdxRichEditDocumentImportOptions }

  TdxRichEditDocumentImportOptions = class(TdxRichEditNotificationOptions)
  private const
    FallbackFormatValue = TdxRichEditDocumentFormat.PlainText;
  protected type
    TAction = class sealed
    public const
      FallbackFormat = TdxRichEditOptionsAction.FallbackFormat;
    end;
    TDocumentImporterOptionsTable = array[TdxRichEditDocumentFormat] of TdxDocumentImporterOptions;
  strict private
    FRtf: TdxRtfDocumentImporterOptions;
    FPlainText: TdxPlainTextDocumentImporterOptions;
    FHtml: TdxHtmlDocumentImporterOptions;
    FDoc: TdxDocDocumentImporterOptions;
    FMht: TdxMhtDocumentImporterOptions;
    FOpenXml: TdxOpenXmlDocumentImporterOptions;
    FOpenDocument: TdxOpenDocumentImporterOptions;
    FWordML: TdxWordMLDocumentImporterOptions;
    FFallbackFormat: TdxRichEditDocumentFormat;
    FOptionsTable: TDocumentImporterOptionsTable;
    procedure SetFallbackFormat(const Value: TdxRichEditDocumentFormat);
    procedure SetHtml(const Value: TdxHtmlDocumentImporterOptions);
    procedure SetMht(const Value: TdxMhtDocumentImporterOptions);
    procedure SetOpenDocument(const Value: TdxOpenDocumentImporterOptions);
    procedure SetOpenXml(const Value: TdxOpenXmlDocumentImporterOptions);
    procedure SetPlainText(const Value: TdxPlainTextDocumentImporterOptions);
    procedure SetRtf(const Value: TdxRtfDocumentImporterOptions);
    procedure SetWordML(const Value: TdxWordMLDocumentImporterOptions);
    procedure SetDoc(const Value: TdxDocDocumentImporterOptions);
  protected
    procedure CreateInnerOptions; override;

    function ShouldSerializeFallbackFormat: Boolean;
    procedure ResetFallbackFormat;
    procedure RegisterOptions(AOptions: TdxDocumentImporterOptions);
    procedure DoReset; override;
    procedure CopyFrom(AOptions: TdxRichEditDocumentImportOptions);
    function CreateDocOptions: TdxDocDocumentImporterOptions;
    function CreateRtfOptions: TdxRtfDocumentImporterOptions;
    function CreatePlainTextOptions: TdxPlainTextDocumentImporterOptions;
    function CreateHtmlOptions: TdxHtmlDocumentImporterOptions;
    function CreateMhtOptions: TdxMhtDocumentImporterOptions;
    function CreateOpenXmlOptions: TdxOpenXmlDocumentImporterOptions;
    function CreateOpenDocumentOptions: TdxOpenDocumentImporterOptions;
    function CreateWordMLOptions: TdxWordMLDocumentImporterOptions;

    property OptionsTable: TDocumentImporterOptionsTable read FOptionsTable;
  public
    destructor Destroy; override;

    function GetOptions(AFormat: TdxRichEditDocumentFormat): TdxDocumentImporterOptions; virtual;

    property Mht: TdxMhtDocumentImporterOptions read FMht write SetMht;
    property OpenDocument: TdxOpenDocumentImporterOptions read FOpenDocument write SetOpenDocument;
    property WordML: TdxWordMLDocumentImporterOptions read FWordML write SetWordML;

    property FallbackFormat: TdxRichEditDocumentFormat read FFallbackFormat write SetFallbackFormat;
  published
    property Doc: TdxDocDocumentImporterOptions read FDoc write SetDoc;
    property Html: TdxHtmlDocumentImporterOptions read FHtml write SetHtml;
    property OpenXml: TdxOpenXmlDocumentImporterOptions read FOpenXml write SetOpenXml;
    property PlainText: TdxPlainTextDocumentImporterOptions read FPlainText write SetPlainText;
    property Rtf: TdxRtfDocumentImporterOptions read FRtf write SetRtf;
  end;

  { TdxDocumentExporterOptions }

  TdxDocumentExporterOptions = class abstract(TdxRichEditNotificationOptions, IdxExporterOptions)
  protected type
    TAction = class sealed
    public const
      Encoding = TdxDocumentImporterOptions.TAction.Encoding;
    end;
  private
    FActualEncoding: TEncoding;
    FTargetUri: string;
  protected
    function ShouldSerializeTargetUri: Boolean; virtual;
    procedure DoReset; override;

    function GetActualEncoding: TEncoding; virtual;
    procedure SetActualEncoding(const Value: TEncoding); virtual;

    procedure CopyFrom(const Source: TObject); overload;

    //IdxExporterOptions
    function GetTargetUri: string;
    procedure SetTargetUri(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    procedure CopyFrom(const AValue: IdxExporterOptions); overload; virtual;
    function Format: TdxRichEditDocumentFormat; virtual; abstract;
    function IsDefaultEncoding: Boolean; virtual;

    property ActualEncoding: TEncoding read GetActualEncoding write SetActualEncoding;
    property TargetUri: string read FTargetUri write FTargetUri;
  end;

  { TdxRtfRunBackColorExportMode }

  TdxRtfRunBackColorExportMode = (
    Chcbpat,
    Highlight,
    Both
  );

  { TdxRtfDocumentExporterCompatibilityOptions }

  TdxRtfDocumentExporterCompatibilityOptions = class(TdxRichEditNotificationOptions)
  protected type
    TAction = class sealed
    public const
      BackColorExportMode       = TdxRichEditOptionsAction.BackColorExportMode;
      DuplicateObjectAsMetafile = TdxRichEditOptionsAction.DuplicateObjectAsMetafile;
    end;
  private
    FBackColorExportMode: TdxRtfRunBackColorExportMode;
    FDuplicateObjectAsMetafile: Boolean;
    FKerning: Boolean;
    procedure SetBackColorExportMode(const Value: TdxRtfRunBackColorExportMode);
    procedure SetDuplicateObjectAsMetafile(const Value: Boolean);
  protected
    function ShouldSerializeDuplicateObjectAsMetafile: Boolean;
    procedure ResetDuplicateObjectAsMetafile;
    function ShouldSerializeBackColorExportMode: Boolean;
    procedure ResetBackColorExportMode;
    procedure DoReset; override;
    property Kerning: Boolean read FKerning write FKerning;
  public
    procedure CopyFrom(const AValue: TdxRtfDocumentExporterCompatibilityOptions); reintroduce;
  published
    property BackColorExportMode: TdxRtfRunBackColorExportMode read FBackColorExportMode write SetBackColorExportMode default TdxRtfRunBackColorExportMode.Chcbpat;
    property DuplicateObjectAsMetafile: Boolean read FDuplicateObjectAsMetafile write SetDuplicateObjectAsMetafile default False;
  end;

  { TdxExportFinalParagraphMark }

  TdxExportFinalParagraphMark = (
    Always,
    Never,
    SelectedOnly
  );

  { TdxRtfNumberingListExportFormat }

  TdxRtfNumberingListExportFormat = (
    PlainTextFormat,
    RtfFormat
  );

  { TdxRtfDocumentExporterOptions }

  TdxRtfDocumentExporterOptions = class(TdxDocumentExporterOptions)
  protected type
    TAction = class sealed
    public const
      ExportFinalParagraphMark  = TdxRichEditOptionsAction.ExportFinalParagraphMark;
      ListExportFormat          = TdxRichEditOptionsAction.ListExportFormat;
      WrapContentInGroup        = TdxRichEditOptionsAction.WrapContentInGroup;
    end;
  private
    FWrapContentInGroup: Boolean;
    FCompatibility: TdxRtfDocumentExporterCompatibilityOptions;
    FExportFinalParagraphMark: TdxExportFinalParagraphMark;
    FListExportFormat: TdxRtfNumberingListExportFormat;
    procedure SetWrapContentInGroup(const Value: Boolean);
    procedure SetListExportFormat(const Value: TdxRtfNumberingListExportFormat);
    procedure SetExportFinalParagraphMark(const Value: TdxExportFinalParagraphMark);
    procedure SetCompatibility(const Value: TdxRtfDocumentExporterCompatibilityOptions);
  protected
    function ShouldSerializeWrapContentInGroup: Boolean;
    procedure ResetWrapContentInGroup;
    function ShouldSerializeListExportFormat: Boolean;
    procedure ResetListExportFormat;
    function ShouldSerializeExportFinalParagraphMark: Boolean;
    procedure ResetExportFinalParagraphMark;
    procedure DoReset; override;
    function CreateCompatibilityOptions: TdxRtfDocumentExporterCompatibilityOptions;
    procedure SetActualEncoding(const Value: TEncoding); override;
    procedure CreateInnerOptions; override;
  public
    destructor Destroy; override;

    procedure CopyFrom(const Value: IdxExporterOptions); override;
    function Format: TdxRichEditDocumentFormat; override;
  published
    property WrapContentInGroup: Boolean read FWrapContentInGroup write SetWrapContentInGroup default False;
    property ListExportFormat: TdxRtfNumberingListExportFormat read FListExportFormat write SetListExportFormat default TdxRtfNumberingListExportFormat.RtfFormat;
    property ExportFinalParagraphMark: TdxExportFinalParagraphMark read FExportFinalParagraphMark write SetExportFinalParagraphMark default TdxExportFinalParagraphMark.Always;
    property Compatibility: TdxRtfDocumentExporterCompatibilityOptions read FCompatibility write SetCompatibility;
  end;

  { TdxPlainTextDocumentExporterOptions }

  TdxPlainTextDocumentExporterOptions = class(TdxDocumentExporterOptions)
  protected type
    TAction = class sealed
    public const
      ExportBulletsAndNumbering = TdxRichEditOptionsAction.ExportBulletsAndNumbering;
      ExportFinalParagraphMark = TdxRichEditOptionsAction.ExportFinalParagraphMark;
      ExportHiddenText = TdxRichEditOptionsAction.ExportHiddenText;
      FieldCodeStartMarker = TdxRichEditOptionsAction.FieldCodeStartMarker;
      FieldCodeEndMarker = TdxRichEditOptionsAction.FieldCodeEndMarker;
      FieldResultEndMarker = TdxRichEditOptionsAction.FieldResultEndMarker;
      FootNoteNumberStringFormat = TdxRichEditOptionsAction.FootNoteNumberStringFormat;
      EndNoteNumberStringFormat = TdxRichEditOptionsAction.EndNoteNumberStringFormat;
      FootNoteSeparator = TdxRichEditOptionsAction.FootNoteSeparator;
      EndNoteSeparator = TdxRichEditOptionsAction.EndNoteSeparator;
    end;
  public const
    DefaultFootNoteSeparator = '------------------------------------------------------------';
    DefaultEndNoteSeparator = DefaultFootNoteSeparator;
    DefaultExportFinalParagraphMark = TdxExportFinalParagraphMark.Never;
  strict private
    FExportBulletsAndNumbering: Boolean;
    FExportFinalParagraphMark: TdxExportFinalParagraphMark;
    FExportHiddenText: Boolean;
    FFieldCodeStartMarker: string;
    FFieldCodeEndMarker: string;
    FFieldResultEndMarker: string;
    FFootNoteNumberStringFormat: string;
    FEndNoteNumberStringFormat: string;
    FFootNoteSeparator: string;
    FEndNoteSeparator: string;
    FWriteEncodingPreamble: Boolean;
    function GetActualEndNoteNumberStringFormat: string;
    function GetActualEndNoteSeparator: string;
    function GetActualFootNoteNumberStringFormat: string;
    function GetActualFootNoteSeparator: string;
    function GetEncoding: Word;
    procedure SetEndNoteNumberStringFormat(const Value: string);
    procedure SetExportBulletsAndNumbering(const Value: Boolean);
    procedure SetExportFinalParagraphMark(const Value: TdxExportFinalParagraphMark);
    procedure SetExportHiddenText(const Value: Boolean);
    procedure SetFieldCodeEndMarker(const Value: string);
    procedure SetFieldCodeStartMarker(const Value: string);
    procedure SetFieldResultEndMarker(const Value: string);
    procedure SetFootNoteNumberStringFormat(const Value: string);
    procedure SetFootNoteSeparator(const Value: string);
    procedure SetEncoding(const Value: Word);
  protected
    procedure DoReset; override;
  public
    procedure CopyFrom(const Value: IdxExporterOptions); override;
    function Format: TdxRichEditDocumentFormat; override;
    function IsDefaultEncoding: Boolean; override;

    property ActualFootNoteSeparator: string read GetActualFootNoteSeparator;
    property ActualEndNoteSeparator: string read GetActualEndNoteSeparator;

    property ActualEndNoteNumberStringFormat: string read GetActualEndNoteNumberStringFormat;
    property ActualFootNoteNumberStringFormat: string read GetActualFootNoteNumberStringFormat;
    property WriteEncodingPreamble: Boolean read FWriteEncodingPreamble write FWriteEncodingPreamble;
  published
    property Encoding: Word read GetEncoding write SetEncoding default CP_ACP;
    property EndNoteNumberStringFormat: string read FEndNoteNumberStringFormat write SetEndNoteNumberStringFormat;
    property EndNoteSeparator: string read FEndNoteSeparator write FEndNoteSeparator;
    property ExportBulletsAndNumbering: Boolean read FExportBulletsAndNumbering write SetExportBulletsAndNumbering default True;
    property ExportFinalParagraphMark: TdxExportFinalParagraphMark read FExportFinalParagraphMark write SetExportFinalParagraphMark default DefaultExportFinalParagraphMark;
    property ExportHiddenText: Boolean read FExportHiddenText write SetExportHiddenText default False;
    property FieldCodeEndMarker: string read FFieldCodeEndMarker write SetFieldCodeEndMarker;
    property FieldCodeStartMarker: string read FFieldCodeStartMarker write SetFieldCodeStartMarker;
    property FieldResultEndMarker: string read FFieldResultEndMarker write SetFieldResultEndMarker;
    property FootNoteNumberStringFormat: string read FFootNoteNumberStringFormat write SetFootNoteNumberStringFormat;
    property FootNoteSeparator: string read FFootNoteSeparator write SetFootNoteSeparator;
  end;

  { TdxHtmlDocumentExporterOptions }

  TdxCssPropertiesExportType = (
    Style,
    Link,
    &Inline);

  TdxExportImageSize = (Auto,
    Always);

  TdxExportRootTag = (
    Html,
    Body);

  TdxHtmlFontUnit = (
    Point,
    Pixel);

  TdxHtmlNumberingListExportFormat = (
    PlainTextFormat,
    HtmlFormat);

  TdxUriExportType = (
    Relative,
    Absolute);

  TdxHtmlDocumentExporterOptions = class(TdxDocumentExporterOptions)
  public const
    DefaultOverrideImageResolution = 96;
  protected type
  {$REGION 'protected type'}
    TAction = class sealed
    public const
      CssPropertiesExportType = TdxRichEditOptionsAction.CssPropertiesExportType;
      DefaultCharacterPropertiesExportToCss = TdxRichEditOptionsAction.DefaultCharacterPropertiesExportToCss;
      EmbedImages = TdxRichEditOptionsAction.EmbedImages;
      EndNoteNamePrefix = TdxRichEditOptionsAction.EndNoteNamePrefix;
      EndNoteNumberStringFormat = TdxRichEditOptionsAction.EndNoteNumberStringFormat;
      ExportImageSize = TdxRichEditOptionsAction.ExportImageSize;
      ExportRootTag = TdxRichEditOptionsAction.ExportRootTag;
      FontUnit = TdxRichEditOptionsAction.FontUnit;
      FootNoteNamePrefix = TdxRichEditOptionsAction.FootNoteNamePrefix;
      FootNoteNumberStringFormat = TdxRichEditOptionsAction.FootNoteNumberStringFormat;
      HtmlNumberingListExportFormat = TdxRichEditOptionsAction.HtmlNumberingListExportFormat;
      IgnoreParagraphOutlineLevel = TdxRichEditOptionsAction.IgnoreParagraphOutlineLevel;
      KeepExternalImageSize = TdxRichEditOptionsAction.KeepExternalImageSize;
      OverrideImageResolution = TdxRichEditOptionsAction.OverrideImageResolution;
      TabMarker = TdxRichEditOptionsAction.TabMarker;
      UnderlineTocHyperlinks = TdxRichEditOptionsAction.UnderlineTocHyperlinks;
      UriExportType = TdxRichEditOptionsAction.UriExportType;
      UseHtml5 = TdxRichEditOptionsAction.UseHtml5;
    end;
  {$ENDREGION}
  strict private
    FCssPropertiesExportType: TdxCssPropertiesExportType;
    FDefaultCharacterPropertiesExportToCss: Boolean;
    FDisposeConvertedImagesImmediately: Boolean;
    FEmbedImages: Boolean;
    FEndNoteNamePrefix: string;
    FEndNoteNumberStringFormat: string;
    FExportImageSize: TdxExportImageSize;
    FExportRootTag: TdxExportRootTag;
    FExportToClipboard: Boolean;
    FFontUnit: TdxHtmlFontUnit;
    FFootNoteNamePrefix: string;
    FFootNoteNumberStringFormat: string;
    FHtmlNumberingListExportFormat: TdxHtmlNumberingListExportFormat;
    FIgnoreParagraphOutlineLevel: Boolean;
    FKeepExternalImageSize: Boolean;
    FOverrideImageResolution: Integer;
    FTabMarker: string;
    FUnderlineTocHyperlinks: Boolean;
    FUriExportType: TdxUriExportType;
    FUseHtml5: Boolean;
    function GetActualEndNoteNamePrefix: string;
    function GetActualEndNoteNumberStringFormat: string;
    function GetActualFootNoteNamePrefix: string;
    function GetActualFootNoteNumberStringFormat: string;
    function GetEncoding: Word;
    function IsTabMarkerStored: Boolean;
    procedure SetCssPropertiesExportType(const Value: TdxCssPropertiesExportType);
    procedure SetDisposeConvertedImagesImmediately(const Value: Boolean);
    procedure SetEncoding(const Value: Word);
    procedure SetEndNoteNamePrefix(const Value: string);
    procedure SetEndNoteNumberStringFormat(const Value: string);
    procedure SetExportImageSize(const Value: TdxExportImageSize);
    procedure SetExportRootTag(const Value: TdxExportRootTag);
    procedure SetFontUnit(const Value: TdxHtmlFontUnit);
    procedure SetFootNoteNamePrefix(const Value: string);
    procedure SetFootNoteNumberStringFormat(const Value: string);
    procedure SetHtmlNumberingListExportFormat(const Value: TdxHtmlNumberingListExportFormat);
    procedure SetIgnoreParagraphOutlineLevel(const Value: Boolean);
    procedure SetKeepExternalImageSize(const Value: Boolean);
    procedure SetOverrideImageResolution(const Value: Integer);
    procedure SetTabMarker(const Value: string);
    procedure SetUriExportType(const Value: TdxUriExportType);
    procedure SetUseHtml5(const Value: Boolean);
  protected
    procedure DoReset; override;

    function GetDefaultCharacterPropertiesExportToCss: Boolean;
    function GetEmbedImages: Boolean;
    function GetUnderlineTocHyperlinks: Boolean;
    function ShouldSerializeCssPropertiesExportType: Boolean;
    function ShouldSerializeEncoding: Boolean;
    function ShouldSerializeExportImageSize: Boolean;
    function ShouldSerializeExportRootTag: Boolean;
    function ShouldSerializeHtmlNumberingListExportFormat: Boolean;
    function ShouldSerializeOverrideImageResolution: Boolean;
    function ShouldSerializeUriExportType: Boolean;
    procedure ResetCssPropertiesExportType;
    procedure ResetEncoding;
    procedure ResetExportImageTag;
    procedure ResetExportRootTag;
    procedure ResetHtmlNumberingListExportFormat;
    procedure ResetOverrideImageResolution;
    procedure ResetUriExportType;
    procedure SetDefaultCharacterPropertiesExportToCss(const Value: Boolean);
    procedure SetEmbedImages(const Value: Boolean);
    procedure SetUnderlineTocHyperlinks(const Value: Boolean);

    property ExportToClipboard: Boolean read FExportToClipboard write FExportToClipboard;
  public
    procedure CopyFrom(const Value: IdxExporterOptions); override;
    function Format: TdxRichEditDocumentFormat; override;
    function IsDefaultEncoding: Boolean; override;

    property ActualEndNoteNamePrefix: string read GetActualEndNoteNamePrefix;
    property ActualEndNoteNumberStringFormat: string read GetActualEndNoteNumberStringFormat;
    property ActualFootNoteNamePrefix: string read GetActualFootNoteNamePrefix;
    property ActualFootNoteNumberStringFormat: string read GetActualFootNoteNumberStringFormat;
    property DisposeConvertedImagesImmediately: Boolean read FDisposeConvertedImagesImmediately write SetDisposeConvertedImagesImmediately;
  published
    property CssPropertiesExportType: TdxCssPropertiesExportType read FCssPropertiesExportType write SetCssPropertiesExportType default TdxCssPropertiesExportType.Style;
    property DefaultCharacterPropertiesExportToCss: Boolean read GetDefaultCharacterPropertiesExportToCss write SetDefaultCharacterPropertiesExportToCss default True;
    property EmbedImages: Boolean read GetEmbedImages write SetEmbedImages default False;
    property Encoding: Word read GetEncoding write SetEncoding default CP_UTF8;
    property EndNoteNamePrefix: string read FEndNoteNamePrefix write SetEndNoteNamePrefix;
    property EndNoteNumberStringFormat: string read FEndNoteNumberStringFormat write SetEndNoteNumberStringFormat;
    property ExportImageSize: TdxExportImageSize read FExportImageSize write SetExportImageSize default TdxExportImageSize.Auto;
    property ExportRootTag: TdxExportRootTag read FExportRootTag write SetExportRootTag default TdxExportRootTag.Html;
    property FontUnit: TdxHtmlFontUnit read FFontUnit write SetFontUnit default TdxHtmlFontUnit.Point;
    property FootNoteNamePrefix: string read FFootNoteNamePrefix write SetFootNoteNamePrefix;
    property FootNoteNumberStringFormat: string read FFootNoteNumberStringFormat write SetFootNoteNumberStringFormat;
    property HtmlNumberingListExportFormat: TdxHtmlNumberingListExportFormat read FHtmlNumberingListExportFormat write SetHtmlNumberingListExportFormat default TdxHtmlNumberingListExportFormat.HtmlFormat;
    property IgnoreParagraphOutlineLevel: Boolean read FIgnoreParagraphOutlineLevel write SetIgnoreParagraphOutlineLevel default False;
    property KeepExternalImageSize: Boolean read FKeepExternalImageSize write SetKeepExternalImageSize default False;
    property OverrideImageResolution: Integer read FOverrideImageResolution write SetOverrideImageResolution default DefaultOverrideImageResolution;
    property TabMarker: string read FTabMarker write SetTabMarker stored IsTabMarkerStored;
    property UnderlineTocHyperlinks: Boolean read GetUnderlineTocHyperlinks write SetUnderlineTocHyperlinks default True;
    property UriExportType: TdxUriExportType read FUriExportType write SetUriExportType default TdxUriExportType.Relative;
    property UseHtml5: Boolean read FUseHtml5 write SetUseHtml5 default False;
  end;

  { TdxMhtDocumentExporterOptions }

  TdxMhtDocumentExporterOptions = class(TdxHtmlDocumentExporterOptions)
  public
    function Format: TdxRichEditDocumentFormat; override;
  end;

  { TdxOpenXmlDocumentExporterOptions }

  TdxOpenXmlDocumentExporterOptions = class(TdxDocumentExporterOptions)
  protected type
    TAction = class sealed
    public const
      AllowAlternateStyleNames = TdxRichEditOptionsAction.AllowAlternateStyleNames;
      AlternateImageFolder = TdxRichEditOptionsAction.AlternateImageFolder;
      LimitBookmarkNameTo40Chars = TdxRichEditOptionsAction.LimitBookmarkNameTo40Chars;
      LimitStyleNameTo253Chars = TdxRichEditOptionsAction.LimitStyleNameTo253Chars;
      LimitFontNameTo31Chars = TdxRichEditOptionsAction.LimitFontNameTo31Chars;
    end;
  public const
    DefaultAllowAlternateStyleNames = True;
    DefaultAlternateImageFolder = False;
    DefaultLimitBookmarkNameTo40Chars = True;
    DefaultLimitStyleNameTo253Chars = True;
    DefaultLimitFontNameTo31Chars = True;
  strict private
    FAlternateImageFolder: Boolean;
    FLimitBookmarkNameTo40Chars: Boolean;
    FLimitStyleNameTo253Chars: Boolean;
    FLimitFontNameTo31Chars: Boolean;
    FAllowAlternateStyleNames: Boolean;
    procedure SetAllowAlternateStyleNames(const AValue: Boolean);
    procedure SetAlternateImageFolder(const AValue: Boolean);
    procedure SetLimitBookmarkNameTo40Chars(const AValue: Boolean);
    procedure SetLimitStyleNameTo253Chars(const AValue: Boolean);
    procedure SetLimitFontNameTo31Chars(const AValue: Boolean);
  protected
    procedure DoReset; override;
  public
    procedure CopyFrom(const Value: IdxExporterOptions); override;
    function Format: TdxRichEditDocumentFormat; override;
  published
    property AllowAlternateStyleNames: Boolean read FAllowAlternateStyleNames write SetAllowAlternateStyleNames default DefaultAllowAlternateStyleNames;
    property AlternateImageFolder: Boolean read FAlternateImageFolder write SetAlternateImageFolder default DefaultAlternateImageFolder;
    property LimitBookmarkNameTo40Chars: Boolean read FLimitBookmarkNameTo40Chars write SetLimitBookmarkNameTo40Chars default DefaultLimitBookmarkNameTo40Chars;
    property LimitStyleNameTo253Chars: Boolean read FLimitStyleNameTo253Chars write SetLimitStyleNameTo253Chars default DefaultLimitStyleNameTo253Chars;
    property LimitFontNameTo31Chars: Boolean read FLimitFontNameTo31Chars write SetLimitFontNameTo31Chars default DefaultLimitFontNameTo31Chars;
  end;

  { TdxOpenDocumentExporterOptions }

  TdxOpenDocumentExporterOptions = class(TdxDocumentExporterOptions)
  public
    function Format: TdxRichEditDocumentFormat; override;
  end;

  { TdxWordMLDocumentExporterOptions }

  TdxWordMLDocumentExporterOptions = class(TdxDocumentExporterOptions)
  public
    function Format: TdxRichEditDocumentFormat; override;
  end;

  { TdxDocDocumentExporterCompatibilityOptions  }

  TdxDocDocumentExporterCompatibilityOptions = class(TdxRichEditNotificationOptions)
  protected type
    TAction = class sealed
    public const
      AllowNonLinkedListDefinitions = TdxRichEditOptionsAction.AllowNonLinkedListDefinitions;
    end;
  strict private
    FAllowNonLinkedListDefinitions: Boolean;
    procedure SetAllowNonLinkedListDefinitions(const AValue: Boolean);
  protected
    procedure ResetAllowNonLinkedListDefinitions; virtual;
    procedure DoReset; override;
  public
    procedure CopyFrom(const AValue: TdxDocDocumentExporterCompatibilityOptions); reintroduce;
  published
    property AllowNonLinkedListDefinitions: Boolean read FAllowNonLinkedListDefinitions write SetAllowNonLinkedListDefinitions default False;
  end;

  { TdxDocDocumentExporterOptions }

  TdxDocDocumentExporterOptions = class(TdxDocumentExporterOptions)
  strict private
    FCompatibility: TdxDocDocumentExporterCompatibilityOptions;
  private
    procedure SetCompatibility(const AValue: TdxDocDocumentExporterCompatibilityOptions);
  protected
    function CreateCompatibilityOptions: TdxDocDocumentExporterCompatibilityOptions; virtual;
    procedure DoReset; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CopyFrom(const AValue: IdxExporterOptions); override;
    function Format: TdxRichEditDocumentFormat; override;
  published
    property Compatibility: TdxDocDocumentExporterCompatibilityOptions read FCompatibility write SetCompatibility;
  end;

  { TdxRichEditDocumentExportOptions }

  TdxRichEditDocumentExportOptions = class(TdxRichEditNotificationOptions)
  strict private type
    TDocumentExporterOptionsTable = array[TdxRichEditDocumentFormat] of TdxDocumentExporterOptions;
  strict private
    FRtf: TdxRtfDocumentExporterOptions;
    FPlainText: TdxPlainTextDocumentExporterOptions;
    FHtml: TdxHtmlDocumentExporterOptions;
    FMht: TdxMhtDocumentExporterOptions;
    FOpenXml: TdxOpenXmlDocumentExporterOptions;
    FOpenDocument: TdxOpenDocumentExporterOptions;
    FWordML: TdxWordMLDocumentExporterOptions;
    FDoc: TdxDocDocumentExporterOptions;
    FOptionsTable: TDocumentExporterOptionsTable;
    procedure SetDoc(const Value: TdxDocDocumentExporterOptions);
    procedure SetHtml(const Value: TdxHtmlDocumentExporterOptions);
    procedure SetMht(const Value: TdxMhtDocumentExporterOptions);
    procedure SetOpenDocument(const Value: TdxOpenDocumentExporterOptions);
    procedure SetOpenXml(const Value: TdxOpenXmlDocumentExporterOptions);
    procedure SetPlainText(const Value: TdxPlainTextDocumentExporterOptions);
    procedure SetRtf(const Value: TdxRtfDocumentExporterOptions);
    procedure SetWordML(const Value: TdxWordMLDocumentExporterOptions);
  protected
    procedure RegisterOptions(AOptions: TdxDocumentExporterOptions);
    procedure DoReset; override;

    function CreateRtfOptions: TdxRtfDocumentExporterOptions;
    function CreatePlainTextOptions: TdxPlainTextDocumentExporterOptions;
    function CreateHtmlOptions: TdxHtmlDocumentExporterOptions;
    function CreateMhtOptions: TdxMhtDocumentExporterOptions;
    function CreateOpenXmlOptions: TdxOpenXmlDocumentExporterOptions;
    function CreateOpenDocumentOptions: TdxOpenDocumentExporterOptions;
    function CreateWordMLOptions: TdxWordMLDocumentExporterOptions;
    function CreateDocOptions: TdxDocDocumentExporterOptions;
    procedure CreateInnerOptions; override;

    property OptionsTable: TDocumentExporterOptionsTable read FOptionsTable;
  public
    destructor Destroy; override;

    procedure CopyFrom(AOptions: TdxRichEditDocumentExportOptions);
    function GetOptions(AFormat: TdxRichEditDocumentFormat): TdxDocumentExporterOptions; virtual;

    property Mht: TdxMhtDocumentExporterOptions read FMht write SetMht;
    property OpenDocument: TdxOpenDocumentExporterOptions read FOpenDocument write SetOpenDocument;
    property WordML: TdxWordMLDocumentExporterOptions read FWordML write SetWordML;
  published
    property Doc: TdxDocDocumentExporterOptions read FDoc write SetDoc;
    property Html: TdxHtmlDocumentExporterOptions read FHtml write SetHtml;
    property Rtf: TdxRtfDocumentExporterOptions read FRtf write SetRtf;
    property PlainText: TdxPlainTextDocumentExporterOptions read FPlainText write SetPlainText;
    property OpenXml: TdxOpenXmlDocumentExporterOptions read FOpenXml write SetOpenXml;
  end;

  { TdxCopyPasteOptions }

  TdxInsertOptions = TdxRichEditInsertOptions;

  TdxCopyPasteOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      InsertOptions                   = TdxRichEditOptionsAction.InsertOptions;
      MaintainDocumentSectionSettings = TdxRichEditOptionsAction.MaintainDocumentSectionSettings;
    end;
  protected const
    DefaultInsertOptions = TdxInsertOptions.MatchDestinationFormatting;
  private
    FInsertOptions: TdxInsertOptions;
    FMaintainDocumentSectionSettings: Boolean;
    procedure SetInsertOptions(const Value: TdxInsertOptions);
    procedure SetMaintainDocumentSectionSettings(const Value: Boolean);
  protected
    procedure DoReset; override;
  published
    property InsertOptions: TdxInsertOptions read FInsertOptions write SetInsertOptions default DefaultInsertOptions;
    property MaintainDocumentSectionSettings: Boolean read FMaintainDocumentSectionSettings write SetMaintainDocumentSectionSettings default False;
  end;

  { TdxFormattingMarkVisibilityOptions }

  TdxFormattingMarkVisibilityOptions = class(TdxSimpleFormattingMarkVisibilityOptions)
  public type
    TAction = class sealed
    public const
      Separator       = TdxRichEditOptionsAction.Separator;
      TabCharacter    = TdxRichEditOptionsAction.TabCharacter;
    end;
  protected const
    DefaultTabCharacterVisibility = TdxRichEditFormattingMarkVisibility.Auto;
    DefaultSeparatorVisibility = TdxRichEditFormattingMarkVisibility.Auto;
  private
    FTabCharacter: TdxRichEditFormattingMarkVisibility;
    FSeparator: TdxRichEditFormattingMarkVisibility;
    procedure SetTabCharacter(const Value: TdxRichEditFormattingMarkVisibility);
    procedure SetSeparator(const Value: TdxRichEditFormattingMarkVisibility);
  protected
    procedure DoReset; override;
  published
    property TabCharacter: TdxRichEditFormattingMarkVisibility read FTabCharacter write SetTabCharacter default DefaultTabCharacterVisibility;
    property Separator: TdxRichEditFormattingMarkVisibility read FSeparator write SetSeparator default DefaultSeparatorVisibility;
  end;

  { TdxNumberingOptions }

  TdxNumberingOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      Bulleted    = TdxRichEditOptionsAction.Bulleted;
      Simple      = TdxRichEditOptionsAction.Simple;
      MultiLevel  = TdxRichEditOptionsAction.MultiLevel;
    end;
  private
    FBulleted: TdxDocumentCapability;
    FSimple: TdxDocumentCapability;
    FMultiLevel: TdxDocumentCapability;
    procedure SetBulleted(const Value: TdxDocumentCapability);
    procedure SetSimple(const Value: TdxDocumentCapability);
    procedure SetMultiLevel(const Value: TdxDocumentCapability);
    function IsAllowed(AOption: TdxDocumentCapability): Boolean;
    function GetBulletedAllowed: Boolean;
    function GetSimpleAllowed: Boolean;
    function GetMultiLevelAllowed: Boolean;
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;

    property BulletedAllowed: Boolean read GetBulletedAllowed;
    property SimpleAllowed: Boolean read GetSimpleAllowed;
    property MultiLevelAllowed: Boolean read GetMultiLevelAllowed;
 published
    property Bulleted: TdxDocumentCapability read FBulleted write SetBulleted default TdxDocumentCapability.Default;
    property Simple: TdxDocumentCapability read FSimple write SetSimple default TdxDocumentCapability.Default;
    property MultiLevel: TdxDocumentCapability read FMultiLevel write SetMultiLevel default TdxDocumentCapability.Default;
  end;

  { TdxCharacterFormattingDetailedOptions }

  TdxCharacterFormattingDetailedOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      AllCaps             = TdxRichEditOptionsAction.AllCaps;
      BackColor           = TdxRichEditOptionsAction.BackColor;
      FontBold            = TdxRichEditOptionsAction.FontBold;
      FontItalic          = TdxRichEditOptionsAction.FontItalic;
      FontName            = TdxRichEditOptionsAction.FontName;
      FontSize            = TdxRichEditOptionsAction.FontSize;
      FontStrikeout       = TdxRichEditOptionsAction.FontStrikeout;
      FontUnderline       = TdxRichEditOptionsAction.FontUnderline;
      ForeColor           = TdxRichEditOptionsAction.ForeColor;
      Hidden              = TdxRichEditOptionsAction.Hidden;
      Script              = TdxRichEditOptionsAction.Script;
      StrikeoutColor      = TdxRichEditOptionsAction.StrikeoutColor;
      StrikeoutWordsOnly  = TdxRichEditOptionsAction.StrikeoutWordsOnly;
      UnderlineColor      = TdxRichEditOptionsAction.UnderlineColor;
      UnderlineWordsOnly  = TdxRichEditOptionsAction.UnderlineWordsOnly;
    end;
  strict private
    FAllCaps: TdxDocumentCapability;
    FBackColor: TdxDocumentCapability;
    FFontBold: TdxDocumentCapability;
    FFontItalic: TdxDocumentCapability;
    FFontName: TdxDocumentCapability;
    FFontSize: TdxDocumentCapability;
    FFontStrikeout: TdxDocumentCapability;
    FFontUnderline: TdxDocumentCapability;
    FForeColor: TdxDocumentCapability;
    FHidden: TdxDocumentCapability;
    FScript: TdxDocumentCapability;
    FStrikeoutColor: TdxDocumentCapability;
    FStrikeoutWordsOnly: TdxDocumentCapability;
    FUnderlineColor: TdxDocumentCapability;
    FUnderlineWordsOnly: TdxDocumentCapability;
  private
    procedure SetAllCaps(const Value: TdxDocumentCapability);
    procedure SetBackColor(const Value: TdxDocumentCapability);
    procedure SetFontBold(const Value: TdxDocumentCapability);
    procedure SetFontItalic(const Value: TdxDocumentCapability);
    procedure SetFontName(const Value: TdxDocumentCapability);
    procedure SetFontSize(const Value: TdxDocumentCapability);
    procedure SetFontStrikeout(const Value: TdxDocumentCapability);
    procedure SetFontUnderline(const Value: TdxDocumentCapability);
    procedure SetForeColor(const Value: TdxDocumentCapability);
    procedure SetHidden(const Value: TdxDocumentCapability);
    procedure SetScript(const Value: TdxDocumentCapability);
    procedure SetStrikeoutColor(const Value: TdxDocumentCapability);
    procedure SetStrikeoutWordsOnly(const Value: TdxDocumentCapability);
    procedure SetUnderlineColor(const Value: TdxDocumentCapability);
    procedure SetUnderlineWordsOnly(const Value: TdxDocumentCapability);
  protected
    procedure DoReset; override;
    function IsAllowed(AOption: TdxDocumentCapability): Boolean;
  public
    procedure Assign(Source: TPersistent); override;

    function AllCapsAllowed: Boolean;
    function BackColorAllowed: Boolean;
    function FontBoldAllowed: Boolean;
    function FontItalicAllowed: Boolean;
    function FontNameAllowed: Boolean;
    function FontSizeAllowed: Boolean;
    function FontStrikeoutAllowed: Boolean;
    function FontUnderlineAllowed: Boolean;
    function ForeColorAllowed: Boolean;
    function HiddenAllowed: Boolean;
    function ScriptAllowed: Boolean;
    function StrikeoutColorAllowed: Boolean;
    function StrikeoutWordsOnlyAllowed: Boolean;
    function UnderlineColorAllowed: Boolean;
    function UnderlineWordsOnlyAllowed: Boolean;
  published
    property AllCaps: TdxDocumentCapability read FAllCaps write SetAllCaps default TdxDocumentCapability.Default;
    property BackColor: TdxDocumentCapability read FBackColor write SetBackColor default TdxDocumentCapability.Default;
    property FontBold: TdxDocumentCapability read FFontBold write SetFontBold default TdxDocumentCapability.Default;
    property FontItalic: TdxDocumentCapability read FFontItalic write SetFontItalic default TdxDocumentCapability.Default;
    property FontName: TdxDocumentCapability read FFontName write SetFontName default TdxDocumentCapability.Default;
    property FontSize: TdxDocumentCapability read FFontSize write SetFontSize default TdxDocumentCapability.Default;
    property FontStrikeout: TdxDocumentCapability read FFontStrikeout write SetFontStrikeout default TdxDocumentCapability.Default;
    property FontUnderline: TdxDocumentCapability read FFontUnderline write SetFontUnderline default TdxDocumentCapability.Default;
    property ForeColor: TdxDocumentCapability read FForeColor write SetForeColor default TdxDocumentCapability.Default;
    property Hidden: TdxDocumentCapability read FHidden write SetHidden default TdxDocumentCapability.Default;
    property Script: TdxDocumentCapability read FScript write SetScript default TdxDocumentCapability.Default;
    property StrikeoutColor: TdxDocumentCapability read FStrikeoutColor write SetStrikeoutColor default TdxDocumentCapability.Default;
    property StrikeoutWordsOnly: TdxDocumentCapability read FStrikeoutWordsOnly write SetStrikeoutWordsOnly default TdxDocumentCapability.Default;
    property UnderlineColor: TdxDocumentCapability read FUnderlineColor write SetUnderlineColor default TdxDocumentCapability.Default;
    property UnderlineWordsOnly: TdxDocumentCapability read FUnderlineWordsOnly write SetUnderlineWordsOnly default TdxDocumentCapability.Default;
  end;

  { TdxDocumentSaveOptions }

  TdxDocumentSaveOptions = class(TdxRichEditNotificationOptions,
    IdxDocumentSaveOptions)
  public type
    TAction = class sealed
    public const
      CurrentFileName = TdxRichEditOptionsAction.CurrentFileName;
      CurrentFormat   = TdxRichEditOptionsAction.CurrentFormat;
      DefaultFileName = TdxRichEditOptionsAction.DefaultFileName;
      DefaultFormat   = TdxRichEditOptionsAction.DefaultFormat;
    end;
  strict private
    FCurrentFileName: string;
    FCurrentFormat: TdxRichEditDocumentFormat;
    FDefaultFileName: string;
    FDefaultFormat: TdxRichEditDocumentFormat;
    FHideInOpenDialog: Boolean;
    FHideInSaveDialog: Boolean;
  private
    function IsCurrentFileNameStored: Boolean;
    function IsDefaultFileNameStored: Boolean;
    function GetDefaultFileName: string;
    procedure SetDefaultFileName(const AFileName: string);
    function GetCurrentFileName: string;
    procedure SetCurrentFileName(const AFileName: string);
    function GetDefaultFormat: TdxRichEditDocumentFormat;
    procedure SetDefaultFormat(const AFormat: TdxRichEditDocumentFormat);
    function GetCurrentFormat: TdxRichEditDocumentFormat;
    procedure SetCurrentFormat(const AFormat: TdxRichEditDocumentFormat);
  protected
    procedure DoReset; override;
    procedure ResetDefaultFileName;
    procedure ResetDefaultFormat;
    property HideInOpenDialog: Boolean read FHideInOpenDialog write FHideInOpenDialog default False;
    property HideInSaveDialog: Boolean read FHideInSaveDialog write FHideInSaveDialog default False;
  public
    function CanSaveToCurrentFileName: Boolean;
    // for internal use
    procedure ResetCurrentFileName;
    procedure ResetCurrentFormat;
  published
    property CurrentFileName: string read GetCurrentFileName write SetCurrentFileName stored IsCurrentFileNameStored;
    property CurrentFormat: TdxRichEditDocumentFormat read GetCurrentFormat write SetCurrentFormat default TdxRichEditDocumentFormat.Undefined;
    property DefaultFileName: string read GetDefaultFileName write SetDefaultFileName stored IsDefaultFileNameStored;
    property DefaultFormat: TdxRichEditDocumentFormat read GetDefaultFormat write SetDefaultFormat default TdxRichEditDocumentFormat.Rtf;
  end;

  { TdxRulerOptions }

  TdxRulerOptions = class abstract(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      Visibility = TdxRichEditOptionsAction.Visibility;
    end;
  public const
    DefaultVisibility = TdxRichEditRulerVisibility.Auto;
  strict private
    FVisibility: TdxRichEditRulerVisibility;
    procedure SetVisibility(const AValue: TdxRichEditRulerVisibility);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;

    property Visibility: TdxRichEditRulerVisibility read FVisibility write SetVisibility default TdxRulerOptions.DefaultVisibility;
  end;

  { TdxHorizontalRulerOptions }

  TdxHorizontalRulerOptions = class(TdxRulerOptions)
  public type
    TAction = class sealed
    public const
      ShowTabs        = TdxRichEditOptionsAction.ShowTabs;
      ShowLeftIndent  = TdxRichEditOptionsAction.ShowLeftIndent;
      ShowRightIndent = TdxRichEditOptionsAction.ShowRightIndent;
    end;
  strict private
    FShowTabs: Boolean;
    FShowLeftIndent: Boolean;
    FShowRightIndent: Boolean;
    procedure SetShowTabs(const AValue: Boolean);
    procedure SetShowLeftIndent(const AValue: Boolean);
    procedure SetShowRightIndent(const AValue: Boolean);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
    property ShowLeftIndent: Boolean read FShowLeftIndent write SetShowLeftIndent default True;
    property ShowRightIndent: Boolean read FShowRightIndent write SetShowRightIndent default True;
    property Visibility;
  end;

  { TdxVerticalRulerOptions }

  TdxVerticalRulerOptions = class(TdxRulerOptions)
  published
    property Visibility;
  end;

  { TdxViewLayoutOptionsBase }

  TdxViewLayoutOptionsBase = class abstract(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      AllowTablesToExtendIntoMargins         = TdxRichEditOptionsAction.AllowTablesToExtendIntoMargins;
      MatchHorizontalTableIndentsToTextEdge  = TdxRichEditOptionsAction.MatchHorizontalTableIndentsToTextEdge;
    end;
  strict private
    FAllowTablesToExtendIntoMargins: Boolean;
    FMatchHorizontalTableIndentsToTextEdge: Boolean;
  protected
    procedure DoReset; override;
    function GetDefaultAllowTablesToExtendIntoMargins: Boolean; virtual; abstract;
    function GetDefaultMatchHorizontalTableIndentsToTextEdge: Boolean; virtual; abstract;
    procedure SetAllowTablesToExtendIntoMargins(const AValue: Boolean); virtual;
    procedure SetMatchHorizontalTableIndentsToTextEdge(const AValue: Boolean); virtual;
  public
    procedure Assign(Source: TPersistent); override;

    property AllowTablesToExtendIntoMargins: Boolean read FAllowTablesToExtendIntoMargins write SetAllowTablesToExtendIntoMargins;
    property MatchHorizontalTableIndentsToTextEdge: Boolean read FMatchHorizontalTableIndentsToTextEdge write SetMatchHorizontalTableIndentsToTextEdge;
  end;

  { TdxDraftViewLayoutOptions }

  TdxDraftViewLayoutOptions = class(TdxViewLayoutOptionsBase)
  private const
    AllowTablesToExtendIntoMarginsByDefault = True;
    MatchHorizontalTableIndentsToTextEdgeDefault = False;
  protected
    function GetDefaultAllowTablesToExtendIntoMargins: Boolean; override;
    function GetDefaultMatchHorizontalTableIndentsToTextEdge: Boolean; override;
  published
    property AllowTablesToExtendIntoMargins default AllowTablesToExtendIntoMarginsByDefault;
    property MatchHorizontalTableIndentsToTextEdge default MatchHorizontalTableIndentsToTextEdgeDefault;
  end;

  { TdxPrintLayoutViewLayoutOptions }

  TdxPrintLayoutViewLayoutOptions  = class(TdxViewLayoutOptionsBase)
  private const
    AllowTablesToExtendIntoMarginsByDefault = False;
    MatchHorizontalTableIndentsToTextEdgeDefault = False;
  protected
    function GetDefaultAllowTablesToExtendIntoMargins: Boolean; override;
    function GetDefaultMatchHorizontalTableIndentsToTextEdge: Boolean; override;
  published
    property AllowTablesToExtendIntoMargins default AllowTablesToExtendIntoMarginsByDefault;
    property MatchHorizontalTableIndentsToTextEdge default MatchHorizontalTableIndentsToTextEdgeDefault;
  end;

  { TdxSimpleViewLayoutOptions }

  TdxSimpleViewLayoutOptions = class(TdxViewLayoutOptionsBase)
  private const
    AllowTablesToExtendIntoMarginsByDefault = True;
    MatchHorizontalTableIndentsToTextEdgeDefault = False;
  protected
    function GetDefaultAllowTablesToExtendIntoMargins: Boolean; override;
    function GetDefaultMatchHorizontalTableIndentsToTextEdge: Boolean; override;
  published
    property AllowTablesToExtendIntoMargins default AllowTablesToExtendIntoMarginsByDefault;
    property MatchHorizontalTableIndentsToTextEdge default MatchHorizontalTableIndentsToTextEdgeDefault;
  end;

  { TdxRichEditLayoutOptions }

  TdxRichEditLayoutOptions = class(TdxRichEditNotificationOptions)
  strict private
    FDraftView: TdxDraftViewLayoutOptions;
    FPrintLayoutView: TdxPrintLayoutViewLayoutOptions;
    FSimpleView: TdxSimpleViewLayoutOptions;
  private
    procedure SetDraftView(const Value: TdxDraftViewLayoutOptions);
    procedure SetPrintLayoutView(const Value: TdxPrintLayoutViewLayoutOptions);
    procedure SetSimpleView(const Value: TdxSimpleViewLayoutOptions);
  protected
    procedure CreateInnerOptions; override;
    procedure DoReset; override;
    procedure SubscribeInnerOptions; override;
    procedure OnInnerOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs); virtual;
    function CreateDraftViewLayoutOptions: TdxDraftViewLayoutOptions; virtual;
    function CreatePrintLayoutViewLayoutOptions: TdxPrintLayoutViewLayoutOptions; virtual;
    function CreateSimpleViewLayoutOptions: TdxSimpleViewLayoutOptions; virtual;
  public
    destructor Destroy; override;
  published
    property DraftView: TdxDraftViewLayoutOptions read FDraftView write SetDraftView;
    property PrintLayoutView: TdxPrintLayoutViewLayoutOptions read FPrintLayoutView write SetPrintLayoutView;
    property SimpleView: TdxSimpleViewLayoutOptions read FSimpleView write SetSimpleView;
  end;

  { TdxHyperlinkOptions }

  TdxHyperlinkOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      ShowToolTip   = TdxRichEditOptionsAction.ShowToolTip;
      ModifierKeys  = TdxRichEditOptionsAction.ModifierKeys;
    end;
  strict private
    FModifierKeys: TShortCut;
    FShowToolTip: Boolean;
    procedure SetShowToolTip(const AValue: Boolean);
    procedure SetModifierKeys(const AValue: TShortCut);
  protected
    function ShouldSerializeModifierKeys: Boolean; virtual;
    procedure ResetModifierKeys; virtual;
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ModifierKeys: TShortCut read FModifierKeys write SetModifierKeys default VK_CONTROL;
    property ShowToolTip: Boolean read FShowToolTip write SetShowToolTip default True;
  end;

  { TdxFieldOptions }

  TdxFieldOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      HighlightColor                          = TdxRichEditOptionsAction.HighlightColor;
      HighlightMode                           = TdxRichEditOptionsAction.HighlightMode;
      UseCurrentCultureForDateTimeFormatting  = TdxRichEditOptionsAction.UseCurrentCultureForDateTimeFormatting;
      UpdateDocVariablesBeforePrint           = TdxRichEditOptionsAction.UpdateDocVariablesBeforePrint;
      UpdateDocVariablesBeforeCopy            = TdxRichEditOptionsAction.UpdateDocVariablesBeforeCopy;
      UpdateFieldsOnPaste                     = TdxRichEditOptionsAction.UpdateFieldsOnPaste;
      ThrowExceptionOnInvalidFormatSwitch     = TdxRichEditOptionsAction.ThrowExceptionOnInvalidFormatSwitch;
    end;
  strict private const
    DefaultHighlightColor = TdxAlphaColors.Silver;
    DefaultHighlightMode = TdxFieldsHighlightMode.Auto;
  strict private
    FHighlightMode: TdxFieldsHighlightMode;
    FHighlightColor: TdxAlphaColor;
    FUseCurrentCultureDateTimeFormat: Boolean;
    FUpdateDocVariablesBeforePrint: Boolean;
    FUpdateDocVariablesBeforeCopy: Boolean;
    FUpdateFieldsOnPaste: Boolean;
    FThrowExceptionOnInvalidFormatSwitch: Boolean;
    procedure SetHighlightMode(const AValue: TdxFieldsHighlightMode);
    procedure SetHighlightColor(const AValue: TdxAlphaColor);
    procedure SetUseCurrentCultureDateTimeFormat(const AValue: Boolean);
    procedure SetUpdateDocVariablesBeforePrint(const Value: Boolean);
    procedure SetUpdateDocVariablesBeforeCopy(const Value: Boolean);
    procedure SetUpdateFieldsOnPaste(const Value: Boolean);
    procedure SetThrowExceptionOnInvalidFormatSwitch(const Value: Boolean);
  protected
    function ShouldSerializeHighlightColor: Boolean;
    procedure ResetHighlightColor;
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure CopyFrom(AOptions: TdxFieldOptions);
  published
    property HighlightColor: TdxAlphaColor read FHighlightColor write SetHighlightColor default DefaultHighlightColor;
    property HighlightMode: TdxFieldsHighlightMode read FHighlightMode write SetHighlightMode default DefaultHighlightMode;
    property UseCurrentCultureDateTimeFormat: Boolean read FUseCurrentCultureDateTimeFormat write SetUseCurrentCultureDateTimeFormat default False;
    property UpdateDocVariablesBeforePrint: Boolean read FUpdateDocVariablesBeforePrint write SetUpdateDocVariablesBeforePrint default True;
    property UpdateDocVariablesBeforeCopy: Boolean read FUpdateDocVariablesBeforeCopy write SetUpdateDocVariablesBeforeCopy default True;
    property UpdateFieldsOnPaste: Boolean read FUpdateFieldsOnPaste write SetUpdateFieldsOnPaste default True;
    property ThrowExceptionOnInvalidFormatSwitch: Boolean read FThrowExceptionOnInvalidFormatSwitch write SetThrowExceptionOnInvalidFormatSwitch default True;
  end;

  { TdxDocumentSearchOptions }

  TdxDocumentSearchOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      RegExResultMaxGuaranteedLength = TdxRichEditOptionsAction.RegExResultMaxGuaranteedLength;
    end;
  strict private const
    DefaultRegExResultMaxGuaranteedLength = 128;
  strict private
    FRegExResultMaxGuaranteedLength: Integer;
    procedure SetRegExResultMaxGuaranteedLength(const AValue: Integer);
  protected
    procedure DoReset; override;
  published
    property RegExResultMaxGuaranteedLength: Integer
      read FRegExResultMaxGuaranteedLength write SetRegExResultMaxGuaranteedLength default DefaultRegExResultMaxGuaranteedLength;
  end;

  { TdxPrintingOptions }

  TdxPrintingOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      EnablePageBackgroundOnPrint   = TdxRichEditOptionsAction.EnablePageBackgroundOnPrint;
      UpdateDocVariablesBeforePrint = TdxRichEditOptionsAction.UpdateDocVariablesBeforePrint;
    end;
  strict private
    FEnablePageBackgroundOnPrint: Boolean;
    FUpdateDocVariablesBeforePrint: Boolean;
    procedure SetEnablePageBackgroundOnPrint(const Value: Boolean);
    procedure SetUpdateDocVariablesBeforePrint(const Value: Boolean);
  protected
    procedure DoReset; override;
  published
    property EnablePageBackgroundOnPrint: Boolean read FEnablePageBackgroundOnPrint write SetEnablePageBackgroundOnPrint default False;
    property UpdateDocVariablesBeforePrint: Boolean read FUpdateDocVariablesBeforePrint write SetUpdateDocVariablesBeforePrint default True;
  end;

  { TdxTableOptions }

  TdxTableOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      GridLines = TdxRichEditOptionsAction.GridLines;
    end;
  strict private const
    FDefaultGridLinesVisibility = TdxRichEditTableGridLinesVisibility.Auto;
  strict private
    FGridLines: TdxRichEditTableGridLinesVisibility;
    procedure SetGridLines(const AValue: TdxRichEditTableGridLinesVisibility);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property GridLines: TdxRichEditTableGridLinesVisibility read FGridLines write SetGridLines default TdxRichEditTableGridLinesVisibility.Auto;
  end;

  { TdxBookmarkOptions }

  TdxBookmarkOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      BookmarkVisibility = TdxRichEditOptionsAction.BookmarkVisibility;
      BookmarkColor = TdxRichEditOptionsAction.BookmarkColor;
      AllowNameResolution = TdxRichEditOptionsAction.AllowNameResolution;
    end;
  strict private const
    DefaultVisibility = TdxRichEditBookmarkVisibility.Auto;
    DefaultAllowNameResolution = False;
  strict private class var
    FDefaultColor: TdxAlphaColor;
  strict private
    FDefaultVisibility: TdxRichEditBookmarkVisibility;
    FDefaultAllowNameResolution: Boolean;
    FVisibility: TdxRichEditBookmarkVisibility;
    FColor: TdxAlphaColor;
    FAllowNameResolution: Boolean;
    class constructor Initialize;
    function IsColorStored: Boolean;
    procedure SetVisibility(const AValue: TdxRichEditBookmarkVisibility);
    procedure SetColor(const AValue: TdxAlphaColor);
    procedure SetAllowNameResolution(const AValue: Boolean);
  protected
    function ShouldSerializeColor: Boolean;
    procedure ResetColor;
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
    property AllowNameResolution: Boolean read FAllowNameResolution
      write SetAllowNameResolution default False;
  published
    property Visibility: TdxRichEditBookmarkVisibility read FVisibility
      write SetVisibility default TdxRichEditBookmarkVisibility.Auto;
    property Color: TdxAlphaColor read FColor write SetColor stored IsColorStored;
  end;

  { TdxSpellCheckerOptions }

  TdxSpellCheckerOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      AutoDetectDocumentCulture = TdxRichEditOptionsAction.AutoDetectDocumentCulture;
      IgnoreNoProof = TdxRichEditOptionsAction.IgnoreNoProof;
    end;
  strict private
    FAutoDetectDocumentCulture: Boolean;
    FIgnoreNoProof: Boolean;
    procedure SetAutoDetectDocumentCulture(const AValue: Boolean);
    procedure SetIgnoreNoProof(const AValue: Boolean);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property AutoDetectDocumentCulture: Boolean read FAutoDetectDocumentCulture write SetAutoDetectDocumentCulture default True;
    property IgnoreNoProof: Boolean read FIgnoreNoProof write SetIgnoreNoProof default False;
  end;

  { TdxAutoCorrectOptions }

  TdxAutoCorrectOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      DetectUrls = TdxRichEditOptionsAction.DetectUrls;
    end;
  strict private
    FDetectUrls: Boolean;
    procedure SetDetectUrls(const AValue: Boolean);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DetectUrls: Boolean read FDetectUrls write SetDetectUrls default True;
  end;

  { TdxAuthenticationOptions }

  TdxAuthenticationOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      EMail = TdxRichEditOptionsAction.AuthenticationEMail;
      Group = TdxRichEditOptionsAction.AuthenticationGroup;
      Password = TdxRichEditOptionsAction.AuthenticationPassword;
      UserName = TdxRichEditOptionsAction.AuthenticationUserName;
    end;
  strict private
    FUserName: string;
    FGroup: string;
    FEMail: string;
    FPassword: string;
    procedure SetUserName(const AValue: string);
    procedure SetGroup(const AValue: string);
    procedure SetEMail(const AValue: string);
    procedure SetPassword(const AValue: string);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;

    property Password: string read FPassword write SetPassword;
  published
    property EMail: string read FEMail write SetEMail;
    property Group: string read FGroup write SetGroup;
    property UserName: string read FUserName write SetUserName;
  end;

  { TdxRangePermissionOptions }

  TdxRichEditRangePermissionVisibility = (Auto, Visible, Hidden);

  TdxRangePermissionOptions = class(TdxRichEditNotificationOptions)
  public
    const
      DefaultColors: array[0..7] of TdxAlphaColor =
        ($FFD5D5FF, $FFFFD5FF, $FFFFE6D5, $FFD5FFFE, $FFFFFED5, $FFE9E9E9, $FFFFD5D5, $FFD5FFD5);
      DefaultColor = $FF7F7F7F;
      DefaultHighlightBracketsColor = $FFA4A000;
      DefaultBracketsColor = $FF7F7F7F;
      DefaultHighlightColor = $FFFFFED5;
      DefaultVisibility = TdxRichEditRangePermissionVisibility.Auto;
    type
      TAction = class sealed
      public const
        BracketsColor = TdxRichEditOptionsAction.RangePermissionsBracketsColor;
        Color = TdxRichEditOptionsAction.RangePermissionsColor;
        HighlightBracketsColor = TdxRichEditOptionsAction.RangePermissionsHighlightBracketsColor;
        HighlightColor = TdxRichEditOptionsAction.RangePermissionsHighlightColor;
        Visibility = TdxRichEditOptionsAction.RangePermissionsVisibility;
      end;
  strict private
    class var
      FColorIndex: Integer;
  strict private
    FVisibility: TdxRichEditRangePermissionVisibility;
    FColor: TdxAlphaColor;
    FBracketsColor: TdxAlphaColor;
    FHighlightColor: TdxAlphaColor;
    FHighlightBracketsColor: TdxAlphaColor;
    procedure SetVisibility(const AValue: TdxRichEditRangePermissionVisibility);
    procedure SetColor(const AValue: TdxAlphaColor);
    procedure SetBracketsColor(const AValue: TdxAlphaColor);
    procedure SetHighlightBracketsColor(const AValue: TdxAlphaColor);
    procedure SetHighlightColor(const AValue: TdxAlphaColor);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
    class function GetColor: TdxAlphaColor; static;
  published
    property BracketsColor: TdxAlphaColor read FBracketsColor write SetBracketsColor default DefaultBracketsColor;
    property Color: TdxAlphaColor read FColor write SetColor default DefaultColor;
    property HighlightBracketsColor: TdxAlphaColor read FHighlightBracketsColor write SetHighlightBracketsColor default DefaultHighlightBracketsColor;
    property HighlightColor: TdxAlphaColor read FHighlightColor write SetHighlightColor default DefaultHighlightColor;
    property Visibility: TdxRichEditRangePermissionVisibility read FVisibility write SetVisibility default DefaultVisibility;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Utils.Exceptions,
  dxEncoding;

{ TdxDocumentCapabilitiesOptions }

destructor TdxDocumentCapabilitiesOptions.Destroy;
begin
  FNumbering.Free;
  FCharacterFormattingDetailed.Free;
  inherited Destroy;
end;

procedure TdxDocumentCapabilitiesOptions.Assign(Source: TPersistent);
var
  ASource: TdxDocumentCapabilitiesOptions;
begin
  BeginUpdate;
  try
    if Source is TdxDocumentCapabilitiesOptions then
    begin
      ASource := TdxDocumentCapabilitiesOptions(Source);
      Bookmarks := ASource.Bookmarks;
      CharacterStyle := ASource.CharacterStyle;
      EndNotes := ASource.EndNotes;
      Fields := ASource.Fields;
      FloatingObjects := ASource.FloatingObjects;
      FootNotes := ASource.FootNotes;
      HeadersFooters := ASource.HeadersFooters;
      ParagraphFrames := ASource.ParagraphFrames;
      ParagraphStyle := ASource.ParagraphStyle;
      ParagraphTabs := ASource.ParagraphTabs;
      Sections := ASource.Sections;
      TableCellStyle := ASource.TableCellStyle;
      Tables := ASource.Tables;
      TableStyle := ASource.TableStyle;
      TabSymbol := ASource.TabSymbol;
      Numbering.Assign(ASource.Numbering);
      CharacterFormattingDetailed.Assign(ASource.CharacterFormattingDetailed);
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

function TdxDocumentCapabilitiesOptions.BookmarksAllowed: Boolean;
begin
  Result := IsAllowed(Bookmarks);
end;

function TdxDocumentCapabilitiesOptions.CharacterStyleAllowed: Boolean;
begin
  Result := IsAllowed(CharacterStyle);
end;

function TdxDocumentCapabilitiesOptions.EndNotesAllowed: Boolean;
begin
  Result := IsAllowed(EndNotes);
end;

function TdxDocumentCapabilitiesOptions.FieldsAllowed: Boolean;
begin
  Result := IsAllowed(Fields);
end;

function TdxDocumentCapabilitiesOptions.FloatingObjectsAllowed: Boolean;
begin
  Result := IsAllowed(FloatingObjects);
end;

function TdxDocumentCapabilitiesOptions.FootNotesAllowed: Boolean;
begin
  Result := IsAllowed(FootNotes);
end;

function TdxDocumentCapabilitiesOptions.HeadersFootersAllowed: Boolean;
begin
  Result := IsAllowed(HeadersFooters);
end;

procedure TdxDocumentCapabilitiesOptions.CreateInnerOptions;
begin
  inherited CreateInnerOptions;
  FNumbering := TdxNumberingOptions.Create;
  FCharacterFormattingDetailed := TdxCharacterFormattingDetailedOptions.Create;
end;

procedure TdxDocumentCapabilitiesOptions.SubscribeInnerOptions;
begin
  inherited SubscribeInnerOptions;
  FNumbering.Changed.Add(DoInnerOptionsChanged);
  FCharacterFormattingDetailed.Changed.Add(DoInnerOptionsChanged);
end;

function TdxDocumentCapabilitiesOptions.ParagraphFramesAllowed: Boolean;
begin
  Result := IsAllowed(ParagraphFrames);
end;

function TdxDocumentCapabilitiesOptions.ParagraphStyleAllowed: Boolean;
begin
  Result := IsAllowed(ParagraphStyle);
end;

function TdxDocumentCapabilitiesOptions.ParagraphTabsAllowed: Boolean;
begin
  Result := IsAllowed(ParagraphTabs);
end;

function TdxDocumentCapabilitiesOptions.SectionsAllowed: Boolean;
begin
  Result := IsAllowed(Sections);
end;

function TdxDocumentCapabilitiesOptions.TableCellStyleAllowed: Boolean;
begin
  Result := IsAllowed(TableCellStyle);
end;

function TdxDocumentCapabilitiesOptions.TablesAllowed: Boolean;
begin
  Result := IsAllowed(Tables);
end;

function TdxDocumentCapabilitiesOptions.TableStyleAllowed: Boolean;
begin
  Result := IsAllowed(TableStyle);
end;

function TdxDocumentCapabilitiesOptions.TabSymbolAllowed: Boolean;
begin
  Result := IsAllowed(TabSymbol);
end;

procedure TdxDocumentCapabilitiesOptions.DoReset;
begin
  inherited DoReset;
  Bookmarks := TdxDocumentCapability.Default;
  CharacterStyle := TdxDocumentCapability.Default;
  EndNotes := TdxDocumentCapability.Default;
  Fields := TdxDocumentCapability.Default;
  FloatingObjects := TdxDocumentCapability.Default;
  FootNotes := TdxDocumentCapability.Default;
  HeadersFooters := TdxDocumentCapability.Default;
  ParagraphFrames := TdxDocumentCapability.Default;
  ParagraphStyle := TdxDocumentCapability.Default;
  ParagraphTabs := TdxDocumentCapability.Default;
  Sections := TdxDocumentCapability.Default;
  TableCellStyle := TdxDocumentCapability.Default;
  Tables := TdxDocumentCapability.Default;
  TableStyle := TdxDocumentCapability.Default;
  TabSymbol := TdxDocumentCapability.Default;
  Numbering.DoReset;
  CharacterFormattingDetailed.DoReset;
end;

procedure TdxDocumentCapabilitiesOptions.SetBookmarks(const Value: TdxDocumentCapability);
begin
  if FBookmarks <> Value then
  begin
    FBookmarks := Value;
    DoChanged(TAction.Bookmarks);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetCharacterStyle(const Value: TdxDocumentCapability);
begin
  if FCharacterStyle <> Value then
  begin
    FCharacterStyle := Value;
    DoChanged(TAction.CharacterStyle);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetEndNotes(const Value: TdxDocumentCapability);
begin
  if FEndNotes <> Value then
  begin
    FEndNotes := Value;
    DoChanged(TAction.EndNotes);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetFields(const Value: TdxDocumentCapability);
begin
  if FFields <> Value then
  begin
    FFields := Value;
    DoChanged(TAction.Fields);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetFloatingObjects(const Value: TdxDocumentCapability);
begin
  if FFloatingObjects <> Value then
  begin
    FFloatingObjects := Value;
    DoChanged(TAction.FloatingObjects);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetFootNotes(const Value: TdxDocumentCapability);
begin
  if FFootNotes <> Value then
  begin
    FFootNotes := Value;
    DoChanged(TAction.FootNotes);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetHeadersFooters(const Value: TdxDocumentCapability);
begin
  if FHeadersFooters <> Value then
  begin
    FHeadersFooters := Value;
    DoChanged(TAction.HeadersFooters);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetParagraphFrames(const Value: TdxDocumentCapability);
begin
  if FParagraphFrames <> Value then
  begin
    FParagraphFrames := Value;
    DoChanged(TAction.ParagraphFrames);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetParagraphStyle(const Value: TdxDocumentCapability);
begin
  if FParagraphStyle <> Value then
  begin
    FParagraphStyle := Value;
    DoChanged(TAction.ParagraphStyle);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetParagraphTabs(const Value: TdxDocumentCapability);
begin
  if FParagraphTabs <> Value then
  begin
    FParagraphTabs := Value;
    DoChanged(TAction.ParagraphTabs);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetSections(const Value: TdxDocumentCapability);
begin
  if FSections <> Value then
  begin
    FSections := Value;
    DoChanged(TAction.Sections);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetTableCellStyle(const Value: TdxDocumentCapability);
begin
  if FTableCellStyle <> Value then
  begin
    FTableCellStyle := Value;
    DoChanged(TAction.TableCellStyle);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetTables(const Value: TdxDocumentCapability);
begin
  if FTables <> Value then
  begin
    FTables := Value;
    DoChanged(TAction.Tables);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetTableStyle(const Value: TdxDocumentCapability);
begin
  if FTableStyle <> Value then
  begin
    FTableStyle := Value;
    DoChanged(TAction.TableStyle);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetTabSymbol(const Value: TdxDocumentCapability);
begin
  if FTabSymbol <> Value then
  begin
    FTabSymbol := Value;
    DoChanged(TAction.TabSymbol);
  end;
end;

procedure TdxDocumentCapabilitiesOptions.SetNumbering(const Value: TdxNumberingOptions);
begin
  Numbering.Assign(Value);
end;

{ TdxRichEditBehaviorOptions }

procedure TdxRichEditBehaviorOptions.Assign(Source: TPersistent);
var
  ASource: TdxRichEditBehaviorOptions;
begin
  BeginUpdate;
  try
    if Source is TdxRichEditBehaviorOptions then
    begin
      ASource := TdxRichEditBehaviorOptions(Source);
      Drag := ASource.Drag;
      Drop := ASource.Drop;
      Copy := ASource.Copy;
      Paste := ASource.Paste;
      Cut := ASource.Cut;
      Printing := ASource.Printing;
      Zooming := ASource.Zooming;
      SaveAs := ASource.SaveAs;
      Save := ASource.Save;
      CreateNew := ASource.CreateNew;
      Open := ASource.Open;
      ShowPopupMenu := ASource.ShowPopupMenu;

      MinZoomFactor := ASource.MinZoomFactor;
      MaxZoomFactor := ASource.MaxZoomFactor;

      FontSource := ASource.FontSource;
      ForeColorSource := ASource.ForeColorSource;
      PasteSingleCellAsText := ASource.PasteSingleCellAsText;
      TabMarker := ASource.TabMarker;
      UseFontSubstitution := ASource.UseFontSubstitution;
      OvertypeAllowed := ASource.OvertypeAllowed;
      PageBreakInsertMode := ASource.pageBreakInsertMode;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

function TdxRichEditBehaviorOptions.IsAllowed(AOption: TdxDocumentCapability): Boolean;
begin
  Result := (AOption = TdxDocumentCapability.Default) or (AOption = TdxDocumentCapability.Enabled);
end;

procedure TdxRichEditBehaviorOptions.DoReset;
begin
  BeginUpdate;
  try
    Drag := TdxDocumentCapability.Default;
    Drop := TdxDocumentCapability.Default;
    Copy := TdxDocumentCapability.Default;
    Paste := TdxDocumentCapability.Default;
    Cut := TdxDocumentCapability.Default;
    ShowPopupMenu := TdxDocumentCapability.Default;

    PageBreakInsertMode := TdxPageBreakInsertMode.NewLine;
    MaxZoomFactor := DefaultMaxZoomFactor;
    MinZoomFactor := DefaultMinZoomFactor;

    FontSource := TdxRichEditBaseValueSource.Auto;
    ForeColorSource := TdxRichEditBaseValueSource.Auto;
    TabMarker := TdxCharacters.TabMark;

    PasteSingleCellAsText := False;
    UseFontSubstitution := True;
    OvertypeAllowed := True;

    ResetPrinting;
    ResetZooming;
    ResetSaveAs;
    ResetSave;
    ResetCreateNew;
    ResetOpen;
  finally
    EndUpdate;
  end;
end;

function TdxRichEditBehaviorOptions.ShouldSerializePrinting: Boolean;
begin
  Result := Printing <> TdxDocumentCapability.Default;
end;

procedure TdxRichEditBehaviorOptions.ResetPrinting;
begin
  Printing := TdxDocumentCapability.Default;
end;

function TdxRichEditBehaviorOptions.ShouldSerializeSaveAs: Boolean;
begin
  Result := SaveAs <> TdxDocumentCapability.Default;
end;

procedure TdxRichEditBehaviorOptions.ResetSaveAs;
begin
  SaveAs := TdxDocumentCapability.Default;
end;

function TdxRichEditBehaviorOptions.ShouldSerializeSave: Boolean;
begin
  Result := Save <> TdxDocumentCapability.Default;
end;

procedure TdxRichEditBehaviorOptions.ResetSave;
begin
  Save := TdxDocumentCapability.Default;
end;

function TdxRichEditBehaviorOptions.ShouldSerializeZooming: Boolean;
begin
  Result := Zooming <> TdxDocumentCapability.Default;
end;

procedure TdxRichEditBehaviorOptions.ResetZooming;
begin
  Zooming := TdxDocumentCapability.Default;
end;

function TdxRichEditBehaviorOptions.ShouldSerializeCreateNew: Boolean;
begin
  Result := CreateNew <> TdxDocumentCapability.Default;
end;

procedure TdxRichEditBehaviorOptions.ResetCreateNew;
begin
  CreateNew := TdxDocumentCapability.Default;
end;

function TdxRichEditBehaviorOptions.ShouldSerializeOpen: Boolean;
begin
  Result := Open <> TdxDocumentCapability.Default;
end;

procedure TdxRichEditBehaviorOptions.ResetOpen;
begin
  Open := TdxDocumentCapability.Default;
end;

function TdxRichEditBehaviorOptions.GetDragAllowed: Boolean;
begin
  Result := IsAllowed(Drag);
end;

function TdxRichEditBehaviorOptions.GetDropAllowed: Boolean;
begin
  Result := IsAllowed(Drop);
end;

function TdxRichEditBehaviorOptions.GetCopyAllowed: Boolean;
begin
  Result := IsAllowed(Copy);
end;

function TdxRichEditBehaviorOptions.GetPasteAllowed: Boolean;
begin
  Result := IsAllowed(Paste);
end;

function TdxRichEditBehaviorOptions.GetCutAllowed: Boolean;
begin
  Result := IsAllowed(Cut);
end;

function TdxRichEditBehaviorOptions.GetPrintingAllowed: Boolean;
begin
  Result := IsAllowed(Printing);
end;

function TdxRichEditBehaviorOptions.GetSaveAllowed: Boolean;
begin
  Result := IsAllowed(Save);
end;

function TdxRichEditBehaviorOptions.GetSaveAsAllowed: Boolean;
begin
  Result := IsAllowed(SaveAs);
end;

function TdxRichEditBehaviorOptions.GetCreateNewAllowed: Boolean;
begin
  Result := IsAllowed(CreateNew);
end;

function TdxRichEditBehaviorOptions.GetOpenAllowed: Boolean;
begin
  Result := IsAllowed(Open);
end;

function TdxRichEditBehaviorOptions.GetZoomingAllowed: Boolean;
begin
  Result := IsAllowed(Zooming);
end;

function TdxRichEditBehaviorOptions.GetShowPopupMenuAllowed: Boolean;
begin
  Result := IsAllowed(ShowPopupMenu);
end;

function TdxRichEditBehaviorOptions.GetOfficeScrollingAllowed: Boolean;
begin
  Result := IsAllowed(OfficeScrolling);
end;

function TdxRichEditBehaviorOptions.GetTouchAllowed: Boolean;
begin
  Result := IsAllowed(Touch);
end;

function TdxRichEditBehaviorOptions.IsMinZoomFactorStored: Boolean;
var
  ADefaultMinZoomFactor: Single;
begin
  ADefaultMinZoomFactor := DefaultMinZoomFactor;
  Result := not SameValue(FMinZoomFactor, ADefaultMinZoomFactor);
end;

function TdxRichEditBehaviorOptions.IsMaxZoomFactorStored: Boolean;
var
  ADefaultMaxZoomFactor: Single;
begin
  ADefaultMaxZoomFactor := DefaultMaxZoomFactor;
  Result := not SameValue(FMaxZoomFactor, ADefaultMaxZoomFactor);
end;

function TdxRichEditBehaviorOptions.IsTabMarkerStored: Boolean;
begin
  Result := FTabMarker <> TdxCharacters.TabMark;
end;

procedure TdxRichEditBehaviorOptions.SetCopy(const Value: TdxDocumentCapability);
begin
  if FAllowCopy <> Value then
  begin
    FAllowCopy := Value;
    DoChanged(TAction.AllowCopy);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetPaste(const Value: TdxDocumentCapability);
begin
  if FAllowPaste <> Value then
  begin
    FAllowPaste := Value;
    DoChanged(TAction.AllowPaste);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetCreateNew(const Value: TdxDocumentCapability);
begin
  if FAllowCreateNew <> Value then
  begin
    FAllowCreateNew := Value;
    DoChanged(TAction.AllowCreateNew);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetCut(const Value: TdxDocumentCapability);
begin
  if FAllowCut <> Value then
  begin
    FAllowCut := Value;
    DoChanged(TAction.AllowCut);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetDrag(const Value: TdxDocumentCapability);
begin
  if FAllowDrag <> Value then
  begin
    FAllowDrag := Value;
    DoChanged(TAction.AllowDrag);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetDrop(const Value: TdxDocumentCapability);
begin
  if FAllowDrop <> Value then
  begin
    FAllowDrop := Value;
    DoChanged(TAction.AllowDrop);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetFontSource(const Value: TdxRichEditBaseValueSource);
begin
  if FFontSource <> Value then
  begin
    FFontSource := Value;
    DoChanged(TAction.FontSource);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetForeColorSource(const Value: TdxRichEditBaseValueSource);
begin
  if FForeColorSource <> Value then
  begin
    FForeColorSource := Value;
    DoChanged(TAction.ForeColorSource);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetMaxZoomFactor(Value: Single);
begin
  Value := Min(Max(FMinZoomFactor, Value), DefaultMaxZoomFactor);
  if not SameValue(FMaxZoomFactor, Value) then
  begin
    FMaxZoomFactor := Value;
    DoChanged(TAction.MaxZoomFactor);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetMinZoomFactor(Value: Single);
begin
  Value := Max(Min(FMaxZoomFactor, Value), DefaultMinZoomFactor);
  if not SameValue(FMinZoomFactor, Value) then
  begin
    FMinZoomFactor := Value;
    DoChanged(TAction.MinZoomFactor);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetOfficeScrolling(const Value: TdxDocumentCapability);
begin
  if FAllowOfficeScrolling <> Value then
  begin
    FAllowOfficeScrolling := Value;
    DoChanged(TAction.AllowOfficeScrolling);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetOpen(const Value: TdxDocumentCapability);
begin
  if FAllowOpen <> Value then
  begin
    FAllowOpen := Value;
    DoChanged(TAction.AllowOpen);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetOvertypeAllowed(const Value: Boolean);
begin
  if FOvertypeAllowed <> Value then
  begin
    FOvertypeAllowed := Value;
    DoChanged(TAction.OvertypeAllowed);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetPageBreakInsertMode(const Value: TdxPageBreakInsertMode);
begin
  if FPageBreakInsertMode <> Value then
  begin
    FPageBreakInsertMode := Value;
    DoChanged(TAction.PageBreakInsertMode);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetPasteSingleCellAsText(const Value: Boolean);
begin
  if FPasteSingleCellAsText <> Value then
  begin
    FPasteSingleCellAsText := Value;
    DoChanged(TAction.PasteSingleCellAsText);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetPasteLineBreakSubstitution(const Value: TdxLineBreakSubstitute);
begin
  if FPasteLineBreakSubstitution <> Value then
  begin
    FPasteLineBreakSubstitution := Value;
    DoChanged(TAction.PasteLineBreakSubstitution);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetPrinting(const Value: TdxDocumentCapability);
begin
  if FAllowPrinting <> Value then
  begin
    FAllowPrinting := Value;
    DoChanged(TAction.AllowPrinting);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetSave(const Value: TdxDocumentCapability);
begin
  if FAllowSave <> Value then
  begin
    FAllowSave := Value;
    DoChanged(TAction.AllowSave);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetSaveAs(const Value: TdxDocumentCapability);
begin
  if FAllowSaveAs <> Value then
  begin
    FAllowSaveAs := Value;
    DoChanged(TAction.AllowSaveAs);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetShowPopupMenu(const Value: TdxDocumentCapability);
begin
  if FAllowShowPopupMenu <> Value then
  begin
    FAllowShowPopupMenu := Value;
    DoChanged(TAction.AllowShowPopupMenu);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetTabMarker(const Value: string);
begin
  if FTabMarker <> Value then
  begin
    FTabMarker := Value;
    DoChanged(TAction.TabMarker);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetTouch(const Value: TdxDocumentCapability);
begin
  if FAllowTouch <> Value then
  begin
    FAllowTouch := Value;
    DoChanged(TAction.AllowTouch);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetUseFontSubstitution(const Value: Boolean);
begin
  if FUseFontSubstitution <> Value then
  begin
    FUseFontSubstitution := Value;
    DoChanged(TAction.UseFontSubstitution);
  end;
end;

procedure TdxRichEditBehaviorOptions.SetZooming(const Value: TdxDocumentCapability);
begin
  if FAllowZooming <> Value then
  begin
    FAllowZooming := Value;
    DoChanged(TAction.AllowZooming);
  end;
end;

{ TdxRichEditEditingOptions }

procedure TdxRichEditEditingOptions.Assign(Source: TPersistent);
var
  ASource: TdxRichEditEditingOptions;
begin
  BeginUpdate;
  try
    if Source is TdxRichEditEditingOptions then
    begin
      ASource := TdxRichEditEditingOptions(Source);
      MergeParagraphsContent := ASource.MergeParagraphsContent;
      MergeUseFirstParagraphStyle := ASource.MergeUseFirstParagraphStyle;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditEditingOptions.DoReset;
begin
  inherited DoReset;
  FMergeParagraphsContent := True;
  FMergeUseFirstParagraphStyle := True;
end;

procedure TdxRichEditEditingOptions.SetMergeParagraphsContent(const Value: Boolean);
begin
  if FMergeParagraphsContent <> Value then
  begin
    FMergeParagraphsContent := Value;
    DoChanged(TAction.MergeParagraphsContent);
  end;
end;

procedure TdxRichEditEditingOptions.SetMergeUseFirstParagraphStyle(const Value: Boolean);
begin
  if FMergeUseFirstParagraphStyle <> Value then
  begin
    FMergeUseFirstParagraphStyle := Value;
    DoChanged(TAction.MergeUseFirstParagraphStyle);
  end;
end;

{ TdxMailMergeCustomSeparators }

constructor TdxMailMergeCustomSeparators.Create;
begin
  inherited Create;
  Clear;
end;

constructor TdxMailMergeCustomSeparators.Create(const AMaskGroupSeparator: string; const AMaskDecimalSeparator: string;
  const AFieldResultGroupSeparator: string; const AFieldResultDecimalSeparator: string);
begin
  Create;
  FMaskGroupSeparator := AMaskGroupSeparator;
  FMaskDecimalSeparator := AMaskDecimalSeparator;
  FFieldResultGroupSeparator := AFieldResultGroupSeparator;
  FFieldResultDecimalSeparator := AFieldResultDecimalSeparator;
end;

procedure TdxMailMergeCustomSeparators.ResetFieldResultDecimalSeparator;
begin
  FieldResultDecimalSeparator := '';
end;

function TdxMailMergeCustomSeparators.ShouldSerializeFieldResultDecimalSeparator: Boolean;
begin
  Result := FieldResultDecimalSeparator <> '';
end;

procedure TdxMailMergeCustomSeparators.ResetFieldResultGroupSeparator;
begin
  FieldResultGroupSeparator := '';
end;

function TdxMailMergeCustomSeparators.ShouldSerializeFieldResultGroupSeparator: Boolean;
begin
  Result := FieldResultGroupSeparator <> '';
end;

procedure TdxMailMergeCustomSeparators.ResetMaskDecimalSeparator;
begin
  MaskDecimalSeparator := '';
end;

function TdxMailMergeCustomSeparators.ShouldSerializeMaskDecimalSeparator: Boolean;
begin
  Result := MaskDecimalSeparator <> '';
end;

procedure TdxMailMergeCustomSeparators.ResetMaskGroupSeparator;
begin
  MaskGroupSeparator := '';
end;

function TdxMailMergeCustomSeparators.ShouldSerializeMaskGroupSeparator: Boolean;
begin
  Result := MaskGroupSeparator <> '';
end;

procedure TdxMailMergeCustomSeparators.Assign(Source: TPersistent);
var
  ASource: TdxMailMergeCustomSeparators;
begin
  if Source is TdxMailMergeCustomSeparators then
  begin
    ASource := TdxMailMergeCustomSeparators(Source);
    MaskGroupSeparator := ASource.MaskGroupSeparator;
    FieldResultGroupSeparator := ASource.FieldResultGroupSeparator;
    MaskDecimalSeparator := ASource.MaskDecimalSeparator;
    FieldResultDecimalSeparator := ASource.FieldResultDecimalSeparator;
  end;
end;

procedure TdxMailMergeCustomSeparators.Clear;
begin
  ResetFieldResultGroupSeparator;
  ResetFieldResultDecimalSeparator;
  ResetMaskDecimalSeparator;
  ResetMaskGroupSeparator;
end;

function TdxMailMergeCustomSeparators.IsEmpty: Boolean;
begin
  Result := not (ShouldSerializeFieldResultDecimalSeparator or ShouldSerializeFieldResultGroupSeparator or
    ShouldSerializeMaskDecimalSeparator or ShouldSerializeMaskGroupSeparator);
end;

{ TdxRichEditMailMergeOptions }

destructor TdxRichEditMailMergeOptions.Destroy;
begin
  FreeAndNil(FFreeNotificator);
  FreeAndNil(FCustomSeparators);
  inherited Destroy;
end;

procedure TdxRichEditMailMergeOptions.Assign(Source: TPersistent);
var
  ASource: TdxRichEditMailMergeOptions;
begin
  BeginUpdate;
  try
    if Source is TdxRichEditMailMergeOptions then
    begin
      ASource := TdxRichEditMailMergeOptions(Source);
      CustomSeparators := ASource.CustomSeparators;
      DataSource := ASource.DataSource;
      KeepLastParagraph := ASource.KeepLastParagraph;
      ViewMergedData := ASource.ViewMergedData;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditMailMergeOptions.CreateInnerOptions;
begin
  inherited CreateInnerOptions;
  FCustomSeparators := TdxMailMergeCustomSeparators.Create;
end;

procedure TdxRichEditMailMergeOptions.DoReset;
begin
  DataSource := nil;
  ViewMergedData := False;
  KeepLastParagraph := False;
  CustomSeparators.Clear;
end;

procedure TdxRichEditMailMergeOptions.Initialize;
begin
  inherited Initialize;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotificationHandler;
end;

procedure TdxRichEditMailMergeOptions.FreeNotificationHandler(Sender: TComponent);
begin
  if DataSource = Sender then
    DataSource := nil;
end;

procedure TdxRichEditMailMergeOptions.SetCustomSeparators(const Value: TdxMailMergeCustomSeparators);
begin
  FCustomSeparators.Assign(Value);
end;

procedure TdxRichEditMailMergeOptions.SetDataSource(const Value: TDataSource);
begin
  if FDataSource = Value then
    Exit;
  if FDataSource <> nil then
    FFreeNotificator.RemoveSender(FDataSource);
  FDataSource := Value;
  if FDataSource <> nil then
    FFreeNotificator.AddSender(FDataSource);
  DoChanged(TAction.DataSource);
end;

procedure TdxRichEditMailMergeOptions.SetViewMergedData(const Value: Boolean);
begin
  if Value <> FViewMergedData then
  begin
    FViewMergedData := Value;
    DoChanged(TAction.ViewMergedData);
  end;
end;

{ TdxUpdateFieldOptions }

procedure TdxUpdateFieldOptions.CopyFrom(ASource: TdxUpdateFieldOptions);
begin
  Date := ASource.Date;
  Time := ASource.Time;
end;

procedure TdxUpdateFieldOptions.DoReset;
begin
  Date := DefaultDate;
  Time := DefaultTime;
end;

function TdxUpdateFieldOptions.GetNativeOptions: TdxFieldUpdateOnLoadOptions;
begin
  Result := TdxFieldUpdateOnLoadOptions.Create(Date, Time);
end;

procedure TdxUpdateFieldOptions.SetDate(const Value: Boolean);
begin
  if Date <> Value then
  begin
    FDate := Value;
    DoChanged(TAction.Date);
  end;
end;

procedure TdxUpdateFieldOptions.SetTime(const Value: Boolean);
begin
  if Time <> Value then
  begin
    FTime := Value;
    DoChanged(TAction.Time);
  end;
end;

{ TdxDocumentImporterOptions }

destructor TdxDocumentImporterOptions.Destroy;
begin
  FreeAndNil(FUpdateField);
  inherited Destroy;
end;

procedure TdxDocumentImporterOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    CopyFrom(Source);
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentImporterOptions.CopyFrom(const AValue: TObject);
var
  AOptions: TdxDocumentImporterOptions;
begin
  if AValue is TdxDocumentImporterOptions then
  begin
    AOptions := TdxDocumentImporterOptions(AValue);
    ActualEncoding := AOptions.ActualEncoding;
    LineBreakSubstitute := AOptions.LineBreakSubstitute;
    UpdateField.CopyFrom(AOptions.UpdateField);
  end;
end;

function TdxDocumentImporterOptions.IsDefaultEncoding: Boolean;
begin
  Result := False;
end;

procedure TdxDocumentImporterOptions.DoReset;
begin
  FActualEncoding := TEncoding.Default;
  SourceUri := EmptyStr;
  LineBreakSubstitute := TdxLineBreakSubstitute.None;
  CreateUpdateFieldOptions;
  UpdateField.DoReset;
end;

procedure TdxDocumentImporterOptions.CreateInnerOptions;
begin
  inherited CreateInnerOptions;
  CreateUpdateFieldOptions;
end;

procedure TdxDocumentImporterOptions.CreateUpdateFieldOptions;
begin
  if FUpdateField <> nil then
    Exit;
  FUpdateField := TdxUpdateFieldOptions.Create;
end;

function TdxDocumentImporterOptions.GetActualEncoding: TEncoding;
begin
  Result := FActualEncoding;
end;

function TdxDocumentImporterOptions.GetSourceUri: string;
begin
  Result := FSourceUri;
end;

procedure TdxDocumentImporterOptions.SetActualEncoding(const Value: TEncoding);
begin
  if FActualEncoding <> Value then
  begin
    FActualEncoding := Value;
    DoChanged(TAction.Encoding);
  end;
end;

procedure TdxDocumentImporterOptions.SetSourceUri(const Value: string);
begin
  FSourceUri := Value;
end;

procedure TdxDocumentImporterOptions.SetUpdateField(const Value: TdxUpdateFieldOptions);
begin
  FUpdateField.Assign(Value);
end;

function TdxDocumentImporterOptions.ShouldSerializeSourceUri: Boolean;
begin
  Result := False;
end;

procedure TdxDocumentImporterOptions.SubscribeInnerOptions;
begin
  inherited SubscribeInnerOptions;
  FUpdateField.Changed.Add(DoInnerOptionsChanged);
end;

{ TdxRtfDocumentImporterOptions }

class destructor TdxRtfDocumentImporterOptions.Initialize;
begin
  FDefaultEncoding := TEncoding.Default;
  if (FDefaultEncoding.CodePage = 65000) or
      (FDefaultEncoding.CodePage = 65001) or
      (FDefaultEncoding.CodePage = 1200) or
      (FDefaultEncoding.CodePage = 1201) then
    FDefaultEncoding := TdxEncoding.GetEncoding(1252);
end;

procedure TdxRtfDocumentImporterOptions.CopyFrom(const AValue: TObject);
var
  AOptions: TdxRtfDocumentImporterOptions;
begin
  inherited CopyFrom(AValue);
  if AValue is TdxRtfDocumentImporterOptions then
  begin
    AOptions := TdxRtfDocumentImporterOptions(AValue);
    SuppressLastParagraphDelete := AOptions.SuppressLastParagraphDelete;
    CopySingleCellAsText := AOptions.CopySingleCellAsText;
    PasteFromIE := AOptions.PasteFromIE;
  end;
end;

function TdxRtfDocumentImporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Rtf;
end;

procedure TdxRtfDocumentImporterOptions.DoReset;
begin
  inherited DoReset;
  SuppressLastParagraphDelete := False;
  CopySingleCellAsText := False;
  PasteFromIE := False;
  IgnoreDeletedText := False;
end;

procedure TdxRtfDocumentImporterOptions.SetActualEncoding(const Value: TEncoding);
begin
end;

procedure TdxRtfDocumentImporterOptions.SetIgnoreDeletedText(
  const Value: Boolean);
begin
  if IgnoreDeletedText <> Value then
  begin
    FIgnoreDeletedText := Value;
    DoChanged(TAction.IgnoreDeletedText);
  end;
end;

{ TdxPlainTextDocumentImporterOptions }

procedure TdxPlainTextDocumentImporterOptions.CopyFrom(const AValue: TObject);
var
  AOptions: TdxPlainTextDocumentImporterOptions;
begin
  inherited CopyFrom(AValue);
  if AValue is TdxPlainTextDocumentImporterOptions then
  begin
    AOptions := TdxPlainTextDocumentImporterOptions(AValue);
    AutoDetectEncoding := AOptions.AutoDetectEncoding;
    Encoding := AOptions.Encoding;
  end;
end;

function TdxPlainTextDocumentImporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.PlainText;
end;

function TdxPlainTextDocumentImporterOptions.GetEncoding: Word;
begin
  if IsDefaultEncoding then
    Result := CP_ACP
  else
    Result := ActualEncoding.CodePage;
end;

function TdxPlainTextDocumentImporterOptions.IsDefaultEncoding: Boolean;
begin
  Result := ActualEncoding = TEncoding.Default;
end;

procedure TdxPlainTextDocumentImporterOptions.DoReset;
begin
  inherited DoReset;
  ActualEncoding := TEncoding.Default;
  AutoDetectEncoding := True;
end;

procedure TdxPlainTextDocumentImporterOptions.SetAutoDetectEncoding(const AValue: Boolean);
begin
  if FAutoDetectEncoding <> AValue then
  begin
    FAutoDetectEncoding := AValue;
    DoChanged(TAction.AutoDetectEncoding);
  end;
end;

procedure TdxPlainTextDocumentImporterOptions.SetEncoding(const AValue: Word);
begin
  if Encoding <> AValue then
  begin
    if AValue = CP_ACP then
      ActualEncoding := TEncoding.Default
    else
      ActualEncoding := TdxEncoding.GetEncoding(AValue);
  end;
end;

{ TdxXmlBasedDocumentImporterOptions }

procedure TdxXmlBasedDocumentImporterOptions.SetIgnoreParseErrors(const AValue: Boolean);
begin
  if AValue <> FIgnoreParseErrors then
  begin
    FIgnoreParseErrors := AValue;
    DoChanged(TAction.IgnoreParseErrors);
  end;
end;

procedure TdxXmlBasedDocumentImporterOptions.DoReset;
begin
  inherited DoReset;
  IgnoreParseErrors := False;
end;

procedure TdxXmlBasedDocumentImporterOptions.CopyFrom(const AValue: TObject);
var
  AOptions: TdxXmlBasedDocumentImporterOptions;
begin
  inherited CopyFrom(AValue);
  AOptions := Safe<TdxXmlBasedDocumentImporterOptions>.Cast(AValue);
  if AOptions <> nil then
    IgnoreParseErrors := AOptions.IgnoreParseErrors;
end;

{ TdxHtmlDocumentImporterOptions }

procedure TdxHtmlDocumentImporterOptions.SetAutoDetectEncoding(const AValue: Boolean);
begin
  if AValue = FAutoDetectEncoding then
    Exit;

  FAutoDetectEncoding := AValue;
  DoChanged(TAction.AutoDetectEncoding);
end;

procedure TdxHtmlDocumentImporterOptions.SetReplaceSpaceWithNonBreakingSpaceInsidePre(const AValue: Boolean);
begin
  if AValue = FReplaceSpaceWithNonBreakingSpaceInsidePre then
    Exit;

  FReplaceSpaceWithNonBreakingSpaceInsidePre := AValue;
  DoChanged(TAction.ReplaceSpaceWithNonBreakingSpaceInsidePre);
end;

procedure TdxHtmlDocumentImporterOptions.SetIgnoreMetaCharset(const AValue: Boolean);
begin
  if FIgnoreMetaCharset = AValue then
    Exit;
  FIgnoreMetaCharset := AValue;
  DoChanged(TAction.IgnoreMetaCharset);
end;

procedure TdxHtmlDocumentImporterOptions.SetIgnoreFloatProperty(const AValue: Boolean);
begin
  if FIgnoreFloatProperty = AValue then
    Exit;
  FIgnoreFloatProperty := AValue;
  DoChanged(TAction.IgnoreFloatProperty);
end;

procedure TdxHtmlDocumentImporterOptions.SetAsyncImageLoading(const AValue: Boolean);
begin
  if FAsyncImageLoading = AValue then
    Exit;
  FAsyncImageLoading := AValue;
  DoChanged(TAction.AsyncImageLoading);
end;

procedure TdxHtmlDocumentImporterOptions.SetDefaultTableCellMargin(const AValue: Integer);
begin
  if FDefaultTableCellMargin = AValue then
    Exit;
  FDefaultTableCellMargin := AValue;
  DoChanged(TAction.DefaultTableCellMargin);
end;

procedure TdxHtmlDocumentImporterOptions.SetDefaultTableCellSpacing(const AValue: Integer);
begin
  if FDefaultTableCellSpacing = AValue then
    Exit;
  FDefaultTableCellSpacing := AValue;
  DoChanged(TAction.DefaultTableCellSpacing);
end;

procedure TdxHtmlDocumentImporterOptions.SetEncoding(const AValue: Word);
begin
  ActualEncoding := TdxEncoding.GetEncoding(AValue);
end;

procedure TdxHtmlDocumentImporterOptions.CopyFrom(const AValue: TObject);
var
  AOptions: TdxHtmlDocumentImporterOptions;
begin
  inherited CopyFrom(AValue);
  AOptions := Safe<TdxHtmlDocumentImporterOptions>.Cast(AValue);
  if AOptions <> nil then
  begin
    DefaultAsyncImageLoading := AOptions.DefaultAsyncImageLoading;
    AsyncImageLoading := AOptions.AsyncImageLoading;
    AutoDetectEncoding := AOptions.AutoDetectEncoding;
    IgnoreMetaCharset := AOptions.IgnoreMetaCharset;
    DefaultTableCellSpacing := AOptions.DefaultTableCellSpacing;
    FDefaultTableCellMargin := AOptions.FDefaultTableCellMargin;
    ReplaceSpaceWithNonBreakingSpaceInsidePre := AOptions.ReplaceSpaceWithNonBreakingSpaceInsidePre;
    IgnoreFloatProperty := AOptions.IgnoreFloatProperty;
  end;
end;

procedure TdxHtmlDocumentImporterOptions.DoReset;
begin
  inherited DoReset;

  Encoding := CP_UTF8;
  AutoDetectEncoding := True;
  IgnoreMetaCharset := False;
  AsyncImageLoading := True;
  DefaultTableCellSpacing := DefaultTableCellSpacingDefaultValue;
  FDefaultTableCellMargin := DefaultTableCellMargingDefaultValue;
  ReplaceSpaceWithNonBreakingSpaceInsidePre := False;
  IgnoreFloatProperty := False;
end;

function TdxHtmlDocumentImporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Html;
end;

function TdxHtmlDocumentImporterOptions.GetEncoding: Word;
begin
  Result := ActualEncoding.CodePage;
end;

function TdxHtmlDocumentImporterOptions.IsDefaultEncoding: Boolean;
begin
  Result := ActualEncoding = TdxEncoding.UTF8;
end;

{ TdxMhtDocumentImporterOptions }

function TdxMhtDocumentImporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat(NotImplemented);
end;

{ TdxOpenXmlDocumentImporterOptions }

procedure TdxOpenXmlDocumentImporterOptions.DoReset;
begin
  inherited DoReset;
  IgnoreDeletedText := True;
  IgnoreInsertedText := True;
end;

function TdxOpenXmlDocumentImporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.OpenXml;
end;

procedure TdxOpenXmlDocumentImporterOptions.SetIgnoreDeletedText(const Value: Boolean);
begin
  if FIgnoreDeletedText <> Value then
  begin
    FIgnoreDeletedText := Value;
    DoChanged(TAction.IgnoreDeletedText);
  end;
end;

procedure TdxOpenXmlDocumentImporterOptions.SetIgnoreInsertedText(const Value: Boolean);
begin
  if Value <> FIgnoreInsertedText then
  begin
    FIgnoreInsertedText := Value;
    DoChanged(TAction.IgnoreInsertedText);
  end;
end;

{ TdxOpenDocumentImporterOptions }

function TdxOpenDocumentImporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat(NotImplemented);
end;

{ TdxDocDocumentImporterOptions }

procedure TdxDocDocumentImporterOptions.CopyFrom(const AValue: TObject);
var
  AOptions: TdxDocDocumentImporterOptions;
begin
  inherited CopyFrom(AValue);
  AOptions := Safe<TdxDocDocumentImporterOptions>.Cast(AValue);
  if AOptions <> nil then
  begin
    IgnoreDeletedText := AOptions.IgnoreDeletedText;
    KeepBookmarksForRemovedRanges := AOptions.KeepBookmarksForRemovedRanges;
    KeepPermissionsForRemovedRanges := AOptions.KeepPermissionsForRemovedRanges;
  end;
end;

procedure TdxDocDocumentImporterOptions.DoReset;
begin
  inherited DoReset;
  FIgnoreDeletedText := DefaultIgnoreDeletedText;
  FKeepBookmarksForRemovedRanges := DefaultKeepBookmarksForRemovedRanges;
  FKeepPermissionsForRemovedRanges := DefaultKeepPermissionsForRemovedRanges;
end;

function TdxDocDocumentImporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Doc;
end;

procedure TdxDocDocumentImporterOptions.SetIgnoreDeletedText(const AValue: Boolean);
begin
  if AValue <> FIgnoreDeletedText then
  begin
    FIgnoreDeletedText := AValue;
    DoChanged(TAction.IgnoreDeletedText);
  end;
end;

procedure TdxDocDocumentImporterOptions.SetKeepBookmarksForRemovedRanges(const AValue: Boolean);
begin
  if AValue <> FKeepBookmarksForRemovedRanges then
  begin
    FKeepBookmarksForRemovedRanges := AValue;
    DoChanged(TAction.KeepBookmarksForRemovedRanges);
  end;
end;

procedure TdxDocDocumentImporterOptions.SetKeepPermissionsForRemovedRanges(const AValue: Boolean);
begin
  if AValue <> FKeepPermissionsForRemovedRanges then
  begin
    FKeepPermissionsForRemovedRanges := AValue;
    DoChanged(TAction.KeepPermissionsForRemovedRanges);
  end;
end;

{ TdxWordMLDocumentImporterOptions }

procedure TdxWordMLDocumentImporterOptions.DoReset;
begin
  inherited DoReset;
  ActualEncoding := TEncoding.UTF8;
end;

function TdxWordMLDocumentImporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat(NotImplemented);
end;

{ TdxRichEditDocumentImportOptions }

procedure TdxRichEditDocumentImportOptions.CopyFrom(AOptions: TdxRichEditDocumentImportOptions);
var
  AKey: TdxRichEditDocumentFormat;
  ASourceOptions: TdxDocumentImporterOptions;
begin
  for AKey := Low(TdxRichEditDocumentFormat) to High(TdxRichEditDocumentFormat) do
  begin
    ASourceOptions := AOptions.OptionsTable[AKey];
    if ASourceOptions <> nil then
      FOptionsTable[AKey].CopyFrom(ASourceOptions);
  end;
  FallbackFormat := AOptions.FallbackFormat;
end;

destructor TdxRichEditDocumentImportOptions.Destroy;
begin
  FreeAndNil(FDoc);
  FreeAndNil(FWordML);
  FreeAndNil(FOpenDocument);
  FreeAndNil(FOpenXml);
  FreeAndNil(FMht);
  FreeAndNil(FHtml);
  FreeAndNil(FPlainText);
  FreeAndNil(FRtf);
  inherited Destroy;
end;

procedure TdxRichEditDocumentImportOptions.DoReset;
var
  AOptions: TdxDocumentImporterOptions;
begin
  for AOptions in FOptionsTable do
    if AOptions <> nil then
      AOptions.Reset;
  FallbackFormat := FallbackFormatValue;
end;

function TdxRichEditDocumentImportOptions.GetOptions(AFormat: TdxRichEditDocumentFormat): TdxDocumentImporterOptions;
begin
  Result := FOptionsTable[AFormat];
end;

procedure TdxRichEditDocumentImportOptions.RegisterOptions(AOptions: TdxDocumentImporterOptions);
begin
  FOptionsTable[AOptions.Format] := AOptions;
end;

procedure TdxRichEditDocumentImportOptions.ResetFallbackFormat;
begin
  FallbackFormat := FallbackFormatValue;
end;

procedure TdxRichEditDocumentImportOptions.SetDoc(const Value: TdxDocDocumentImporterOptions);
begin
  FDoc.Assign(Value);
end;

procedure TdxRichEditDocumentImportOptions.SetFallbackFormat(const Value: TdxRichEditDocumentFormat);
begin
  if FFallbackFormat <> Value then
  begin
    FFallbackFormat := Value;
    DoChanged(TAction.FallbackFormat);
  end;
end;

procedure TdxRichEditDocumentImportOptions.SetHtml(const Value: TdxHtmlDocumentImporterOptions);
begin
  FHtml.Assign(Value);
end;

procedure TdxRichEditDocumentImportOptions.SetMht(const Value: TdxMhtDocumentImporterOptions);
begin
  FMht.Assign(Value);
end;

procedure TdxRichEditDocumentImportOptions.SetOpenDocument(const Value: TdxOpenDocumentImporterOptions);
begin
  FOpenDocument.Assign(Value);
end;

procedure TdxRichEditDocumentImportOptions.SetOpenXml(const Value: TdxOpenXmlDocumentImporterOptions);
begin
  FOpenXml.Assign(Value);
end;

procedure TdxRichEditDocumentImportOptions.SetPlainText(const Value: TdxPlainTextDocumentImporterOptions);
begin
  FPlainText.Assign(Value);
end;

procedure TdxRichEditDocumentImportOptions.SetRtf(const Value: TdxRtfDocumentImporterOptions);
begin
  FRtf.Assign(Value);
end;

procedure TdxRichEditDocumentImportOptions.SetWordML(const Value: TdxWordMLDocumentImporterOptions);
begin
  FWordML.Assign(Value);
end;

function TdxRichEditDocumentImportOptions.ShouldSerializeFallbackFormat: Boolean;
begin
  Result := FallbackFormat <> FallbackFormatValue;
end;

procedure TdxRichEditDocumentImportOptions.CreateInnerOptions;
begin
  inherited CreateInnerOptions;
  FDoc := CreateDocOptions;
  FRtf := CreateRtfOptions;
  FPlainText := CreatePlainTextOptions;
  FHtml := CreateHtmlOptions;
  FMht := CreateMhtOptions;
  FOpenXml := CreateOpenXmlOptions;
  FOpenDocument := CreateOpenDocumentOptions;
  FWordML := CreateWordMLOptions;

  RegisterOptions(FDoc);
  RegisterOptions(FRtf);
  RegisterOptions(FPlainText);
  RegisterOptions(FHtml);
  RegisterOptions(FOpenXml);
end;

function TdxRichEditDocumentImportOptions.CreateDocOptions: TdxDocDocumentImporterOptions;
begin
  Result := TdxDocDocumentImporterOptions.Create;
end;

function TdxRichEditDocumentImportOptions.CreateHtmlOptions: TdxHtmlDocumentImporterOptions;
begin
  Result := TdxHtmlDocumentImporterOptions.Create;
end;

function TdxRichEditDocumentImportOptions.CreateMhtOptions: TdxMhtDocumentImporterOptions;
begin
  Result := TdxMhtDocumentImporterOptions.Create;
end;

function TdxRichEditDocumentImportOptions.CreateOpenDocumentOptions: TdxOpenDocumentImporterOptions;
begin
  Result := TdxOpenDocumentImporterOptions.Create;
end;

function TdxRichEditDocumentImportOptions.CreateOpenXmlOptions: TdxOpenXmlDocumentImporterOptions;
begin
  Result := TdxOpenXmlDocumentImporterOptions.Create;
end;

function TdxRichEditDocumentImportOptions.CreatePlainTextOptions: TdxPlainTextDocumentImporterOptions;
begin
  Result := TdxPlainTextDocumentImporterOptions.Create;
end;

function TdxRichEditDocumentImportOptions.CreateRtfOptions: TdxRtfDocumentImporterOptions;
begin
  Result := TdxRtfDocumentImporterOptions.Create;
end;

function TdxRichEditDocumentImportOptions.CreateWordMLOptions: TdxWordMLDocumentImporterOptions;
begin
  Result := TdxWordMLDocumentImporterOptions.Create;
end;

{ TdxDocumentExporterOptions }

procedure TdxDocumentExporterOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    CopyFrom(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxDocumentExporterOptions.CopyFrom(const AValue: IdxExporterOptions);
begin
  if AValue is TdxDocumentExporterOptions then
    ActualEncoding := TdxDocumentExporterOptions(AValue).ActualEncoding;
end;

procedure TdxDocumentExporterOptions.DoReset;
begin
  FActualEncoding := TEncoding.Default;
  TargetUri := EmptyStr;
end;

function TdxDocumentExporterOptions.GetActualEncoding: TEncoding;
begin
  Result := FActualEncoding;
end;

procedure TdxDocumentExporterOptions.SetActualEncoding(const Value: TEncoding);
begin
  if FActualEncoding <> Value then
  begin
    FActualEncoding := Value;
    DoChanged(TAction.Encoding);
  end;
end;

procedure TdxDocumentExporterOptions.CopyFrom(const Source: TObject);
var
  AIntf: IdxExporterOptions;
begin
  if Supports(Source, IdxExporterOptions, AIntf) then
    CopyFrom(AIntf);
end;

function TdxDocumentExporterOptions.GetTargetUri: string;
begin
  Result := FTargetUri;
end;

function TdxDocumentExporterOptions.IsDefaultEncoding: Boolean;
begin
  Result := False;
end;

procedure TdxDocumentExporterOptions.SetTargetUri(const Value: string);
begin
  FTargetUri := Value;
end;

function TdxDocumentExporterOptions.ShouldSerializeTargetUri: Boolean;
begin
  Result := False;
end;

{ TdxRtfDocumentExporterCompatibilityOptions }

procedure TdxRtfDocumentExporterCompatibilityOptions.CopyFrom(const AValue: TdxRtfDocumentExporterCompatibilityOptions);
begin
  DuplicateObjectAsMetafile := AValue.DuplicateObjectAsMetafile;
  BackColorExportMode := AValue.BackColorExportMode;
  Kerning := AValue.Kerning;
end;

procedure TdxRtfDocumentExporterCompatibilityOptions.DoReset;
begin
  ResetDuplicateObjectAsMetafile;
  ResetBackColorExportMode;
  Kerning := False;
end;

procedure TdxRtfDocumentExporterCompatibilityOptions.ResetBackColorExportMode;
begin
  BackColorExportMode := TdxRtfRunBackColorExportMode.Chcbpat;
end;

procedure TdxRtfDocumentExporterCompatibilityOptions.ResetDuplicateObjectAsMetafile;
begin
  DuplicateObjectAsMetafile := False;
end;

procedure TdxRtfDocumentExporterCompatibilityOptions.SetBackColorExportMode(const Value: TdxRtfRunBackColorExportMode);
begin
  if FBackColorExportMode <> Value then
  begin
    FBackColorExportMode := Value;
    DoChanged(TAction.BackColorExportMode);
  end;
end;

procedure TdxRtfDocumentExporterCompatibilityOptions.SetDuplicateObjectAsMetafile(const Value: Boolean);
begin
  if FDuplicateObjectAsMetafile <> Value then
  begin
    FDuplicateObjectAsMetafile := Value;
    DoChanged(TAction.DuplicateObjectAsMetafile);
  end;
end;

function TdxRtfDocumentExporterCompatibilityOptions.ShouldSerializeBackColorExportMode: Boolean;
begin
  Result := BackColorExportMode <> TdxRtfRunBackColorExportMode.Chcbpat;
end;

function TdxRtfDocumentExporterCompatibilityOptions.ShouldSerializeDuplicateObjectAsMetafile: Boolean;
begin
  Result := DuplicateObjectAsMetafile <> False;
end;

{ TdxRtfDocumentExporterOptions }

destructor TdxRtfDocumentExporterOptions.Destroy;
begin
  FreeAndNil(FCompatibility);
  inherited Destroy;
end;

procedure TdxRtfDocumentExporterOptions.CopyFrom(const Value: IdxExporterOptions);
var
  AOptions: TdxRtfDocumentExporterOptions;
begin
  inherited CopyFrom(Value);
  if Value is TdxRtfDocumentExporterOptions then
  begin
    AOptions := TdxRtfDocumentExporterOptions(Value);
    WrapContentInGroup := AOptions.WrapContentInGroup;
    ListExportFormat := AOptions.ListExportFormat;
    Compatibility.CopyFrom(AOptions.Compatibility);
    ExportFinalParagraphMark := AOptions.ExportFinalParagraphMark;
  end;
end;

function TdxRtfDocumentExporterOptions.CreateCompatibilityOptions: TdxRtfDocumentExporterCompatibilityOptions;
begin
  Result := TdxRtfDocumentExporterCompatibilityOptions.Create;
end;

procedure TdxRtfDocumentExporterOptions.CreateInnerOptions;
begin
  inherited CreateInnerOptions;
  FCompatibility := CreateCompatibilityOptions;
end;

procedure TdxRtfDocumentExporterOptions.DoReset;
begin
  inherited DoReset;
  ListExportFormat := TdxRtfNumberingListExportFormat.RtfFormat;
  WrapContentInGroup := False;
  ExportFinalParagraphMark := TdxExportFinalParagraphMark.Always;
  if FCompatibility <> nil then
    FCompatibility.Reset;
end;

function TdxRtfDocumentExporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Rtf;
end;

procedure TdxRtfDocumentExporterOptions.ResetExportFinalParagraphMark;
begin
  ExportFinalParagraphMark := TdxExportFinalParagraphMark.Always;
end;

procedure TdxRtfDocumentExporterOptions.ResetListExportFormat;
begin
  ListExportFormat := TdxRtfNumberingListExportFormat.RtfFormat;
end;

procedure TdxRtfDocumentExporterOptions.ResetWrapContentInGroup;
begin
  WrapContentInGroup := False;
end;

procedure TdxRtfDocumentExporterOptions.SetActualEncoding(const Value: TEncoding);
begin
  inherited SetActualEncoding(Value);
end;

procedure TdxRtfDocumentExporterOptions.SetCompatibility(const Value: TdxRtfDocumentExporterCompatibilityOptions);
begin
  FCompatibility.CopyFrom(Value);
end;

procedure TdxRtfDocumentExporterOptions.SetExportFinalParagraphMark(const Value: TdxExportFinalParagraphMark);
begin
  if FExportFinalParagraphMark <> Value then
  begin
    FExportFinalParagraphMark := Value;
    DoChanged(TAction.ExportFinalParagraphMark);
  end;
end;

procedure TdxRtfDocumentExporterOptions.SetListExportFormat(const Value: TdxRtfNumberingListExportFormat);
begin
  if FListExportFormat <> Value then
  begin
    FListExportFormat := Value;
    DoChanged(TAction.ListExportFormat);
  end;
end;

procedure TdxRtfDocumentExporterOptions.SetWrapContentInGroup(const Value: Boolean);
begin
  if FWrapContentInGroup <> Value then
  begin
    FWrapContentInGroup := Value;
    DoChanged(TAction.WrapContentInGroup);
  end;
end;

function TdxRtfDocumentExporterOptions.ShouldSerializeExportFinalParagraphMark: Boolean;
begin
  Result := ExportFinalParagraphMark <> TdxExportFinalParagraphMark.Always;
end;

function TdxRtfDocumentExporterOptions.ShouldSerializeListExportFormat: Boolean;
begin
  Result := ListExportFormat <> TdxRtfNumberingListExportFormat.RtfFormat;
end;

function TdxRtfDocumentExporterOptions.ShouldSerializeWrapContentInGroup: Boolean;
begin
  Result := WrapContentInGroup <> False;
end;

{ TdxPlainTextDocumentExporterOptions }

procedure TdxPlainTextDocumentExporterOptions.CopyFrom(const Value: IdxExporterOptions);
var
  AOptions: TdxPlainTextDocumentExporterOptions;
begin
  inherited CopyFrom(Value);
  if TObject(Value) is TdxPlainTextDocumentExporterOptions then
  begin
    AOptions := TdxPlainTextDocumentExporterOptions(Value);
    WriteEncodingPreamble := AOptions.WriteEncodingPreamble;
    ExportBulletsAndNumbering := AOptions.ExportBulletsAndNumbering;
    ExportFinalParagraphMark := AOptions.ExportFinalParagraphMark;
    ExportHiddenText := AOptions.ExportHiddenText;
    FieldCodeStartMarker := AOptions.FieldCodeStartMarker;
    FieldCodeEndMarker := AOptions.FieldCodeEndMarker;
    FieldResultEndMarker := AOptions.FieldResultEndMarker;
    FootNoteNumberStringFormat := AOptions.FootNoteNumberStringFormat;
    EndNoteNumberStringFormat := AOptions.EndNoteNumberStringFormat;
    FootNoteSeparator := AOptions.FootNoteSeparator;
    EndNoteSeparator := AOptions.EndNoteSeparator;
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.DoReset;
begin
  inherited DoReset;
  WriteEncodingPreamble := True;
  ExportBulletsAndNumbering := True;
  ExportFinalParagraphMark := DefaultExportFinalParagraphMark;
  ExportHiddenText := False;
  ExportBulletsAndNumbering := True;
  FieldCodeStartMarker := EmptyStr;
  FieldCodeEndMarker := EmptyStr;
  FieldResultEndMarker := EmptyStr;
  FootNoteNumberStringFormat := EmptyStr;
  EndNoteNumberStringFormat := EmptyStr;
  FootNoteSeparator := EmptyStr;
  EndNoteSeparator := EmptyStr;
  ActualEncoding := TEncoding.Default;
end;

function TdxPlainTextDocumentExporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.PlainText;
end;

function TdxPlainTextDocumentExporterOptions.GetActualEndNoteNumberStringFormat: string;
begin
  if EndNoteNumberStringFormat = '' then
    Result := '[%d]'
  else
    Result := EndNoteNumberStringFormat;
end;

function TdxPlainTextDocumentExporterOptions.GetActualEndNoteSeparator: string;
begin
  if EndNoteSeparator = '' then
    Result := DefaultEndNoteSeparator
  else
    Result := EndNoteSeparator;
end;

function TdxPlainTextDocumentExporterOptions.GetActualFootNoteNumberStringFormat: string;
begin
  if FootNoteNumberStringFormat = '' then
    Result := '[%d]'
  else
    Result := FootNoteNumberStringFormat;
end;

function TdxPlainTextDocumentExporterOptions.GetActualFootNoteSeparator: string;
begin
  if FootNoteSeparator = '' then
    Result := DefaultFootNoteSeparator
  else
    Result := FootNoteSeparator;
end;

function TdxPlainTextDocumentExporterOptions.GetEncoding: Word;
begin
  if IsDefaultEncoding then
    Result := CP_ACP
  else
    Result := ActualEncoding.CodePage;
end;

function TdxPlainTextDocumentExporterOptions.IsDefaultEncoding: Boolean;
begin
  Result := ActualEncoding = TEncoding.Default;
end;

procedure TdxPlainTextDocumentExporterOptions.SetEncoding(const Value: Word);
begin
  if Encoding <> Value then
  begin
    if Value = CP_ACP then
      ActualEncoding := TEncoding.Default
    else
      ActualEncoding := TdxEncoding.GetEncoding(Value);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetEndNoteNumberStringFormat(const Value: string);
begin
  if FEndNoteNumberStringFormat <> Value then
  begin
    FEndNoteNumberStringFormat := Value;
    DoChanged(TAction.EndNoteNumberStringFormat);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetExportBulletsAndNumbering(const Value: Boolean);
begin
  if FExportBulletsAndNumbering <> Value then
  begin
    FExportBulletsAndNumbering := Value;
    DoChanged(TAction.ExportBulletsAndNumbering);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetExportFinalParagraphMark(const Value: TdxExportFinalParagraphMark);
begin
  if FExportFinalParagraphMark <> Value then
  begin
    FExportFinalParagraphMark := Value;
    DoChanged(TAction.ExportFinalParagraphMark);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetExportHiddenText(const Value: Boolean);
begin
  if FExportHiddenText <> Value then
  begin
    FExportHiddenText := Value;
    DoChanged(TAction.ExportHiddenText);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetFieldCodeEndMarker(const Value: string);
begin
  if FFieldCodeEndMarker <> Value then
  begin
    FFieldCodeEndMarker := Value;
    DoChanged(TAction.FieldCodeEndMarker);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetFieldCodeStartMarker(const Value: string);
begin
  if FFieldCodeStartMarker <> Value then
  begin
    FFieldCodeStartMarker := Value;
    DoChanged(TAction.FieldCodeStartMarker);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetFieldResultEndMarker(const Value: string);
begin
  if FFieldResultEndMarker <> Value then
  begin
    FFieldResultEndMarker := Value;
    DoChanged(TAction.FieldResultEndMarker);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetFootNoteNumberStringFormat(const Value: string);
begin
  if FFootNoteNumberStringFormat <> Value then
  begin
    FFootNoteNumberStringFormat := Value;
    DoChanged(TAction.FootNoteNumberStringFormat);
  end;
end;

procedure TdxPlainTextDocumentExporterOptions.SetFootNoteSeparator(const Value: string);
begin
  if FFootNoteSeparator <> Value then
  begin
    FFootNoteSeparator := Value;
    DoChanged(TAction.FootNoteSeparator);
  end;
end;

{ TdxHtmlDocumentExporterOptions }

procedure TdxHtmlDocumentExporterOptions.CopyFrom(const Value: IdxExporterOptions);
var
  AOptions: TdxHtmlDocumentExporterOptions;
begin
  inherited CopyFrom(Value);
  AOptions := TdxHtmlDocumentExporterOptions(Value);
  if AOptions <> nil then
  begin
    HtmlNumberingListExportFormat := AOptions.HtmlNumberingListExportFormat;
    CssPropertiesExportType := AOptions.CssPropertiesExportType;
    ExportRootTag := AOptions.ExportRootTag;
    UriExportType := AOptions.UriExportType;
    EmbedImages := AOptions.EmbedImages;
    TabMarker := AOptions.TabMarker;
    FontUnit := AOptions.FontUnit;
    UnderlineTocHyperlinks := AOptions.UnderlineTocHyperlinks;
    OverrideImageResolution := AOptions.OverrideImageResolution;
    FootNoteNumberStringFormat := AOptions.FootNoteNumberStringFormat;
    EndNoteNumberStringFormat := AOptions.EndNoteNumberStringFormat;
    DisposeConvertedImagesImmediately := AOptions.DisposeConvertedImagesImmediately;
    DefaultCharacterPropertiesExportToCss := AOptions.DefaultCharacterPropertiesExportToCss;
    ExportImageSize := AOptions.ExportImageSize;
    KeepExternalImageSize := AOptions.KeepExternalImageSize;
    UseHtml5 := AOptions.UseHtml5;
    IgnoreParagraphOutlineLevel := AOptions.IgnoreParagraphOutlineLevel;
  end;
end;

function TdxHtmlDocumentExporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Html;
end;

procedure TdxHtmlDocumentExporterOptions.DoReset;
begin
  inherited DoReset;
  HtmlNumberingListExportFormat := TdxHtmlNumberingListExportFormat.HtmlFormat;
  CssPropertiesExportType := TdxCssPropertiesExportType.Style;
  ExportRootTag := TdxExportRootTag.Html;
  Encoding := CP_UTF8;
  UriExportType := TdxUriExportType.Relative;
  EmbedImages := False;
  TabMarker := TdxCharacters.TabMark;
  FontUnit := TdxHtmlFontUnit.Point;
  UnderlineTocHyperlinks := True;
  OverrideImageResolution := DefaultOverrideImageResolution;
  FootNoteNumberStringFormat := '';
  EndNoteNumberStringFormat := '';
  DisposeConvertedImagesImmediately := True;
  DefaultCharacterPropertiesExportToCss := True;
  ExportImageSize := TdxExportImageSize.Auto;
  KeepExternalImageSize := False;
  UseHtml5 := False;
  IgnoreParagraphOutlineLevel := False;
end;

procedure TdxHtmlDocumentExporterOptions.SetHtmlNumberingListExportFormat(const Value: TdxHtmlNumberingListExportFormat);
begin
  if FHtmlNumberingListExportFormat = Value then
    Exit;

  FHtmlNumberingListExportFormat := Value;
  DoChanged(TAction.HtmlNumberingListExportFormat);
end;

function TdxHtmlDocumentExporterOptions.ShouldSerializeHtmlNumberingListExportFormat: Boolean;
begin
  Result := HtmlNumberingListExportFormat <> TdxHtmlNumberingListExportFormat.HtmlFormat;
end;

procedure TdxHtmlDocumentExporterOptions.ResetHtmlNumberingListExportFormat;
begin
  HtmlNumberingListExportFormat := TdxHtmlNumberingListExportFormat.HtmlFormat;
end;

procedure TdxHtmlDocumentExporterOptions.SetCssPropertiesExportType(const Value: TdxCssPropertiesExportType);
begin
  if FCssPropertiesExportType = Value then
    Exit;

  FCssPropertiesExportType := Value;
  DoChanged(TAction.CssPropertiesExportType);
end;

function TdxHtmlDocumentExporterOptions.ShouldSerializeCssPropertiesExportType: Boolean;
begin
  Result := CssPropertiesExportType <> TdxCssPropertiesExportType.Style;
end;

procedure TdxHtmlDocumentExporterOptions.ResetCssPropertiesExportType;
begin
  CssPropertiesExportType := TdxCssPropertiesExportType.Style;
end;

procedure TdxHtmlDocumentExporterOptions.SetUriExportType(const Value: TdxUriExportType);
begin
  if FUriExportType = Value then
    Exit;

  FUriExportType := Value;
  DoChanged(TAction.UriExportType);
end;

function TdxHtmlDocumentExporterOptions.ShouldSerializeUriExportType: Boolean;
begin
  Result := UriExportType <> TdxUriExportType.Relative;
end;

procedure TdxHtmlDocumentExporterOptions.ResetUriExportType;
begin
  UriExportType := TdxUriExportType.Relative;
end;

procedure TdxHtmlDocumentExporterOptions.SetExportRootTag(const Value: TdxExportRootTag);
begin
  if FExportRootTag = Value then
    Exit;

  FExportRootTag := Value;
  DoChanged(TAction.ExportRootTag);
end;

function TdxHtmlDocumentExporterOptions.ShouldSerializeExportRootTag: Boolean;
begin
  Result := ExportRootTag <> TdxExportRootTag.Html;
end;

procedure TdxHtmlDocumentExporterOptions.ResetExportRootTag;
begin
  ExportRootTag := TdxExportRootTag.Html;
end;

procedure TdxHtmlDocumentExporterOptions.SetExportImageSize(const Value: TdxExportImageSize);
begin
  if FExportImageSize = Value then
    Exit;

  FExportImageSize := Value;
  DoChanged(TAction.ExportImageSize);
end;

function TdxHtmlDocumentExporterOptions.ShouldSerializeExportImageSize: Boolean;
begin
  Result := ExportImageSize <> TdxExportImageSize.Auto;
end;

procedure TdxHtmlDocumentExporterOptions.ResetExportImageTag;
begin
  ExportImageSize := TdxExportImageSize.Auto;
end;

procedure TdxHtmlDocumentExporterOptions.SetKeepExternalImageSize(const Value: Boolean);
begin
  if KeepExternalImageSize = Value then
    Exit;

  FKeepExternalImageSize := Value;
  DoChanged(TAction.KeepExternalImageSize);
end;

function TdxHtmlDocumentExporterOptions.ShouldSerializeEncoding: Boolean;
begin
  Result := Encoding <> CP_UTF8;
end;

procedure TdxHtmlDocumentExporterOptions.ResetEncoding;
begin
  Encoding := CP_UTF8;
end;

function TdxHtmlDocumentExporterOptions.GetEmbedImages: Boolean;
begin
  Result := FEmbedImages;
end;

procedure TdxHtmlDocumentExporterOptions.SetEmbedImages(const Value: Boolean);
begin
  if FEmbedImages = Value then
    Exit;
  FEmbedImages := Value;
  DoChanged(TAction.EmbedImages);
end;

function TdxHtmlDocumentExporterOptions.GetDefaultCharacterPropertiesExportToCss: Boolean;
begin
  Result := FDefaultCharacterPropertiesExportToCss;
end;

procedure TdxHtmlDocumentExporterOptions.SetDefaultCharacterPropertiesExportToCss(const Value: Boolean);
begin
  if FDefaultCharacterPropertiesExportToCss = Value then
    Exit;
  FDefaultCharacterPropertiesExportToCss := Value;
  DoChanged(TAction.DefaultCharacterPropertiesExportToCss);
end;

procedure TdxHtmlDocumentExporterOptions.SetTabMarker(const Value: string);
begin
  if FTabMarker = Value then
    Exit;
  FTabMarker := Value;
  DoChanged(TAction.TabMarker);
end;

procedure TdxHtmlDocumentExporterOptions.SetFontUnit(const Value: TdxHtmlFontUnit);
begin
  if FFontUnit = Value then
    Exit;
  FFontUnit := Value;
  DoChanged(TAction.FontUnit);
end;

function TdxHtmlDocumentExporterOptions.GetUnderlineTocHyperlinks: Boolean;
begin
  Result := FUnderlineTocHyperlinks;
end;

procedure TdxHtmlDocumentExporterOptions.SetUnderlineTocHyperlinks(const Value: Boolean);
begin
  if FUnderlineTocHyperlinks = Value then
    Exit;
  FUnderlineTocHyperlinks := Value;
  DoChanged(TAction.UnderlineTocHyperlinks);
end;

procedure TdxHtmlDocumentExporterOptions.SetOverrideImageResolution(const Value: Integer);
begin
  if FOverrideImageResolution = Value then
    Exit;
  FOverrideImageResolution := Value;
  DoChanged(TAction.OverrideImageResolution);
end;

function TdxHtmlDocumentExporterOptions.ShouldSerializeOverrideImageResolution: Boolean;
begin
  Result := OverrideImageResolution <> DefaultOverrideImageResolution;
end;

procedure TdxHtmlDocumentExporterOptions.ResetOverrideImageResolution;
begin
  OverrideImageResolution := DefaultOverrideImageResolution;
end;

procedure TdxHtmlDocumentExporterOptions.SetFootNoteNumberStringFormat(const Value: string);
begin
  if FFootNoteNumberStringFormat = Value then
    Exit;
  FFootNoteNumberStringFormat := Value;
  DoChanged(TAction.FootNoteNumberStringFormat);
end;

function TdxHtmlDocumentExporterOptions.GetActualFootNoteNumberStringFormat: string;
begin
  if FootNoteNumberStringFormat = '' then
    Result := '[%0:s]'
  else
    Result := FootNoteNumberStringFormat;
end;

function TdxHtmlDocumentExporterOptions.GetEncoding: Word;
begin
  Result := ActualEncoding.CodePage;
end;

function TdxHtmlDocumentExporterOptions.IsDefaultEncoding: Boolean;
begin
  Result := ActualEncoding = TdxEncoding.UTF8;
end;

function TdxHtmlDocumentExporterOptions.IsTabMarkerStored: Boolean;
begin
  Result := TabMarker <> TdxCharacters.TabMark;
end;

procedure TdxHtmlDocumentExporterOptions.SetEndNoteNumberStringFormat(const Value: string);
begin
  if FEndNoteNumberStringFormat = Value then
    Exit;
  FEndNoteNumberStringFormat := Value;
  DoChanged(TAction.EndNoteNumberStringFormat);
end;

function TdxHtmlDocumentExporterOptions.GetActualEndNoteNumberStringFormat: string;
begin
  if EndNoteNumberStringFormat = '' then
    Result := '[%0:s]'
  else
    Result := EndNoteNumberStringFormat;
end;

procedure TdxHtmlDocumentExporterOptions.SetFootNoteNamePrefix(const Value: string);
begin
  if FFootNoteNamePrefix = Value then
    Exit;
  FFootNoteNamePrefix := Value;
  DoChanged(TAction.FootNoteNamePrefix);
end;

function TdxHtmlDocumentExporterOptions.GetActualFootNoteNamePrefix: string;
begin
  if FootNoteNamePrefix = '' then
    Result := '_ftn'
  else
    Result := FootNoteNamePrefix;
end;

procedure TdxHtmlDocumentExporterOptions.SetUseHtml5(const Value: Boolean);
begin
  if FUseHtml5 = Value then
    Exit;
  FUseHtml5 := Value;
  DoChanged(TAction.UseHtml5);
end;

procedure TdxHtmlDocumentExporterOptions.SetIgnoreParagraphOutlineLevel(const Value: Boolean);
begin
  if FIgnoreParagraphOutlineLevel = Value then
    Exit;
  FIgnoreParagraphOutlineLevel := Value;
  DoChanged(TAction.IgnoreParagraphOutlineLevel);
end;

procedure TdxHtmlDocumentExporterOptions.SetEndNoteNamePrefix(const Value: string);
begin
  if FEndNoteNamePrefix = Value then
    Exit;
  FEndNoteNamePrefix := Value;
  DoChanged(TAction.EndNoteNamePrefix);
end;

function TdxHtmlDocumentExporterOptions.GetActualEndNoteNamePrefix: string;
begin
  if EndNoteNamePrefix = '' then
    Result := '_endn'
  else
    Result := EndNoteNamePrefix;
end;

procedure TdxHtmlDocumentExporterOptions.SetDisposeConvertedImagesImmediately(const Value: Boolean);
begin
  if FDisposeConvertedImagesImmediately = Value then
    Exit;
  FDisposeConvertedImagesImmediately := Value;
  DoChanged(TAction.FontUnit);
end;

procedure TdxHtmlDocumentExporterOptions.SetEncoding(const Value: Word);
begin
  ActualEncoding := TdxEncoding.GetEncoding(Value);
end;

{ TdxMhtDocumentExporterOptions }

function TdxMhtDocumentExporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat(NotImplemented);
end;

{ TdxOpenXmlDocumentExporterOptions }

procedure TdxOpenXmlDocumentExporterOptions.CopyFrom(const Value: IdxExporterOptions);
var
  AOptions: TdxOpenXmlDocumentExporterOptions;
begin
  inherited CopyFrom(Value);
  AOptions := TdxOpenXmlDocumentExporterOptions(Value);
  if AOptions <> nil then
  begin
    AllowAlternateStyleNames := AOptions.AllowAlternateStyleNames;
    AlternateImageFolder := AOptions.AlternateImageFolder;
    LimitBookmarkNameTo40Chars := AOptions.LimitBookmarkNameTo40Chars;
    LimitStyleNameTo253Chars := AOptions.LimitStyleNameTo253Chars;
    LimitFontNameTo31Chars := AOptions.LimitFontNameTo31Chars;
  end;
end;

procedure TdxOpenXmlDocumentExporterOptions.DoReset;
begin
  inherited DoReset;
  AllowAlternateStyleNames := DefaultAllowAlternateStyleNames;
  AlternateImageFolder := DefaultAlternateImageFolder;
  LimitBookmarkNameTo40Chars := DefaultLimitBookmarkNameTo40Chars;
  LimitStyleNameTo253Chars := DefaultLimitStyleNameTo253Chars;
  LimitFontNameTo31Chars := DefaultLimitFontNameTo31Chars;
end;

function TdxOpenXmlDocumentExporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.OpenXml;
end;

procedure TdxOpenXmlDocumentExporterOptions.SetAlternateImageFolder(const AValue: Boolean);
begin
  if FAlternateImageFolder <> AValue then
  begin
    FAlternateImageFolder := AValue;
    DoChanged(TAction.AlternateImageFolder);
  end;
end;

procedure TdxOpenXmlDocumentExporterOptions.SetLimitBookmarkNameTo40Chars(const AValue: Boolean);
begin
  if FLimitBookmarkNameTo40Chars <> AValue then
  begin
    FLimitBookmarkNameTo40Chars := AValue;
    DoChanged(TAction.LimitBookmarkNameTo40Chars);
  end;
end;

procedure TdxOpenXmlDocumentExporterOptions.SetLimitStyleNameTo253Chars(const AValue: Boolean);
begin
  if FLimitStyleNameTo253Chars <> AValue then
  begin
    FLimitStyleNameTo253Chars := AValue;
    DoChanged(TAction.LimitStyleNameTo253Chars);
  end;
end;

procedure TdxOpenXmlDocumentExporterOptions.SetLimitFontNameTo31Chars(const AValue: Boolean);
begin
  if FLimitFontNameTo31Chars <> AValue then
  begin
    FLimitFontNameTo31Chars := AValue;
    DoChanged(TAction.LimitFontNameTo31Chars);
  end;
end;

procedure TdxOpenXmlDocumentExporterOptions.SetAllowAlternateStyleNames(const AValue: Boolean);
begin
  if FAllowAlternateStyleNames <> AValue then
  begin
    FAllowAlternateStyleNames := AValue;
    DoChanged(TAction.AllowAlternateStyleNames);
  end;
end;

{ TdxOpenDocumentExporterOptions }

function TdxOpenDocumentExporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat(NotImplemented);
end;

{ TdxWordMLDocumentExporterOptions }

function TdxWordMLDocumentExporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat(NotImplemented);
end;

{ TdxDocDocumentExporterCompatibilityOptions }

procedure TdxDocDocumentExporterCompatibilityOptions.SetAllowNonLinkedListDefinitions(const AValue: Boolean);
begin
  if FAllowNonLinkedListDefinitions <> AValue then
  begin
    FAllowNonLinkedListDefinitions := AValue;
    DoChanged(TAction.AllowNonLinkedListDefinitions);
  end;
end;

procedure TdxDocDocumentExporterCompatibilityOptions.ResetAllowNonLinkedListDefinitions;
begin
  AllowNonLinkedListDefinitions := False;
end;

procedure TdxDocDocumentExporterCompatibilityOptions.DoReset;
begin
  ResetAllowNonLinkedListDefinitions;
end;

procedure TdxDocDocumentExporterCompatibilityOptions.CopyFrom(const AValue: TdxDocDocumentExporterCompatibilityOptions);
begin
  AllowNonLinkedListDefinitions := AValue.AllowNonLinkedListDefinitions;
end;

{ TdxDocDocumentExporterOptions }

constructor TdxDocDocumentExporterOptions.Create;
begin
  inherited Create;
  FCompatibility := CreateCompatibilityOptions;
end;

destructor TdxDocDocumentExporterOptions.Destroy;
begin
  FCompatibility.Free;
  inherited Destroy;
end;

procedure TdxDocDocumentExporterOptions.CopyFrom(const AValue: IdxExporterOptions);
var
  AOptions: TdxDocDocumentExporterOptions;
begin
  inherited CopyFrom(AValue);
  AOptions := TdxDocDocumentExporterOptions(AValue);
  if AOptions <> nil then
    Compatibility.CopyFrom(AOptions.Compatibility);
end;

function TdxDocDocumentExporterOptions.CreateCompatibilityOptions: TdxDocDocumentExporterCompatibilityOptions;
begin
  Result := TdxDocDocumentExporterCompatibilityOptions.Create;
end;

procedure TdxDocDocumentExporterOptions.DoReset;
begin
  inherited DoReset;
  if FCompatibility <> nil then
    FCompatibility.Reset;
end;

function TdxDocDocumentExporterOptions.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Doc;
end;

procedure TdxDocDocumentExporterOptions.SetCompatibility(const AValue: TdxDocDocumentExporterCompatibilityOptions);
begin
  FCompatibility.CopyFrom(AValue);
end;

{ TdxRichEditDocumentExportOptions }

destructor TdxRichEditDocumentExportOptions.Destroy;
begin
  FreeAndNil(FDoc);
  FreeAndNil(FWordML);
  FreeAndNil(FOpenDocument);
  FreeAndNil(FOpenXml);
  FreeAndNil(FMht);
  FreeAndNil(FHtml);
  FreeAndNil(FPlainText);
  FreeAndNil(FRtf);
  inherited Destroy;
end;

procedure TdxRichEditDocumentExportOptions.CopyFrom(AOptions: TdxRichEditDocumentExportOptions);
var
  AKey: TdxRichEditDocumentFormat;
  ASourceOptions: TdxDocumentExporterOptions;
begin
  for AKey := Low(TdxRichEditDocumentFormat) to High(TdxRichEditDocumentFormat) do
  begin
    ASourceOptions := AOptions.OptionsTable[AKey];
    if ASourceOptions <> nil then
      FOptionsTable[AKey].CopyFrom(ASourceOptions);
  end;
end;

function TdxRichEditDocumentExportOptions.CreateDocOptions: TdxDocDocumentExporterOptions;
begin
  Result := TdxDocDocumentExporterOptions.Create;
end;

function TdxRichEditDocumentExportOptions.CreateHtmlOptions: TdxHtmlDocumentExporterOptions;
begin
  Result := TdxHtmlDocumentExporterOptions.Create;
end;

function TdxRichEditDocumentExportOptions.CreateMhtOptions: TdxMhtDocumentExporterOptions;
begin
  Result := TdxMhtDocumentExporterOptions.Create;
end;

function TdxRichEditDocumentExportOptions.CreateOpenDocumentOptions: TdxOpenDocumentExporterOptions;
begin
  Result := TdxOpenDocumentExporterOptions.Create;
end;

function TdxRichEditDocumentExportOptions.CreateOpenXmlOptions: TdxOpenXmlDocumentExporterOptions;
begin
  Result := TdxOpenXmlDocumentExporterOptions.Create;
end;

function TdxRichEditDocumentExportOptions.CreatePlainTextOptions: TdxPlainTextDocumentExporterOptions;
begin
  Result := TdxPlainTextDocumentExporterOptions.Create;
end;

function TdxRichEditDocumentExportOptions.CreateRtfOptions: TdxRtfDocumentExporterOptions;
begin
  Result := TdxRtfDocumentExporterOptions.Create;
end;

function TdxRichEditDocumentExportOptions.CreateWordMLOptions: TdxWordMLDocumentExporterOptions;
begin
  Result := TdxWordMLDocumentExporterOptions.Create;
end;

procedure TdxRichEditDocumentExportOptions.CreateInnerOptions;
begin
  inherited CreateInnerOptions;
  FRtf := CreateRtfOptions;
  FPlainText := CreatePlainTextOptions;
  FHtml := CreateHtmlOptions;
  FMht := CreateMhtOptions;
  FOpenXml := CreateOpenXmlOptions;
  FOpenDocument := CreateOpenDocumentOptions;
  FWordML := CreateWordMLOptions;
  FDoc := CreateDocOptions;

  RegisterOptions(FDoc);
  RegisterOptions(FRtf);
  RegisterOptions(FPlainText);
  RegisterOptions(FHtml);
  RegisterOptions(FOpenXml);
end;

procedure TdxRichEditDocumentExportOptions.DoReset;
var
  AOptions: TdxDocumentExporterOptions;
begin
  for AOptions in FOptionsTable do
    if AOptions <> nil then
      AOptions.Reset;
end;

function TdxRichEditDocumentExportOptions.GetOptions(AFormat: TdxRichEditDocumentFormat): TdxDocumentExporterOptions;
begin
  Result := FOptionsTable[AFormat];
end;

procedure TdxRichEditDocumentExportOptions.RegisterOptions(AOptions: TdxDocumentExporterOptions);
begin
  FOptionsTable[AOptions.Format] := AOptions;
end;

procedure TdxRichEditDocumentExportOptions.SetDoc(const Value: TdxDocDocumentExporterOptions);
begin
  FDoc.Assign(Value);
end;

procedure TdxRichEditDocumentExportOptions.SetHtml(const Value: TdxHtmlDocumentExporterOptions);
begin
  FHtml.Assign(Value);
end;

procedure TdxRichEditDocumentExportOptions.SetMht(const Value: TdxMhtDocumentExporterOptions);
begin
  FMht.Assign(Value);
end;

procedure TdxRichEditDocumentExportOptions.SetOpenDocument(const Value: TdxOpenDocumentExporterOptions);
begin
  FOpenDocument.Assign(Value);
end;

procedure TdxRichEditDocumentExportOptions.SetOpenXml(const Value: TdxOpenXmlDocumentExporterOptions);
begin
  FOpenXml.Assign(Value);
end;

procedure TdxRichEditDocumentExportOptions.SetPlainText(const Value: TdxPlainTextDocumentExporterOptions);
begin
  FPlainText.Assign(Value);
end;

procedure TdxRichEditDocumentExportOptions.SetRtf(const Value: TdxRtfDocumentExporterOptions);
begin
  FRtf.Assign(Value);
end;

procedure TdxRichEditDocumentExportOptions.SetWordML(const Value: TdxWordMLDocumentExporterOptions);
begin
  FWordML.Assign(Value);
end;

{ TdxCopyPasteOptions }

procedure TdxCopyPasteOptions.DoReset;
begin
  inherited DoReset;
  InsertOptions := DefaultInsertOptions;
  MaintainDocumentSectionSettings := False;
end;

procedure TdxCopyPasteOptions.SetInsertOptions(const Value: TdxInsertOptions);
begin
  if FInsertOptions <> Value then
  begin
    FInsertOptions := Value;
    DoChanged(TAction.InsertOptions);
  end;
end;

procedure TdxCopyPasteOptions.SetMaintainDocumentSectionSettings(const Value: Boolean);
begin
  if FMaintainDocumentSectionSettings <> Value then
  begin
    FMaintainDocumentSectionSettings := Value;
    DoChanged(TAction.MaintainDocumentSectionSettings);
  end;
end;

{ TdxFormattingMarkVisibilityOptions }

procedure TdxFormattingMarkVisibilityOptions.DoReset;
begin
  inherited DoReset;
  TabCharacter := DefaultTabCharacterVisibility;
  Separator := DefaultSeparatorVisibility;
end;

procedure TdxFormattingMarkVisibilityOptions.SetSeparator(const Value: TdxRichEditFormattingMarkVisibility);
begin
  if Separator <> Value then
  begin
    FSeparator := Value;
    DoChanged(TAction.Separator);
  end;
end;

procedure TdxFormattingMarkVisibilityOptions.SetTabCharacter(const Value: TdxRichEditFormattingMarkVisibility);
begin
  if TabCharacter <> Value then
  begin
    FTabCharacter := Value;
    DoChanged(TAction.TabCharacter);
  end;
end;

{ TdxNumberingOptions }

procedure TdxNumberingOptions.Assign(Source: TPersistent);
var
  ASource: TdxNumberingOptions;
begin
  BeginUpdate;
  try
    if Source is TdxNumberingOptions then
    begin
      ASource := TdxNumberingOptions(Source);
      Bulleted := ASource.Bulleted;
      Simple := ASource.Simple;
      MultiLevel := ASource.MultiLevel;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxNumberingOptions.DoReset;
begin
  FBulleted := TdxDocumentCapability.Default;
  FMultiLevel := TdxDocumentCapability.Default;
  FSimple := TdxDocumentCapability.Default;
end;

function TdxNumberingOptions.GetBulletedAllowed: Boolean;
begin
  Result := IsAllowed(Bulleted);
end;

function TdxNumberingOptions.GetMultiLevelAllowed: Boolean;
begin
  Result := IsAllowed(MultiLevel);
end;

function TdxNumberingOptions.GetSimpleAllowed: Boolean;
begin
  Result := IsAllowed(Simple);
end;

function TdxNumberingOptions.IsAllowed(AOption: TdxDocumentCapability): Boolean;
begin
  Result := (AOption = TdxDocumentCapability.Default) or (AOption = TdxDocumentCapability.Enabled);
end;

procedure TdxNumberingOptions.SetBulleted(const Value: TdxDocumentCapability);
begin
  if FBulleted = Value then
    Exit;
  FBulleted := Value;
  DoChanged(TAction.Bulleted);
end;

procedure TdxNumberingOptions.SetMultiLevel(const Value: TdxDocumentCapability);
begin
  if FMultiLevel = Value then
    Exit;
  FMultiLevel := Value;
  DoChanged(TAction.MultiLevel);
end;

procedure TdxNumberingOptions.SetSimple(const Value: TdxDocumentCapability);
begin
  if FSimple = Value then
    Exit;
  FSimple := Value;
  DoChanged(TAction.Simple);
end;

{ TdxCharacterFormattingDetailedOptions }

procedure TdxCharacterFormattingDetailedOptions.DoReset;
begin
  AllCaps := TdxDocumentCapability.Default;
  BackColor := TdxDocumentCapability.Default;
  FontBold := TdxDocumentCapability.Default;
  FontItalic := TdxDocumentCapability.Default;
  FontName := TdxDocumentCapability.Default;
  FontSize := TdxDocumentCapability.Default;
  FontStrikeout := TdxDocumentCapability.Default;
  FontUnderline := TdxDocumentCapability.Default;
  ForeColor := TdxDocumentCapability.Default;
  Hidden := TdxDocumentCapability.Default;
  Script := TdxDocumentCapability.Default;
  StrikeoutColor := TdxDocumentCapability.Default;
  StrikeoutWordsOnly := TdxDocumentCapability.Default;
  UnderlineColor := TdxDocumentCapability.Default;
  UnderlineWordsOnly := TdxDocumentCapability.Default;
  inherited DoReset;
end;

function TdxCharacterFormattingDetailedOptions.IsAllowed(AOption: TdxDocumentCapability): Boolean;
begin
  Result := (AOption = TdxDocumentCapability.Default) or (AOption = TdxDocumentCapability.Enabled);
end;

procedure TdxCharacterFormattingDetailedOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TdxCharacterFormattingDetailedOptions then
    begin
      AllCaps := TdxCharacterFormattingDetailedOptions(Source).AllCaps;
      BackColor := TdxCharacterFormattingDetailedOptions(Source).BackColor;
      FontBold := TdxCharacterFormattingDetailedOptions(Source).FontBold;
      FontItalic := TdxCharacterFormattingDetailedOptions(Source).FontItalic;
      FontName := TdxCharacterFormattingDetailedOptions(Source).FontName;
      FontSize := TdxCharacterFormattingDetailedOptions(Source).FontSize;
      FontStrikeout := TdxCharacterFormattingDetailedOptions(Source).FontStrikeout;
      FontUnderline := TdxCharacterFormattingDetailedOptions(Source).FontUnderline;
      ForeColor := TdxCharacterFormattingDetailedOptions(Source).ForeColor;
      Hidden := TdxCharacterFormattingDetailedOptions(Source).Hidden;
      Script := TdxCharacterFormattingDetailedOptions(Source).Script;
      StrikeoutColor := TdxCharacterFormattingDetailedOptions(Source).StrikeoutColor;
      StrikeoutWordsOnly := TdxCharacterFormattingDetailedOptions(Source).StrikeoutWordsOnly;
      UnderlineColor := TdxCharacterFormattingDetailedOptions(Source).UnderlineColor;
      UnderlineWordsOnly := TdxCharacterFormattingDetailedOptions(Source).UnderlineWordsOnly;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

function TdxCharacterFormattingDetailedOptions.AllCapsAllowed: Boolean;
begin
  Result := IsAllowed(AllCaps);
end;

function TdxCharacterFormattingDetailedOptions.BackColorAllowed: Boolean;
begin
  Result := IsAllowed(AllCaps);
end;

function TdxCharacterFormattingDetailedOptions.FontBoldAllowed: Boolean;
begin
  Result := IsAllowed(FontBold);
end;

function TdxCharacterFormattingDetailedOptions.FontItalicAllowed: Boolean;
begin
  Result := IsAllowed(FontItalic);
end;

function TdxCharacterFormattingDetailedOptions.FontNameAllowed: Boolean;
begin
  Result := IsAllowed(FontName);
end;

function TdxCharacterFormattingDetailedOptions.FontSizeAllowed: Boolean;
begin
  Result := IsAllowed(FontSize);
end;

function TdxCharacterFormattingDetailedOptions.FontStrikeoutAllowed: Boolean;
begin
  Result := IsAllowed(FontStrikeout);
end;

function TdxCharacterFormattingDetailedOptions.FontUnderlineAllowed: Boolean;
begin
  Result := IsAllowed(FontUnderline);
end;

function TdxCharacterFormattingDetailedOptions.ForeColorAllowed: Boolean;
begin
  Result := IsAllowed(ForeColor);
end;

function TdxCharacterFormattingDetailedOptions.HiddenAllowed: Boolean;
begin
  Result := IsAllowed(Hidden);
end;

function TdxCharacterFormattingDetailedOptions.ScriptAllowed: Boolean;
begin
  Result := IsAllowed(Script);
end;

function TdxCharacterFormattingDetailedOptions.StrikeoutColorAllowed: Boolean;
begin
  Result := IsAllowed(StrikeoutColor);
end;

function TdxCharacterFormattingDetailedOptions.StrikeoutWordsOnlyAllowed: Boolean;
begin
  Result := IsAllowed(StrikeoutWordsOnly);
end;

function TdxCharacterFormattingDetailedOptions.UnderlineColorAllowed: Boolean;
begin
  Result := IsAllowed(UnderlineColor);
end;

function TdxCharacterFormattingDetailedOptions.UnderlineWordsOnlyAllowed: Boolean;
begin
  Result := IsAllowed(UnderlineWordsOnly);
end;

procedure TdxCharacterFormattingDetailedOptions.SetAllCaps(const Value: TdxDocumentCapability);
begin
  if AllCaps <> Value then
  begin
    FAllCaps := Value;
    DoChanged(TAction.AllCaps);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetBackColor(const Value: TdxDocumentCapability);
begin
  if BackColor <> Value then
  begin
    FBackColor := Value;
    DoChanged(TAction.BackColor);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetFontBold(const Value: TdxDocumentCapability);
begin
  if FontBold <> Value then
  begin
    FFontBold := Value;
    DoChanged(TAction.FontBold);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetFontItalic(const Value: TdxDocumentCapability);
begin
  if FontItalic <> Value then
  begin
    FFontItalic := Value;
    DoChanged(TAction.FontItalic);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetFontName(const Value: TdxDocumentCapability);
begin
  if FontName <> Value then
  begin
    FFontName := Value;
    DoChanged(TAction.FontName);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetFontSize(const Value: TdxDocumentCapability);
begin
  if FontSize <> Value then
  begin
    FFontSize := Value;
    DoChanged(TAction.FontSize);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetFontStrikeout(const Value: TdxDocumentCapability);
begin
  if FontStrikeout <> Value then
  begin
    FFontStrikeout := Value;
    DoChanged(TAction.FontStrikeout);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetFontUnderline(const Value: TdxDocumentCapability);
begin
  if FontUnderline <> Value then
  begin
    FUnderlineColor := Value;
    DoChanged(TAction.FontUnderline);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetForeColor(const Value: TdxDocumentCapability);
begin
  if ForeColor <> Value then
  begin
    FForeColor := Value;
    DoChanged(TAction.ForeColor);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetHidden(const Value: TdxDocumentCapability);
begin
  if Hidden <> Value then
  begin
    FHidden := Value;
    DoChanged(TAction.Hidden);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetScript(const Value: TdxDocumentCapability);
begin
  if Script <> Value then
  begin
    FScript := Value;
    DoChanged(TAction.Script);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetStrikeoutColor(const Value: TdxDocumentCapability);
begin
  if StrikeoutColor <> Value then
  begin
    FStrikeoutColor := Value;
    DoChanged(TAction.StrikeoutColor);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetStrikeoutWordsOnly(const Value: TdxDocumentCapability);
begin
  if StrikeoutWordsOnly <> Value then
  begin
    FStrikeoutWordsOnly := Value;
    DoChanged(TAction.StrikeoutWordsOnly);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetUnderlineColor(const Value: TdxDocumentCapability);
begin
  if UnderlineColor <> Value then
  begin
    FUnderlineColor := Value;
    DoChanged(TAction.UnderlineColor);
  end;
end;

procedure TdxCharacterFormattingDetailedOptions.SetUnderlineWordsOnly(const Value: TdxDocumentCapability);
begin
  if UnderlineWordsOnly <> Value then
  begin
    FUnderlineWordsOnly := Value;
    DoChanged(TAction.UnderlineWordsOnly);
  end;
end;

{ TdxDocumentSaveOptions }

function TdxDocumentSaveOptions.CanSaveToCurrentFileName: Boolean;
begin
  Result := True;
end;

procedure TdxDocumentSaveOptions.DoReset;
begin
  inherited DoReset;
  FHideInOpenDialog := False;
  FHideInSaveDialog := False;
  ResetDefaultFileName;
  ResetCurrentFileName;
  ResetDefaultFormat;
  ResetCurrentFormat;
end;

function TdxDocumentSaveOptions.GetCurrentFileName: string;
begin
  Result := FCurrentFileName;
end;

function TdxDocumentSaveOptions.GetCurrentFormat: TdxRichEditDocumentFormat;
begin
  Result := FCurrentFormat;
end;

function TdxDocumentSaveOptions.GetDefaultFileName: string;
begin
  Result := FDefaultFileName;
end;

function TdxDocumentSaveOptions.GetDefaultFormat: TdxRichEditDocumentFormat;
begin
  Result := FDefaultFormat;
end;

function TdxDocumentSaveOptions.IsCurrentFileNameStored: Boolean;
begin
  Result := CurrentFileName <> EmptyStr;
end;

function TdxDocumentSaveOptions.IsDefaultFileNameStored: Boolean;
begin
  Result := DefaultFileName <> EmptyStr;
end;

procedure TdxDocumentSaveOptions.ResetCurrentFileName;
begin
  CurrentFileName := EmptyStr;
end;

procedure TdxDocumentSaveOptions.ResetCurrentFormat;
begin
  CurrentFormat := TdxRichEditDocumentFormat.Undefined;
end;

procedure TdxDocumentSaveOptions.ResetDefaultFileName;
begin
  DefaultFileName := EmptyStr;
end;

procedure TdxDocumentSaveOptions.ResetDefaultFormat;
begin
  DefaultFormat := TdxRichEditDocumentFormat.Rtf;
end;

procedure TdxDocumentSaveOptions.SetCurrentFileName(const AFileName: string);
var
  AValue: string;
begin
  if Trim(AFileName) <> '' then
    AValue := AFileName
  else
    AValue := '';
  if FCurrentFileName <> AValue then
  begin
    FCurrentFileName := AValue;
    DoChanged(TAction.CurrentFileName);
  end;
end;

procedure TdxDocumentSaveOptions.SetCurrentFormat(const AFormat: TdxRichEditDocumentFormat);
begin
  if FCurrentFormat <> AFormat then
  begin
    FCurrentFormat := AFormat;
    DoChanged(TAction.CurrentFormat);
  end;
end;

procedure TdxDocumentSaveOptions.SetDefaultFileName(const AFileName: string);
var
  AValue: string;
begin
  if Trim(AFileName) <> '' then
    AValue := AFileName
  else
    AValue := '';
  if FDefaultFileName <> AFileName then
  begin
    FDefaultFileName := AValue;
    DoChanged(TAction.DefaultFileName);
  end;
end;

procedure TdxDocumentSaveOptions.SetDefaultFormat(const AFormat: TdxRichEditDocumentFormat);
begin
  if FDefaultFormat <> AFormat then
  begin
    FDefaultFormat := AFormat;
    DoChanged(TAction.DefaultFormat);
  end;
end;

{ TdxRulerOptions }

procedure TdxRulerOptions.Assign(Source: TPersistent);
begin
  if Source is TdxRulerOptions then
    Visibility := TdxRulerOptions(Source).Visibility;
  inherited Assign(Source);
end;

procedure TdxRulerOptions.SetVisibility(const AValue: TdxRichEditRulerVisibility);
begin
  if FVisibility <> AValue then
  begin
    FVisibility := AValue;
    DoChanged(TAction.Visibility);
  end;
end;

procedure TdxRulerOptions.DoReset;
begin
  inherited DoReset;
  Visibility := DefaultVisibility;
end;

{ TdxHorizontalRulerOptions }

procedure TdxHorizontalRulerOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxHorizontalRulerOptions then
  begin
    ShowTabs := TdxHorizontalRulerOptions(Source).ShowTabs;
    ShowLeftIndent := TdxHorizontalRulerOptions(Source).ShowLeftIndent;
    ShowRightIndent := TdxHorizontalRulerOptions(Source).ShowRightIndent;
  end;
end;

procedure TdxHorizontalRulerOptions.SetShowTabs(const AValue: Boolean);
begin
  if FShowTabs <> AValue then
  begin
    FShowTabs := AValue;
    DoChanged(TAction.ShowTabs);
  end;
end;

procedure TdxHorizontalRulerOptions.SetShowLeftIndent(const AValue: Boolean);
begin
  if FShowLeftIndent <> AValue then
  begin
    FShowLeftIndent := AValue;
    DoChanged(TAction.ShowLeftIndent);
  end;
end;

procedure TdxHorizontalRulerOptions.SetShowRightIndent(const AValue: Boolean);
begin
  if FShowRightIndent <> AValue then
  begin
    FShowRightIndent := AValue;
    DoChanged(TAction.ShowRightIndent);
  end;
end;

procedure TdxHorizontalRulerOptions.DoReset;
begin
  inherited DoReset;
  ShowTabs := True;
  ShowLeftIndent := True;
  ShowRightIndent := True;
end;

{ TdxViewLayoutOptionsBase }

procedure TdxViewLayoutOptionsBase.SetAllowTablesToExtendIntoMargins(const AValue: Boolean);
begin
  if FAllowTablesToExtendIntoMargins = AValue then
    Exit;
  FAllowTablesToExtendIntoMargins := AValue;
  DoChanged(TAction.AllowTablesToExtendIntoMargins);
end;

procedure TdxViewLayoutOptionsBase.SetMatchHorizontalTableIndentsToTextEdge(const AValue: Boolean);
begin
  if FMatchHorizontalTableIndentsToTextEdge = AValue then
    Exit;
  FMatchHorizontalTableIndentsToTextEdge := AValue;
  DoChanged(TAction.MatchHorizontalTableIndentsToTextEdge);
end;

procedure TdxViewLayoutOptionsBase.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxViewLayoutOptionsBase then
  begin
    AllowTablesToExtendIntoMargins := TdxViewLayoutOptionsBase(Source).AllowTablesToExtendIntoMargins;
    MatchHorizontalTableIndentsToTextEdge := TdxViewLayoutOptionsBase(Source).MatchHorizontalTableIndentsToTextEdge;
  end;
end;

procedure TdxViewLayoutOptionsBase.DoReset;
begin
  inherited DoReset;
  AllowTablesToExtendIntoMargins := GetDefaultAllowTablesToExtendIntoMargins;
  MatchHorizontalTableIndentsToTextEdge := GetDefaultMatchHorizontalTableIndentsToTextEdge;
end;

{ TdxDraftViewLayoutOptions }

function TdxDraftViewLayoutOptions.GetDefaultAllowTablesToExtendIntoMargins: Boolean;
begin
  Result := AllowTablesToExtendIntoMarginsByDefault;
end;

function TdxDraftViewLayoutOptions.GetDefaultMatchHorizontalTableIndentsToTextEdge: Boolean;
begin
  Result := MatchHorizontalTableIndentsToTextEdgeDefault;
end;

{ TdxPrintLayoutViewLayoutOptions }

function TdxPrintLayoutViewLayoutOptions.GetDefaultAllowTablesToExtendIntoMargins: Boolean;
begin
  Result := AllowTablesToExtendIntoMarginsByDefault;
end;

function TdxPrintLayoutViewLayoutOptions.GetDefaultMatchHorizontalTableIndentsToTextEdge: Boolean;
begin
  Result := MatchHorizontalTableIndentsToTextEdgeDefault;
end;

{ TdxSimpleViewLayoutOptions }

function TdxSimpleViewLayoutOptions.GetDefaultAllowTablesToExtendIntoMargins: Boolean;
begin
  Result := AllowTablesToExtendIntoMarginsByDefault;
end;

function TdxSimpleViewLayoutOptions.GetDefaultMatchHorizontalTableIndentsToTextEdge: Boolean;
begin
  Result := MatchHorizontalTableIndentsToTextEdgeDefault;
end;

{ TdxRichEditLayoutOptions }

destructor TdxRichEditLayoutOptions.Destroy;
begin
  FDraftView.Free;
  FPrintLayoutView.Free;
  FSimpleView.Free;
  inherited Destroy;
end;

procedure TdxRichEditLayoutOptions.OnInnerOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
begin
  DoChanged(E.Actions);
end;

procedure TdxRichEditLayoutOptions.SetDraftView(const Value: TdxDraftViewLayoutOptions);
begin
  FDraftView.Assign(Value);
end;

procedure TdxRichEditLayoutOptions.SetPrintLayoutView(const Value: TdxPrintLayoutViewLayoutOptions);
begin
  FPrintLayoutView.Assign(Value);
end;

procedure TdxRichEditLayoutOptions.SetSimpleView(const Value: TdxSimpleViewLayoutOptions);
begin
  FSimpleView.Assign(Value);
end;

procedure TdxRichEditLayoutOptions.SubscribeInnerOptions;
begin
  inherited SubscribeInnerOptions;
  FDraftView.Changed.Add(OnInnerOptionsChanged);
  FPrintLayoutView.Changed.Add(OnInnerOptionsChanged);
  FSimpleView.Changed.Add(OnInnerOptionsChanged);
end;

procedure TdxRichEditLayoutOptions.CreateInnerOptions;
begin
  FDraftView := CreateDraftViewLayoutOptions;
  FPrintLayoutView := CreatePrintLayoutViewLayoutOptions;
  FSimpleView := CreateSimpleViewLayoutOptions;
end;

function TdxRichEditLayoutOptions.CreateDraftViewLayoutOptions: TdxDraftViewLayoutOptions;
begin
  Result := TdxDraftViewLayoutOptions.Create;
end;

function TdxRichEditLayoutOptions.CreatePrintLayoutViewLayoutOptions: TdxPrintLayoutViewLayoutOptions;
begin
  Result := TdxPrintLayoutViewLayoutOptions.Create;
end;

function TdxRichEditLayoutOptions.CreateSimpleViewLayoutOptions: TdxSimpleViewLayoutOptions;
begin
  Result := TdxSimpleViewLayoutOptions.Create;
end;

procedure TdxRichEditLayoutOptions.DoReset;
begin
  inherited DoReset;
  DraftView.DoReset;
  PrintLayoutView.DoReset;
  SimpleView.DoReset;
end;

{ TdxHyperlinkOptions }

procedure TdxHyperlinkOptions.Assign(Source: TPersistent);
begin
  if Source is TdxHyperlinkOptions then
  begin
    ShowToolTip := TdxHyperlinkOptions(Source).ShowToolTip;
    ModifierKeys := TdxHyperlinkOptions(Source).ModifierKeys;
  end;
  inherited Assign(Source);
end;

procedure TdxHyperlinkOptions.SetShowToolTip(const AValue: Boolean);
begin
  if AValue = FShowToolTip then
    Exit;
  FShowToolTip := AValue;
  DoChanged(TAction.ShowToolTip);
end;

procedure TdxHyperlinkOptions.SetModifierKeys(const AValue: TShortCut);
begin
  if AValue = FModifierKeys then
    Exit;
  FModifierKeys := AValue;
  DoChanged(TAction.ModifierKeys);
end;

function TdxHyperlinkOptions.ShouldSerializeModifierKeys: Boolean;
begin
  Result := ModifierKeys <> scCtrl;
end;

procedure TdxHyperlinkOptions.ResetModifierKeys;
begin
  ModifierKeys := scCtrl;
end;

procedure TdxHyperlinkOptions.DoReset;
begin
  ShowToolTip := True;
  ModifierKeys := VK_CONTROL;
end;

{ TdxFieldOptions }

procedure TdxFieldOptions.Assign(Source: TPersistent);
begin
  if Source is TdxFieldOptions then
    CopyFrom(TdxFieldOptions(Source));
  inherited Assign(Source);
end;

procedure TdxFieldOptions.SetHighlightMode(const AValue: TdxFieldsHighlightMode);
begin
  if FHighlightMode = AValue then
    Exit;
  FHighlightMode := AValue;
  DoChanged(TAction.HighlightMode);
end;

procedure TdxFieldOptions.SetHighlightColor(const AValue: TdxAlphaColor);
begin
  if FHighlightColor = AValue then
    Exit;
  FHighlightColor := AValue;
  DoChanged(TAction.HighlightColor);
end;

function TdxFieldOptions.ShouldSerializeHighlightColor: Boolean;
begin
  Result := HighlightColor <> DefaultHighlightColor;
end;

procedure TdxFieldOptions.ResetHighlightColor;
begin
  HighlightColor := DefaultHighlightColor;
end;

procedure TdxFieldOptions.SetUseCurrentCultureDateTimeFormat(const AValue: Boolean);
begin
  if FUseCurrentCultureDateTimeFormat = AValue then
    Exit;
  FUseCurrentCultureDateTimeFormat := AValue;
  DoChanged(TAction.UseCurrentCultureForDateTimeFormatting);
end;

procedure TdxFieldOptions.SetUpdateDocVariablesBeforePrint(const Value: Boolean);
begin
  if UpdateDocVariablesBeforePrint = Value then
    Exit;
  FUpdateDocVariablesBeforePrint := Value;
  DoChanged(TAction.UpdateDocVariablesBeforePrint);
end;

procedure TdxFieldOptions.SetUpdateDocVariablesBeforeCopy(const Value: Boolean);
begin
  if UpdateDocVariablesBeforeCopy = Value then
    Exit;
  FUpdateDocVariablesBeforeCopy := Value;
  DoChanged(TAction.UpdateDocVariablesBeforeCopy);
end;

procedure TdxFieldOptions.SetUpdateFieldsOnPaste(const Value: Boolean);
begin
  if UpdateFieldsOnPaste = Value then
    Exit;
  FUpdateFieldsOnPaste := Value;
  DoChanged(TAction.UpdateFieldsOnPaste);
end;

procedure TdxFieldOptions.SetThrowExceptionOnInvalidFormatSwitch(const Value: Boolean);
begin
  if ThrowExceptionOnInvalidFormatSwitch = Value then
    Exit;
  FThrowExceptionOnInvalidFormatSwitch := Value;
  DoChanged(TAction.ThrowExceptionOnInvalidFormatSwitch);
end;

procedure TdxFieldOptions.DoReset;
begin
  HighlightMode := DefaultHighlightMode;
  HighlightColor := DefaultHighlightColor;
  UseCurrentCultureDateTimeFormat := False;
  UpdateDocVariablesBeforePrint := True;
  UpdateDocVariablesBeforeCopy := True;
  UpdateFieldsOnPaste := True;
  ThrowExceptionOnInvalidFormatSwitch := True;
end;

procedure TdxFieldOptions.CopyFrom(AOptions: TdxFieldOptions);
begin
  FHighlightColor := AOptions.HighlightColor;
  FHighlightMode := AOptions.HighlightMode;
  FUseCurrentCultureDateTimeFormat := AOptions.UseCurrentCultureDateTimeFormat;
end;

{ TdxDocumentSearchOptions }

procedure TdxDocumentSearchOptions.SetRegExResultMaxGuaranteedLength(const AValue: Integer);
begin
  if FRegExResultMaxGuaranteedLength = AValue then
    Exit;

  FRegExResultMaxGuaranteedLength := AValue;
  DoChanged(TAction.RegExResultMaxGuaranteedLength);
end;

procedure TdxDocumentSearchOptions.DoReset;
begin
  RegExResultMaxGuaranteedLength := DefaultRegExResultMaxGuaranteedLength;
end;

{ TdxPrintingOptions }

procedure TdxPrintingOptions.DoReset;
begin
  EnablePageBackgroundOnPrint := False;
  UpdateDocVariablesBeforePrint := True;
end;

procedure TdxPrintingOptions.SetEnablePageBackgroundOnPrint(const Value: Boolean);
begin
  if EnablePageBackgroundOnPrint = Value then
    Exit;
  FEnablePageBackgroundOnPrint := Value;
  DoChanged(TAction.EnablePageBackgroundOnPrint);
end;

procedure TdxPrintingOptions.SetUpdateDocVariablesBeforePrint(const Value: Boolean);
begin
  if UpdateDocVariablesBeforePrint = Value then
    Exit;
  FUpdateDocVariablesBeforePrint := Value;
  DoChanged(TAction.UpdateDocVariablesBeforePrint);
end;

{ TdxTableOptions }

procedure TdxTableOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TdxTableOptions then
      GridLines := TdxTableOptions(Source).GridLines;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxTableOptions.SetGridLines(const AValue: TdxRichEditTableGridLinesVisibility);
begin
  if FGridLines = AValue then
    Exit;
  FGridLines := AValue;
  DoChanged(TAction.GridLines);
end;

procedure TdxTableOptions.DoReset;
begin
  GridLines := FDefaultGridLinesVisibility;
end;

{ TdxBookmarkOptions }

class constructor TdxBookmarkOptions.Initialize;
begin
  FDefaultColor := TdxAlphaColors.FromArgb(127, 127, 127);
end;

function TdxBookmarkOptions.IsColorStored: Boolean;
begin
  Result := Color <> FDefaultColor;
end;

procedure TdxBookmarkOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TdxBookmarkOptions then
    begin
      Visibility := TdxBookmarkOptions(Source).Visibility;
      Color := TdxBookmarkOptions(Source).Color;
      AllowNameResolution := TdxBookmarkOptions(Source).AllowNameResolution;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxBookmarkOptions.SetVisibility(const AValue: TdxRichEditBookmarkVisibility);
begin
  if Visibility = AValue then
    Exit;
  FVisibility := AValue;
  DoChanged(TAction.BookmarkVisibility);
end;

procedure TdxBookmarkOptions.SetColor(const AValue: TdxAlphaColor);
begin
  if Color = AValue then
    Exit;
  FColor := AValue;
  DoChanged(TAction.BookmarkColor);
end;

function TdxBookmarkOptions.ShouldSerializeColor: Boolean;
begin
  Result := Color <> FDefaultColor;
end;

procedure TdxBookmarkOptions.ResetColor;
begin
  Color := FDefaultColor;
end;

procedure TdxBookmarkOptions.SetAllowNameResolution(const AValue: Boolean);
begin
  if FAllowNameResolution = AValue then
    Exit;
  FAllowNameResolution := AValue;
  DoChanged(TAction.AllowNameResolution);
end;

procedure TdxBookmarkOptions.DoReset;
begin
  Visibility := FDefaultVisibility;
  Color := FDefaultColor;
  AllowNameResolution := FDefaultAllowNameResolution;
end;

{ TdxSpellCheckerOptions }

procedure TdxSpellCheckerOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TdxSpellCheckerOptions then
    begin
      AutoDetectDocumentCulture := TdxSpellCheckerOptions(Source).AutoDetectDocumentCulture;
      IgnoreNoProof := TdxSpellCheckerOptions(Source).IgnoreNoProof;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxSpellCheckerOptions.SetAutoDetectDocumentCulture(const AValue: Boolean);
begin
  if AutoDetectDocumentCulture = AValue then
    Exit;
  FAutoDetectDocumentCulture := AValue;
  DoChanged(TAction.AutoDetectDocumentCulture);
end;

procedure TdxSpellCheckerOptions.SetIgnoreNoProof(const AValue: Boolean);
begin
  if IgnoreNoProof = AValue then
    Exit;
  FIgnoreNoProof := AValue;
  DoChanged(TAction.IgnoreNoProof);
end;

procedure TdxSpellCheckerOptions.DoReset;
begin
  AutoDetectDocumentCulture := True;
  IgnoreNoProof := False;
end;

{ TdxAutoCorrectOptions }

procedure TdxAutoCorrectOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TdxAutoCorrectOptions then
      DetectUrls := TdxAutoCorrectOptions(Source).DetectUrls;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxAutoCorrectOptions.SetDetectUrls(const AValue: Boolean);
begin
  if FDetectUrls = AValue then
    Exit;
  FDetectUrls := AValue;
  DoChanged(TAction.DetectUrls);
end;

procedure TdxAutoCorrectOptions.DoReset;
begin
  DetectUrls := True;
end;

{ TdxAuthenticationOptions }

procedure TdxAuthenticationOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TdxAuthenticationOptions then
    begin
      EMail := TdxAuthenticationOptions(Source).EMail;
      Group := TdxAuthenticationOptions(Source).Group;
      Password := TdxAuthenticationOptions(Source).Password;
      UserName := TdxAuthenticationOptions(Source).UserName;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxAuthenticationOptions.SetEMail(const AValue: string);
begin
  if FEMail = AValue then
    Exit;
  FEMail := AValue;
  DoChanged(TAction.EMail);
end;

procedure TdxAuthenticationOptions.SetGroup(const AValue: string);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;
  DoChanged(TAction.Group);
end;

procedure TdxAuthenticationOptions.SetPassword(const AValue: string);
begin
  if FPassword = AValue then
    Exit;
  FPassword := AValue;
  DoChanged(TAction.Password);
end;

procedure TdxAuthenticationOptions.SetUserName(const AValue: string);
begin
  if FUserName = AValue then
    Exit;
  FUserName := AValue;
  DoChanged(TAction.UserName);
end;

procedure TdxAuthenticationOptions.DoReset;
begin
  UserName := '';
  Group := '';
  EMail := '';
  Password := '';
end;

{ TdxRangePermissionOptions }

procedure TdxRangePermissionOptions.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TdxRangePermissionOptions then
    begin
      BracketsColor := TdxRangePermissionOptions(Source).BracketsColor;
      Color := TdxRangePermissionOptions(Source).Color;
      HighlightBracketsColor := TdxRangePermissionOptions(Source).HighlightBracketsColor;
      HighlightColor := TdxRangePermissionOptions(Source).HighlightColor;
      Visibility := TdxRangePermissionOptions(Source).Visibility;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

class function TdxRangePermissionOptions.GetColor: TdxAlphaColor;
begin
  if FColorIndex >= Length(DefaultColors) then
    FColorIndex := 0;
  Result := DefaultColors[FColorIndex];
  Inc(FColorIndex);
end;

procedure TdxRangePermissionOptions.SetVisibility(const AValue: TdxRichEditRangePermissionVisibility);
begin
  if Visibility = AValue then
    Exit;
  FVisibility := AValue;
  DoChanged(TAction.Visibility);
end;

procedure TdxRangePermissionOptions.SetColor(const AValue: TdxAlphaColor);
begin
  if Color = AValue then
    Exit;
  FColor := AValue;
  DoChanged(TAction.Color);
end;

procedure TdxRangePermissionOptions.SetBracketsColor(const AValue: TdxAlphaColor);
begin
  if BracketsColor = AValue then
    Exit;
  FBracketsColor := AValue;
  DoChanged(TAction.BracketsColor);
end;

procedure TdxRangePermissionOptions.SetHighlightBracketsColor(const AValue: TdxAlphaColor);
begin
  if HighlightBracketsColor = AValue then
    Exit;
  FHighlightBracketsColor := AValue;
  DoChanged(TAction.HighlightBracketsColor);
end;

procedure TdxRangePermissionOptions.SetHighlightColor(const AValue: TdxAlphaColor);
begin
  if HighlightColor = AValue then
    Exit;
  FHighlightColor := AValue;
  DoChanged(TAction.HighlightColor);
end;

procedure TdxRangePermissionOptions.DoReset;
begin
  Visibility := DefaultVisibility;
  Color := DefaultColor;
  HighlightColor := DefaultHighlightColor;
  BracketsColor := DefaultBracketsColor;
  HighlightBracketsColor := DefaultHighlightBracketsColor;
end;

end.
