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

unit dxRichEdit.DocumentModel.ParagraphFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Classes, SysUtils, Graphics,
  dxCoreClasses, dxCoreGraphics,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.MergedProperties;

const
  NumberingListIndexNoNumberingList = -2;
  NumberingListIndexListIndexNotSetted = -1;
  NumberingListIndexMinValue = 0;
  NumberingListIndexMaxValue = MaxInt;

type
  TdxParagraphAlignment = TdxRichEditParagraphAlignment;

  TdxParagraphLineSpacing = TdxRichEditParagraphLineSpacing;

  TdxParagraphFirstLineIndent = TdxRichEditParagraphFirstLineIndent;

  IdxParagraphProperties = interface
  ['{AF526453-1103-4CAD-8569-255F8342D704}']
    function GetAfterAutoSpacing: Boolean;
    function GetAlignment: TdxParagraphAlignment;
    function GetBackColor: TdxAlphaColor;
    function GetBeforeAutoSpacing: Boolean;
    function GetContextualSpacing: Boolean;
    function GetFirstLineIndent: Integer;
    function GetFirstLineIndentType: TdxParagraphFirstLineIndent;
    function GetKeepLinesTogether: Boolean;
    function GetKeepWithNext: Boolean;
    function GetLeftIndent: Integer;
    function GetLineSpacing: Single;
    function GetLineSpacingType: TdxParagraphLineSpacing;
    function GetOutlineLevel: Integer;
    function GetPageBreakBefore: Boolean;
    function GetRightIndent: Integer;
    function GetSpacingAfter: Integer;
    function GetSpacingBefore: Integer;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;
    function GetWidowOrphanControl: Boolean;
    function GetLeftBorder: TdxBorderInfo;
    function GetRightBorder: TdxBorderInfo;
    function GetTopBorder: TdxBorderInfo;
    function GetBottomBorder: TdxBorderInfo;
    procedure SetAfterAutoSpacing(const Value: Boolean);
    procedure SetAlignment(const Value: TdxParagraphAlignment);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetBeforeAutoSpacing(const Value: Boolean);
    procedure SetContextualSpacing(const Value: Boolean);
    procedure SetFirstLineIndent(const Value: Integer);
    procedure SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
    procedure SetKeepLinesTogether(const Value: Boolean);
    procedure SetKeepWithNext(const Value: Boolean);
    procedure SetLeftIndent(const Value: Integer);
    procedure SetLineSpacing(const Value: Single);
    procedure SetLineSpacingType(const Value: TdxParagraphLineSpacing);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetPageBreakBefore(const Value: Boolean);
    procedure SetRightIndent(const Value: Integer);
    procedure SetSpacingAfter(const Value: Integer);
    procedure SetSpacingBefore(const Value: Integer);
    procedure SetSuppressHyphenation(const Value: Boolean);
    procedure SetSuppressLineNumbers(const Value: Boolean);
    procedure SetWidowOrphanControl(const Value: Boolean);
    procedure SetLeftBorder(const Value: TdxBorderInfo);
    procedure SetRightBorder(const Value: TdxBorderInfo);
    procedure SetTopBorder(const Value: TdxBorderInfo);
    procedure SetBottomBorder(const Value: TdxBorderInfo);

    property AfterAutoSpacing: Boolean read GetAfterAutoSpacing write SetAfterAutoSpacing;
    property Alignment: TdxParagraphAlignment read GetAlignment write SetAlignment;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property BeforeAutoSpacing: Boolean read GetBeforeAutoSpacing write SetBeforeAutoSpacing;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property FirstLineIndent: Integer read GetFirstLineIndent write SetFirstLineIndent;
    property FirstLineIndentType: TdxParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property KeepWithNext: Boolean read GetKeepWithNext write SetKeepWithNext;
    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property LineSpacing: Single read GetLineSpacing write SetLineSpacing;
    property LineSpacingType: TdxParagraphLineSpacing read GetLineSpacingType write SetLineSpacingType;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property RightIndent: Integer read GetRightIndent write SetRightIndent;
    property SpacingAfter: Integer read GetSpacingAfter write SetSpacingAfter;
    property SpacingBefore: Integer read GetSpacingBefore write SetSpacingBefore;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
    property WidowOrphanControl: Boolean read GetWidowOrphanControl write SetWidowOrphanControl;

    property LeftBorder: TdxBorderInfo read GetLeftBorder write SetLeftBorder;
    property RightBorder: TdxBorderInfo read GetRightBorder write SetRightBorder;
    property TopBorder: TdxBorderInfo read GetTopBorder write SetTopBorder;
    property BottomBorder: TdxBorderInfo read GetBottomBorder write SetBottomBorder;
  end;

  IdxParagraphPropertiesContainer = interface
  ['{B65775E3-D102-4DD4-968C-33EBC09EE673}']
    function GetPieceTable: TdxCustomPieceTable;
    function CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnParagraphPropertiesChanged;

    property PieceTable: TdxCustomPieceTable read GetPieceTable;
  end;

  TdxParagraphFormattingInfo = class(TdxCloneable)
  private const
    MaskLineSpacingType     = $00000007;
    MaskFirstLineIndentType = $00000018;
    MaskAlignment           = $00000060;
    MaskOutlineLevel        = $00000780;
    MaskSuppressHyphenation = $00000800;
    MaskSuppressLineNumbers = $00001000;
    MaskContextualSpacing   = $00002000;
    MaskPageBreakBefore     = $00004000;
    MaskBeforeAutoSpacing   = $00008000;
    MaskAfterAutoSpacing    = $00010000;
    MaskKeepWithNext        = $00020000;
    MaskKeepLinesTogether   = $00040000;
    MaskWidowOrphanControl  = $00080000;
  private
    FPackedValues: Integer;
    FLeftIndent: Integer;
    FRightIndent: Integer;
    FSpacingBefore: Integer;
    FSpacingAfter: Integer;
    FFirstLineIndent: Integer;
    FLineSpacing: Single;
    FBackColor: TdxAlphaColor;
    FLeftBorder: TdxBorderInfo;
    FRightBorder: TdxBorderInfo;
    FTopBorder: TdxBorderInfo;
    FBottomBorder: TdxBorderInfo;
    function GetBooleanValue(AMask: Integer): Boolean;
    procedure SetBooleanValue(AMask: Integer; AValue: Boolean);

    function GetAfterAutoSpacing: Boolean;
    function GetAlignment: TdxParagraphAlignment;
    function GetBeforeAutoSpacing: Boolean;
    function GetContextualSpacing: Boolean;
    function GetFirstLineIndentType: TdxParagraphFirstLineIndent;
    function GetKeepLinesTogether: Boolean;
    function GetKeepWithNext: Boolean;
    function GetLineSpacingType: TdxParagraphLineSpacing;
    function GetOutlineLevel: Integer;
    function GetPageBreakBefore: Boolean;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;
    function GetWidowOrphanControl: Boolean;
    procedure SetAfterAutoSpacing(const Value: Boolean);
    procedure SetAlignment(const Value: TdxParagraphAlignment);
    procedure SetBeforeAutoSpacing(const Value: Boolean);
    procedure SetContextualSpacing(const Value: Boolean);
    procedure SetFirstLineIndent(const Value: Integer);
    procedure SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
    procedure SetKeepLinesTogether(const Value: Boolean);
    procedure SetKeepWithNext(const Value: Boolean);
    procedure SetLineSpacing(const Value: Single);inline;
    procedure SetLineSpacingType(const Value: TdxParagraphLineSpacing);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetPageBreakBefore(const Value: Boolean);
    procedure SetSpacingAfter(const Value: Integer);
    procedure SetSpacingBefore(const Value: Integer);
    procedure SetSuppressHyphenation(const Value: Boolean);
    procedure SetSuppressLineNumbers(const Value: Boolean);
    procedure SetWidowOrphanControl(const Value: Boolean);
    procedure SetLeftBorder(const Value: TdxBorderInfo);
    procedure SetRightBorder(const Value: TdxBorderInfo);
    procedure SetTopBorder(const Value: TdxBorderInfo);
    procedure SetBottomBorder(const Value: TdxBorderInfo);
  protected
    property PackedValues: Integer read FPackedValues;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxParagraphFormattingInfo; reintroduce; inline;

    function Equals(Obj: TObject): Boolean; override; final;
    function GetHashCode: Integer; override;
    function SkipCompareFirstLineIndent: Boolean;
    function SkipCompareLineSpacing: Boolean;

    property AfterAutoSpacing: Boolean read GetAfterAutoSpacing write SetAfterAutoSpacing;
    property Alignment: TdxParagraphAlignment read GetAlignment write SetAlignment;
    property BackColor: TdxAlphaColor read FBackColor write FBackColor;
    property BeforeAutoSpacing: Boolean read GetBeforeAutoSpacing write SetBeforeAutoSpacing;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property FirstLineIndent: Integer read FFirstLineIndent write SetFirstLineIndent;
    property FirstLineIndentType: TdxParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property KeepWithNext: Boolean read GetKeepWithNext write SetKeepWithNext;
    property LeftIndent: Integer read FLeftIndent write FLeftIndent;
    property LineSpacing: Single read FLineSpacing write SetLineSpacing;
    property LineSpacingType: TdxParagraphLineSpacing read GetLineSpacingType write SetLineSpacingType;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property RightIndent: Integer read FRightIndent write FRightIndent;
    property SpacingAfter: Integer read FSpacingAfter write SetSpacingAfter;
    property SpacingBefore: Integer read FSpacingBefore write SetSpacingBefore;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
    property WidowOrphanControl: Boolean read GetWidowOrphanControl write SetWidowOrphanControl;
    property LeftBorder: TdxBorderInfo read FLeftBorder write SetLeftBorder;
    property RightBorder: TdxBorderInfo read FRightBorder write SetRightBorder;
    property TopBorder: TdxBorderInfo read FTopBorder write SetTopBorder;
    property BottomBorder: TdxBorderInfo read FBottomBorder write SetBottomBorder;
  end;
  TdxParagraphFormattingInfoClass = class of TdxParagraphFormattingInfo;

  TdxUsedParagraphFormattingOption = TdxRichEditUsedParagraphFormattingOption;

  TdxUsedParagraphFormattingOptions = set of TdxUsedParagraphFormattingOption;

  { TdxParagraphFormattingOptions }

  TdxParagraphFormattingOptions = record
  public const
    MaskUseAll = [
      TdxUsedParagraphFormattingOption.UseAlignment,
      TdxUsedParagraphFormattingOption.UseLeftIndent,
      TdxUsedParagraphFormattingOption.UseRightIndent,
      TdxUsedParagraphFormattingOption.UseSpacingBefore,
      TdxUsedParagraphFormattingOption.UseSpacingAfter,
      TdxUsedParagraphFormattingOption.UseLineSpacing,
      TdxUsedParagraphFormattingOption.UseFirstLineIndent,
      TdxUsedParagraphFormattingOption.UseSuppressHyphenation,
      TdxUsedParagraphFormattingOption.UseSuppressLineNumbers,
      TdxUsedParagraphFormattingOption.UseContextualSpacing,
      TdxUsedParagraphFormattingOption.UsePageBreakBefore,
      TdxUsedParagraphFormattingOption.UseBeforeAutoSpacing,
      TdxUsedParagraphFormattingOption.UseAfterAutoSpacing,
      TdxUsedParagraphFormattingOption.UseKeepWithNext,
      TdxUsedParagraphFormattingOption.UseKeepLinesTogether,
      TdxUsedParagraphFormattingOption.UseWidowOrphanControl,
      TdxUsedParagraphFormattingOption.UseOutlineLevel,
      TdxUsedParagraphFormattingOption.UseBackColor,
      TdxUsedParagraphFormattingOption.UseLeftBorder,
      TdxUsedParagraphFormattingOption.UseRightBorder,
      TdxUsedParagraphFormattingOption.UseTopBorder,
      TdxUsedParagraphFormattingOption.UseBottomBorder];
    MaskUseNone = [];
  private
    FValue: TdxUsedParagraphFormattingOptions;
    class function GetRootParagraphFormattingOption: TdxParagraphFormattingOptions; static; inline;
    class function GetEmptyParagraphFormattingOption: TdxParagraphFormattingOptions; static; inline;
  public
    constructor Create(AUsedValues: TdxUsedParagraphFormattingOptions);

    class operator Equal(const A, B: TdxParagraphFormattingOptions): Boolean;
    class property RootParagraphFormattingOption: TdxParagraphFormattingOptions read GetRootParagraphFormattingOption;
    class property EmptyParagraphFormattingOption: TdxParagraphFormattingOptions read GetEmptyParagraphFormattingOption;

    procedure CopyFrom(const Source: TdxParagraphFormattingOptions); inline;
    function Clone: TdxParagraphFormattingOptions; inline;
    function GetHashCode: Integer;

    procedure ResetUse(AOptions: TdxUsedParagraphFormattingOptions);

    function GetValue(const Index: TdxUsedParagraphFormattingOption): Boolean;
    procedure SetValue(const Index: TdxUsedParagraphFormattingOption; const Value: Boolean);
    property Value: TdxUsedParagraphFormattingOptions read FValue write FValue;
    property UseAlignment: Boolean index TdxUsedParagraphFormattingOption.UseAlignment read GetValue write SetValue;
    property UseLeftIndent: Boolean index TdxUsedParagraphFormattingOption.UseLeftIndent read GetValue write SetValue;
    property UseRightIndent: Boolean index TdxUsedParagraphFormattingOption.UseRightIndent read GetValue write SetValue;
    property UseSpacingBefore: Boolean index TdxUsedParagraphFormattingOption.UseSpacingBefore read GetValue write SetValue;
    property UseSpacingAfter: Boolean index TdxUsedParagraphFormattingOption.UseSpacingAfter read GetValue write SetValue;
    property UseLineSpacing: Boolean index TdxUsedParagraphFormattingOption.UseLineSpacing read GetValue write SetValue;
    property UseFirstLineIndent: Boolean index TdxUsedParagraphFormattingOption.UseFirstLineIndent read GetValue write SetValue;
    property UseSuppressHyphenation: Boolean index TdxUsedParagraphFormattingOption.UseSuppressHyphenation read GetValue write SetValue;
    property UseSuppressLineNumbers: Boolean index TdxUsedParagraphFormattingOption.UseSuppressLineNumbers read GetValue write SetValue;
    property UseContextualSpacing: Boolean index TdxUsedParagraphFormattingOption.UseContextualSpacing read GetValue write SetValue;
    property UsePageBreakBefore: Boolean index TdxUsedParagraphFormattingOption.UsePageBreakBefore read GetValue write SetValue;
    property UseBeforeAutoSpacing: Boolean index TdxUsedParagraphFormattingOption.UseBeforeAutoSpacing read GetValue write SetValue;
    property UseAfterAutoSpacing: Boolean index TdxUsedParagraphFormattingOption.UseAfterAutoSpacing read GetValue write SetValue;
    property UseKeepWithNext: Boolean index TdxUsedParagraphFormattingOption.UseKeepWithNext read GetValue write SetValue;
    property UseKeepLinesTogether: Boolean index TdxUsedParagraphFormattingOption.UseKeepLinesTogether read GetValue write SetValue;
    property UseWidowOrphanControl: Boolean index TdxUsedParagraphFormattingOption.UseWidowOrphanControl read GetValue write SetValue;
    property UseOutlineLevel: Boolean index TdxUsedParagraphFormattingOption.UseOutlineLevel read GetValue write SetValue;
    property UseBackColor: Boolean index TdxUsedParagraphFormattingOption.UseBackColor read GetValue write SetValue;
    property UseLeftBorder: Boolean index TdxUsedParagraphFormattingOption.UseLeftBorder read GetValue write SetValue;
    property UseRightBorder: Boolean index TdxUsedParagraphFormattingOption.UseRightBorder read GetValue write SetValue;
    property UseTopBorder: Boolean index TdxUsedParagraphFormattingOption.UseTopBorder read GetValue write SetValue;
    property UseBottomBorder: Boolean index TdxUsedParagraphFormattingOption.UseBottomBorder read GetValue write SetValue;
  end;
  PdxParagraphFormattingOptions = ^TdxParagraphFormattingOptions;

  { TdxParagraphFormattingBase }

  TdxParagraphFormattingBase = class(TdxIndexBasedObjectB<TdxParagraphFormattingInfo, TdxParagraphFormattingOptions>,
    IdxParagraphProperties)
  private
    //IdxParagraphProperties
    function GetAfterAutoSpacing: Boolean;
    function GetAlignment: TdxParagraphAlignment;
    function GetBackColor: TdxAlphaColor;
    function GetBeforeAutoSpacing: Boolean;
    function GetContextualSpacing: Boolean;
    function GetFirstLineIndent: Integer;
    function GetFirstLineIndentType: TdxParagraphFirstLineIndent;
    function GetKeepLinesTogether: Boolean;
    function GetKeepWithNext: Boolean;
    function GetLeftIndent: Integer;
    function GetLineSpacing: Single;
    function GetLineSpacingType: TdxParagraphLineSpacing;
    function GetOutlineLevel: Integer;
    function GetPageBreakBefore: Boolean;
    function GetRightIndent: Integer;
    function GetSpacingAfter: Integer;
    function GetSpacingBefore: Integer;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;
    function GetWidowOrphanControl: Boolean;

    procedure SetAfterAutoSpacing(const Value: Boolean);
    procedure SetAfterAutoSpacingCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    procedure SetAlignment(const Value: TdxParagraphAlignment);
    procedure SetAlignmentCore(const AInfo: TdxParagraphFormattingInfo; const Value: TdxParagraphAlignment);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetBackColorCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
    procedure SetBeforeAutoSpacing(const Value: Boolean);
    procedure SetBeforeAutoSpacingCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    procedure SetContextualSpacing(const Value: Boolean);
    procedure SetContextualSpacingCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    procedure SetFirstLineIndent(const Value: Integer);
    procedure SetFirstLineIndentCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
    procedure SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
    procedure SetFirstLineIndentTypeCore(const AInfo: TdxParagraphFormattingInfo; const Value: TdxParagraphFirstLineIndent);
    procedure SetKeepLinesTogether(const Value: Boolean);
    procedure SetKeepLinesTogetherCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    procedure SetKeepWithNext(const Value: Boolean);
    procedure SetKeepWithNextCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    procedure SetLeftIndent(const Value: Integer);
    procedure SetLeftIndentCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
    procedure SetLineSpacing(const Value: Single);
    procedure SetLineSpacingCore(const AInfo: TdxParagraphFormattingInfo; const Value: Single);
    procedure SetLineSpacingType(const Value: TdxParagraphLineSpacing);
    procedure SetLineSpacingTypeCore(const AInfo: TdxParagraphFormattingInfo; const Value: TdxParagraphLineSpacing);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetOutlineLevelCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
    procedure SetPageBreakBefore(const Value: Boolean);
    procedure SetPageBreakBeforeCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    procedure SetRightIndent(const Value: Integer);
    procedure SetRightIndentCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
    procedure SetSpacingAfter(const Value: Integer);
    procedure SetSpacingAfterCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
    procedure SetSpacingBefore(const Value: Integer);
    procedure SetSpacingBeforeCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
    procedure SetSuppressHyphenation(const Value: Boolean);
    procedure SetSuppressHyphenationCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    procedure SetSuppressLineNumbers(const Value: Boolean);
    procedure SetSuppressLineNumbersCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    procedure SetWidowOrphanControl(const Value: Boolean);
    procedure SetWidowOrphanControlCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
    function GetBottomBorder: TdxBorderInfo;
    function GetLeftBorder: TdxBorderInfo;
    function GetRightBorder: TdxBorderInfo;
    function GetTopBorder: TdxBorderInfo;
    procedure SetBottomBorder(const AValue: TdxBorderInfo);
    procedure SetBottomBorderCore(const AInfo: TdxParagraphFormattingInfo; const AValue: TdxBorderInfo);
    procedure SetLeftBorder(const AValue: TdxBorderInfo);
    procedure SetLeftBorderCore(const AInfo: TdxParagraphFormattingInfo; const AValue: TdxBorderInfo);
    procedure SetRightBorder(const AValue: TdxBorderInfo);
    procedure SetRightBorderCore(const AInfo: TdxParagraphFormattingInfo; const AValue: TdxBorderInfo);
    procedure SetTopBorder(const AValue: TdxBorderInfo);
    procedure SetTopBorderCore(const AInfo: TdxParagraphFormattingInfo; const AValue: TdxBorderInfo);
  protected
    function CloneCore: TdxParagraphFormattingBase; virtual;
    function PropertyEquals(const AOther: TdxIndexBasedObject<TdxParagraphFormattingInfo, TdxParagraphFormattingOptions>): Boolean; override;
    function SupportsFormatting: Boolean;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TdxParagraphFormattingInfo; const AFormattingOptions: TdxParagraphFormattingOptions); reintroduce;

    procedure SetMultipleLineSpacing(ANewLineSpacing: Single);
    procedure CopyFrom(Source: TdxCloneable); overload; override;
    procedure CopyFrom(const AInfo: TdxParagraphFormattingInfo; const AOptions: TdxParagraphFormattingOptions); reintroduce; overload;
    function Clone: TdxCloneable; override;

    property AfterAutoSpacing: Boolean read GetAfterAutoSpacing write SetAfterAutoSpacing;
    property Alignment: TdxParagraphAlignment read GetAlignment write SetAlignment;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property BeforeAutoSpacing: Boolean read GetBeforeAutoSpacing write SetBeforeAutoSpacing;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property FirstLineIndent: Integer read GetFirstLineIndent write SetFirstLineIndent;
    property FirstLineIndentType: TdxParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property KeepWithNext: Boolean read GetKeepWithNext write SetKeepWithNext;
    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property LineSpacing: Single read GetLineSpacing write SetLineSpacing;
    property LineSpacingType: TdxParagraphLineSpacing read GetLineSpacingType write SetLineSpacingType;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property RightIndent: Integer read GetRightIndent write SetRightIndent;
    property SpacingAfter: Integer read GetSpacingAfter write SetSpacingAfter;
    property SpacingBefore: Integer read GetSpacingBefore write SetSpacingBefore;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
    property WidowOrphanControl: Boolean read GetWidowOrphanControl write SetWidowOrphanControl;
    property LeftBorder: TdxBorderInfo read GetLeftBorder write SetLeftBorder;
    property RightBorder: TdxBorderInfo read GetRightBorder write SetRightBorder;
    property TopBorder: TdxBorderInfo read GetTopBorder write SetTopBorder;
    property BottomBorder: TdxBorderInfo read GetBottomBorder write SetBottomBorder;
  end;

  { TdxParagraphProperties }

  TdxParagraphProperties = class(TdxRichEditIndexBasedObject<TdxParagraphFormattingBase>, IdxParagraphProperties)
  private
    FOwner: IdxParagraphPropertiesContainer;

    function GetAfterAutoSpacing: Boolean;
    function GetAlignment: TdxParagraphAlignment;
    function GetBackColor: TdxAlphaColor;
    function GetBeforeAutoSpacing: Boolean;
    function GetContextualSpacing: Boolean;
    function GetFirstLineIndent: Integer;
    function GetFirstLineIndentType: TdxParagraphFirstLineIndent;
    function GetKeepLinesTogether: Boolean;
    function GetKeepWithNext: Boolean;
    function GetLeftIndent: Integer;
    function GetLineSpacing: Single;
    function GetLineSpacingType: TdxParagraphLineSpacing;
    function GetOutlineLevel: Integer;
    function GetPageBreakBefore: Boolean;
    function GetRightIndent: Integer;
    function GetSpacingAfter: Integer;
    function GetSpacingBefore: Integer;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;
    function GetWidowOrphanControl: Boolean;

    function GetUseAlignment: Boolean;
    function GetUseLeftIndent: Boolean;
    function GetUseRightIndent: Boolean;
    function GetUseSpacingBefore: Boolean;
    function GetUseSpacingAfter: Boolean;
    function GetUseLineSpacing: Boolean;
    function GetUseLineSpacingType: Boolean;
    function GetUseFirstLineIndent: Boolean;
    function GetUseFirstLineIndentType: Boolean;
    function GetUseSuppressHyphenation: Boolean;
    function GetUseSuppressLineNumbers: Boolean;
    function GetUseContextualSpacing: Boolean;
    function GetUsePageBreakBefore: Boolean;
    function GetUseBeforeAutoSpacing: Boolean;
    function GetUseAfterAutoSpacing: Boolean;
    function GetUseKeepWithNext: Boolean;
    function GetUseKeepLinesTogether: Boolean;
    function GetUseWidowOrphanControl: Boolean;
    function GetUseOutlineLevel: Boolean;
    function GetUseBackColor: Boolean;

    function SetAfterAutoSpacingCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetAfterAutoSpacing(const Value: Boolean);
    procedure SetAlignment(const Value: TdxParagraphAlignment);
    function SetAlignmentCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetBackColor(const Value: TdxAlphaColor);
    function SetBackColorCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetBeforeAutoSpacing(const Value: Boolean);
    function SetBeforeAutoSpacingCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetContextualSpacing(const Value: Boolean);
    function SetContextualSpacingCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetFirstLineIndent(const Value: Integer);
    function SetFirstLineIndentCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
    function SetFirstLineIndentTypeCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetKeepLinesTogether(const Value: Boolean);
    function SetKeepLinesTogetherCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetKeepWithNext(const Value: Boolean);
    function SetKeepWithNextCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetLeftIndent(const Value: Integer);
    function SetLeftIndentCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetLineSpacing(const Value: Single);
    function SetLineSpacingCore(const AInfo: TdxParagraphFormattingBase; const Value: Single): TdxDocumentModelChangeActions;
    procedure SetLineSpacingType(const Value: TdxParagraphLineSpacing);
    function SetLineSpacingTypeCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetOutlineLevel(const Value: Integer);
    function SetOutlineLevelCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetPageBreakBefore(const Value: Boolean);
    function SetPageBreakBeforeCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetRightIndent(const Value: Integer);
    function SetRightIndentCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetSpacingAfter(const Value: Integer);
    function SetSpacingAfterCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetSpacingBefore(const Value: Integer);
    function SetSpacingBeforeCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetSuppressHyphenation(const Value: Boolean);
    function SetSuppressHyphenationCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetSuppressLineNumbers(const Value: Boolean);
    function SetSuppressLineNumbersCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetWidowOrphanControl(const Value: Boolean);
    function SetWidowOrphanControlCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    function GetLeftBorder: TdxBorderInfo;
    procedure SetLeftBorder(const AValue: TdxBorderInfo);
    function GetUseLeftBorder: Boolean;
    function GetRightBorder: TdxBorderInfo;
    procedure SetRightBorder(const AValue: TdxBorderInfo);
    function GetUseRightBorder: Boolean;
    function GetTopBorder: TdxBorderInfo;
    procedure SetTopBorder(const AValue: TdxBorderInfo);
    function GetUseTopBorder: Boolean;
    function GetBottomBorder: TdxBorderInfo;
    procedure SetBottomBorder(const AValue: TdxBorderInfo);
    function GetUseBottomBorder: Boolean;
  strict protected
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxParagraphFormattingBase>; override;
    procedure OnIndexChanged; override;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
    function GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener; override;
    function SetLeftBorderCore(const AInfo: TdxParagraphFormattingBase; const AValue: TdxBorderInfo): TdxDocumentModelChangeActions; virtual;
    function SetRightBorderCore(const AInfo: TdxParagraphFormattingBase; const AValue: TdxBorderInfo): TdxDocumentModelChangeActions; virtual;
    function SetTopBorderCore(const AInfo: TdxParagraphFormattingBase; const AValue: TdxBorderInfo): TdxDocumentModelChangeActions; virtual;
    function SetBottomBorderCore(const AInfo: TdxParagraphFormattingBase; const AValue: TdxBorderInfo): TdxDocumentModelChangeActions; virtual;
  public
    constructor Create(const AOwner: IdxParagraphPropertiesContainer); reintroduce;
    destructor Destroy; override;

    class procedure ApplyPropertiesDiff(ATarget: TdxParagraphProperties; ATargetMergedInfo: TdxParagraphFormattingInfo; ASourceMergedInfo: TdxParagraphFormattingInfo);
    procedure CopyFrom(const AParagraphProperties: TdxMergedProperties<TdxParagraphFormattingInfo, TdxParagraphFormattingOptions>); overload;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const Source: TdxParagraphFormattingBase); override;
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxParagraphFormattingBase>); override;
  {$ENDIF}
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    procedure Merge(AProperties: TdxParagraphProperties); virtual;
    procedure Reset;
    procedure ResetUse(AMask: TdxUsedParagraphFormattingOptions);
    procedure ResetAllUse;
    function UseVal(AMask: TdxUsedParagraphFormattingOption): Boolean;

    property AfterAutoSpacing: Boolean read GetAfterAutoSpacing write SetAfterAutoSpacing;
    property Alignment: TdxParagraphAlignment read GetAlignment write SetAlignment;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property BeforeAutoSpacing: Boolean read GetBeforeAutoSpacing write SetBeforeAutoSpacing;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property FirstLineIndent: Integer read GetFirstLineIndent write SetFirstLineIndent;
    property FirstLineIndentType: TdxParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property KeepWithNext: Boolean read GetKeepWithNext write SetKeepWithNext;
    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property LineSpacing: Single read GetLineSpacing write SetLineSpacing;
    property LineSpacingType: TdxParagraphLineSpacing read GetLineSpacingType write SetLineSpacingType;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property RightIndent: Integer read GetRightIndent write SetRightIndent;
    property SpacingAfter: Integer read GetSpacingAfter write SetSpacingAfter;
    property SpacingBefore: Integer read GetSpacingBefore write SetSpacingBefore;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
    property WidowOrphanControl: Boolean read GetWidowOrphanControl write SetWidowOrphanControl;

    property UseAlignment: Boolean read GetUseAlignment;
    property UseLeftIndent: Boolean read GetUseLeftIndent;
    property UseRightIndent: Boolean read GetUseRightIndent;
    property UseSpacingBefore: Boolean read GetUseSpacingBefore;
    property UseSpacingAfter: Boolean read GetUseSpacingAfter;
    property UseLineSpacing: Boolean read GetUseLineSpacing;
    property UseLineSpacingType: Boolean read GetUseLineSpacingType;
    property UseFirstLineIndent: Boolean read GetUseFirstLineIndent;
    property UseFirstLineIndentType: Boolean read GetUseFirstLineIndentType;
    property UseSuppressHyphenation: Boolean read GetUseSuppressHyphenation;
    property UseSuppressLineNumbers: Boolean read GetUseSuppressLineNumbers;
    property UseContextualSpacing: Boolean read GetUseContextualSpacing;
    property UsePageBreakBefore: Boolean read GetUsePageBreakBefore;
    property UseBeforeAutoSpacing: Boolean read GetUseBeforeAutoSpacing;
    property UseAfterAutoSpacing: Boolean read GetUseAfterAutoSpacing;
    property UseKeepWithNext: Boolean read GetUseKeepWithNext;
    property UseKeepLinesTogether: Boolean read GetUseKeepLinesTogether;
    property UseWidowOrphanControl: Boolean read GetUseWidowOrphanControl;
    property UseOutlineLevel: Boolean read GetUseOutlineLevel;
    property UseBackColor: Boolean read GetUseBackColor;

    property LeftBorder: TdxBorderInfo read GetLeftBorder write SetLeftBorder;
    property UseLeftBorder: Boolean read GetUseLeftBorder;
    property RightBorder: TdxBorderInfo read GetRightBorder write SetRightBorder;
    property UseRightBorder: Boolean read GetUseRightBorder;
    property TopBorder: TdxBorderInfo read GetTopBorder write SetTopBorder;
    property UseTopBorder: Boolean read GetUseTopBorder;
    property BottomBorder: TdxBorderInfo read GetBottomBorder write SetBottomBorder;
    property UseBottomBorder: Boolean read GetUseBottomBorder;
  end;

  { TdxParagraphFormattingInfoCache }

  TdxParagraphFormattingInfoCache = class(TdxUniqueItemsCache<TdxParagraphFormattingInfo>)
  public const
    DefaultItemIndex = 0;
  protected
    class function CreateDefaultItemMSO2007(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo;
    class function CreateDefaultItemMSO2003(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo;

    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo; override;
  end;

  { TdxParagraphFormattingCache }

  TdxParagraphFormattingCache = class(TdxUniqueItemsCache<TdxParagraphFormattingBase>)
  public const
    EmptyParagraphFormattingIndex = 0;
    RootParagraphFormattingIndex = 1;
  strict private
    function GetDefaultInfo: TdxParagraphFormattingInfo;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingBase; override;
    function CreateDefaultParagraphFormattingInfo(ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingInfo; virtual;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;
    property DefaultInfo: TdxParagraphFormattingInfo read GetDefaultInfo;
  end;

  { TdxMergedParagraphProperties }

  TdxMergedParagraphProperties = class(TdxMergedProperties<TdxParagraphFormattingInfo, TdxParagraphFormattingOptions>)
  public
    constructor Create(const AProperties: TdxParagraphProperties); reintroduce; overload;
    constructor Create(const AProperties: TdxMergedParagraphProperties); reintroduce; overload;

    procedure Merge(const AProperties: TdxParagraphProperties); overload;
    procedure Merge(const AProperties: TdxMergedParagraphProperties); overload;
    procedure MergeCore(const AInfo: TdxParagraphFormattingInfo; const AOptions: TdxParagraphFormattingOptions);
  end;

  { TdxParagraphFormattingChangeActionsCalculator }

  TdxParagraphFormattingChangeType = (
    None,
    Alignment,
    LeftIndent,
    RightIndent,
    SpacingBefore,
    SpacingAfter,
    LineSpacingType,
    LineSpacing,
    FirstLineIndentType,
    FirstLineIndent,
    SuppressHyphenation,
    SuppressLineNumbers,
    ContextualSpacing,
    PageBreakBefore,
    BeforeAutoSpacing,
    AfterAutoSpacing,
    KeepWithNext,
    KeepLinesTogether,
    WidowOrphanControl,
    OutlineLevel,
    BackColor,
    ParagraphStyle,
    BatchUpdate,
    NumberingListIndex,
    LeftBorder,
    RightBorder,
    TopBorder,
    BottomBorder
  );

  TdxParagraphFormattingChangeActionsCalculator = class
  public
    class function CalculateChangeActions(const AChange: TdxParagraphFormattingChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxParagraphParagraphPropertiesChangedHistoryItem }

  TdxParagraphParagraphPropertiesChangedHistoryItem = class(TdxIndexChangedHistoryItemCore)
  private
    FParagraphIndex: Integer;
  public
    constructor Create(const APieceTable: TdxCustomPieceTable; AParagraphIndex: Integer); reintroduce;

    function GetObject: TdxIndexBasedObject; override;

    property ParagraphIndex: Integer read FParagraphIndex;
  end;

  { TdxParagraphMergedParagraphPropertiesCachedResult }

  TdxParagraphMergedParagraphPropertiesCachedResult = class
  private
    FTableStyleParagraphPropertiesIndex: Integer;
    FOwnListLevelParagraphPropertiesIndex: Integer;
    FParagraphPropertiesIndex: Integer;
    FMergedParagraphProperties: TdxMergedParagraphProperties;
    FParagraphStyleIndex: Integer;
    procedure SetMergedParagraphProperties(Value: TdxMergedParagraphProperties);
  public
    constructor Create;
    destructor Destroy; override;

    property ParagraphPropertiesIndex: Integer read FParagraphPropertiesIndex write FParagraphPropertiesIndex;
    property ParagraphStyleIndex: Integer read FParagraphStyleIndex write FParagraphStyleIndex;
    property TableStyleParagraphPropertiesIndex: Integer read FTableStyleParagraphPropertiesIndex write FTableStyleParagraphPropertiesIndex;
    property OwnListLevelParagraphPropertiesIndex: Integer read FOwnListLevelParagraphPropertiesIndex write FOwnListLevelParagraphPropertiesIndex;
    property MergedParagraphProperties: TdxMergedParagraphProperties read FMergedParagraphProperties write SetMergedParagraphProperties;
  end;

implementation

uses
  RTLConsts,
  Math,

  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Cache.Simple,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.Exceptions;

type

  { TdxDefaultParagraphFormattingBase }

  TdxDefaultParagraphFormattingBase = class(TdxParagraphFormattingBase)
  private
    FDefaultInfo: TdxParagraphFormattingInfo;
  protected
    function CloneCore: TdxParagraphFormattingBase; override;
    property DefaultInfo: TdxParagraphFormattingInfo read FDefaultInfo;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TdxParagraphFormattingInfo; const AFormattingOptions: TdxParagraphFormattingOptions);
    destructor Destroy; override;
  end;

constructor TdxDefaultParagraphFormattingBase.Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TdxParagraphFormattingInfo; const AFormattingOptions: TdxParagraphFormattingOptions);
begin
  inherited Create(APieceTable, ADocumentModel, nil, AFormattingOptions);
  SetInfo(AFormattingInfo);
  FDefaultInfo := AFormattingInfo;
end;

destructor TdxDefaultParagraphFormattingBase.Destroy;
begin
  SetInfo(nil);
  inherited Destroy;
end;

function TdxDefaultParagraphFormattingBase.CloneCore: TdxParagraphFormattingBase;
begin
  Result := TdxParagraphFormattingBase.Create(PieceTable, DocumentModel, Info, Options);
end;

{ TdxParagraphFormattingInfo }

constructor TdxParagraphFormattingInfo.Create;
begin
  inherited Create;
  FLeftBorder := TdxBorderInfo.Create;
  FRightBorder := TdxBorderInfo.Create;
  FTopBorder := TdxBorderInfo.Create;
  FBottomBorder := TdxBorderInfo.Create;
end;

destructor TdxParagraphFormattingInfo.Destroy;
begin
  FLeftBorder.Free;
  FRightBorder.Free;
  FTopBorder.Free;
  FBottomBorder.Free;
  inherited Destroy;
end;

//procedure TdxParagraphFormattingInfo.CopyFrom(const Source: TdxParagraphFormattingInfo);
function TdxParagraphFormattingInfo.Clone: TdxParagraphFormattingInfo;
begin
  Result := TdxParagraphFormattingInfo(inherited Clone);
end;

procedure TdxParagraphFormattingInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxParagraphFormattingInfo absolute Source;
begin
  FPackedValues := ASource.PackedValues;

  LeftIndent := ASource.LeftIndent;
  RightIndent := ASource.RightIndent;
  SpacingBefore := ASource.SpacingBefore;
  SpacingAfter := ASource.SpacingAfter;
  LineSpacing := ASource.LineSpacing;
  FirstLineIndent := ASource.FirstLineIndent;
  BackColor := ASource.BackColor;

  TopBorder.CopyFrom(ASource.TopBorder);
  BottomBorder.CopyFrom(ASource.BottomBorder);
  LeftBorder.CopyFrom(ASource.LeftBorder);
  RightBorder.CopyFrom(ASource.RightBorder);
end;

function TdxParagraphFormattingInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxParagraphFormattingInfo absolute Obj;
begin
  Assert(Obj is TdxParagraphFormattingInfo);
  Result :=
    (FPackedValues = AInfo.PackedValues) and
    (LeftIndent = AInfo.LeftIndent) and
    (RightIndent = AInfo.RightIndent) and
    (SpacingAfter = AInfo.SpacingAfter) and
    (SpacingBefore = AInfo.SpacingBefore) and
    (BackColor = AInfo.BackColor) and
    (SkipCompareFirstLineIndent or (FirstLineIndent = AInfo.FirstLineIndent)) and
    (SkipCompareLineSpacing or (LineSpacing = AInfo.LineSpacing)) and
    FTopBorder.Equals(AInfo.TopBorder) and
    FBottomBorder.Equals(AInfo.BottomBorder) and
    FLeftBorder.Equals(AInfo.LeftBorder) and
    FRightBorder.Equals(AInfo.RightBorder);
end;

function TdxParagraphFormattingInfo.GetHashCode: Integer;
begin
  Result :=
    FPackedValues xor
    FLeftIndent xor
    FRightIndent xor
    FSpacingAfter xor
    FSpacingBefore;

  if not SkipCompareLineSpacing then
    Result := Result xor PInteger(@FLineSpacing)^;

  if not SkipCompareFirstLineIndent then
    Result := Result xor Integer(FFirstLineIndent);

  Result := Result xor
    Integer(FBackColor) xor
    FTopBorder.GetHashCode xor
    FBottomBorder.GetHashCode xor
    FLeftBorder.GetHashCode xor
    FRightBorder.GetHashCode;
end;


function TdxParagraphFormattingInfo.SkipCompareLineSpacing: Boolean;
begin
  Result := False;
end;

function TdxParagraphFormattingInfo.SkipCompareFirstLineIndent: Boolean;
begin
  Result := False;
end;

function TdxParagraphFormattingInfo.GetBooleanValue(AMask: Integer): Boolean;
begin
  Result := FPackedValues and AMask <> 0;
end;

procedure TdxParagraphFormattingInfo.SetBooleanValue(AMask: Integer; AValue: Boolean);
begin
  if AValue then
    FPackedValues := FPackedValues or AMask
  else
    FPackedValues := FPackedValues and not AMask;
end;

function TdxParagraphFormattingInfo.GetAfterAutoSpacing: Boolean;
begin
  Result := GetBooleanValue(MaskAfterAutoSpacing);
end;

function TdxParagraphFormattingInfo.GetAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment((FPackedValues and MaskAlignment) shr 5);
end;

function TdxParagraphFormattingInfo.GetBeforeAutoSpacing: Boolean;
begin
  Result := GetBooleanValue(MaskBeforeAutoSpacing);
end;

function TdxParagraphFormattingInfo.GetContextualSpacing: Boolean;
begin
  Result := GetBooleanValue(MaskContextualSpacing);
end;

function TdxParagraphFormattingInfo.GetFirstLineIndentType: TdxParagraphFirstLineIndent;
begin
  Result := TdxParagraphFirstLineIndent((FPackedValues and MaskFirstLineIndentType) shr 3);
end;

function TdxParagraphFormattingInfo.GetKeepLinesTogether: Boolean;
begin
  Result := GetBooleanValue(MaskKeepLinesTogether);
end;

function TdxParagraphFormattingInfo.GetKeepWithNext: Boolean;
begin
  Result := GetBooleanValue(MaskKeepWithNext);
end;

function TdxParagraphFormattingInfo.GetLineSpacingType: TdxParagraphLineSpacing;
begin
  Result := TdxParagraphLineSpacing(FPackedValues and MaskLineSpacingType);
end;

function TdxParagraphFormattingInfo.GetOutlineLevel: Integer;
begin
  Result := (FPackedValues and MaskOutlineLevel) shr 7;
end;

function TdxParagraphFormattingInfo.GetPageBreakBefore: Boolean;
begin
  Result := GetBooleanValue(MaskPageBreakBefore);
end;

function TdxParagraphFormattingInfo.GetSuppressHyphenation: Boolean;
begin
  Result := GetBooleanValue(MaskSuppressHyphenation);
end;

function TdxParagraphFormattingInfo.GetSuppressLineNumbers: Boolean;
begin
  Result := GetBooleanValue(MaskSuppressLineNumbers);
end;

function TdxParagraphFormattingInfo.GetWidowOrphanControl: Boolean;
begin
  Result := GetBooleanValue(MaskWidowOrphanControl);
end;

procedure TdxParagraphFormattingInfo.SetAfterAutoSpacing(const Value: Boolean);
begin
  SetBooleanValue(MaskAfterAutoSpacing, Value);
end;

procedure TdxParagraphFormattingInfo.SetAlignment(const Value: TdxParagraphAlignment);
begin
  FPackedValues := FPackedValues and not MaskAlignment;
  FPackedValues := FPackedValues or (Integer(Ord(Value)) shl 5);
end;

procedure TdxParagraphFormattingInfo.SetBeforeAutoSpacing(const Value: Boolean);
begin
  SetBooleanValue(MaskBeforeAutoSpacing, Value);
end;

procedure TdxParagraphFormattingInfo.SetContextualSpacing(const Value: Boolean);
begin
  SetBooleanValue(MaskContextualSpacing, Value);
end;

procedure TdxParagraphFormattingInfo.SetFirstLineIndent(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('FirstLineIndent', value);
  FFirstLineIndent := Value;
end;

procedure TdxParagraphFormattingInfo.SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
begin
  FPackedValues := FPackedValues and not MaskFirstLineIndentType;
  FPackedValues := FPackedValues or (Integer(Ord(Value)) shl 3);
end;

procedure TdxParagraphFormattingInfo.SetKeepLinesTogether(const Value: Boolean);
begin
  SetBooleanValue(MaskKeepLinesTogether, Value);
end;

procedure TdxParagraphFormattingInfo.SetKeepWithNext(const Value: Boolean);
begin
  SetBooleanValue(MaskKeepWithNext, Value);
end;

procedure TdxParagraphFormattingInfo.SetLineSpacing(const Value: Single);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('LineSpacing', FloatToStr(Value));
  FLineSpacing := Value;
end;

procedure TdxParagraphFormattingInfo.SetLineSpacingType(const Value: TdxParagraphLineSpacing);
begin
  FPackedValues := FPackedValues and not MaskLineSpacingType;
  FPackedValues := FPackedValues or Integer(Ord(Value));
end;

procedure TdxParagraphFormattingInfo.SetOutlineLevel(const Value: Integer);
begin
  FPackedValues := FPackedValues and not MaskOutlineLevel;
  FPackedValues := FPackedValues or ((Integer(Value) shl 7) and MaskOutlineLevel);
end;

procedure TdxParagraphFormattingInfo.SetPageBreakBefore(const Value: Boolean);
begin
  SetBooleanValue(MaskPageBreakBefore, Value);
end;

procedure TdxParagraphFormattingInfo.SetSpacingAfter(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('SpacingAfter', Value);
  FSpacingAfter := Value;
end;

procedure TdxParagraphFormattingInfo.SetSpacingBefore(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('SpacingBefore', Value);
  FSpacingBefore := Value;
end;

procedure TdxParagraphFormattingInfo.SetSuppressHyphenation(const Value: Boolean);
begin
  SetBooleanValue(MaskSuppressHyphenation, Value);
end;

procedure TdxParagraphFormattingInfo.SetSuppressLineNumbers(const Value: Boolean);
begin
  SetBooleanValue(MaskSuppressLineNumbers, Value);
end;

procedure TdxParagraphFormattingInfo.SetWidowOrphanControl(const Value: Boolean);
begin
  SetBooleanValue(MaskWidowOrphanControl, Value);
end;

procedure TdxParagraphFormattingInfo.SetLeftBorder(const Value: TdxBorderInfo);
begin
  FLeftBorder.CopyFrom(Value);
end;

procedure TdxParagraphFormattingInfo.SetRightBorder(const Value: TdxBorderInfo);
begin
  FRightBorder.CopyFrom(Value);
end;

procedure TdxParagraphFormattingInfo.SetTopBorder(const Value: TdxBorderInfo);
begin
  FTopBorder.CopyFrom(Value);
end;

procedure TdxParagraphFormattingInfo.SetBottomBorder(const Value: TdxBorderInfo);
begin
  FBottomBorder.CopyFrom(Value);
end;

{ TdxParagraphFormattingOptions }

constructor TdxParagraphFormattingOptions.Create(AUsedValues: TdxUsedParagraphFormattingOptions);
begin
  FValue := AUsedValues;
end;

class operator TdxParagraphFormattingOptions.Equal(const A, B: TdxParagraphFormattingOptions): Boolean;
begin
  Result := A.FValue = B.FValue;
end;

procedure TdxParagraphFormattingOptions.CopyFrom(const Source: TdxParagraphFormattingOptions);
begin
  FValue := Source.FValue;
end;

function TdxParagraphFormattingOptions.Clone: TdxParagraphFormattingOptions;
begin
  Result.CopyFrom(Self);
end;

class function TdxParagraphFormattingOptions.GetEmptyParagraphFormattingOption: TdxParagraphFormattingOptions;
begin
  Result := TdxParagraphFormattingOptions.Create(MaskUseNone);
end;

function TdxParagraphFormattingOptions.GetHashCode: Integer;
begin
  Result := Integer(Value);
end;

procedure TdxParagraphFormattingOptions.ResetUse(AOptions: TdxUsedParagraphFormattingOptions);
begin
  FValue := FValue - AOptions;
end;

class function TdxParagraphFormattingOptions.GetRootParagraphFormattingOption: TdxParagraphFormattingOptions;
begin
  Result := TdxParagraphFormattingOptions.Create(TdxParagraphFormattingOptions.MaskUseAll);
end;

function TdxParagraphFormattingOptions.GetValue(const Index: TdxUsedParagraphFormattingOption): Boolean;
begin
  Result := Index in FValue;
end;

procedure TdxParagraphFormattingOptions.SetValue(const Index: TdxUsedParagraphFormattingOption; const Value: Boolean);
begin
  if Value then
    Include(FValue, Index)
  else
    Exclude(FValue, Index);
end;

{ TdxParagraphFormattingBase }

constructor TdxParagraphFormattingBase.Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TdxParagraphFormattingInfo; const AFormattingOptions: TdxParagraphFormattingOptions);
begin
  inherited Create(APieceTable, ADocumentModel, AFormattingInfo, AFormattingOptions);
end;

procedure TdxParagraphFormattingBase.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxParagraphFormattingBase absolute Source;
begin
  CopyFrom(ASource.Info, ASource.Options);
end;

function TdxParagraphFormattingBase.CloneCore: TdxParagraphFormattingBase;
begin
  Result := TdxParagraphFormattingBase.Create(PieceTable, DocumentModel, Info, Options);
end;

procedure TdxParagraphFormattingBase.CopyFrom(const AInfo: TdxParagraphFormattingInfo; const AOptions: TdxParagraphFormattingOptions);
begin
  CopyFromCore(AInfo, AOptions);
end;

function TdxParagraphFormattingBase.SupportsFormatting: Boolean;
begin
  Result := DocumentModel.DocumentCapabilities.ParagraphFormattingAllowed;
end;

function TdxParagraphFormattingBase.Clone: TdxCloneable;
begin
  if Self = nil then
    Exit(nil);
  Result := CloneCore;
end;

function TdxParagraphFormattingBase.PropertyEquals(const AOther: TdxIndexBasedObject<TdxParagraphFormattingInfo, TdxParagraphFormattingOptions>): Boolean;
begin
  Result := (Options.Value = AOther.Options.Value) and Info.Equals(AOther.Info);
end;

procedure TdxParagraphFormattingBase.SetMultipleLineSpacing(ANewLineSpacing: Single);
begin
  if ANewLineSpacing <= 0 then
  begin
    LineSpacingType := TdxParagraphLineSpacing.Single;
    Exit;
  end;
  LineSpacingType := TdxParagraphLineSpacing.Multiple;
  if SameValue(ANewLineSpacing, 2.0) then
    LineSpacingType := TdxParagraphLineSpacing.Double;
  if SameValue(ANewLineSpacing, 1.5) then
    LineSpacingType := TdxParagraphLineSpacing.Sesquialteral;
  if SameValue(ANewLineSpacing, 1) then
    LineSpacingType := TdxParagraphLineSpacing.Single;
  LineSpacing := ANewLineSpacing;
end;

function TdxParagraphFormattingBase.GetAfterAutoSpacing: Boolean;
begin
  Result := Info.AfterAutoSpacing;
end;

function TdxParagraphFormattingBase.GetAlignment: TdxParagraphAlignment;
begin
  Result := Info.Alignment;
end;

function TdxParagraphFormattingBase.GetBackColor: TdxAlphaColor;
begin
  Result := Info.BackColor;
end;

function TdxParagraphFormattingBase.GetBeforeAutoSpacing: Boolean;
begin
  Result := Info.BeforeAutoSpacing;
end;

function TdxParagraphFormattingBase.GetContextualSpacing: Boolean;
begin
  Result := Info.ContextualSpacing;
end;

function TdxParagraphFormattingBase.GetFirstLineIndent: Integer;
begin
  Result := Info.FirstLineIndent;
end;

function TdxParagraphFormattingBase.GetFirstLineIndentType: TdxParagraphFirstLineIndent;
begin
  Result := Info.FirstLineIndentType;
end;

function TdxParagraphFormattingBase.GetKeepLinesTogether: Boolean;
begin
  Result := Info.KeepLinesTogether;
end;

function TdxParagraphFormattingBase.GetKeepWithNext: Boolean;
begin
  Result := Info.KeepWithNext;
end;

function TdxParagraphFormattingBase.GetLeftIndent: Integer;
begin
  Result := Info.LeftIndent;
end;

function TdxParagraphFormattingBase.GetLineSpacing: Single;
begin
  Result := Info.LineSpacing;
end;

function TdxParagraphFormattingBase.GetLineSpacingType: TdxParagraphLineSpacing;
begin
  Result := Info.LineSpacingType;
end;

function TdxParagraphFormattingBase.GetOutlineLevel: Integer;
begin
  Result := Info.OutlineLevel;
end;

function TdxParagraphFormattingBase.GetPageBreakBefore: Boolean;
begin
  Result := Info.PageBreakBefore;
end;

function TdxParagraphFormattingBase.GetRightIndent: Integer;
begin
  Result := Info.RightIndent;
end;

function TdxParagraphFormattingBase.GetSpacingAfter: Integer;
begin
  Result := Info.SpacingAfter;
end;

function TdxParagraphFormattingBase.GetSpacingBefore: Integer;
begin
  Result := Info.SpacingBefore;
end;

function TdxParagraphFormattingBase.GetSuppressHyphenation: Boolean;
begin
  Result := Info.SuppressHyphenation;
end;

function TdxParagraphFormattingBase.GetSuppressLineNumbers: Boolean;
begin
  Result := Info.SuppressLineNumbers;
end;

function TdxParagraphFormattingBase.GetWidowOrphanControl: Boolean;
begin
  Result := Info.WidowOrphanControl;
end;

procedure TdxParagraphFormattingBase.SetAfterAutoSpacingCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.AfterAutoSpacing := Value;
end;

procedure TdxParagraphFormattingBase.SetAfterAutoSpacing(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.AfterAutoSpacing = Value) and Options.UseAfterAutoSpacing then
    Exit;
  SetAfterAutoSpacingCore(Info, Value);
  Options.UseAfterAutoSpacing := True;
end;

procedure TdxParagraphFormattingBase.SetAlignmentCore(const AInfo: TdxParagraphFormattingInfo; const Value: TdxParagraphAlignment);
begin
  AInfo.Alignment := TdxParagraphAlignment(Value);
end;

procedure TdxParagraphFormattingBase.SetAlignment(const Value: TdxParagraphAlignment);
begin
  if not SupportsFormatting or (Info.Alignment = Value) and Options.UseAlignment then
    Exit;
  SetAlignmentCore(Info, Value);
  Options.UseAlignment := True;
end;

procedure TdxParagraphFormattingBase.SetBackColorCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
begin
  AInfo.BackColor := Value;
end;

procedure TdxParagraphFormattingBase.SetBackColor(const Value: TdxAlphaColor);
begin
  if not SupportsFormatting or (Info.BackColor = Value) and Options.UseBackColor then
    Exit;
  SetBackColorCore(Info, Value);
  Options.UseBackColor := True;
end;

procedure TdxParagraphFormattingBase.SetBeforeAutoSpacingCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.BeforeAutoSpacing := Value;
end;

procedure TdxParagraphFormattingBase.SetBeforeAutoSpacing(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.BeforeAutoSpacing = Value) and Options.UseBeforeAutoSpacing then
    Exit;
  SetBeforeAutoSpacingCore(Info, Value);
  Options.UseBeforeAutoSpacing := True;
end;

procedure TdxParagraphFormattingBase.SetContextualSpacingCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.ContextualSpacing := Value;
end;

procedure TdxParagraphFormattingBase.SetContextualSpacing(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.ContextualSpacing = Value) and Options.UseContextualSpacing then
    Exit;
  SetContextualSpacingCore(Info, Value);
  Options.UseContextualSpacing := True;
end;

procedure TdxParagraphFormattingBase.SetFirstLineIndentCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
begin
  AInfo.FirstLineIndent := Value;
end;

procedure TdxParagraphFormattingBase.SetFirstLineIndent(const Value: Integer);
begin
  if not SupportsFormatting or (Info.FirstLineIndent = Value) and Options.UseFirstLineIndent then
    Exit;
  SetFirstLineIndentCore(Info, Value);
  Options.UseFirstLineIndent := True;
end;

procedure TdxParagraphFormattingBase.SetFirstLineIndentTypeCore(const AInfo: TdxParagraphFormattingInfo; const Value: TdxParagraphFirstLineIndent);
begin
  AInfo.FirstLineIndentType := Value;
  AInfo.FirstLineIndent := 0;
end;

procedure TdxParagraphFormattingBase.SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
begin
  if not SupportsFormatting or (Info.FirstLineIndentType = Value) and Options.UseFirstLineIndent then
    Exit;
  SetFirstLineIndentTypeCore(Info, Value);
  Options.UseFirstLineIndent := True;
end;

procedure TdxParagraphFormattingBase.SetKeepLinesTogetherCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.KeepLinesTogether := Value;
end;

procedure TdxParagraphFormattingBase.SetKeepLinesTogether(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.KeepLinesTogether = Value) and Options.UseKeepLinesTogether then
    Exit;
  SetKeepLinesTogetherCore(Info, Value);
  Options.UseKeepLinesTogether := True;
end;

procedure TdxParagraphFormattingBase.SetKeepWithNextCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.KeepWithNext := Value;
end;

procedure TdxParagraphFormattingBase.SetKeepWithNext(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.KeepWithNext = Value) and Options.UseKeepWithNext then
    Exit;
  SetKeepWithNextCore(Info, Value);
  Options.UseKeepWithNext := True;
end;

procedure TdxParagraphFormattingBase.SetLeftIndentCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
begin
  AInfo.LeftIndent := Value;
end;

procedure TdxParagraphFormattingBase.SetLeftIndent(const Value: Integer);
begin
  if not SupportsFormatting or (Info.LeftIndent = Value) and Options.UseLeftIndent then
    Exit;
  SetLeftIndentCore(Info, Value);
  Options.UseLeftIndent := True;
end;

procedure TdxParagraphFormattingBase.SetLineSpacingCore(const AInfo: TdxParagraphFormattingInfo; const Value: Single);
begin
  AInfo.LineSpacing := Value;
end;

procedure TdxParagraphFormattingBase.SetLineSpacing(const Value: Single);
begin
  if not SupportsFormatting or (Info.LineSpacing = Value) and Options.UseLineSpacing then
    Exit;
  SetLineSpacingCore(Info, Value);
  Options.UseLineSpacing := True;
end;

procedure TdxParagraphFormattingBase.SetLineSpacingTypeCore(const AInfo: TdxParagraphFormattingInfo; const Value: TdxParagraphLineSpacing);
begin
  AInfo.LineSpacingType := Value;
  AInfo.LineSpacing := 0;
end;

procedure TdxParagraphFormattingBase.SetLineSpacingType(const Value: TdxParagraphLineSpacing);
begin
  if not SupportsFormatting or (Info.LineSpacingType = Value) and Options.UseLineSpacing then
    Exit;
  SetLineSpacingTypeCore(Info, Value);
  Options.UseLineSpacing := True;
end;

procedure TdxParagraphFormattingBase.SetOutlineLevelCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
begin
  AInfo.OutlineLevel := Value;
end;

procedure TdxParagraphFormattingBase.SetOutlineLevel(const Value: Integer);
begin
  if not SupportsFormatting or (Info.OutlineLevel = Value) and Options.UseOutlineLevel then
    Exit;
  SetOutlineLevelCore(Info, Value);
  Options.UseOutlineLevel := True;
end;

procedure TdxParagraphFormattingBase.SetPageBreakBeforeCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.PageBreakBefore := Value;
end;

procedure TdxParagraphFormattingBase.SetPageBreakBefore(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.PageBreakBefore = Value) and Options.UsePageBreakBefore then
    Exit;
  SetPageBreakBeforeCore(Info, Value);
  Options.UsePageBreakBefore := True;
end;

procedure TdxParagraphFormattingBase.SetRightIndentCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
begin
  AInfo.RightIndent := Value;
end;

procedure TdxParagraphFormattingBase.SetRightIndent(const Value: Integer);
begin
  if not SupportsFormatting or (Info.RightIndent = Value) and Options.UseRightIndent then
    Exit;
  SetRightIndentCore(Info, Value);
  Options.UseRightIndent := True;
end;

procedure TdxParagraphFormattingBase.SetSpacingAfterCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
begin
  AInfo.SpacingAfter := Value;
end;

procedure TdxParagraphFormattingBase.SetSpacingAfter(const Value: Integer);
begin
  if not SupportsFormatting or (Info.SpacingAfter = Value) and Options.UseSpacingAfter then
    Exit;
  SetSpacingAfterCore(Info, Value);
  Options.UseSpacingAfter := True;
end;

procedure TdxParagraphFormattingBase.SetSpacingBeforeCore(const AInfo: TdxParagraphFormattingInfo; const Value: Integer);
begin
  AInfo.SpacingBefore := Value;
end;

procedure TdxParagraphFormattingBase.SetSpacingBefore(const Value: Integer);
begin
  if not SupportsFormatting or (Info.SpacingBefore = Value) and Options.UseSpacingBefore then
    Exit;
  SetSpacingBeforeCore(Info, Value);
  Options.UseSpacingBefore := True;
end;

procedure TdxParagraphFormattingBase.SetSuppressHyphenationCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.SuppressHyphenation := Value;
end;

procedure TdxParagraphFormattingBase.SetSuppressHyphenation(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.SuppressHyphenation = Value) and Options.UseSuppressHyphenation then
    Exit;
  SetSuppressHyphenationCore(Info, Value);
  Options.UseSuppressHyphenation := True;
end;

procedure TdxParagraphFormattingBase.SetSuppressLineNumbersCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.SuppressLineNumbers := Value;
end;

procedure TdxParagraphFormattingBase.SetSuppressLineNumbers(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.SuppressLineNumbers = Value) and Options.UseSuppressLineNumbers then
    Exit;
  SetSuppressLineNumbersCore(Info, Value);
  Options.UseSuppressLineNumbers := True;
end;

procedure TdxParagraphFormattingBase.SetWidowOrphanControlCore(const AInfo: TdxParagraphFormattingInfo; const Value: Boolean);
begin
  AInfo.WidowOrphanControl := Value;
end;

procedure TdxParagraphFormattingBase.SetWidowOrphanControl(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.WidowOrphanControl = Value) and Options.UseWidowOrphanControl then
    Exit;
  SetWidowOrphanControlCore(Info, Value);
  Options.UseWidowOrphanControl := True;
end;

function TdxParagraphFormattingBase.GetLeftBorder: TdxBorderInfo;
begin
  Result := Info.LeftBorder;
end;

procedure TdxParagraphFormattingBase.SetLeftBorderCore(const AInfo: TdxParagraphFormattingInfo; const AValue: TdxBorderInfo);
begin
  AInfo.LeftBorder := AValue;
end;

procedure TdxParagraphFormattingBase.SetLeftBorder(const AValue: TdxBorderInfo);
begin
  if not SupportsFormatting or (Options.UseLeftBorder and TdxBorderInfo.AreBordersEqual(Info.LeftBorder, AValue)) then
    Exit;
  SetLeftBorderCore(Info, AValue);
  Options.UseLeftBorder := True;
end;

function TdxParagraphFormattingBase.GetRightBorder: TdxBorderInfo;
begin
  Result := Info.RightBorder;
end;

procedure TdxParagraphFormattingBase.SetRightBorderCore(const AInfo: TdxParagraphFormattingInfo; const AValue: TdxBorderInfo);
begin
  AInfo.RightBorder := AValue;
end;

procedure TdxParagraphFormattingBase.SetRightBorder(const AValue: TdxBorderInfo);
begin
  if not SupportsFormatting or (Options.UseRightBorder and TdxBorderInfo.AreBordersEqual(Info.RightBorder, AValue)) then
    Exit;
  SetRightBorderCore(Info, AValue);
  Options.UseRightBorder := True;
end;

function TdxParagraphFormattingBase.GetTopBorder: TdxBorderInfo;
begin
  Result := Info.TopBorder;
end;

procedure TdxParagraphFormattingBase.SetTopBorderCore(const AInfo: TdxParagraphFormattingInfo; const AValue: TdxBorderInfo);
begin
  AInfo.TopBorder := AValue;
end;

procedure TdxParagraphFormattingBase.SetTopBorder(const AValue: TdxBorderInfo);
begin
  if not SupportsFormatting or (Options.UseTopBorder and TdxBorderInfo.AreBordersEqual(Info.TopBorder, AValue)) then
    Exit;
  SetTopBorderCore(Info, AValue);
  Options.UseTopBorder := True;
end;

function TdxParagraphFormattingBase.GetBottomBorder: TdxBorderInfo;
begin
  Result := Info.BottomBorder;
end;

procedure TdxParagraphFormattingBase.SetBottomBorderCore(const AInfo: TdxParagraphFormattingInfo; const AValue: TdxBorderInfo);
begin
  AInfo.BottomBorder := AValue;
end;

procedure TdxParagraphFormattingBase.SetBottomBorder(const AValue: TdxBorderInfo);
begin
  if not SupportsFormatting or (Options.UseBottomBorder and TdxBorderInfo.AreBordersEqual(Info.BottomBorder, AValue)) then
    Exit;
  SetBottomBorderCore(Info, AValue);
  Options.UseBottomBorder := True;
end;

{ TdxParagraphProperties }

constructor TdxParagraphProperties.Create(const AOwner: IdxParagraphPropertiesContainer);
begin
  Assert(AOwner <> nil);
  inherited Create(AOwner.PieceTable);
  FOwner := AOwner;
end;

destructor TdxParagraphProperties.Destroy;
begin
  inherited Destroy;
end;

function TdxParagraphProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.BatchUpdate);
end;

class procedure TdxParagraphProperties.ApplyPropertiesDiff(ATarget: TdxParagraphProperties; ATargetMergedInfo,
  ASourceMergedInfo: TdxParagraphFormattingInfo);
begin
  if ATargetMergedInfo.Alignment <> ASourceMergedInfo.Alignment then
    ATarget.Alignment := ASourceMergedInfo.Alignment;

  if ATargetMergedInfo.FirstLineIndent <> ASourceMergedInfo.FirstLineIndent then
    ATarget.FirstLineIndent := ASourceMergedInfo.FirstLineIndent;

  if ATargetMergedInfo.FirstLineIndentType <> ASourceMergedInfo.FirstLineIndentType then
    ATarget.FirstLineIndentType := ASourceMergedInfo.FirstLineIndentType;

  if ATargetMergedInfo.LeftIndent <> ASourceMergedInfo.LeftIndent then
    ATarget.LeftIndent := ASourceMergedInfo.LeftIndent;

  if ATargetMergedInfo.LineSpacingType <> ASourceMergedInfo.LineSpacingType then
    ATarget.LineSpacingType := ASourceMergedInfo.LineSpacingType;

  if ATargetMergedInfo.LineSpacing <> ASourceMergedInfo.LineSpacing then
    ATarget.LineSpacing := ASourceMergedInfo.LineSpacing;

  if ATargetMergedInfo.RightIndent <> ASourceMergedInfo.RightIndent then
    ATarget.RightIndent := ASourceMergedInfo.RightIndent;

  if ATargetMergedInfo.SpacingAfter <> ASourceMergedInfo.SpacingAfter then
    ATarget.SpacingAfter := ASourceMergedInfo.SpacingAfter;

  if ATargetMergedInfo.SpacingBefore <> ASourceMergedInfo.SpacingBefore then
    ATarget.SpacingBefore := ASourceMergedInfo.SpacingBefore;

  if ATargetMergedInfo.SuppressHyphenation <> ASourceMergedInfo.SuppressHyphenation then
    ATarget.SuppressHyphenation := ASourceMergedInfo.SuppressHyphenation;

  if ATargetMergedInfo.SuppressLineNumbers <> ASourceMergedInfo.SuppressLineNumbers then
    ATarget.SuppressLineNumbers := ASourceMergedInfo.SuppressLineNumbers;

  if ATargetMergedInfo.ContextualSpacing <> ASourceMergedInfo.ContextualSpacing then
    ATarget.ContextualSpacing := ASourceMergedInfo.ContextualSpacing;

  if ATargetMergedInfo.PageBreakBefore <> ASourceMergedInfo.PageBreakBefore then
    ATarget.PageBreakBefore := ASourceMergedInfo.PageBreakBefore;

  if ATargetMergedInfo.OutlineLevel <> ASourceMergedInfo.OutlineLevel then
    ATarget.OutlineLevel := ASourceMergedInfo.OutlineLevel;

  if ATargetMergedInfo.BackColor <> ASourceMergedInfo.BackColor then
    ATarget.BackColor := ASourceMergedInfo.BackColor;
end;

procedure TdxParagraphProperties.CopyFrom(const AParagraphProperties: TdxMergedProperties<TdxParagraphFormattingInfo, TdxParagraphFormattingOptions>);
var
  AInfo: TdxParagraphFormattingBase;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AInfo.CopyFromCore(AParagraphProperties.Info, AParagraphProperties.Options);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

{$IFNDEF DELPHI17}
procedure TdxParagraphProperties.CopyFrom(const Source: TdxParagraphFormattingBase);
begin
  inherited CopyFrom(Source);
end;

procedure TdxParagraphProperties.CopyFrom(
  const Source: TdxUndoableIndexBasedObject<TdxParagraphFormattingBase>);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

function TdxParagraphProperties.Equals(Obj: TObject): Boolean;
var
  AOther: TdxParagraphProperties absolute Obj;
begin
  Result := Obj is TdxParagraphProperties;
  if Result then
  begin
    if DocumentModel = AOther.DocumentModel then
      Result := Index = AOther.Index
    else
      Result := Info.Equals(AOther.Info);
  end;
end;

procedure TdxParagraphProperties.Reset;
var
  AInfo: TdxParagraphFormattingBase;
  AEmptyInfo: TdxParagraphFormattingBase;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AEmptyInfo := GetCache(DocumentModel)[TdxParagraphFormattingCache.EmptyParagraphFormattingIndex];
  AInfo.ReplaceInfo(AEmptyInfo.Info, AEmptyInfo.Options);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

procedure TdxParagraphProperties.ResetUse(AMask: TdxUsedParagraphFormattingOptions);
var
  AInfo: TdxParagraphFormattingBase;
  AOptions: TdxParagraphFormattingOptions;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AOptions := AInfo.Options;
  AOptions.Value := AOptions.Value - AMask;
  AInfo.Options := AOptions;
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

procedure TdxParagraphProperties.ResetAllUse;
var
  AInfo: TdxParagraphFormattingBase;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AInfo.Options := TdxParagraphFormattingOptions.Create([]);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

procedure TdxParagraphProperties.Merge(AProperties: TdxParagraphProperties);
var
  AMergedProperties: TdxMergedParagraphProperties;
begin
  AMergedProperties := TdxMergedParagraphProperties.Create(Self);
  try
    AMergedProperties.Merge(AProperties);
    CopyFrom(AMergedProperties);
  finally
    AMergedProperties.Free;
  end;
end;

function TdxParagraphProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxParagraphFormattingBase>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).ParagraphFormattingCache;
end;

procedure TdxParagraphProperties.OnIndexChanged;
begin
  inherited OnIndexChanged;
  FOwner.OnParagraphPropertiesChanged
end;

function TdxParagraphProperties.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := FOwner.CreateParagraphPropertiesChangedHistoryItem;
end;

function TdxParagraphProperties.GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener;
begin
  if not Supports(FOwner, IdxObtainAffectedRangeListener, Result) then
    Result := nil;
end;

function TdxParagraphProperties.GetAfterAutoSpacing: Boolean;
begin
  Result := Info.AfterAutoSpacing;
end;

function TdxParagraphProperties.GetAlignment: TdxParagraphAlignment;
begin
  Result := Info.Alignment;
end;

function TdxParagraphProperties.GetBackColor: TdxAlphaColor;
begin
  Result := Info.BackColor;
end;

function TdxParagraphProperties.GetBeforeAutoSpacing: Boolean;
begin
  Result := Info.BeforeAutoSpacing;
end;

function TdxParagraphProperties.GetContextualSpacing: Boolean;
begin
  Result := Info.ContextualSpacing;
end;

function TdxParagraphProperties.GetFirstLineIndent: Integer;
begin
  Result := Info.FirstLineIndent;
end;

function TdxParagraphProperties.GetFirstLineIndentType: TdxParagraphFirstLineIndent;
begin
  Result := Info.FirstLineIndentType;
end;

function TdxParagraphProperties.GetHashCode: Integer;
begin
  Result := Index;
end;

function TdxParagraphProperties.GetKeepLinesTogether: Boolean;
begin
  Result := Info.KeepLinesTogether;
end;

function TdxParagraphProperties.GetKeepWithNext: Boolean;
begin
  Result := Info.KeepWithNext;
end;

function TdxParagraphProperties.GetLeftIndent: Integer;
begin
  Result := Info.LeftIndent;
end;

function TdxParagraphProperties.GetLineSpacing: Single;
begin
  Result := Info.LineSpacing;
end;

function TdxParagraphProperties.GetLineSpacingType: TdxParagraphLineSpacing;
begin
  Result := Info.LineSpacingType;
end;

function TdxParagraphProperties.GetOutlineLevel: Integer;
begin
  Result := Info.OutlineLevel;
end;

function TdxParagraphProperties.GetPageBreakBefore: Boolean;
begin
  Result := Info.PageBreakBefore;
end;

function TdxParagraphProperties.GetRightIndent: Integer;
begin
  Result := Info.RightIndent;
end;

function TdxParagraphProperties.GetSpacingAfter: Integer;
begin
  Result := Info.SpacingAfter;
end;

function TdxParagraphProperties.GetSpacingBefore: Integer;
begin
  Result := Info.SpacingBefore;
end;

function TdxParagraphProperties.GetSuppressHyphenation: Boolean;
begin
  Result := Info.SuppressHyphenation;
end;

function TdxParagraphProperties.GetSuppressLineNumbers: Boolean;
begin
  Result := Info.SuppressLineNumbers;
end;

function TdxParagraphProperties.GetWidowOrphanControl: Boolean;
begin
  Result := Info.WidowOrphanControl;
end;

function TdxParagraphProperties.GetUseAlignment: Boolean;
begin
  Result := Info.Options.UseAlignment;
end;

function TdxParagraphProperties.GetUseLeftIndent: Boolean;
begin
  Result := Info.Options.UseLeftIndent;
end;

function TdxParagraphProperties.GetUseRightIndent: Boolean;
begin
  Result := Info.Options.UseRightIndent;
end;

function TdxParagraphProperties.GetUseSpacingBefore: Boolean;
begin
  Result := Info.Options.UseSpacingBefore;
end;

function TdxParagraphProperties.GetUseSpacingAfter: Boolean;
begin
  Result := Info.Options.UseSpacingAfter;
end;

function TdxParagraphProperties.GetUseLineSpacing: Boolean;
begin
  Result := Info.Options.UseLineSpacing;
end;

function TdxParagraphProperties.GetUseLineSpacingType: Boolean;
begin
  Result := Info.Options.UseLineSpacing;
end;

function TdxParagraphProperties.GetUseFirstLineIndent: Boolean;
begin
  Result := Info.Options.UseFirstLineIndent;
end;

function TdxParagraphProperties.GetUseFirstLineIndentType: Boolean;
begin
  Result := Info.Options.UseFirstLineIndent;
end;

function TdxParagraphProperties.GetUseSuppressHyphenation: Boolean;
begin
  Result := Info.Options.UseSuppressHyphenation;
end;

function TdxParagraphProperties.GetUseSuppressLineNumbers: Boolean;
begin
  Result := Info.Options.UseSuppressLineNumbers;
end;

function TdxParagraphProperties.GetUseContextualSpacing: Boolean;
begin
  Result := Info.Options.UseContextualSpacing;
end;

function TdxParagraphProperties.GetUsePageBreakBefore: Boolean;
begin
  Result := Info.Options.UsePageBreakBefore;
end;

function TdxParagraphProperties.GetUseBeforeAutoSpacing: Boolean;
begin
  Result := Info.Options.UseBeforeAutoSpacing;
end;

function TdxParagraphProperties.GetUseAfterAutoSpacing: Boolean;
begin
  Result := Info.Options.UseAfterAutoSpacing;
end;

function TdxParagraphProperties.GetUseKeepWithNext: Boolean;
begin
  Result := Info.Options.UseKeepWithNext;
end;

function TdxParagraphProperties.GetUseKeepLinesTogether: Boolean;
begin
  Result := Info.Options.UseKeepLinesTogether;
end;

function TdxParagraphProperties.GetUseWidowOrphanControl: Boolean;
begin
  Result := Info.Options.UseWidowOrphanControl;
end;

function TdxParagraphProperties.GetUseOutlineLevel: Boolean;
begin
  Result := Info.Options.UseOutlineLevel;
end;

function TdxParagraphProperties.GetUseBackColor: Boolean;
begin
  Result := Info.Options.UseBackColor;
end;

function TdxParagraphProperties.SetAfterAutoSpacingCore(const AInfo: TdxParagraphFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.AfterAutoSpacing := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.AfterAutoSpacing);
end;

procedure TdxParagraphProperties.SetAfterAutoSpacing(const Value: Boolean);
begin
  if (Info.AfterAutoSpacing = Value) and UseAfterAutoSpacing then
    Exit;
  SetPropertyValue<Boolean>(SetAfterAutoSpacingCore, Value);
end;

procedure TdxParagraphProperties.SetAlignment(const Value: TdxParagraphAlignment);
begin
  if (Alignment = Value) and UseAlignment then
    Exit;
  SetPropertyValue<Integer>(SetAlignmentCore, Ord(Value));
end;

function TdxParagraphProperties.SetAlignmentCore(const AInfo: TdxParagraphFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Alignment := TdxParagraphAlignment(Value);
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.Alignment);
end;

procedure TdxParagraphProperties.SetBackColor(const Value: TdxAlphaColor);
begin
  if (BackColor = Value) and UseBackColor then
    Exit;
  SetPropertyValue<Integer>(SetBackColorCore, Value);
end;

function TdxParagraphProperties.SetBackColorCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.BackColor := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.BackColor);
end;

procedure TdxParagraphProperties.SetBeforeAutoSpacing(const Value: Boolean);
begin
  if (BeforeAutoSpacing = Value) and UseBeforeAutoSpacing then
    Exit;
  SetPropertyValue<Boolean>(SetBeforeAutoSpacingCore, Value);
end;

function TdxParagraphProperties.SetBeforeAutoSpacingCore(
  const AInfo: TdxParagraphFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.BeforeAutoSpacing := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.BeforeAutoSpacing);
end;

procedure TdxParagraphProperties.SetContextualSpacing(const Value: Boolean);
begin
  if (ContextualSpacing = Value) and UseContextualSpacing then
    Exit;
  SetPropertyValue<Boolean>(SetContextualSpacingCore, Value);
end;

function TdxParagraphProperties.SetContextualSpacingCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.ContextualSpacing := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.ContextualSpacing);
end;

procedure TdxParagraphProperties.SetFirstLineIndent(const Value: Integer);
begin
  if (FirstLineIndent = Value) and UseFirstLineIndent then
    Exit;
  SetPropertyValue<Integer>(SetFirstLineIndentCore, Value);
end;

function TdxParagraphProperties.SetFirstLineIndentCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.FirstLineIndent := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.FirstLineIndent);
end;

procedure TdxParagraphProperties.SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
begin
  if (FirstLineIndentType = Value) and UseFirstLineIndent then
    Exit;
  SetPropertyValue<Integer>(SetFirstLineIndentTypeCore, Ord(Value));
end;

function TdxParagraphProperties.SetFirstLineIndentTypeCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent(Value);
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.FirstLineIndentType);
end;

procedure TdxParagraphProperties.SetKeepLinesTogether(const Value: Boolean);
begin
  if (KeepLinesTogether = Value) and UseKeepLinesTogether then
    Exit;
  SetPropertyValue<Boolean>(SetKeepLinesTogetherCore, Value);
end;

function TdxParagraphProperties.SetKeepLinesTogetherCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.KeepLinesTogether := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.Alignment);
end;

procedure TdxParagraphProperties.SetKeepWithNext(const Value: Boolean);
begin
  if (KeepWithNext = Value) and UseKeepWithNext then
    Exit;
  SetPropertyValue<Boolean>(SetKeepWithNextCore, Value);
end;

function TdxParagraphProperties.SetKeepWithNextCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.KeepWithNext := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.KeepWithNext);
end;

procedure TdxParagraphProperties.SetLeftIndent(const Value: Integer);
begin
  if (LeftIndent = Value) and UseLeftIndent then
    Exit;
  SetPropertyValue<Integer>(SetLeftIndentCore, Value);
end;

function TdxParagraphProperties.SetLeftIndentCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.LeftIndent := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.LeftIndent);
end;

procedure TdxParagraphProperties.SetLineSpacing(const Value: Single);
begin
  if (LineSpacing = Value) and UseLineSpacing then
    Exit;
  SetPropertyValue<Single>(SetLineSpacingCore, Value);
end;

function TdxParagraphProperties.SetLineSpacingCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Single): TdxDocumentModelChangeActions;
begin
  AInfo.LineSpacing := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.LineSpacing);
end;

procedure TdxParagraphProperties.SetLineSpacingType(const Value: TdxParagraphLineSpacing);
begin
  if (LineSpacingType = Value) and UseLineSpacing then
    Exit;
  SetPropertyValue<Integer>(SetLineSpacingTypeCore, Ord(Value));
end;

function TdxParagraphProperties.SetLineSpacingTypeCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.LineSpacingType := TdxParagraphLineSpacing(Value);
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.LineSpacingType);
end;

procedure TdxParagraphProperties.SetOutlineLevel(const Value: Integer);
begin
  if (OutlineLevel = Value) and UseOutlineLevel then
    Exit;
  SetPropertyValue<Integer>(SetOutlineLevelCore, Value);
end;

function TdxParagraphProperties.SetOutlineLevelCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.OutlineLevel := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.OutlineLevel);
end;

procedure TdxParagraphProperties.SetPageBreakBefore(const Value: Boolean);
begin
  if (PageBreakBefore = Value) and UsePageBreakBefore then
    Exit;
  SetPropertyValue<Boolean>(SetPageBreakBeforeCore, Value);
end;

function TdxParagraphProperties.SetPageBreakBeforeCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.PageBreakBefore := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.PageBreakBefore);
end;

procedure TdxParagraphProperties.SetRightIndent(const Value: Integer);
begin
  if (RightIndent = Value) and UseRightIndent then
    Exit;
  SetPropertyValue<Integer>(SetRightIndentCore, Value);
end;

function TdxParagraphProperties.SetRightIndentCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.RightIndent := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.RightIndent);
end;

procedure TdxParagraphProperties.SetSpacingAfter(const Value: Integer);
begin
  if (SpacingAfter = Value) and UseSpacingAfter then
    Exit;
  SetPropertyValue<Integer>(SetSpacingAfterCore, Value);
end;

function TdxParagraphProperties.SetSpacingAfterCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.SpacingAfter := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.SpacingAfter);
end;

procedure TdxParagraphProperties.SetSpacingBefore(const Value: Integer);
begin
  if (SpacingBefore = Value) and UseSpacingBefore then
    Exit;
  SetPropertyValue<Integer>(SetSpacingBeforeCore, Value);
end;

function TdxParagraphProperties.SetSpacingBeforeCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.SpacingBefore := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.SpacingBefore);
end;

procedure TdxParagraphProperties.SetSuppressHyphenation(const Value: Boolean);
begin
  if (SuppressHyphenation = Value) and UseSuppressHyphenation then
    Exit;
  SetPropertyValue<Boolean>(SetSuppressHyphenationCore, Value);
end;

function TdxParagraphProperties.SetSuppressHyphenationCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.SuppressHyphenation := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.SuppressHyphenation);
end;

procedure TdxParagraphProperties.SetSuppressLineNumbers(const Value: Boolean);
begin
  if (SuppressLineNumbers = Value) and UseSuppressLineNumbers then
    Exit;
  SetPropertyValue<Boolean>(SetSuppressLineNumbersCore, Value);
end;

function TdxParagraphProperties.SetSuppressLineNumbersCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.SuppressLineNumbers := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.SuppressLineNumbers);
end;

procedure TdxParagraphProperties.SetWidowOrphanControl(const Value: Boolean);
begin
  if (WidowOrphanControl = Value) and UseWidowOrphanControl then
    Exit;
  SetPropertyValue<Boolean>(SetWidowOrphanControlCore, Value);
end;

function TdxParagraphProperties.SetWidowOrphanControlCore(const AInfo: TdxParagraphFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.WidowOrphanControl := Value;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.WidowOrphanControl);
end;

function TdxParagraphProperties.UseVal(AMask: TdxUsedParagraphFormattingOption): Boolean;
begin
  Result := AMask in Info.Options.Value;
end;

function TdxParagraphProperties.GetLeftBorder: TdxBorderInfo;
begin
  Result := Info.LeftBorder;
end;

function TdxParagraphProperties.GetUseLeftBorder: Boolean;
begin
  Result := Info.Options.UseLeftBorder;
end;

function TdxParagraphProperties.SetLeftBorderCore(const AInfo: TdxParagraphFormattingBase; const AValue: TdxBorderInfo): TdxDocumentModelChangeActions;
begin
  AInfo.LeftBorder := AValue;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.LeftBorder);
end;

procedure TdxParagraphProperties.SetLeftBorder(const AValue: TdxBorderInfo);
begin
  if UseLeftBorder and TdxBorderInfo.AreBordersEqual(Info.LeftBorder, AValue) then
    Exit;
  SetPropertyValue<TdxBorderInfo>(SetLeftBorderCore, AValue);
end;

function TdxParagraphProperties.GetRightBorder: TdxBorderInfo;
begin
  Result := Info.RightBorder;
end;

function TdxParagraphProperties.GetUseRightBorder: Boolean;
begin
  Result := Info.Options.UseRightBorder;
end;

function TdxParagraphProperties.SetRightBorderCore(const AInfo: TdxParagraphFormattingBase; const AValue: TdxBorderInfo): TdxDocumentModelChangeActions;
begin
  AInfo.RightBorder := AValue;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.RightBorder);
end;

procedure TdxParagraphProperties.SetRightBorder(const AValue: TdxBorderInfo);
begin
  if UseRightBorder and TdxBorderInfo.AreBordersEqual(Info.RightBorder, AValue) then
    Exit;
  SetPropertyValue<TdxBorderInfo>(SetRightBorderCore, AValue);
end;

function TdxParagraphProperties.GetTopBorder: TdxBorderInfo;
begin
  Result := Info.TopBorder;
end;

function TdxParagraphProperties.GetUseTopBorder: Boolean;
begin
  Result := Info.Options.UseTopBorder;
end;

function TdxParagraphProperties.SetTopBorderCore(const AInfo: TdxParagraphFormattingBase; const AValue: TdxBorderInfo): TdxDocumentModelChangeActions;
begin
  AInfo.TopBorder := AValue;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.TopBorder);
end;

procedure TdxParagraphProperties.SetTopBorder(const AValue: TdxBorderInfo);
begin
  if UseTopBorder and TdxBorderInfo.AreBordersEqual(Info.TopBorder, AValue) then
    Exit;
  SetPropertyValue<TdxBorderInfo>(SetTopBorderCore, AValue);
end;

function TdxParagraphProperties.GetBottomBorder: TdxBorderInfo;
begin
  Result := Info.BottomBorder;
end;

function TdxParagraphProperties.GetUseBottomBorder: Boolean;
begin
  Result := Info.Options.UseBottomBorder;
end;

function TdxParagraphProperties.SetBottomBorderCore(const AInfo: TdxParagraphFormattingBase; const AValue: TdxBorderInfo): TdxDocumentModelChangeActions;
begin
  AInfo.BottomBorder := AValue;
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.BottomBorder);
end;

procedure TdxParagraphProperties.SetBottomBorder(const AValue: TdxBorderInfo);
begin
  if UseBottomBorder and TdxBorderInfo.AreBordersEqual(Info.BottomBorder, AValue) then
    Exit;
  SetPropertyValue<TdxBorderInfo>(SetBottomBorderCore, AValue);
end;

{ TdxParagraphFormattingInfoCache }

function TdxParagraphFormattingInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo;
begin
  Result := CreateDefaultItemMSO2007(AUnitConverter);
end;

class function TdxParagraphFormattingInfoCache.CreateDefaultItemMSO2003(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo;
begin
  Result := TdxParagraphFormattingInfo.Create;
  Result.WidowOrphanControl := True;
end;

class function TdxParagraphFormattingInfoCache.CreateDefaultItemMSO2007(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo;
begin
  Result := TdxParagraphFormattingInfo.Create;
  Result.SpacingAfter := AUnitConverter.PointsToModelUnits(10);
  Result.LineSpacingType := TdxParagraphLineSpacing.Multiple;
  Result.LineSpacing := 1.15;
  Result.WidowOrphanControl := True;
end;

{ TdxParagraphFormattingCache }

constructor TdxParagraphFormattingCache.Create(ADocumentModel: TdxCustomDocumentModel);
var
  ADefaultParagraphFormattingInfo: TdxParagraphFormattingInfo;
begin
  inherited Create(ADocumentModel.UnitConverter, ADocumentModel);
  ADefaultParagraphFormattingInfo := TdxSimpleDocumentCache(ADocumentModel.Cache).ParagraphFormattingInfoCache.DefaultItem;
  AppendItem(TdxDefaultParagraphFormattingBase.Create(DocumentModel.MainPart,
    DocumentModel, ADefaultParagraphFormattingInfo,
    TdxParagraphFormattingOptions.EmptyParagraphFormattingOption));
  AppendItem(TdxDefaultParagraphFormattingBase.Create(DocumentModel.MainPart,
    DocumentModel, ADefaultParagraphFormattingInfo,
    TdxParagraphFormattingOptions.RootParagraphFormattingOption));
end;

function TdxParagraphFormattingCache.GetDefaultInfo: TdxParagraphFormattingInfo;
begin
  Result := DefaultItem.Info;
end;

function TdxParagraphFormattingCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingBase;
begin
  Result := nil;
end;

function TdxParagraphFormattingCache.CreateDefaultParagraphFormattingInfo(ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingInfo;
begin
  Result := TdxParagraphFormattingInfoCache.CreateDefaultItemMSO2007(ADocumentModel.UnitConverter);
end;

{ TdxCharacterFormattingChangeActionsCalculator }

class function TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(
  const AChange: TdxParagraphFormattingChangeType): TdxDocumentModelChangeActions;
const
  ParagraphFormattingChangeActionsMap: array[TdxParagraphFormattingChangeType] of TdxDocumentModelChangeActions =
    (
    [],
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
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
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
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
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
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler]
    );
begin
  Result := ParagraphFormattingChangeActionsMap[AChange];
end;

{ TdxParagraphParagraphPropertiesChangedHistoryItem }

constructor TdxParagraphParagraphPropertiesChangedHistoryItem.Create(
  const APieceTable: TdxCustomPieceTable; AParagraphIndex: Integer);
begin
  Assert(AParagraphIndex >= 0);
  inherited Create(APieceTable);
  FParagraphIndex := AParagraphIndex;
end;

function TdxParagraphParagraphPropertiesChangedHistoryItem.GetObject: TdxIndexBasedObject;
begin
  Result := TdxSimpleParagraph(PieceTable.Paragraphs[ParagraphIndex]).ParagraphProperties;
end;

{ TdxMergedParagraphProperties }

constructor TdxMergedParagraphProperties.Create(const AProperties: TdxParagraphProperties);
var
  AInfo: TdxParagraphFormattingBase;
begin
  AInfo := AProperties.Info;
  inherited Create(AInfo.Info, AInfo.Options);
end;

constructor TdxMergedParagraphProperties.Create(const AProperties: TdxMergedParagraphProperties);
begin
  inherited Create(AProperties.Info, AProperties.Options);
end;

procedure TdxMergedParagraphProperties.Merge(const AProperties: TdxParagraphProperties);
var
  AInfo: TdxParagraphFormattingBase;
begin
  AInfo := AProperties.Info;
  MergeCore(AInfo.Info, AInfo.Options);
end;

procedure TdxMergedParagraphProperties.Merge(const AProperties: TdxMergedParagraphProperties);
begin
  MergeCore(AProperties.Info, AProperties.Options);
end;

procedure TdxMergedParagraphProperties.MergeCore(const AInfo: TdxParagraphFormattingInfo;
  const AOptions: TdxParagraphFormattingOptions);
begin
  if not Options.UseAlignment and AOptions.UseAlignment then
  begin
    Info.Alignment := AInfo.Alignment;
    Options.UseAlignment := True;
  end;
  if not Options.UseFirstLineIndent and AOptions.UseFirstLineIndent then
  begin
    Info.FirstLineIndentType := AInfo.FirstLineIndentType;
    Info.FirstLineIndent := AInfo.FirstLineIndent;
    Options.UseFirstLineIndent := True;
  end;
  if not Options.UseLeftIndent and AOptions.UseLeftIndent then
  begin
    Info.LeftIndent := AInfo.LeftIndent;
    Options.UseLeftIndent := True;
  end;
  if not Options.UseLineSpacing and AOptions.UseLineSpacing then
  begin
    Info.LineSpacing := AInfo.LineSpacing;
    Info.LineSpacingType := AInfo.LineSpacingType;
    Options.UseLineSpacing := True;
  end;
  if not Options.UseRightIndent and AOptions.UseRightIndent then
  begin
    Info.RightIndent := AInfo.RightIndent;
    Options.UseRightIndent := True;
  end;
  if not Options.UseSpacingAfter and AOptions.UseSpacingAfter then
  begin
    Info.SpacingAfter := AInfo.SpacingAfter;
    Options.UseSpacingAfter := True;
  end;
  if not Options.UseSpacingBefore and AOptions.UseSpacingBefore then
  begin
    Info.SpacingBefore := AInfo.SpacingBefore;
    Options.UseSpacingBefore := True;
  end;
  if not Options.UseSuppressHyphenation and AOptions.UseSuppressHyphenation then
  begin
    Info.SuppressHyphenation := AInfo.SuppressHyphenation;
    Options.UseSuppressHyphenation := True;
  end;
  if not Options.UseSuppressLineNumbers and AOptions.UseSuppressLineNumbers then
  begin
    Info.SuppressLineNumbers := AInfo.SuppressLineNumbers;
    Options.UseSuppressLineNumbers := True;
  end;
  if not Options.UseContextualSpacing and AOptions.UseContextualSpacing then
  begin
    Info.ContextualSpacing := AInfo.ContextualSpacing;
    Options.UseContextualSpacing := True;
  end;
  if not Options.UsePageBreakBefore and AOptions.UsePageBreakBefore then
  begin
    Info.PageBreakBefore := AInfo.PageBreakBefore;
    Options.UsePageBreakBefore := True;
  end;
  if not Options.UseBeforeAutoSpacing and AOptions.UseBeforeAutoSpacing then
  begin
    Info.BeforeAutoSpacing := AInfo.BeforeAutoSpacing;
    Options.UseBeforeAutoSpacing := True;
  end;
  if not Options.UseAfterAutoSpacing and AOptions.UseAfterAutoSpacing then
  begin
    Info.AfterAutoSpacing := AInfo.AfterAutoSpacing;
    Options.UseAfterAutoSpacing := True;
  end;
  if not Options.UseKeepWithNext and AOptions.UseKeepWithNext then
  begin
    Info.KeepWithNext := AInfo.KeepWithNext;
    Options.UseKeepWithNext := True;
  end;
  if not Options.UseKeepLinesTogether and AOptions.UseKeepLinesTogether then
  begin
    Info.KeepLinesTogether := AInfo.KeepLinesTogether;
    Options.UseKeepLinesTogether := True;
  end;
  if not Options.UseWidowOrphanControl and AOptions.UseWidowOrphanControl then
  begin
    Info.WidowOrphanControl := AInfo.WidowOrphanControl;
    Options.UseWidowOrphanControl := True;
  end;
  if not Options.UseOutlineLevel and AOptions.UseOutlineLevel then
  begin
    Info.OutlineLevel := AInfo.OutlineLevel;
    Options.UseOutlineLevel := True;
  end;
  if not Options.UseBackColor and AOptions.UseBackColor then
  begin
    Info.BackColor := AInfo.BackColor;
    Options.UseBackColor := True;
  end;
  if not Options.UseTopBorder and AOptions.UseTopBorder then
  begin
    Info.TopBorder := AInfo.TopBorder;
    Options.UseTopBorder := True;
  end;
  if not Options.UseBottomBorder and AOptions.UseBottomBorder then
  begin
    Info.BottomBorder := AInfo.BottomBorder;
    Options.UseBottomBorder := True;
  end;
  if not Options.UseLeftBorder and AOptions.UseLeftBorder then
  begin
    Info.LeftBorder := AInfo.LeftBorder;
    Options.UseLeftBorder := True;
  end;
  if not Options.UseRightBorder and AOptions.UseRightBorder then
  begin
    Info.RightBorder := AInfo.RightBorder;
    Options.UseRightBorder := True;
  end;
end;

{ TdxParagraphMergedParagraphPropertiesCachedResult }

constructor TdxParagraphMergedParagraphPropertiesCachedResult.Create;
begin
  inherited Create;
  FTableStyleParagraphPropertiesIndex := -1;
  FOwnListLevelParagraphPropertiesIndex := -1;
  FParagraphPropertiesIndex := -1;
  FParagraphStyleIndex := -1;
end;

destructor TdxParagraphMergedParagraphPropertiesCachedResult.Destroy;
begin
  FreeAndNil(FMergedParagraphProperties);
  inherited Destroy;
end;

procedure TdxParagraphMergedParagraphPropertiesCachedResult.SetMergedParagraphProperties(Value: TdxMergedParagraphProperties);
begin
  if MergedParagraphProperties <> Value then
  begin
    FMergedParagraphProperties.Free;
    FMergedParagraphProperties := Value;
  end;
end;

end.
