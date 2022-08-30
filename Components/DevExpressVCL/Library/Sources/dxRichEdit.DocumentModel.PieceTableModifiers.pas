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

unit dxRichEdit.DocumentModel.PieceTableModifiers;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.FastComparer,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.PieceTableModifiers.Core,
  dxRichEdit.DocumentModel.PieceTableModifiers.Simple,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.History.Run,
  dxRichEdit.DocumentModel.History.FloatingObject,
  dxRichEdit.DocumentModel.History.Paragraph,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.Control.HitTest;

type

  { TdxTextInserter }

  TdxTextInserter = class(TdxSimpleTextInserter)
  public
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
  end;

  { TdxFieldSymbolResultInserter }

  TdxFieldSymbolResultInserter = class(TdxTextInserter)
  protected
    function CreateRunInsertedHistoryItem: TdxTextRunInsertedHistoryItem; override;
  public
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
  end;

  { TdxPieceTableObjectInserter }

  TdxPieceTableObjectInserter = class abstract(TdxObjectInserter)
  strict private
    function GetPieceTable: TdxPieceTable;
  public
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxLayoutDependentTextInserter }

  TdxLayoutDependentTextInserter = class(TdxPieceTableObjectInserter)
  strict private
    FFieldResultFormatting: TdxFieldResultFormatting;
    procedure InsertLayoutDependentTextRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex; AStartIndex, ALength: Integer;
      AForceVisible: Boolean);
    procedure SetFieldResultFormatting(const Value: TdxFieldResultFormatting);
  protected
    procedure AssignHistoryItemProperties(AItem: TdxLayoutDependentTextRunInsertedHistoryItem; AParagraph: TdxParagraphBase;
      AWhere: TdxRunIndex; AStartIndex, ALength: Integer; AForceVisible: Boolean); virtual;
    function CreateRunInsertedHistoryItem: TdxLayoutDependentTextRunInsertedHistoryItem; virtual;
  public
    destructor Destroy; override;
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); override;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean); override;

    property FieldResultFormatting: TdxFieldResultFormatting read FFieldResultFormatting write SetFieldResultFormatting;
  end;

  { TdxSectionInserter }

  TdxSectionInserter = class(TdxParagraphInserter)
  public
    function CreateInsertParagraphRangeHistoryItem: TdxParagraphRunInsertedHistoryItem; override;
  end;

  { TdxFloatingObjectAnchorInserter }

  TdxFloatingObjectAnchorInserter = class(TdxPieceTableObjectInserter)
  strict private
    procedure InsertFloatingObjectAnchorRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex; AStartIndex: Integer;
      AForceVisible: Boolean);
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; override;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); override;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex;
      ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean); override;
  end;

  { TdxFootNoteRunInserterBase }

  TdxFootNoteRunInserterBase = class abstract(TdxLayoutDependentTextInserter)
  strict private
    FNoteIndex: Integer;
  protected
    procedure AssignHistoryItemProperties(AItem: TdxLayoutDependentTextRunInsertedHistoryItem;
      AParagraph: TdxParagraphBase; AWhere: TdxRunIndex;
      AStartIndex: Integer; ALength: Integer; AForceVisible: Boolean); override;
  public
    property NoteIndex: Integer read FNoteIndex write FNoteIndex;
  end;

  { TdxFootNoteRunInserter }

  TdxFootNoteRunInserter = class(TdxFootNoteRunInserterBase)
  protected
    function CreateRunInsertedHistoryItem: TdxLayoutDependentTextRunInsertedHistoryItem; override;
  end;

  { TdxEndNoteRunInserter }

  TdxEndNoteRunInserter = class(TdxFootNoteRunInserterBase)
  protected
    function CreateRunInsertedHistoryItem: TdxLayoutDependentTextRunInsertedHistoryItem; override;
  end;

  { TdxInlinePictureRunPropertyModifierBase }

  TdxInlinePictureRunPropertyModifierBase = class abstract
  public
    procedure ModifyPictureRun(ARun: TdxInlinePictureRun; ARunIndex: TdxRunIndex); virtual; abstract;
  end;

  { TdxRectangularObjectPropertyModifierBase }

  TdxRectangularObjectPropertyModifierBase = class abstract
  public
    procedure ModifyRectangularObject(const ARectangularObject: IdxRectangularObject; ARunIndex: TdxRunIndex); virtual; abstract;
  end;

  { TdxRunChangeCaseModifierBase }

  TdxRunChangeCaseModifierBase = class abstract (TdxRunPropertyModifierBase)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    procedure ChangeRunCase(ATextRun: TdxTextRunBase; ARunIndex: TdxRunIndex); virtual;
    function CreateHistoryItem(APieceTable: TdxPieceTable; ARunIndex: TdxRunIndex): TdxTextRunChangeCaseHistoryItem; virtual; abstract;
  end;

  { TdxRunMakeUpperCaseModifier }

  TdxRunMakeUpperCaseModifier = class(TdxRunChangeCaseModifierBase)
  public
    function CreateHistoryItem(APieceTable: TdxPieceTable; ARunIndex: TdxRunIndex): TdxTextRunChangeCaseHistoryItem; override;
  end;

  { TdxRunMakeLowerCaseModifier }

  TdxRunMakeLowerCaseModifier = class(TdxRunChangeCaseModifierBase)
  public
    function CreateHistoryItem(APieceTable: TdxPieceTable; ARunIndex: TdxRunIndex): TdxTextRunChangeCaseHistoryItem; override;
  end;

  { TdxRunToggleCaseModifier }

  TdxRunToggleCaseModifier = class(TdxRunChangeCaseModifierBase)
  public
    function CreateHistoryItem(APieceTable: TdxPieceTable; ARunIndex: TdxRunIndex): TdxTextRunChangeCaseHistoryItem; override;
  end;

  { TdxFloatingObjectRunPropertyModifierBase }

  TdxFloatingObjectRunPropertyModifierBase = class
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); virtual; abstract;
  end;

  { TdxFloatingObjectRunPropertyModifier }

  TdxFloatingObjectRunPropertyModifier<T> = class abstract(TdxFloatingObjectRunPropertyModifierBase)
  strict private
    FNewValue: T;
  protected
    function ValidateNewValue(const ANewValue: T): T; virtual;
  public
    constructor Create(const ANewValue: T);
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): T; virtual; abstract;

    property NewValue: T read FNewValue;
  end;

  { TdxFloatingObjectRunTextWrapTypeModifier }

  TdxFloatingObjectRunTextWrapTypeModifier = class(TdxFloatingObjectRunPropertyModifier<TdxFloatingObjectTextWrapType>)
  protected
    function ValidateNewValue(const ANewValue: TdxFloatingObjectTextWrapType): TdxFloatingObjectTextWrapType; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectTextWrapType; override;
  end;

  { TdxFloatingObjectRunHorizontalPositionAlignmentModifier }

  TdxFloatingObjectRunHorizontalPositionAlignmentModifier = class(TdxFloatingObjectRunPropertyModifier<TdxFloatingObjectHorizontalPositionAlignment>)
  protected
    function ValidateNewValue(const ANewValue: TdxFloatingObjectHorizontalPositionAlignment): TdxFloatingObjectHorizontalPositionAlignment; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectHorizontalPositionAlignment; override;
  end;

  { TdxFloatingObjectRunVerticalPositionAlignmentModifier }

  TdxFloatingObjectRunVerticalPositionAlignmentModifier = class(TdxFloatingObjectRunPropertyModifier<TdxFloatingObjectVerticalPositionAlignment>)
  protected
    function ValidateNewValue(const ANewValue: TdxFloatingObjectVerticalPositionAlignment): TdxFloatingObjectVerticalPositionAlignment; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectVerticalPositionAlignment; override;
  end;

  { TdxFloatingObjectRunHorizontalPositionTypeModifier }

  TdxFloatingObjectRunHorizontalPositionTypeModifier = class(TdxFloatingObjectRunPropertyModifier<TdxFloatingObjectHorizontalPositionType>)
  protected
    function ValidateNewValue(const ANewValue: TdxFloatingObjectHorizontalPositionType): TdxFloatingObjectHorizontalPositionType; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectHorizontalPositionType; override;
  end;

  { TdxFloatingObjectRunVerticalPositionTypeModifier }

  TdxFloatingObjectRunVerticalPositionTypeModifier = class(TdxFloatingObjectRunPropertyModifier<TdxFloatingObjectVerticalPositionType>)
  protected
    function ValidateNewValue(const ANewValue: TdxFloatingObjectVerticalPositionType): TdxFloatingObjectVerticalPositionType; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectVerticalPositionType; override;
  end;

  { TdxFloatingObjectRunFillColorModifier }

  TdxFloatingObjectRunFillColorModifier = class(TdxFloatingObjectRunPropertyModifier<TdxAlphaColor>)
  protected
    function ValidateNewValue(const ANewValue: TdxAlphaColor): TdxAlphaColor; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxAlphaColor; override;
  end;

  { TdxFloatingObjectRunOutlineColorModifier }

  TdxFloatingObjectRunOutlineColorModifier = class(TdxFloatingObjectRunPropertyModifier<TdxAlphaColor>)
  protected
    function ValidateNewValue(const ANewValue: TdxAlphaColor): TdxAlphaColor; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxAlphaColor; override;
  end;

  { TdxFloatingObjectRunOutlineWidthModifier }

  TdxFloatingObjectRunOutlineWidthModifier = class(TdxFloatingObjectRunPropertyModifier<Integer>)
  protected
    function ValidateNewValue(const ANewValue: Integer): Integer; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): Integer; override;
  end;

  { TdxFloatingObjectRunOutlineWidthAndColorModifier }

  TdxFloatingObjectRunOutlineWidthAndColorModifier = class(TdxFloatingObjectRunOutlineWidthModifier)
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
  end;

  { TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier }

  TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier = class(TdxFloatingObjectRunPropertyModifier<Boolean>)
  protected
    function ValidateNewValue(const ANewValue: Boolean): Boolean; override;
  public
    procedure ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex); override;
    function GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): Boolean; override;
  end;

  { TdxParagraphPropertyModifier<T> }

  TdxParagraphPropertyModifier<T> = class abstract (TdxParagraphPropertyModifierBase)
  strict private
    FComparerType: TdxFastComparerType;
    FNewValue: T;
  public
    constructor Create(const ANewValue: T); reintroduce;
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): T; virtual; abstract;
    function IsValueEquals(const AValue1, AValue2: T): Boolean;

    property NewValue: T read FNewValue;
  end;

  { TdxMergedParagraphPropertyModifier<T> }

  TdxMergedParagraphPropertyModifier<T> = class abstract (TdxParagraphPropertyModifier<T>)
  public
    function Merge(const ALeftValue, ARightValue: T): T; virtual; abstract;
  end;

  { TdxRunFontNamePropertyModifier }

  TdxRunFontNamePropertyModifier = class(TdxRunPropertyModifier<string>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): string; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): string; override;
    function IsValueEquals(const AValue1, AValue2: string): Boolean; override;
  end;

  { TdxRunFontSizePropertyModifier }

  TdxRunFontSizePropertyModifier = class(TdxRunPropertyModifier<Single>)
  protected
    function GetDoubleFontSize(AFontSize: Single): Integer;
    function ValidateNewValue(const ANewValue: Single): Single; override;
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): Single; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): Single; override;
  end;

  { TdxRunFontBoldModifier }

  TdxRunFontBoldModifier = class(TdxRunPropertyModifier<Boolean>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): Boolean; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): Boolean; override;
  end;

  { TdxRunFontItalicModifier }

  TdxRunFontItalicModifier = class(TdxRunPropertyModifier<Boolean>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): Boolean; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): Boolean; override;
  end;

  { TdxRunFontUnderlineTypeModifier }

  TdxRunFontUnderlineTypeModifier = class(TdxRunPropertyModifier<TdxUnderlineType>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxUnderlineType; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TdxUnderlineType; override;
  end;

  { TdxRunFontStrikeoutTypeModifier }

  TdxRunFontStrikeoutTypeModifier = class(TdxRunPropertyModifier<TdxStrikeoutType>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxStrikeoutType; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TdxStrikeoutType; override;
  end;

  { TdxRunBackColorModifier }

  TdxRunBackColorModifier = class(TdxRunPropertyModifier<TdxAlphaColor>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxAlphaColor; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TdxAlphaColor; override;
  end;

  { TdxRunForeColorModifier }

  TdxRunForeColorModifier = class(TdxRunPropertyModifier<TdxAlphaColor>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxAlphaColor; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TdxAlphaColor; override;
  end;

  { TdxRunUnderlineColorModifier }

  TdxRunUnderlineColorModifier = class(TdxRunPropertyModifier<TdxAlphaColor>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxAlphaColor; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TdxAlphaColor; override;
  end;

  { TdxRunScriptModifier }

  TdxRunScriptModifier = class(TdxRunPropertyModifier<TdxCharacterFormattingScript>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxCharacterFormattingScript; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TdxCharacterFormattingScript; override;
  end;

  { TdxRunFontModifier }

  TdxRunFontModifier = class(TdxRunPropertyModifier<TFont>)
  public
  {$IFNDEF DELPHIXE3}
    constructor Create(const ANewValue: TFont);
  {$ENDIF}
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TFont; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TFont; override;
  end;

  { TdxRunIncrementFontSizeModifier }

  TdxRunIncrementFontSizeModifier = class(TdxRunPropertyModifier<Integer>)
  public
    constructor Create;
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): Integer; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): Integer; override;
  end;

  { TdxRunDecrementFontSizeModifier }

  TdxRunDecrementFontSizeModifier = class(TdxRunPropertyModifier<Integer>)
  public
    constructor Create;
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): Integer; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): Integer; override;
  end;

  { TdxRunIncreaseFontSizeModifier }

  TdxRunIncreaseFontSizeModifier = class(TdxRunPropertyModifier<Integer>)
  private
    FPredefinedFontSizeCollection: TdxPredefinedFontSizeCollection;
  public
    constructor Create(APredefinedFontSizeCollection: TdxPredefinedFontSizeCollection);
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): Integer; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): Integer; override;
  end;

  { TdxRunDecreaseFontSizeModifier }

  TdxRunDecreaseFontSizeModifier = class(TdxRunPropertyModifier<Integer>)
  private
    FPredefinedFontSizeCollection: TdxPredefinedFontSizeCollection;
  public
    constructor Create(APredefinedFontSizeCollection: TdxPredefinedFontSizeCollection);
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): Integer; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): Integer; override;
  end;

  { TdxParagraphAlignmentModifier }

  TdxParagraphAlignmentModifier = class(TdxParagraphPropertyModifier<TdxParagraphAlignment>)
  public
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxParagraphAlignment; override;
  end;

  { TdxParagraphLeftIndentModifier }

  TdxParagraphLeftIndentModifier = class(TdxParagraphPropertyModifier<Integer>)
  public
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): Integer; override;
  end;

  { TdxParagraphRightIndentModifier }

  TdxParagraphRightIndentModifier = class(TdxParagraphPropertyModifier<Integer>)
  public
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): Integer; override;
  end;

  { TdxAssignParagraphLeftIndentModifier }

  TdxAssignParagraphLeftIndentModifier = class(TdxParagraphPropertyModifier<Integer>)
  private
    FMaxValue: Integer;
  public
    constructor Create(ALeftIndent, AMaxValue: Integer); reintroduce;
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): Integer; override;
  end;

  { TdxParagraphFirstLineIndentModifier }

  TdxParagraphFirstLineIndentModifier = class(TdxParagraphPropertyModifier<Integer>)
  private
    FMaxIndent: Integer;
  public
    constructor Create(const ANewValue: Integer); reintroduce; overload;
    constructor Create(AFirstLineIndent, AMaxIndent: Integer); overload;

    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): Integer; override;
  end;

  { TdxParagraphSpacingModifier }

  TdxParagraphSpacingModifier = class(TdxParagraphPropertyModifier<TdxParagraphLineSpacing>)
  public
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxParagraphLineSpacing; override;
  end;

  { TdxRunFontColorPropertyModifier }

  TdxRunFontColorPropertyModifier = class(TdxRunPropertyModifier<TdxAlphaColor>)
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxAlphaColor; override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TdxAlphaColor; override;
  end;

  { TdxFontPropertiesModifier }

  TdxFontPropertiesModifier = class(TdxMergedRunPropertyModifier<TdxMergedCharacterProperties>)
  protected
    procedure ApplyCharacterProperties(const AProperties: IdxCharacterProperties); virtual;
  public
   {$IFNDEF DELPHIXE3}
    constructor Create(const ANewValue: TdxMergedCharacterProperties);
  {$ENDIF}
    function CanModifyRun(ARun: TdxTextRunBase): Boolean; override;
    procedure CleanupValue(var AValue: TdxMergedCharacterProperties); override;
    function GetRunPropertyValue(ARun: TdxTextRunBase): TdxMergedCharacterProperties; override;
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
    procedure ModifyInputPositionCore(APos: TdxInputPosition); override;
    function GetInputPositionPropertyValue(APos: TdxInputPosition): TdxMergedCharacterProperties; override;
    function Merge(const ALeftValue, ARightValue: TdxMergedCharacterProperties): TdxMergedCharacterProperties; override;
  end;

  { TdxParagraphPropertiesModifier }

  TdxParagraphPropertiesModifier = class(TdxMergedParagraphPropertyModifier<TdxMergedParagraphProperties>)
  public
    constructor Create(const ANewValue: TdxMergedParagraphProperties); reintroduce;

    function GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxMergedParagraphProperties; override;
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function Merge(const ALeftValue, ARightValue: TdxMergedParagraphProperties): TdxMergedParagraphProperties; override;
  end;

  { TdxTabFormattingInfoModifier }

  TdxTabFormattingInfoModifier = class(TdxMergedParagraphPropertyModifier<TdxTabFormattingInfo>)
  private
    FNewDefaultTabWidth: Integer;
  protected
    function GetMergedTabInfo(ANewTabInfo, AOldOwnTabInfo, AStyleTabInfo: TdxTabFormattingInfo): TdxTabFormattingInfo;
  public
    constructor Create(ANewValue: TdxTabFormattingInfo; ADefaultTabWidth: Integer);
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxTabFormattingInfo; override;
    procedure ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex); override;
    function Merge(const ALeftValue, ARightValue: TdxTabFormattingInfo): TdxTabFormattingInfo; override;

		property NewDefaultTabWidth: Integer read FNewDefaultTabWidth write FNewDefaultTabWidth;
  end;

  { TdxOwnTabFormattingInfoModifier }

  TdxOwnTabFormattingInfoModifier = class(TdxTabFormattingInfoModifier)
  public
    function GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxTabFormattingInfo; override;
  end;

  { TdxSectionPropertyModifier }

  TdxSectionPropertyModifier<T> = class abstract(TdxSectionPropertyModifierBase)
  strict private
    FNewValue: T;
  public
    constructor Create(const ANewValue: T);
    function GetSectionPropertyValue(ASection: TdxSection): T; virtual; abstract;

    function ObtainSectionsPropertyValue(ADocumentModel: TdxDocumentModel; ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): TdxNullableValue<T>;
    function ObtainMergedSectionsPropertyValue(ADocumentModel: TdxDocumentModel; ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): T; virtual;

    property NewValue: T read FNewValue;
  end;

  { TdxMergedSectionPropertyModifier }

  TdxMergedSectionPropertyModifier<T> = class abstract(TdxSectionPropertyModifier<T>)
  public
    function Merge(const ALeftValue, ARightValue: T): T; virtual; abstract;
    function ObtainMergedSectionsPropertyValue(ADocumentModel: TdxDocumentModel; ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): T; override;
  end;

  { TdxSectionPageOrientationLandscapeModifier }

  TdxSectionPageOrientationLandscapeModifier = class(TdxSectionPropertyModifier<Boolean>)
  public
    procedure ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex); override;
    function GetSectionPropertyValue(ASection: TdxSection): Boolean; override;
  end;

  { TdxSectionLineNumberingStepAndRestartModifier }

  TdxSectionLineNumberingStepAndRestartModifier = class(TdxSectionPropertyModifier<TdxLineNumberingRestart>)
  public
    procedure ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex); override;
    function GetSectionPropertyValue(ASection: TdxSection): TdxLineNumberingRestart; override;
  end;

  { TdxSectionLineNumberingModifier }

  TdxSectionLineNumberingModifier = class(TdxMergedSectionPropertyModifier<TdxLineNumberingInfo>)
  public
  {$IFNDEF DELPHIXE3}
    constructor Create(const ANewValue: TdxLineNumberingInfo);
  {$ENDIF}
    function GetSectionPropertyValue(ASection: TdxSection): TdxLineNumberingInfo; override;
    procedure ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex); override;
    function Merge(const ALeftValue, ARightValue: TdxLineNumberingInfo): TdxLineNumberingInfo; override;
  end;

  { TdxSectionPageSetupModifier }

  TdxSectionPageSetupModifier = class(TdxMergedSectionPropertyModifier<TdxPageSetupInfo>)
  protected
    procedure ChangeOrientation(ASection: TdxSection; ALandscape: Boolean); virtual;
  public
    constructor Create(const ANewValue: TdxPageSetupInfo);
    function GetSectionPropertyValue(ASection: TdxSection): TdxPageSetupInfo; override;
    procedure ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex); override;
    function Merge(const ALeftValue, ARightValue: TdxPageSetupInfo): TdxPageSetupInfo; override;
  end;

  { TdxSectionColumnsSetupModifier }

  TdxSectionColumnsSetupModifier = class(TdxMergedSectionPropertyModifier<TdxColumnsInfoUI>)
  public
    constructor Create(const ANewValue: TdxColumnsInfoUI);
    function GetSectionPropertyValue(ASection: TdxSection): TdxColumnsInfoUI; override;
    procedure ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex); override;
    function Merge(const ALeftValue, ARightValue: TdxColumnsInfoUI): TdxColumnsInfoUI; override;
  end;

  { TdxFloatingObjectLayoutModifier }

  TdxFloatingObjectLayoutModifier = class
  strict private
    FControl: IdxRichEditControl;
    FRun: TdxFloatingObjectAnchorRun;
    FFloatingObjectAnchorRunIndex: TdxRunIndex;
    FPageViewInfo: TdxPageViewInfo;
    FCurrentHitTestResult: TdxRichEditHitTestResult;
    FCurrentTopLeftCorner: TPoint;
    FOldTopLeftCorner: TPoint;
    FMinAffectedRunIndex: TdxRunIndex;
    function GetDocumentModel: TdxDocumentModel;
    function GetActivePieceTable: TdxPieceTable;
  protected
    procedure CommitCore(const APhysicalPoint: TPoint); overload; virtual;
    function CommitCore(ALogPosition: TdxDocumentLogPosition): Boolean; overload;
    function ApplyInsideParagraphDrag(ALogPosition: TdxDocumentLogPosition): Boolean;
    function ApplyInterParagraphDrag(AItem: TdxFloatingObjectAnchorRunMovedHistoryItem;
      ALogPosition: TdxDocumentLogPosition): Boolean;
    procedure TryJoinRunsAfterFloatingObjectAnchorRunMoved(AItem: TdxFloatingObjectAnchorRunMovedHistoryItem);
    procedure PerformDocumentLayout(AExcludeFloatingObjectRunFromLayout: Boolean);
    procedure PerformDocumentLayoutCore(AExcludeFloatingObjectRunFromLayout: Boolean);
    function CreateChangeParagraphIndexAndRunIndexHistoryItem(
      ANewRunLogPosition: TdxDocumentLogPosition): TdxFloatingObjectAnchorRunMovedHistoryItem;
    function CalculateLogPosition(const APoint: TPoint;
      ADocumentLayoutDetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLogPosition;
    function CalculatePageStartLogPosition(APage: TdxPage;
      APieceTable: TdxPieceTable): TdxDocumentLogPosition;
    function IsPageContainsLogPosition(APage: TdxPage; APieceTable: TdxPieceTable;
      ALogPosition: TdxDocumentLogPosition): Boolean;
    procedure ApplyChangeParagraphIndexAndRunIndexHistoryItem(AItem: TdxFloatingObjectAnchorRunMovedHistoryItem);
    function ResetHorizontalPositionAlignment: Boolean; virtual;
    function ResetVerticalPositionAlignment(ALogPosition: TdxDocumentLogPosition): Boolean; virtual;
    procedure SetHorizontalPositionRelatedToColumn(
      AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure SetVerticalPositionRelatedToParagraph(AFloatingObjectProperties: TdxFloatingObjectProperties;
      ALogPosition: TdxDocumentLogPosition);
    function CreateFloatingObjectTargetPlacementInfo(AHitTestResult: TdxRichEditHitTestResult;
      AOriginX: Integer; AOriginY: Integer): TdxFloatingObjectTargetPlacementInfo;
  public
    constructor Create(const AControl: IdxRichEditControl; ARun: TdxFloatingObjectAnchorRun;
      AFloatingObjectAnchorRunIndex: TdxRunIndex);
    destructor Destroy; override;
    procedure Commit(const APhysicalPoint: TPoint);

    property Control: IdxRichEditControl read FControl;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property ActivePieceTable: TdxPieceTable read GetActivePieceTable;
    property CurrentTopLeftCorner: TPoint read FCurrentTopLeftCorner write FCurrentTopLeftCorner;
    property OldTopLeftCorner: TPoint read FOldTopLeftCorner write FOldTopLeftCorner;
    property MinAffectedRunIndex: TdxRunIndex read FMinAffectedRunIndex write FMinAffectedRunIndex;
    property AnchorRun: TdxFloatingObjectAnchorRun read FRun;
  end;

implementation

uses
  RTLConsts, Contnrs,
  Math,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.BatchUpdateHelper;

type
  TdxFloatingObjectPropertiesAccess = class(TdxFloatingObjectProperties);

{ TdxRunChangeCaseModifierBase }

procedure TdxRunChangeCaseModifierBase.ChangeRunCase(ATextRun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  AItem: TdxTextRunChangeCaseHistoryItem;
  AHistory: TdxDocumentHistory;
begin
  AItem := CreateHistoryItem(TdxPieceTable(ATextRun.Paragraph.PieceTable), ARunIndex);
  AHistory := ATextRun.Paragraph.DocumentModel.History;
  AHistory.Add(AItem);
  AItem.Execute;
  if ATextRun.CharacterProperties.UseAllCaps then
    ATextRun.CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseAllCaps);
  if ATextRun.AllCaps then
    ATextRun.AllCaps := False;
end;

procedure TdxRunChangeCaseModifierBase.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  if (ARun is TdxTextRun) and
      (TdxPieceTable(ARun.PieceTable).VisibleTextFilter.GetRunVisibilityInField(ARunIndex) <> TdxRunVisibility.Hidden) then
    ChangeRunCase(TdxTextRun(ARun), ARunIndex);
end;

{ TdxRunMakeUpperCaseModifier }

function TdxRunMakeUpperCaseModifier.CreateHistoryItem(APieceTable: TdxPieceTable;
  ARunIndex: TdxRunIndex): TdxTextRunChangeCaseHistoryItem;
begin
  Result := TdxTextRunMakeUpperCaseHistoryItem.Create(APieceTable, ARunIndex);
end;

{ TdxRunMakeLowerCaseModifier }

function TdxRunMakeLowerCaseModifier.CreateHistoryItem(APieceTable: TdxPieceTable;
  ARunIndex: TdxRunIndex): TdxTextRunChangeCaseHistoryItem;
begin
  Result := TdxTextRunMakeLowerCaseHistoryItem.Create(APieceTable, ARunIndex);
end;

{ TdxRunToggleCaseModifier }

function TdxRunToggleCaseModifier.CreateHistoryItem(APieceTable: TdxPieceTable;
  ARunIndex: TdxRunIndex): TdxTextRunChangeCaseHistoryItem;
begin
  Result := TdxTextRunToggleCaseHistoryItem.Create(APieceTable, ARunIndex);
end;

{ TdxFloatingObjectRunPropertyModifier }

constructor TdxFloatingObjectRunPropertyModifier<T>.Create(const ANewValue: T);
begin
  inherited Create;
  FNewValue := ValidateNewValue(ANewValue);
end;

function TdxFloatingObjectRunPropertyModifier<T>.ValidateNewValue(const ANewValue: T): T;
begin
  Result := ANewValue;
end;

{ TdxFloatingObjectRunTextWrapTypeModifier }

function TdxFloatingObjectRunTextWrapTypeModifier.ValidateNewValue(const ANewValue: TdxFloatingObjectTextWrapType): TdxFloatingObjectTextWrapType;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunTextWrapTypeModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex);
begin
  ARun.FloatingObjectProperties.TextWrapType := NewValue;
end;

function TdxFloatingObjectRunTextWrapTypeModifier.GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectTextWrapType;
begin
  Result := AAnchorRun.FloatingObjectProperties.TextWrapType;
end;

{ TdxFloatingObjectRunHorizontalPositionAlignmentModifier }

function TdxFloatingObjectRunHorizontalPositionAlignmentModifier.ValidateNewValue(const ANewValue: TdxFloatingObjectHorizontalPositionAlignment): TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunHorizontalPositionAlignmentModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex);
begin
  ARun.FloatingObjectProperties.HorizontalPositionAlignment := NewValue;
end;

function TdxFloatingObjectRunHorizontalPositionAlignmentModifier.GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := AAnchorRun.FloatingObjectProperties.HorizontalPositionAlignment;
end;

{ TdxFloatingObjectRunVerticalPositionAlignmentModifier }

function TdxFloatingObjectRunVerticalPositionAlignmentModifier.ValidateNewValue(const ANewValue: TdxFloatingObjectVerticalPositionAlignment): TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunVerticalPositionAlignmentModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex);
begin
  ARun.FloatingObjectProperties.VerticalPositionAlignment := NewValue;
end;

function TdxFloatingObjectRunVerticalPositionAlignmentModifier.GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := AAnchorRun.FloatingObjectProperties.VerticalPositionAlignment;
end;

{ TdxFloatingObjectRunHorizontalPositionTypeModifier }

function TdxFloatingObjectRunHorizontalPositionTypeModifier.ValidateNewValue(const ANewValue: TdxFloatingObjectHorizontalPositionType): TdxFloatingObjectHorizontalPositionType;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunHorizontalPositionTypeModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex);
begin
  ARun.FloatingObjectProperties.HorizontalPositionType := NewValue;
end;

function TdxFloatingObjectRunHorizontalPositionTypeModifier.GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectHorizontalPositionType;
begin
  Result := AAnchorRun.FloatingObjectProperties.HorizontalPositionType;
end;

{ TdxFloatingObjectRunVerticalPositionTypeModifier }

function TdxFloatingObjectRunVerticalPositionTypeModifier.ValidateNewValue(const ANewValue: TdxFloatingObjectVerticalPositionType): TdxFloatingObjectVerticalPositionType;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunVerticalPositionTypeModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex);
begin
  ARun.FloatingObjectProperties.VerticalPositionType := NewValue;
end;

function TdxFloatingObjectRunVerticalPositionTypeModifier.GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): TdxFloatingObjectVerticalPositionType;
begin
  Result := AAnchorRun.FloatingObjectProperties.VerticalPositionType;
end;

{ TdxFloatingObjectRunFillColorModifier }

function TdxFloatingObjectRunFillColorModifier.ValidateNewValue(const ANewValue: TdxAlphaColor): TdxAlphaColor;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunFillColorModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun;
  ARunIndex: TdxRunIndex);
begin
  ARun.Shape.FillColor := NewValue;
end;

function TdxFloatingObjectRunFillColorModifier.GetFloatingObjectValue(
  AAnchorRun: TdxFloatingObjectAnchorRun): TdxAlphaColor;
begin
  Result := AAnchorRun.Shape.FillColor;
end;

{ TdxFloatingObjectRunOutlineColorModifier }

function TdxFloatingObjectRunOutlineColorModifier.ValidateNewValue(const ANewValue: TdxAlphaColor): TdxAlphaColor;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunOutlineColorModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun;
  ARunIndex: TdxRunIndex);
begin
  ARun.Shape.OutlineColor := NewValue;
end;

function TdxFloatingObjectRunOutlineColorModifier.GetFloatingObjectValue(
  AAnchorRun: TdxFloatingObjectAnchorRun): TdxAlphaColor;
begin
  Result := AAnchorRun.Shape.OutlineColor;
end;

{ TdxFloatingObjectRunOutlineWidthModifier }

function TdxFloatingObjectRunOutlineWidthModifier.ValidateNewValue(const ANewValue: Integer): Integer;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunOutlineWidthModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun;
  ARunIndex: TdxRunIndex);
begin
  ARun.Shape.OutlineWidth := NewValue;
end;

function TdxFloatingObjectRunOutlineWidthModifier.GetFloatingObjectValue(
  AAnchorRun: TdxFloatingObjectAnchorRun): Integer;
begin
  Result := AAnchorRun.Shape.OutlineWidth;
end;

{ TdxFloatingObjectRunOutlineWidthAndColorModifier }

procedure TdxFloatingObjectRunOutlineWidthAndColorModifier.ModifyFloatingObjectRun(
  ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex);
begin
  inherited ModifyFloatingObjectRun(ARun, ARunIndex);
  if TdxAlphaColors.IsTransparentOrEmpty(ARun.Shape.OutlineColor) then
    ARun.Shape.OutlineColor := TdxAlphaColors.Black;
end;

{ TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier }

function TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier.ValidateNewValue(const ANewValue: Boolean): Boolean;
begin
  Result := ANewValue;
end;

procedure TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier.ModifyFloatingObjectRun(ARun: TdxFloatingObjectAnchorRun;
  ARunIndex: TdxRunIndex);
begin
  ARun.FloatingObjectProperties.IsBehindDoc := NewValue;
  ARun.FloatingObjectProperties.TextWrapType := TdxFloatingObjectTextWrapType.None;
end;

function TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier.GetFloatingObjectValue(AAnchorRun: TdxFloatingObjectAnchorRun): Boolean;
begin
  Result := AAnchorRun.FloatingObjectProperties.IsBehindDoc;
end;

{ TdxParagraphPropertyModifier<T> }

constructor TdxParagraphPropertyModifier<T>.Create(const ANewValue: T);
begin
  inherited Create;
  FNewValue := ANewValue;
end;

function TdxParagraphPropertyModifier<T>.IsValueEquals(const AValue1, AValue2: T): Boolean;
begin
  Result := TdxFastComparer<T>.IsValueEquals(FComparerType, AValue1, AValue2);
end;

{ TdxRunFontNamePropertyModifier }

function TdxRunFontNamePropertyModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): string;
begin
  Result := APos.MergedCharacterFormatting.FontName;
end;

function TdxRunFontNamePropertyModifier.IsValueEquals(const AValue1, AValue2: string): Boolean;
begin
  Result := AValue1 = AValue2;
end;

function TdxRunFontNamePropertyModifier.GetRunPropertyValue(ARun: TdxTextRunBase): string;
begin
  Result := ARun.FontName;
end;

procedure TdxRunFontNamePropertyModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.FontName := NewValue;
  APos.MergedCharacterFormatting.FontName := NewValue;
end;

procedure TdxRunFontNamePropertyModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.FontName := NewValue;
end;

{ TdxRunFontSizePropertyModifier }

function TdxRunFontSizePropertyModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): Single;
begin
  Result := APos.MergedCharacterFormatting.DoubleFontSize / 2.0;
end;

function TdxRunFontSizePropertyModifier.GetRunPropertyValue(ARun: TdxTextRunBase): Single;
begin
  Result := ARun.DoubleFontSize / 2.0;
end;

procedure TdxRunFontSizePropertyModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.DoubleFontSize := GetDoubleFontSize(NewValue);
  APos.MergedCharacterFormatting.DoubleFontSize := GetDoubleFontSize(NewValue);
end;

procedure TdxRunFontSizePropertyModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.DoubleFontSize := GetDoubleFontSize(NewValue);
end;

function TdxRunFontSizePropertyModifier.GetDoubleFontSize(AFontSize: Single): Integer;
begin
  Result := Round(AFontSize * 2.0);
end;

function TdxRunFontSizePropertyModifier.ValidateNewValue(const ANewValue: Single): Single;
begin
  Result := TdxPredefinedFontSizeCollection.ValidateFontSize(ANewValue);
end;

{ TdxRunFontBoldModifier }

function TdxRunFontBoldModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): Boolean;
begin
  Result := APos.MergedCharacterFormatting.FontBold;
end;

function TdxRunFontBoldModifier.GetRunPropertyValue(ARun: TdxTextRunBase): Boolean;
begin
  Result := ARun.FontBold;
end;

procedure TdxRunFontBoldModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.FontBold := NewValue;
  APos.MergedCharacterFormatting.FontBold := NewValue;
end;

procedure TdxRunFontBoldModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.FontBold := NewValue;
end;

{ TdxRunFontItalicModifier }

function TdxRunFontItalicModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): Boolean;
begin
  Result := APos.MergedCharacterFormatting.FontItalic;
end;

function TdxRunFontItalicModifier.GetRunPropertyValue(ARun: TdxTextRunBase): Boolean;
begin
  Result := ARun.FontItalic;
end;

procedure TdxRunFontItalicModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.FontItalic := NewValue;
  APos.MergedCharacterFormatting.FontItalic := NewValue;
end;

procedure TdxRunFontItalicModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.FontItalic := NewValue;
end;

{ TdxRunFontUnderlineTypeModifier }

function TdxRunFontUnderlineTypeModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TdxUnderlineType;
begin
  Result := APos.MergedCharacterFormatting.FontUnderlineType;
end;

function TdxRunFontUnderlineTypeModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxUnderlineType;
begin
  Result := ARun.FontUnderlineType;
end;

procedure TdxRunFontUnderlineTypeModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.FontUnderlineType := NewValue;
  APos.MergedCharacterFormatting.FontUnderlineType := NewValue;
end;

procedure TdxRunFontUnderlineTypeModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.FontUnderlineType := NewValue;
end;

{ TdxRunFontStrikeoutTypeModifier }

function TdxRunFontStrikeoutTypeModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TdxStrikeoutType;
begin
  Result := APos.MergedCharacterFormatting.FontStrikeoutType;
end;

function TdxRunFontStrikeoutTypeModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxStrikeoutType;
begin
  Result := ARun.FontStrikeoutType;
end;

procedure TdxRunFontStrikeoutTypeModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.FontStrikeoutType := NewValue;
  APos.MergedCharacterFormatting.FontStrikeoutType := NewValue;
end;

procedure TdxRunFontStrikeoutTypeModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.FontStrikeoutType := NewValue;
end;

{ TdxRunBackColorModifier }

function TdxRunBackColorModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TdxAlphaColor;
begin
  Result := APos.MergedCharacterFormatting.BackColor;
end;

function TdxRunBackColorModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxAlphaColor;
begin
  Result := ARun.BackColor;
end;

procedure TdxRunBackColorModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.BackColor := NewValue;
  APos.MergedCharacterFormatting.BackColor := NewValue;
end;

procedure TdxRunBackColorModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.BackColor := NewValue;
end;

{ TdxRunForeColorModifier }

function TdxRunForeColorModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TdxAlphaColor;
begin
  Result := APos.MergedCharacterFormatting.ForeColor;
end;

function TdxRunForeColorModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxAlphaColor;
begin
  Result := ARun.ForeColor;
end;

procedure TdxRunForeColorModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.ForeColor := NewValue;
  APos.MergedCharacterFormatting.ForeColor := NewValue;
end;

procedure TdxRunForeColorModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.ForeColor := NewValue;
end;

{ TdxRunUnderlineColorModifier }

function TdxRunUnderlineColorModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TdxAlphaColor;
begin
  Result := APos.MergedCharacterFormatting.UnderlineColor;
end;

function TdxRunUnderlineColorModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxAlphaColor;
begin
  Result := ARun.UnderlineColor;
end;

procedure TdxRunUnderlineColorModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.UnderlineColor := NewValue;
  APos.MergedCharacterFormatting.UnderlineColor := NewValue;
end;

procedure TdxRunUnderlineColorModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.UnderlineColor := NewValue;
end;

{ TdxRunScriptModifier }

function TdxRunScriptModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TdxCharacterFormattingScript;
begin
  Result := APos.MergedCharacterFormatting.Script;
end;

function TdxRunScriptModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxCharacterFormattingScript;
begin
  Result := ARun.Script;
end;

procedure TdxRunScriptModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.Script := NewValue;
  APos.MergedCharacterFormatting.Script := NewValue;
end;

procedure TdxRunScriptModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.Script := NewValue;
end;

{ TdxRunFontModifier }

{$IFNDEF DELPHIXE3}
constructor TdxRunFontModifier.Create(const ANewValue: TFont);
begin
  inherited
end;
{$ENDIF}

function TdxRunFontModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TFont;
begin
  Result := nil;
end;

function TdxRunFontModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TFont;
begin
  Result := nil;
end;

procedure TdxRunFontModifier.ModifyInputPositionCore(APos: TdxInputPosition);
var
  AFormatting: TdxCharacterFormattingBase;
begin
  AFormatting := APos.CharacterFormatting;
  AFormatting.BeginUpdate;
  try
    TdxCharacterPropertiesFontAssignmentHelper.AssignFont(AFormatting, NewValue);
  finally
    AFormatting.EndUpdate;
  end;
end;

procedure TdxRunFontModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  ADocumentModel: TdxCustomDocumentModel;
begin
  ADocumentModel := ARun.Paragraph.DocumentModel;
  ADocumentModel.History.BeginTransaction;
  try
    TdxCharacterPropertiesFontAssignmentHelper.AssignFont(ARun, NewValue);
  finally
    ADocumentModel.History.EndTransaction;
  end;
end;

{ TdxRunIncrementFontSizeModifier }

constructor TdxRunIncrementFontSizeModifier.Create;
begin
  inherited Create(0);
end;

function TdxRunIncrementFontSizeModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): Integer;
begin
  Result := APos.MergedCharacterFormatting.DoubleFontSize;
end;

function TdxRunIncrementFontSizeModifier.GetRunPropertyValue(ARun: TdxTextRunBase): Integer;
begin
  Result := ARun.DoubleFontSize;
end;

procedure TdxRunIncrementFontSizeModifier.ModifyInputPositionCore(APos: TdxInputPosition);
var
  AValue: Integer;
begin
  AValue := Trunc(TdxPredefinedFontSizeCollection.ValidateFontSize(APos.CharacterFormatting.DoubleFontSize / 2 + 1)) * 2;
  APos.CharacterFormatting.DoubleFontSize := AValue;
  APos.MergedCharacterFormatting.DoubleFontSize := AValue;
end;

procedure TdxRunIncrementFontSizeModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.DoubleFontSize := Trunc(TdxPredefinedFontSizeCollection.ValidateFontSize(ARun.DoubleFontSize / 2 + 1)) * 2;
end;

{ TdxRunDecrementFontSizeModifier }

constructor TdxRunDecrementFontSizeModifier.Create;
begin
  inherited Create(0);
end;

function TdxRunDecrementFontSizeModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): Integer;
begin
  Result := APos.MergedCharacterFormatting.DoubleFontSize;
end;

function TdxRunDecrementFontSizeModifier.GetRunPropertyValue(ARun: TdxTextRunBase): Integer;
begin
  Result := ARun.DoubleFontSize;
end;

procedure TdxRunDecrementFontSizeModifier.ModifyInputPositionCore(APos: TdxInputPosition);
var
  AValue: Integer;
begin
  AValue := TdxPredefinedFontSizeCollection.ValidateDoubleFontSize(APos.CharacterFormatting.DoubleFontSize - 1);
  APos.CharacterFormatting.DoubleFontSize := AValue;
  APos.MergedCharacterFormatting.DoubleFontSize := AValue;
end;

procedure TdxRunDecrementFontSizeModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.DoubleFontSize := TdxPredefinedFontSizeCollection.ValidateDoubleFontSize(ARun.DoubleFontSize - 1);
end;

{ TdxRunIncreaseFontSizeModifier }

constructor TdxRunIncreaseFontSizeModifier.Create(APredefinedFontSizeCollection: TdxPredefinedFontSizeCollection);
begin
  inherited Create(0);
  FPredefinedFontSizeCollection := APredefinedFontSizeCollection;
end;

function TdxRunIncreaseFontSizeModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): Integer;
begin
  Result := APos.MergedCharacterFormatting.DoubleFontSize;
end;

function TdxRunIncreaseFontSizeModifier.GetRunPropertyValue(ARun: TdxTextRunBase): Integer;
begin
  Result := ARun.DoubleFontSize;
end;

procedure TdxRunIncreaseFontSizeModifier.ModifyInputPositionCore(APos: TdxInputPosition);
var
  AValue: Integer;
begin
  AValue := FPredefinedFontSizeCollection.CalculateNextDoubleFontSize(APos.CharacterFormatting.DoubleFontSize);
  APos.CharacterFormatting.DoubleFontSize := AValue;
  APos.MergedCharacterFormatting.DoubleFontSize := AValue;
end;

procedure TdxRunIncreaseFontSizeModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.DoubleFontSize := FPredefinedFontSizeCollection.CalculateNextDoubleFontSize(ARun.DoubleFontSize);
end;

{ TdxRunDecreaseFontSizeModifier }

constructor TdxRunDecreaseFontSizeModifier.Create(APredefinedFontSizeCollection: TdxPredefinedFontSizeCollection);
begin
  inherited Create(0);
	FPredefinedFontSizeCollection := APredefinedFontSizeCollection;
end;

function TdxRunDecreaseFontSizeModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): Integer;
begin
  Result := APos.MergedCharacterFormatting.DoubleFontSize;
end;

function TdxRunDecreaseFontSizeModifier.GetRunPropertyValue(ARun: TdxTextRunBase): Integer;
begin
  Result := ARun.DoubleFontSize;
end;

procedure TdxRunDecreaseFontSizeModifier.ModifyInputPositionCore(APos: TdxInputPosition);
var
  AValue: Integer;
begin
  AValue := FPredefinedFontSizeCollection.CalculatePreviousDoubleFontSize(APos.CharacterFormatting.DoubleFontSize);
  APos.CharacterFormatting.DoubleFontSize := AValue;
  APos.MergedCharacterFormatting.DoubleFontSize := AValue;
end;

procedure TdxRunDecreaseFontSizeModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.DoubleFontSize := FPredefinedFontSizeCollection.CalculatePreviousDoubleFontSize(ARun.DoubleFontSize);
end;

{ TdxParagraphAlignmentModifier }

function TdxParagraphAlignmentModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxParagraphAlignment;
begin
  Result := AParagraph.Alignment;
end;

procedure TdxParagraphAlignmentModifier.ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex);
begin
  AParagraph.Alignment := NewValue;
end;

{ TdxParagraphLeftIndentModifier }

function TdxParagraphLeftIndentModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): Integer;
begin
  Result := AParagraph.LeftIndent;
end;

procedure TdxParagraphLeftIndentModifier.ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex);
begin
  AParagraph.LeftIndent := NewValue;
end;

{ TdxParagraphRightIndentModifier }

function TdxParagraphRightIndentModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): Integer;
begin
  Result := AParagraph.RightIndent;
end;

procedure TdxParagraphRightIndentModifier.ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex);
begin
  AParagraph.RightIndent := NewValue;
end;

{ TdxAssignParagraphLeftIndentModifier }

constructor TdxAssignParagraphLeftIndentModifier.Create(ALeftIndent, AMaxValue: Integer);
begin
  inherited Create(ALeftIndent);
  FMaxValue := AMaxValue;
end;

function TdxAssignParagraphLeftIndentModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): Integer;
begin
  Result := AParagraph.LeftIndent;
end;

procedure TdxAssignParagraphLeftIndentModifier.ModifyParagraph(AParagraph: TdxParagraph;
  AParagraphIndex: TdxParagraphIndex);
var
  ANewLeftIndent, AFirstLineLeftIndent, ADistanceToRight: Integer;
begin
  ANewLeftIndent := AParagraph.LeftIndent + NewValue;
  if ANewLeftIndent >= 0 then
  begin
    if AParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
    begin
      AFirstLineLeftIndent := ANewLeftIndent - AParagraph.FirstLineIndent;
      if AFirstLineLeftIndent < 0 then
        Dec(ANewLeftIndent, AFirstLineLeftIndent);
    end;
    AParagraph.LeftIndent := Min(FMaxValue, ANewLeftIndent);
    if AParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Indented then
    begin
      ADistanceToRight := FMaxValue - (AParagraph.LeftIndent + AParagraph.FirstLineIndent);
      if ADistanceToRight < 0 then
        AParagraph.FirstLineIndent := AParagraph.FirstLineIndent + ADistanceToRight;
    end;
  end;
end;

{ TdxParagraphFirstLineIndentModifier }

constructor TdxParagraphFirstLineIndentModifier.Create(const ANewValue: Integer);
begin
  Create(ANewValue, MaxInt div 4);
end;

constructor TdxParagraphFirstLineIndentModifier.Create(AFirstLineIndent, AMaxIndent: Integer);
begin
  inherited Create(AFirstLineIndent);
  FMaxIndent := AMaxIndent;
end;

function TdxParagraphFirstLineIndentModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): Integer;
begin
  Result := AParagraph.FirstLineIndent;
end;

procedure TdxParagraphFirstLineIndentModifier.ModifyParagraph(AParagraph: TdxParagraph;
  AParagraphIndex: TdxParagraphIndex);
var
  AValue, ADistanceToRight: Integer;
begin
  if NewValue > 0 then
  begin
    AParagraph.FirstLineIndentType := TdxParagraphFirstLineIndent.Indented;
    AValue := NewValue;
    ADistanceToRight := FMaxIndent - (AParagraph.LeftIndent + AValue);
    if ADistanceToRight < 0 then
      Inc(AValue, ADistanceToRight);
    AParagraph.FirstLineIndent := AValue;
  end
  else
  begin
    AParagraph.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
    AParagraph.FirstLineIndent := -NewValue;
  end;
end;

{ TdxParagraphSpacingModifier }

function TdxParagraphSpacingModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxParagraphLineSpacing;
begin
  Result := AParagraph.LineSpacingType;
end;

procedure TdxParagraphSpacingModifier.ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex);
begin
  AParagraph.LineSpacingType := NewValue;
end;

{ TdxRunFontColorPropertyModifier }

function TdxRunFontColorPropertyModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TdxAlphaColor;
begin
  Result := APos.MergedCharacterFormatting.ForeColor;
end;

function TdxRunFontColorPropertyModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxAlphaColor;
begin
  Result := ARun.ForeColor;
end;

procedure TdxRunFontColorPropertyModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  APos.CharacterFormatting.ForeColor := NewValue;
  APos.MergedCharacterFormatting.ForeColor := NewValue;
end;

procedure TdxRunFontColorPropertyModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ARun.ForeColor := NewValue;
end;

{ TdxFontPropertiesModifier }

{$IFNDEF DELPHIXE3}
constructor TdxFontPropertiesModifier.Create(const ANewValue: TdxMergedCharacterProperties);
begin
  inherited;
end;
{$ENDIF}

procedure TdxFontPropertiesModifier.ApplyCharacterProperties(const AProperties: IdxCharacterProperties);
var
  AInfo: TdxCharacterFormattingInfo;
  AOptions: TdxCharacterFormattingOptions;
begin
  AInfo := NewValue.Info;
  AOptions := NewValue.Options;
  if AOptions.UseFontName then
    AProperties.FontName := AInfo.FontName;
  if AOptions.UseFontBold then
    AProperties.FontBold := AInfo.FontBold;
  if AOptions.UseFontItalic then
    AProperties.FontItalic := AInfo.FontItalic;
  if AOptions.UseDoubleFontSize then
    AProperties.DoubleFontSize := AInfo.DoubleFontSize;
  if AOptions.UseForeColor then
    AProperties.ForeColor := AInfo.ForeColor;
  if AOptions.UseFontUnderlineType then
    AProperties.FontUnderlineType := AInfo.FontUnderlineType;
  if AOptions.UseUnderlineColor then
    AProperties.UnderlineColor := AInfo.UnderlineColor;
  if AOptions.UseFontStrikeoutType then
    AProperties.FontStrikeoutType := AInfo.FontStrikeoutType;
  if AOptions.UseScript then
    AProperties.Script := AInfo.Script;
  if AOptions.UseAllCaps then
    AProperties.AllCaps := AInfo.AllCaps;
  if AOptions.UseUnderlineWordsOnly then
    AProperties.UnderlineWordsOnly := AInfo.UnderlineWordsOnly;
  if AOptions.UseHidden then
    AProperties.Hidden := AInfo.Hidden;
end;

function TdxFontPropertiesModifier.CanModifyRun(ARun: TdxTextRunBase): Boolean;
begin
  Result := not (ARun is TdxSeparatorTextRun);
end;

procedure TdxFontPropertiesModifier.CleanupValue(var AValue: TdxMergedCharacterProperties);
begin
  FreeAndNil(AValue);
end;

function TdxFontPropertiesModifier.GetInputPositionPropertyValue(APos: TdxInputPosition): TdxMergedCharacterProperties;
var
  AOptions: TdxCharacterFormattingOptions;
begin
  AOptions := TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseAll);
  Result := TdxMergedCharacterProperties.Create(APos.MergedCharacterFormatting, AOptions);
end;

function TdxFontPropertiesModifier.GetRunPropertyValue(ARun: TdxTextRunBase): TdxMergedCharacterProperties;
var
  AFormattingInfo: TdxCharacterFormattingInfo;
  AOptions: TdxCharacterFormattingOptions;
begin
  AFormattingInfo := ARun.MergedCharacterFormatting;
  AOptions := TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseAll);
  Result := TdxMergedCharacterProperties.Create(AFormattingInfo, AOptions);
end;

function TdxFontPropertiesModifier.Merge(const ALeftValue, ARightValue: TdxMergedCharacterProperties): TdxMergedCharacterProperties;
var
  ATargetOptions: TdxCharacterFormattingOptions;
  ATargetInfo, ARunInfo: TdxCharacterFormattingInfo;
begin
  ATargetOptions := ALeftValue.Options;
  ATargetInfo := ALeftValue.Info;
  ARunInfo := ARightValue.Info;
  ATargetOptions.UseAllCaps := ATargetOptions.UseAllCaps and (ATargetInfo.AllCaps = ARunInfo.AllCaps);
  ATargetOptions.UseBackColor := ATargetOptions.UseBackColor and (ATargetInfo.BackColor = ARunInfo.BackColor);
  ATargetOptions.UseFontBold := ATargetOptions.UseFontBold and (ATargetInfo.FontBold = ARunInfo.FontBold);
  ATargetOptions.UseFontItalic := ATargetOptions.UseFontItalic and (ATargetInfo.FontItalic = ARunInfo.FontItalic);
  ATargetOptions.UseFontName := ATargetOptions.UseFontName and (ATargetInfo.FontName = ARunInfo.FontName);
  ATargetOptions.UseDoubleFontSize := ATargetOptions.UseDoubleFontSize and (ATargetInfo.DoubleFontSize = ARunInfo.DoubleFontSize);
  ATargetOptions.UseFontStrikeoutType := ATargetOptions.UseFontStrikeoutType and (ATargetInfo.FontStrikeoutType = ARunInfo.FontStrikeoutType);
  ATargetOptions.UseFontUnderlineType := ATargetOptions.UseFontUnderlineType and (ATargetInfo.FontUnderlineType = ARunInfo.FontUnderlineType);
  ATargetOptions.UseForeColor := ATargetOptions.UseForeColor and (ATargetInfo.ForeColor = ARunInfo.ForeColor);
  ATargetOptions.UseScript := ATargetOptions.UseScript and (ATargetInfo.Script = ARunInfo.Script);
  ATargetOptions.UseStrikeoutColor := ATargetOptions.UseStrikeoutColor and (ATargetInfo.StrikeoutColor = ARunInfo.StrikeoutColor);
  ATargetOptions.UseStrikeoutWordsOnly := ATargetOptions.UseStrikeoutWordsOnly and (ATargetInfo.StrikeoutWordsOnly = ARunInfo.StrikeoutWordsOnly);
  ATargetOptions.UseUnderlineColor := ATargetOptions.UseUnderlineColor and (ATargetInfo.UnderlineColor = ARunInfo.UnderlineColor);
  ATargetOptions.UseUnderlineWordsOnly := ATargetOptions.UseUnderlineWordsOnly and (ATargetInfo.UnderlineWordsOnly = ARunInfo.UnderlineWordsOnly);
  ATargetOptions.UseHidden := ATargetOptions.UseHidden and (ATargetInfo.Hidden = ARunInfo.Hidden);
  Result := TdxMergedCharacterProperties.Create(ATargetInfo, ATargetOptions);
end;

procedure TdxFontPropertiesModifier.ModifyInputPositionCore(APos: TdxInputPosition);
begin
  ApplyCharacterProperties(APos.CharacterFormatting);
  ApplyCharacterProperties(APos.MergedCharacterFormatting);
end;

procedure TdxFontPropertiesModifier.ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
begin
  ApplyCharacterProperties(ARun);
end;

{ TdxParagraphPropertiesModifier }

constructor TdxParagraphPropertiesModifier.Create(const ANewValue: TdxMergedParagraphProperties);
begin
  inherited Create(ANewValue);
end;

function TdxParagraphPropertiesModifier.GetParagraphPropertyValue(
  AParagraph: TdxParagraph): TdxMergedParagraphProperties;
begin
  Result := AParagraph.GetMergedParagraphProperties;
end;

function TdxParagraphPropertiesModifier.Merge(const ALeftValue, ARightValue: TdxMergedParagraphProperties): TdxMergedParagraphProperties;
var
  ATargetOptions: TdxParagraphFormattingOptions;
  ATargetInfo: TdxParagraphFormattingInfo;
  ARunInfo: TdxParagraphFormattingInfo;
begin
  ATargetOptions := ALeftValue.Options;
  ATargetInfo := ALeftValue.Info;
  ARunInfo := ARightValue.Info;
  ATargetOptions.UseLeftIndent := ATargetOptions.UseLeftIndent and (ATargetInfo.LeftIndent = ARunInfo.LeftIndent);
  ATargetOptions.UseRightIndent := ATargetOptions.UseRightIndent and (ATargetInfo.RightIndent = ARunInfo.RightIndent);
  ATargetOptions.UseFirstLineIndent := ATargetOptions.UseFirstLineIndent and (ATargetInfo.FirstLineIndent = ARunInfo.FirstLineIndent) and
    (ATargetInfo.FirstLineIndentType = ARunInfo.FirstLineIndentType);
  ATargetOptions.UseAlignment := ATargetOptions.UseAlignment and (ATargetInfo.Alignment = ARunInfo.Alignment);
  ATargetOptions.UseSpacingBefore := ATargetOptions.UseSpacingBefore and (ATargetInfo.SpacingBefore = ARunInfo.SpacingBefore);
  ATargetOptions.UseSpacingAfter := ATargetOptions.UseSpacingAfter and (ATargetInfo.SpacingAfter = ARunInfo.SpacingAfter);
  ATargetOptions.UseLineSpacing := ATargetOptions.UseLineSpacing and (ATargetInfo.LineSpacing = ARunInfo.LineSpacing) and
    (ATargetInfo.LineSpacingType = ARunInfo.LineSpacingType);
  ATargetOptions.UseSuppressHyphenation := ATargetOptions.UseSuppressHyphenation and (ATargetInfo.SuppressHyphenation = ARunInfo.SuppressHyphenation);
  ATargetOptions.UseSuppressLineNumbers := ATargetOptions.UseSuppressLineNumbers and (ATargetInfo.SuppressLineNumbers = ARunInfo.SuppressLineNumbers);
  ATargetOptions.UseContextualSpacing := ATargetOptions.UseContextualSpacing and (ATargetInfo.ContextualSpacing = ARunInfo.ContextualSpacing);
  ATargetOptions.UsePageBreakBefore := ATargetOptions.UsePageBreakBefore and (ATargetInfo.PageBreakBefore = ARunInfo.PageBreakBefore);
  ATargetOptions.UseBeforeAutoSpacing := ATargetOptions.UseBeforeAutoSpacing and (ATargetInfo.BeforeAutoSpacing = ARunInfo.BeforeAutoSpacing);
  ATargetOptions.UseAfterAutoSpacing := ATargetOptions.UseAfterAutoSpacing and (ATargetInfo.AfterAutoSpacing = ARunInfo.AfterAutoSpacing);
  ATargetOptions.UseKeepWithNext := ATargetOptions.UseKeepWithNext and (ATargetInfo.KeepWithNext = ARunInfo.KeepWithNext);
  ATargetOptions.UseKeepLinesTogether := ATargetOptions.UseKeepLinesTogether and (ATargetInfo.KeepLinesTogether = ARunInfo.KeepLinesTogether);
  ATargetOptions.UseWidowOrphanControl := ATargetOptions.UseWidowOrphanControl and (ATargetInfo.WidowOrphanControl = ARunInfo.WidowOrphanControl);
  ATargetOptions.UseOutlineLevel := ATargetOptions.UseOutlineLevel and (ATargetInfo.OutlineLevel = ARunInfo.OutlineLevel);
  ATargetOptions.UseBackColor := ATargetOptions.UseBackColor and (ATargetInfo.BackColor = ARunInfo.BackColor);
  Result := TdxMergedParagraphProperties.Create(ATargetInfo, ATargetOptions);
end;

procedure TdxParagraphPropertiesModifier.ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex);
var
  AInfo: TdxParagraphFormattingInfo;
  AOptions: TdxParagraphFormattingOptions;
begin
  AInfo := NewValue.Info;
  AOptions := NewValue.Options;
  if AOptions.UseRightIndent then
    AParagraph.RightIndent := AInfo.RightIndent;
  if AOptions.UseLeftIndent then
    AParagraph.LeftIndent := AInfo.LeftIndent;
  if AOptions.UseFirstLineIndent then
  begin
    AParagraph.FirstLineIndentType := AInfo.FirstLineIndentType;
    AParagraph.FirstLineIndent := AInfo.FirstLineIndent;
  end;
  if AOptions.UseAlignment then
    AParagraph.Alignment := AInfo.Alignment;
  if AOptions.UseSpacingAfter then
    AParagraph.SpacingAfter := AInfo.SpacingAfter;
  if AOptions.UseSpacingBefore then
    AParagraph.SpacingBefore := AInfo.SpacingBefore;
  if AOptions.UseLineSpacing then
  begin
    AParagraph.LineSpacingType := AInfo.LineSpacingType;
    AParagraph.LineSpacing := AInfo.LineSpacing;
  end;
  if AOptions.UseSuppressHyphenation then
    AParagraph.SuppressHyphenation := AInfo.SuppressHyphenation;
  if AOptions.UseSuppressLineNumbers then
    AParagraph.SuppressLineNumbers := AInfo.SuppressLineNumbers;
  if AOptions.UseContextualSpacing then
    AParagraph.ContextualSpacing := AInfo.ContextualSpacing;
  if AOptions.UsePageBreakBefore then
    AParagraph.PageBreakBefore := AInfo.PageBreakBefore;
  if AOptions.UseBeforeAutoSpacing then
    AParagraph.BeforeAutoSpacing := AInfo.BeforeAutoSpacing;
  if AOptions.UseAfterAutoSpacing then
    AParagraph.AfterAutoSpacing := AInfo.AfterAutoSpacing;
  if AOptions.UseKeepWithNext then
    AParagraph.KeepWithNext := AInfo.KeepWithNext;
  if AOptions.UseKeepLinesTogether then
    AParagraph.KeepLinesTogether := AInfo.KeepLinesTogether;
  if AOptions.UseWidowOrphanControl then
    AParagraph.WidowOrphanControl := AInfo.WidowOrphanControl;
  if AOptions.UseOutlineLevel then
    AParagraph.OutlineLevel := AInfo.OutlineLevel;
  if AOptions.UseBackColor then
    AParagraph.BackColor := AInfo.BackColor;
end;

{ TdxTabFormattingInfoModifier }

constructor TdxTabFormattingInfoModifier.Create(ANewValue: TdxTabFormattingInfo; ADefaultTabWidth: Integer);
begin
  inherited Create(ANewValue);
  NewDefaultTabWidth := ADefaultTabWidth;
end;

function TdxTabFormattingInfoModifier.GetMergedTabInfo(ANewTabInfo, AOldOwnTabInfo,
  AStyleTabInfo: TdxTabFormattingInfo): TdxTabFormattingInfo;
var
  I: Integer;
  AItem: TdxTabInfo;
  AMerged: TdxTabFormattingInfo;
begin
  AMerged := TdxTabFormattingInfo.Merge(ANewTabInfo, AStyleTabInfo);
  try
    for I := 0 to AMerged.Count - 1 do
    begin
      AItem := AMerged[I];
      if not ANewTabInfo.Contains(AItem) and not AOldOwnTabInfo.Contains(AItem) then
        ANewTabInfo.Add(TdxTabInfo.Create(AItem.Position, AItem.Alignment, AItem.Leader, True, False));
    end;
  finally
    AMerged.Free;
  end;
  Result := ANewTabInfo.Clone;
end;

function TdxTabFormattingInfoModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxTabFormattingInfo;
begin
  Result := AParagraph.GetTabs;
end;

function TdxTabFormattingInfoModifier.Merge(const ALeftValue, ARightValue: TdxTabFormattingInfo): TdxTabFormattingInfo;
begin
  if ALeftValue.Equals(ARightValue) then
    Exit(ALeftValue.Clone);
  Result := TdxTabFormattingInfo.Create;
end;

procedure TdxTabFormattingInfoModifier.ModifyParagraph(AParagraph: TdxParagraph; AParagraphIndex: TdxParagraphIndex);
var
  AResult, AOwnTabs, AStyleTabs: TdxTabFormattingInfo;
begin
  AOwnTabs := AParagraph.GetOwnTabs;
  try
    AStyleTabs := AParagraph.ParagraphStyle.GetTabs;
    try
      AResult := GetMergedTabInfo(NewValue, AOwnTabs, AStyleTabs);
      try
        AParagraph.SetOwnTabs(AResult);
      finally
        AResult.Free;
      end;
    finally
      AStyleTabs.Free;
    end;
  finally
    AOwnTabs.Free;
  end;
  AParagraph.DocumentModel.DocumentProperties.DefaultTabWidth := NewDefaultTabWidth;
end;

{ TdxOwnTabFormattingInfoModifier }

function TdxOwnTabFormattingInfoModifier.GetParagraphPropertyValue(AParagraph: TdxParagraph): TdxTabFormattingInfo;
begin
  Result := AParagraph.GetOwnTabs;
end;

{ TdxSectionPropertyModifier }

constructor TdxSectionPropertyModifier<T>.Create(const ANewValue: T);
begin
  inherited Create;
  FNewValue := ANewValue;
end;

function TdxSectionPropertyModifier<T>.ObtainSectionsPropertyValue(ADocumentModel: TdxDocumentModel; ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): TdxNullableValue<T>;
var
  I, AFirstSectionIndex, ALastSectionIndex: TdxSectionIndex;
  AValue, ASectionValue: T;
begin
  AFirstSectionIndex := ADocumentModel.FindSectionIndex(ALogPositionStart);
  ALastSectionIndex := ADocumentModel.FindSectionIndex(ALogPositionStart + ALength - 1);
  AValue := GetSectionPropertyValue(ADocumentModel.Sections[AFirstSectionIndex]);
  for I := AFirstSectionIndex + 1 to ALastSectionIndex do
  begin
    ASectionValue := GetSectionPropertyValue(ADocumentModel.Sections[I]);
    if not TEqualityComparer<T>.Default.Equals(AValue, ASectionValue) then
      Exit(TdxNullableValue<T>.Null);
  end;
  Result := AValue;
end;

function TdxSectionPropertyModifier<T>.ObtainMergedSectionsPropertyValue(ADocumentModel: TdxDocumentModel; ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): T;
begin
  Result := Default(T);
end;

{ TdxMergedSectionPropertyModifier<T> }

function TdxMergedSectionPropertyModifier<T>.ObtainMergedSectionsPropertyValue(ADocumentModel: TdxDocumentModel;
  ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): T;
var
  I, AFirstSectionIndex, ALastSectionIndex: TdxSectionIndex;
  AValue, ANewValue, ASectionValue: T;
begin
  AFirstSectionIndex := ADocumentModel.FindSectionIndex(ALogPositionStart);
  ALastSectionIndex := ADocumentModel.FindSectionIndex(ALogPositionStart + ALength - 1);
  AValue := GetSectionPropertyValue(ADocumentModel.Sections[AFirstSectionIndex]);
  for I := AFirstSectionIndex to ALastSectionIndex do
  begin
    ASectionValue := GetSectionPropertyValue(ADocumentModel.Sections[I]);
    try
      try
        ANewValue := Merge(AValue, ASectionValue);
      finally
        //FreeAndNil(AValue);
      end;
      AValue := ANewValue;
    finally
     // FreeAndNil(ASectionValue);
    end
  end;
  Result := AValue;
end;

{ TdxSectionPageOrientationLandscapeModifier }

procedure TdxSectionPageOrientationLandscapeModifier.ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex);
var
  ASection: TdxSection;
  AWidth: Integer;
begin
  ASection := TdxSection(AValue);
  if ASection.Page.Landscape = NewValue then
    Exit;
  ASection.Page.Landscape := NewValue;
  AWidth := ASection.Page.Width;
  ASection.Page.Width := ASection.Page.Height;
  ASection.Page.Height := AWidth;
end;

function TdxSectionPageOrientationLandscapeModifier.GetSectionPropertyValue(ASection: TdxSection): Boolean;
begin
  Result := ASection.Page.Landscape;
end;

{ TdxSectionLineNumberingStepAndRestartModifier }

procedure TdxSectionLineNumberingStepAndRestartModifier.ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex);
var
  ASection: TdxSection;
begin
  ASection := TdxSection(AValue);
  ASection.LineNumbering.NumberingRestartType := NewValue;
  if NewValue = TdxLineNumberingRestart(-1) then
    ASection.LineNumbering.Step := 0
  else
    ASection.LineNumbering.Step := 1;
end;

function TdxSectionLineNumberingStepAndRestartModifier.GetSectionPropertyValue(ASection: TdxSection): TdxLineNumberingRestart;
begin
  if ASection.LineNumbering.Step > 0 then
    Result := ASection.LineNumbering.NumberingRestartType
  else
    Result := TdxLineNumberingRestart(-1);
end;

{ TdxSectionLineNumberingModifier }

{$IFNDEF DELPHIXE3}
constructor TdxSectionLineNumberingModifier.Create(const ANewValue: TdxLineNumberingInfo);
begin
  inherited;
end;
{$ENDIF}

function TdxSectionLineNumberingModifier.GetSectionPropertyValue(ASection: TdxSection): TdxLineNumberingInfo;
begin
  Result := ASection.LineNumbering.Info.Clone;
end;

procedure TdxSectionLineNumberingModifier.ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex);
var
  ASection: TdxSection;
begin
  ASection := TdxSection(AValue);
  ASection.LineNumbering.ReplaceInfo(NewValue, ASection.LineNumbering.BatchUpdateChangeActions);
end;

function TdxSectionLineNumberingModifier.Merge(const ALeftValue, ARightValue: TdxLineNumberingInfo): TdxLineNumberingInfo;
var
  AValue: TdxLineNumberingInfo;
begin
  AValue := TdxLineNumberingInfo.Create;
  AValue.CopyFrom(ARightValue);
  Result := AValue;
end;

{ TdxSectionPageSetupModifier }

constructor TdxSectionPageSetupModifier.Create(const ANewValue: TdxPageSetupInfo);
begin
  inherited Create(ANewValue);
end;

function TdxSectionPageSetupModifier.GetSectionPropertyValue(ASection: TdxSection): TdxPageSetupInfo;
var
  AMargins: TdxSectionMargins;
  APageInfo: TdxSectionPage;
  AGeneralSettings: TdxSectionGeneralSettings;
begin
  Result := TdxPageSetupInfo.Create;
  AMargins := ASection.Margins;
  Result.LeftMargin := AMargins.Left;
  Result.RightMargin := AMargins.Right;
  Result.TopMargin := AMargins.Top;
  Result.BottomMargin := AMargins.Bottom;
  APageInfo := ASection.Page;
  Result.PaperWidth := APageInfo.Width;
  Result.PaperHeight := APageInfo.Height;
  Result.PaperKind := APageInfo.PaperKind;
  Result.Landscape := APageInfo.Landscape;
  AGeneralSettings := ASection.GeneralSettings;
  Result.DifferentFirstPage := AGeneralSettings.DifferentFirstPage;
  Result.SectionStartType := AGeneralSettings.StartType;
  Result.DifferentOddAndEvenPages := TdxDocumentModel(ASection.DocumentModel).DocumentProperties.DifferentOddAndEvenPages;
end;

procedure TdxSectionPageSetupModifier.ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex);
var
  ASection: TdxSection;
  APageInfo: TdxSectionPage;
  AOldLandscape, ANewLandscape: Boolean;
  AMargins: TdxSectionMargins;
  AGeneralSettings: TdxSectionGeneralSettings;
begin
  ASection := TdxSection(AValue);
  APageInfo := ASection.Page;
  AOldLandscape := APageInfo.Landscape;
  if NewValue.Landscape.HasValue then
    APageInfo.Landscape := NewValue.Landscape.Value;
  ANewLandscape := APageInfo.Landscape;
  if NewValue.PaperKind.HasValue then
    APageInfo.PaperKind := NewValue.PaperKind.Value;
  if AOldLandscape <> ANewLandscape then
    ChangeOrientation(ASection, ANewLandscape);
  if NewValue.PaperWidth.HasValue then
    APageInfo.Width := NewValue.PaperWidth.Value;
  if NewValue.PaperHeight.HasValue then
    APageInfo.Height := NewValue.PaperHeight.Value;
  AMargins := ASection.Margins;
  if NewValue.LeftMargin.HasValue then
    AMargins.Left := NewValue.LeftMargin.Value;
  if NewValue.RightMargin.HasValue then
    AMargins.Right := NewValue.RightMargin.Value;
  if NewValue.TopMargin.HasValue then
    AMargins.Top := NewValue.TopMargin.Value;
  if NewValue.BottomMargin.HasValue then
    AMargins.Bottom := NewValue.BottomMargin.Value;
  AGeneralSettings := ASection.GeneralSettings;
  if NewValue.DifferentFirstPage.HasValue then
    AGeneralSettings.DifferentFirstPage := NewValue.DifferentFirstPage.Value;
  if NewValue.SectionStartType.HasValue then
    AGeneralSettings.StartType := NewValue.SectionStartType.Value;
  if NewValue.DifferentOddAndEvenPages.HasValue then
    TdxDocumentModel(ASection.DocumentModel).DocumentProperties.DifferentOddAndEvenPages := NewValue.DifferentOddAndEvenPages.Value;
end;

procedure TdxSectionPageSetupModifier.ChangeOrientation(ASection: TdxSection; ALandscape: Boolean);
var
  APageInfo: TdxSectionPage;
  AValue, ALeft, ARight, ATop, ABottom: Integer;
  AMargins: TdxSectionMargins;
begin
  APageInfo := ASection.Page;
  AValue := APageInfo.Width;
  APageInfo.Width := APageInfo.Height;
  APageInfo.Height := AValue;
  AMargins := ASection.Margins;
  ALeft := AMargins.Left;
  ARight := AMargins.Right;
  ATop := AMargins.Top;
  ABottom := AMargins.Bottom;
  if ALandscape then
  begin
    AMargins.Left := ATop;
    AMargins.Right := ABottom;
    AMargins.Top := ARight;
    AMargins.Bottom := ALeft;
  end
  else
  begin
    AMargins.Left := ABottom;
    AMargins.Right := ATop;
    AMargins.Top := ALeft;
    AMargins.Bottom := ARight;
  end;
end;

function TdxSectionPageSetupModifier.Merge(const ALeftValue, ARightValue: TdxPageSetupInfo): TdxPageSetupInfo;
begin
  Result := TdxPageSetupInfo.Create;
  Result.AvailableApplyType := ALeftValue.AvailableApplyType;
  Result.ApplyType := ALeftValue.ApplyType;
  if ALeftValue.Landscape = ARightValue.Landscape then
    Result.Landscape := ALeftValue.Landscape;
  if ALeftValue.LeftMargin = ARightValue.LeftMargin then
    Result.LeftMargin := ALeftValue.LeftMargin;
  if ALeftValue.RightMargin = ARightValue.RightMargin then
    Result.RightMargin := ALeftValue.RightMargin;
  if ALeftValue.TopMargin = ARightValue.TopMargin then
    Result.TopMargin := ALeftValue.TopMargin;
  if ALeftValue.BottomMargin = ARightValue.BottomMargin then
    Result.BottomMargin := ALeftValue.BottomMargin;
  if ALeftValue.PaperWidth = ARightValue.PaperWidth then
    Result.PaperWidth := ALeftValue.PaperWidth;
  if ALeftValue.PaperHeight = ARightValue.PaperHeight then
    Result.PaperHeight := ALeftValue.PaperHeight;
  if ALeftValue.PaperKind = ARightValue.PaperKind then
    Result.PaperKind := ALeftValue.PaperKind;
  if ALeftValue.SectionStartType = ARightValue.SectionStartType then
    Result.SectionStartType := ALeftValue.SectionStartType;
  if ALeftValue.DifferentFirstPage = ARightValue.DifferentFirstPage then
    Result.DifferentFirstPage := ALeftValue.DifferentFirstPage;
  if ALeftValue.DifferentOddAndEvenPages = ARightValue.DifferentOddAndEvenPages then
    Result.DifferentOddAndEvenPages := ALeftValue.DifferentOddAndEvenPages;
end;

{ TdxSectionColumnsSetupModifier }

constructor TdxSectionColumnsSetupModifier.Create(const ANewValue: TdxColumnsInfoUI);
begin
  inherited Create(ANewValue);
end;

function TdxSectionColumnsSetupModifier.GetSectionPropertyValue(ASection: TdxSection): TdxColumnsInfoUI;
var
  AColumnInfoCollection: TdxColumnInfoCollection;
  I: Integer;
begin
  Result := TdxColumnsInfoUI.Create;

  Result.PageWidth := ASection.Page.Width - (ASection.Margins.Left + ASection.Margins.Right);
  Result.EqualColumnWidth := ASection.Columns.EqualWidthColumns;
  AColumnInfoCollection := ASection.Columns.GetColumns;
  try
    if ASection.Columns.EqualWidthColumns then
      Result.ChangeColumnCount(ASection.Columns.ColumnCount)
    else
      Result.ChangeColumnCount(AColumnInfoCollection.Count);
    if ASection.Columns.EqualWidthColumns then
      Result.CalculateUniformColumnsByColumnSpacing(ASection.Columns.Space)
    else
    begin
      for I := 0 to Result.ColumnCount.Value - 1 do
      begin
        Result.Columns[I].Width := AColumnInfoCollection[I].Width;
        Result.Columns[I].Spacing := AColumnInfoCollection[I].Space;
      end;
    end;
  finally
    AColumnInfoCollection.Free;
  end;
end;

procedure TdxSectionColumnsSetupModifier.ModifySection(AValue: TObject; ASectionIndex: TdxSectionIndex);
var
  ASection: TdxSection;
  APreviousEqualColumnValue: Boolean;
  AColumnInfoCollection: TdxColumnInfoCollection;
  AColumnsInfo: TdxColumnsInfoUI;
  I: Integer;
  AColumnInfo: TdxColumnInfo;
begin
  ASection := TdxSection(AValue);
  AColumnInfoCollection := ASection.Columns.GetColumns;
  try
    ASection.Columns.ColumnCount := Max(ASection.Columns.ColumnCount, AColumnInfoCollection.Count);
    APreviousEqualColumnValue := ASection.Columns.EqualWidthColumns;
    if not NewValue.EqualColumnWidth.IsNull then
      ASection.Columns.EqualWidthColumns := NewValue.EqualColumnWidth.Value;
    if not NewValue.ColumnCount.IsNull and (NewValue.ColumnCount.Value > 0) then
      ASection.Columns.ColumnCount := NewValue.ColumnCount.Value;
    if ASection.Columns.EqualWidthColumns then
    begin
      if (NewValue.Columns.Count > 0) and not NewValue.Columns[0].Spacing.IsNull then
        ASection.Columns.Space := NewValue.Columns[0].Spacing.Value
      else
        if AColumnInfoCollection.Count > 0 then
          ASection.Columns.Space := AColumnInfoCollection[0].Space;
    end
    else
    begin

      AColumnsInfo := NewValue.Clone;
      try
        if AColumnsInfo.HasColumnsInfoUINull then
        begin
          if (((AColumnsInfo.ColumnCount.IsNull) or (AColumnsInfo.Columns.Count <= 0)) and (AColumnInfoCollection.Count = ASection.Columns.ColumnCount)) and (not APreviousEqualColumnValue) then
            Exit;
          if AColumnsInfo.EqualColumnWidth.IsNull then
            AColumnsInfo.EqualColumnWidth := ASection.Columns.EqualWidthColumns;
          if AColumnsInfo.ColumnCount.IsNull then
            AColumnsInfo.ChangeColumnCount(ASection.Columns.ColumnCount);
          AColumnsInfo.CalculateUniformColumnsByColumnSpacing(ASection.Columns.Space);
        end;
        AColumnInfoCollection.Clear;
        for I := 0 to ASection.Columns.ColumnCount - 1 do
        begin
          AColumnInfo := TdxColumnInfo.Create;
          AColumnInfo.Width := AColumnsInfo.Columns[I].Width.Value;
          AColumnInfo.Space := AColumnsInfo.Columns[I].Spacing.Value;
          AColumnInfoCollection.Add(AColumnInfo);
        end;
        ASection.Columns.SetColumns(AColumnInfoCollection);
      finally
        AColumnsInfo.Free;
      end;
    end;
  finally
    AColumnInfoCollection.Free;
  end;
end;

function TdxSectionColumnsSetupModifier.Merge(const ALeftValue, ARightValue: TdxColumnsInfoUI): TdxColumnsInfoUI;
var
  AValue: TdxColumnsInfoUI;
  I: Integer;
begin
  AValue := TdxColumnsInfoUI.Create;
  AValue.AvailableApplyType := ALeftValue.AvailableApplyType;
  AValue.ApplyType := ALeftValue.ApplyType;
  AValue.PageWidth := Math.Min(ALeftValue.PageWidth, ARightValue.PageWidth);
  if ALeftValue.EqualColumnWidth = ARightValue.EqualColumnWidth then
    AValue.EqualColumnWidth := ALeftValue.EqualColumnWidth;
  if ALeftValue.ColumnCount = ARightValue.ColumnCount then
    AValue.ColumnCount := ALeftValue.ColumnCount;
  if not AValue.ColumnCount.IsNull then
  begin
    if ALeftValue.Columns.Count <> AValue.ColumnCount.Value then
      Exit(AValue);
    if ARightValue.Columns.Count <> AValue.ColumnCount.Value then
      Exit(AValue);
    for I := 0 to AValue.ColumnCount.Value - 1 do
    begin
      if ALeftValue.Columns[I].Width <> ARightValue.Columns[I].Width then
        Exit(AValue);
      if ALeftValue.Columns[I].Spacing <> ARightValue.Columns[I].Spacing then
        Exit(AValue);
    end;
    AValue.ChangeColumnCount(ALeftValue.ColumnCount.Value);
    for I := 0 to AValue.ColumnCount.Value - 1 do
    begin
      AValue.Columns[I].Width := ALeftValue.Columns[I].Width;
      AValue.Columns[I].Spacing := ALeftValue.Columns[I].Spacing;
    end;
  end;
  Result := AValue;
end;

{ TdxFloatingObjectLayoutModifier }

constructor TdxFloatingObjectLayoutModifier.Create(const AControl: IdxRichEditControl;
  ARun: TdxFloatingObjectAnchorRun; AFloatingObjectAnchorRunIndex: TdxRunIndex);
begin
  inherited Create;
  FControl := AControl;
  FRun := ARun;
  FFloatingObjectAnchorRunIndex := AFloatingObjectAnchorRunIndex;
end;

destructor TdxFloatingObjectLayoutModifier.Destroy;
begin
  FreeAndNil(FCurrentHitTestResult);
  inherited Destroy;
end;

function TdxFloatingObjectLayoutModifier.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(FRun.DocumentModel);
end;

function TdxFloatingObjectLayoutModifier.GetActivePieceTable: TdxPieceTable;
begin
  Result := DocumentModel.ActivePieceTable;
end;

procedure TdxFloatingObjectLayoutModifier.Commit(const APhysicalPoint: TPoint);
begin
  PerformDocumentLayout(True);
  DocumentModel.BeginUpdate;
  try
    PerformDocumentLayoutCore(False);
    CommitCore(APhysicalPoint);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxFloatingObjectLayoutModifier.CommitCore(const APhysicalPoint: TPoint);
var
  ANewLogPosition: TdxDocumentLogPosition;
begin
  ANewLogPosition := CalculateLogPosition(APhysicalPoint, TdxDocumentLayoutDetailsLevel.Row);
  if not CommitCore(ANewLogPosition) then
  begin
    if ANewLogPosition < FRun.PieceTable.DocumentEndLogPosition then
      CommitCore(ANewLogPosition + 1);
  end;
end;

function TdxFloatingObjectLayoutModifier.CommitCore(ALogPosition: TdxDocumentLogPosition): Boolean;
var
  AOldPosition: TdxDocumentLogPosition;
  AItem: TdxFloatingObjectAnchorRunMovedHistoryItem;
  ARunInfo: TdxRunInfo;
begin
  Result := False;
  AOldPosition := FRun.PieceTable.GetRunLogPosition(FRun);
  ARunInfo := ActivePieceTable.FindRunInfo(ALogPosition, 1);
  try
    if ActivePieceTable.Runs[ARunInfo.Start.RunIndex] is TdxFieldResultEndRun then
      Exit;
  finally
    ARunInfo.Free;
  end;
  DocumentModel.BeforeFloatingObjectDrop(AOldPosition, ALogPosition, TdxPieceTable(FRun.PieceTable));
  AItem := CreateChangeParagraphIndexAndRunIndexHistoryItem(ALogPosition);
  try
    if AItem = nil then
      Result := ApplyInsideParagraphDrag(ALogPosition)
    else
      Result := ApplyInterParagraphDrag(AItem, ALogPosition);
  finally
    if not Result then
      AItem.Free;
  end;
end;

function TdxFloatingObjectLayoutModifier.ApplyInsideParagraphDrag(ALogPosition: TdxDocumentLogPosition): Boolean;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AFirstAffectedParagraphIndex: TdxParagraphIndex;
  AFirstAffectedParagraph: TdxParagraphBase;
  AProperties: TdxFloatingObjectPropertiesAccess;
begin
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  if not ResetHorizontalPositionAlignment then
    FRun.FloatingObjectProperties.OffsetX := FRun.FloatingObjectProperties.OffsetX + AUnitConverter.ToModelUnits(FCurrentTopLeftCorner.X - FOldTopLeftCorner.X);
  if not ResetVerticalPositionAlignment(ALogPosition) then
    FRun.FloatingObjectProperties.OffsetY := FRun.FloatingObjectProperties.OffsetY + AUnitConverter.ToModelUnits(FCurrentTopLeftCorner.Y - FOldTopLeftCorner.Y);

  AFirstAffectedParagraphIndex := Max(0, FRun.Paragraph.Index - 1);
  AFirstAffectedParagraph := FRun.PieceTable.Paragraphs[AFirstAffectedParagraphIndex];
  AProperties := TdxFloatingObjectPropertiesAccess(FRun.FloatingObjectProperties);
  ActivePieceTable.ApplyChangesCore(AProperties.BatchUpdateChangeActions,
    AFirstAffectedParagraph.FirstRunIndex, FRun.Paragraph.LastRunIndex);
  Result := True;
end;

function TdxFloatingObjectLayoutModifier.ApplyInterParagraphDrag(AItem: TdxFloatingObjectAnchorRunMovedHistoryItem; ALogPosition: TdxDocumentLogPosition): Boolean;
var
  ADocumentLayoutPosition: TdxDocumentLayoutPosition;
  AHorizontalAlignmentReset, AVerticalAlignmentReset: Boolean;
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
  AHorizontalCalculator: TdxFloatingObjectHorizontalPositionCalculator;
  AVerticalCalculator: TdxFloatingObjectVerticalPositionCalculator;
begin
  if (FCurrentHitTestResult = nil) or (not FCurrentHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row)) then
    Exit(False);
  ADocumentLayoutPosition := TdxDocumentLayoutPosition(Control.InnerControl.ActiveView.DocumentLayout.CreateLayoutPosition(ActivePieceTable, ALogPosition, FCurrentHitTestResult.Page.PageIndex));
  try
    ADocumentLayoutPosition.Update(Control.InnerControl.ActiveView.DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row);
    if FCurrentHitTestResult.Page <> ADocumentLayoutPosition.Page then
      Exit(False);
    AHorizontalAlignmentReset := ResetHorizontalPositionAlignment;
    AVerticalAlignmentReset := ResetVerticalPositionAlignment(ALogPosition);
    ApplyChangeParagraphIndexAndRunIndexHistoryItem(AItem);
    TryJoinRunsAfterFloatingObjectAnchorRunMoved(AItem);
    if not AHorizontalAlignmentReset then
    begin
      APlacementInfo := CreateFloatingObjectTargetPlacementInfo(FCurrentHitTestResult, FCurrentHitTestResult.LogicalPoint.X, 0);
      AHorizontalCalculator := TdxFloatingObjectHorizontalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
      try
        FRun.FloatingObjectProperties.OffsetX := AHorizontalCalculator.CalculateFloatingObjectOffsetX(FRun.FloatingObjectProperties.HorizontalPositionType, FCurrentHitTestResult.LogicalPoint.X, APlacementInfo);
      finally
        AHorizontalCalculator.Free;
      end;
    end;
    if not AVerticalAlignmentReset then
    begin
      APlacementInfo := CreateFloatingObjectTargetPlacementInfo(FCurrentHitTestResult, 0, ADocumentLayoutPosition.Row.Bounds.Top);
      AVerticalCalculator := TdxFloatingObjectVerticalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
      try
        FRun.FloatingObjectProperties.OffsetY := AVerticalCalculator.CalculateFloatingObjectOffsetY(FRun.FloatingObjectProperties.VerticalPositionType, FCurrentHitTestResult.LogicalPoint.Y, APlacementInfo);
      finally
        AVerticalCalculator.Free;
      end;
    end;
    Result := True;
  finally
    ADocumentLayoutPosition.Free;
  end;
end;

procedure TdxFloatingObjectLayoutModifier.TryJoinRunsAfterFloatingObjectAnchorRunMoved(AItem: TdxFloatingObjectAnchorRunMovedHistoryItem);
var
  AFirstRunIndex, ALastRunIndex: TdxRunIndex;
  ARuns: TdxTextRunCollection;
begin
  if AItem.RunIndex <= 0 then
    Exit;
  AFirstRunIndex := AItem.RunIndex - 1;
  ALastRunIndex := AItem.RunIndex;
  ARuns := FRun.PieceTable.Runs;
  if ARuns[AFirstRunIndex].CanJoinWith(ARuns[ALastRunIndex]) then
    TdxPieceTable(FRun.PieceTable).JoinTextRuns(AItem.ParagraphIndex, AFirstRunIndex);
end;

procedure TdxFloatingObjectLayoutModifier.PerformDocumentLayout(AExcludeFloatingObjectRunFromLayout: Boolean);
begin
  DocumentModel.BeginUpdate;
  try
    PerformDocumentLayoutCore(AExcludeFloatingObjectRunFromLayout);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxFloatingObjectLayoutModifier.PerformDocumentLayoutCore(AExcludeFloatingObjectRunFromLayout: Boolean);
begin
  FRun.ExcludeFromLayout := AExcludeFloatingObjectRunFromLayout;
  ActivePieceTable.ApplyChangesCore(TdxFloatingObjectPropertiesAccess(FRun.FloatingObjectProperties).BatchUpdateChangeActions,
    FRun.Paragraph.FirstRunIndex, FRun.Paragraph.LastRunIndex);
end;

function TdxFloatingObjectLayoutModifier.CreateChangeParagraphIndexAndRunIndexHistoryItem(
  ANewRunLogPosition: TdxDocumentLogPosition): TdxFloatingObjectAnchorRunMovedHistoryItem;
var
  AOldRunLogPosition: TdxDocumentLogPosition;
  ANewDocumentModelPosition, AOldDocumentModelPosition: TdxDocumentModelPosition;
begin
  AOldRunLogPosition := TdxDocumentModelPosition.FromRunStart(FRun.PieceTable, FFloatingObjectAnchorRunIndex).LogPosition;
  if AOldRunLogPosition = ANewRunLogPosition then
    Exit(nil);

  Result := TdxFloatingObjectAnchorRunMovedHistoryItem.Create(TdxPieceTable(FRun.PieceTable));
  ANewDocumentModelPosition := TdxPositionConverter.ToDocumentModelPosition(FRun.PieceTable, ANewRunLogPosition);
  if ANewDocumentModelPosition.RunOffset > 0 then
  begin
    TdxPieceTable(ANewDocumentModelPosition.PieceTable).SplitTextRun(ANewDocumentModelPosition.ParagraphIndex, ANewDocumentModelPosition.RunIndex,
      ANewDocumentModelPosition.RunOffset);
    ANewDocumentModelPosition.RunIndex := ANewDocumentModelPosition.RunIndex + 1;
    ANewDocumentModelPosition.RunStartLogPosition := ANewRunLogPosition;
  end;
  AOldDocumentModelPosition := TdxPositionConverter.ToDocumentModelPosition(FRun.PieceTable, AOldRunLogPosition);
  Result.RunIndex := AOldDocumentModelPosition.RunIndex;
  Result.ParagraphIndex := AOldDocumentModelPosition.ParagraphIndex;
  Result.NewRunIndex := ANewDocumentModelPosition.RunIndex;
  Result.NewParagraphIndex := ANewDocumentModelPosition.ParagraphIndex;
end;

function TdxFloatingObjectLayoutModifier.CalculateLogPosition(const APoint: TPoint;
  ADocumentLayoutDetailsLevel: TdxDocumentLayoutDetailsLevel): TdxDocumentLogPosition;
var
  ARequest: TdxRichEditHitTestRequest;
  APieceTable: TdxPieceTable;
  APage: TdxPage;
  ALastParagraphIndex, AParagraphIndex: TdxParagraphIndex;
  ARowStartPosition, AOldRunPosition: TdxDocumentModelPosition;
  AParagraphStartPosition: TdxDocumentLogPosition;
begin
  ARequest := TdxRichEditHitTestRequest.Create(DocumentModel.ActivePieceTable);
  ARequest.PhysicalPoint := APoint;
  ARequest.Accuracy := NearestPage or NearestPageArea or NearestColumn or NearestRow or NearestBox or
    NearestTableRow or NearestTableCell or NearestCharacter;
  ARequest.DetailsLevel := ADocumentLayoutDetailsLevel;
  ARequest.IgnoreInvalidAreas := True;

  FPageViewInfo := Control.InnerControl.ActiveView.GetPageViewInfoFromPoint(APoint, False);

  FreeAndNil(FCurrentHitTestResult);
  FCurrentHitTestResult := Control.InnerControl.ActiveView.HitTestCore(ARequest, False);
  if (FCurrentHitTestResult = nil) or not FCurrentHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row) then
    Exit(0);

  APieceTable := TdxPieceTable(FRun.PieceTable);
  APage := FPageViewInfo.Page;
  ALastParagraphIndex := APage.GetLastPosition(APieceTable).ParagraphIndex;
  ARowStartPosition := FCurrentHitTestResult.Row.GetFirstPosition(APieceTable);
  AParagraphIndex := ARowStartPosition.ParagraphIndex;
  repeat
    AParagraphStartPosition := APieceTable.Paragraphs[AParagraphIndex].LogPosition;
    if IsPageContainsLogPosition(APage, APieceTable, AParagraphStartPosition) then
      Exit(AParagraphStartPosition);
    Inc(AParagraphIndex);
  until AParagraphIndex > ALastParagraphIndex;
  AOldRunPosition := TdxDocumentModelPosition.FromRunStart(FRun.PieceTable, FFloatingObjectAnchorRunIndex);
  if IsPageContainsLogPosition(APage, APieceTable, AOldRunPosition.LogPosition) and (AOldRunPosition <= ARowStartPosition) then
    Exit(AOldRunPosition.LogPosition);
  Result := CalculatePageStartLogPosition(APage, APieceTable);
end;

function TdxFloatingObjectLayoutModifier.CalculatePageStartLogPosition(APage: TdxPage; APieceTable: TdxPieceTable): TdxDocumentLogPosition;
var
  APageIndex: Integer;
  APages: TdxPageCollection;
  APrevPage: TdxPage;
begin
  APageIndex := APage.PageIndex;
  if (APageIndex = 0) or not APieceTable.IsMain then
    Exit(0);
  APages := Control.InnerControl.ActiveView.DocumentLayout.Pages;
  APrevPage := APages[APageIndex - 1];
  Result := APrevPage.GetLastPosition(APieceTable).LogPosition + 1;
end;

function TdxFloatingObjectLayoutModifier.IsPageContainsLogPosition(APage: TdxPage; APieceTable: TdxPieceTable; ALogPosition: TdxDocumentLogPosition): Boolean;
var
  AIndex: Integer;
  APages: TdxPageCollection;
  AComparable: TdxBoxAndLogPositionComparable;
begin
  APages := Control.InnerControl.ActiveView.DocumentLayout.Pages;
  AComparable := TdxBoxAndLogPositionComparable.Create(APieceTable, ALogPosition);
  try
    TdxAlgorithms1<TdxBoxBase>.BinarySearch(APages, AComparable, AIndex);
  finally
    AComparable.Free;
  end;
  if AIndex = APages.Count then
    Dec(AIndex);
  Result := APage = APages[AIndex];
end;

procedure TdxFloatingObjectLayoutModifier.ApplyChangeParagraphIndexAndRunIndexHistoryItem(AItem: TdxFloatingObjectAnchorRunMovedHistoryItem);
var
  AChangeActions: TdxDocumentModelChangeActions;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
begin
  DocumentModel.History.Add(AItem);
  AItem.Execute;
  AChangeActions := [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout];
  AStartRunIndex := Min(FMinAffectedRunIndex, Min(AItem.RunIndex, AItem.NewRunIndex));
  AEndRunIndex := Max(AItem.RunIndex, AItem.NewRunIndex);
  ActivePieceTable.ApplyChangesCore(AChangeActions, AStartRunIndex, AEndRunIndex + 1);
  FRun.FloatingObjectProperties.LayoutInTableCell := ActivePieceTable.Paragraphs[AItem.NewParagraphIndex].IsInCell;
end;

function TdxFloatingObjectLayoutModifier.ResetHorizontalPositionAlignment: Boolean;
var
  AFloatingObjectProperties: TdxFloatingObjectProperties;
begin
  AFloatingObjectProperties := FRun.FloatingObjectProperties;
  Result := AFloatingObjectProperties.HorizontalPositionAlignment <> TdxFloatingObjectHorizontalPositionAlignment.None;
  if Result then
    SetHorizontalPositionRelatedToColumn(AFloatingObjectProperties);
end;

function TdxFloatingObjectLayoutModifier.ResetVerticalPositionAlignment(ALogPosition: TdxDocumentLogPosition): Boolean;
var
  AFloatingObjectProperties: TdxFloatingObjectProperties;
begin
  AFloatingObjectProperties := FRun.FloatingObjectProperties;
  Result := AFloatingObjectProperties.VerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.None;
  if Result then
    SetVerticalPositionRelatedToParagraph(AFloatingObjectProperties, ALogPosition);
end;

procedure TdxFloatingObjectLayoutModifier.SetHorizontalPositionRelatedToColumn(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  AFloatingObjectProperties.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment.None;
  AFloatingObjectProperties.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Column;
  if FCurrentHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Column) then
  begin
    if FCurrentHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.TableCell) and
        (FCurrentHitTestResult.TableCell <> nil) then
      AFloatingObjectProperties.OffsetX := DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(FCurrentTopLeftCorner.X - FCurrentHitTestResult.TableCell.GetBounds.Left)
    else
      AFloatingObjectProperties.OffsetX := DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(FCurrentTopLeftCorner.X - FCurrentHitTestResult.Column.Bounds.Left);
  end
  else
    AFloatingObjectProperties.OffsetX := 0;
end;

procedure TdxFloatingObjectLayoutModifier.SetVerticalPositionRelatedToParagraph(AFloatingObjectProperties: TdxFloatingObjectProperties; ALogPosition: TdxDocumentLogPosition);
var
  ADocumentLayoutPosition: TdxDocumentLayoutPosition;
  ACalculator: TdxFloatingObjectVerticalPositionCalculator;
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  AFloatingObjectProperties.VerticalPositionAlignment := TdxFloatingObjectVerticalPositionAlignment.None;
  AFloatingObjectProperties.VerticalPositionType := TdxFloatingObjectVerticalPositionType.Paragraph;
  if (FCurrentHitTestResult = nil) or not FCurrentHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Row) then
    Exit;
  ADocumentLayoutPosition := TdxDocumentLayoutPosition(Control.InnerControl.ActiveView.DocumentLayout.CreateLayoutPosition(ActivePieceTable, ALogPosition, FCurrentHitTestResult.Page.PageIndex));
  try
    ADocumentLayoutPosition.Update(Control.InnerControl.ActiveView.DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row);
    if FCurrentHitTestResult.Page <> ADocumentLayoutPosition.Page then
      Exit;
    ACalculator := TdxFloatingObjectVerticalPositionCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
    try
      APlacementInfo := CreateFloatingObjectTargetPlacementInfo(FCurrentHitTestResult, 0, ADocumentLayoutPosition.Row.Bounds.Top);
      FRun.FloatingObjectProperties.OffsetY := ACalculator.CalculateFloatingObjectOffsetY(FRun.FloatingObjectProperties.VerticalPositionType, FCurrentHitTestResult.LogicalPoint.Y, APlacementInfo);
    finally
      ACalculator.Free;
    end;
  finally
    ADocumentLayoutPosition.Free;
  end;
end;

function TdxFloatingObjectLayoutModifier.CreateFloatingObjectTargetPlacementInfo(AHitTestResult: TdxRichEditHitTestResult; AOriginX: Integer; AOriginY: Integer): TdxFloatingObjectTargetPlacementInfo;
var
  ACellBounds: TRect;
begin
  Result.Init;
  if AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.TableCell) and (AHitTestResult.TableCell <> nil) then
  begin
    ACellBounds := AHitTestResult.TableCell.GetBounds;
    Result.PageBounds := ACellBounds;
    Result.PageClientBounds := ACellBounds;
    Result.ColumnBounds := ACellBounds;
  end
  else
  begin
    Result.PageBounds := AHitTestResult.Page.Bounds;
    Result.PageClientBounds := AHitTestResult.Page.ClientBounds;
    Result.ColumnBounds := AHitTestResult.Column.Bounds;
  end;
  Result.OriginX := AOriginX;
  Result.OriginY := AOriginY;
end;

{ TdxTextInserter }

function TdxTextInserter.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
var
  AInfo: TdxLastInsertedRunInfo;
begin
  AInfo := PieceTable.LastInsertedRunInfo;
  Result := (ALogPosition = AInfo.LogPosition) and
    not (AInfo.Run is TdxLayoutDependentTextRun);
end;

{ TdxFieldSymbolResultInserter }

function TdxFieldSymbolResultInserter.CreateRunInsertedHistoryItem: TdxTextRunInsertedHistoryItem;
begin
  Result := TdxFieldSymbolResultInsertedHistoryItem.Create(PieceTable);
end;

function TdxFieldSymbolResultInserter.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
end;

{ TdxPieceTableObjectInserter }

function TdxPieceTableObjectInserter.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

{ TdxLayoutDependentTextInserter }

destructor TdxLayoutDependentTextInserter.Destroy;
begin
  FreeAndNil(FFieldResultFormatting);
  inherited Destroy;
end;

procedure TdxLayoutDependentTextInserter.AssignHistoryItemProperties(
  AItem: TdxLayoutDependentTextRunInsertedHistoryItem; AParagraph: TdxParagraphBase; AWhere: TdxRunIndex; AStartIndex,
  ALength: Integer; AForceVisible: Boolean);
begin
  AItem.ForceVisible := AForceVisible;
  AItem.RunIndex := AWhere;
  AItem.StartIndex := AStartIndex;
  AItem.NewLength := ALength;
  AItem.ParagraphIndex := AParagraph.Index;
  AItem.FieldResultFormatting := FieldResultFormatting;
end;

function TdxLayoutDependentTextInserter.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
end;

function TdxLayoutDependentTextInserter.CreateRunInsertedHistoryItem: TdxLayoutDependentTextRunInsertedHistoryItem;
begin
  Result := TdxLayoutDependentTextRunInsertedHistoryItem.Create(PieceTable);
end;

procedure TdxLayoutDependentTextInserter.InsertLayoutDependentTextRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex;
  AStartIndex, ALength: Integer; AForceVisible: Boolean);
var
  AItem: TdxLayoutDependentTextRunInsertedHistoryItem;
begin
  AItem := CreateRunInsertedHistoryItem;
  AssignHistoryItemProperties(AItem, AParagraph, AWhere, AStartIndex, ALength, AForceVisible);
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxLayoutDependentTextInserter.SetFieldResultFormatting(const Value: TdxFieldResultFormatting);
begin
  FFieldResultFormatting.Free;
  FFieldResultFormatting := Value;
end;

procedure TdxLayoutDependentTextInserter.Merge(ALogPosition: TdxDocumentLogPosition;
  AParagraphIndex: TdxParagraphIndex);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxLayoutDependentTextInserter.PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  AOldGrowBufferLength: Integer;
begin
  AOldGrowBufferLength := PieceTable.TextBuffer.Length - TextLength;
  InsertLayoutDependentTextRun(AParagraph, ARunIndex, AOldGrowBufferLength, TextLength, AForceVisible);
  PieceTable.LastInsertedRunInfo.LogPosition := ALogPosition + TextLength;
end;

{ TdxSectionInserter }

function TdxSectionInserter.CreateInsertParagraphRangeHistoryItem: TdxParagraphRunInsertedHistoryItem;
begin
  Result := TdxSectionRunInsertedHistoryItem.Create(PieceTable);
end;

{ TdxFloatingObjectAnchorInserter }

function TdxFloatingObjectAnchorInserter.CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
end;

constructor TdxFloatingObjectAnchorInserter.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  TextLength := 1;
end;

procedure TdxFloatingObjectAnchorInserter.InsertFloatingObjectAnchorRun(AParagraph: TdxParagraphBase; AWhere: TdxRunIndex;
  AStartIndex: Integer; AForceVisible: Boolean);
var
  AItem: TdxFloatingObjectAnchorRunInsertedHistoryItem;
begin
  AItem := TdxFloatingObjectAnchorRunInsertedHistoryItem.Create(PieceTable);
  AItem.ForceVisible := AForceVisible;
  AItem.RunIndex := AWhere;
  AItem.StartIndex := AStartIndex;
  AItem.ParagraphIndex := AParagraph.Index;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxFloatingObjectAnchorInserter.Merge(ALogPosition: TdxDocumentLogPosition;
  AParagraphIndex: TdxParagraphIndex);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxFloatingObjectAnchorInserter.PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
var
  AOldGrowBufferLength: Integer;
begin
  AOldGrowBufferLength := PieceTable.TextBuffer.Length - 1;
  InsertFloatingObjectAnchorRun(AParagraph, ARunIndex, AOldGrowBufferLength, AForceVisible);
end;

{ TdxFootNoteRunInserterBase }

procedure TdxFootNoteRunInserterBase.AssignHistoryItemProperties(AItem: TdxLayoutDependentTextRunInsertedHistoryItem;
  AParagraph: TdxParagraphBase; AWhere: TdxRunIndex; AStartIndex: Integer; ALength: Integer; AForceVisible: Boolean);
var
  AFootNoteInsertedHistoryItem: TdxFootNoteRunInsertedHistoryItemBase;
begin
  inherited AssignHistoryItemProperties(AItem, AParagraph, AWhere, AStartIndex, ALength, AForceVisible);
  AFootNoteInsertedHistoryItem := TdxFootNoteRunInsertedHistoryItemBase(AItem);
  AFootNoteInsertedHistoryItem.NoteIndex := NoteIndex;
end;

{ TdxFootNoteRunInserter }

function TdxFootNoteRunInserter.CreateRunInsertedHistoryItem: TdxLayoutDependentTextRunInsertedHistoryItem;
begin
  Result := TdxFootNoteRunInsertedHistoryItem.Create(PieceTable);
end;

{ TdxEndNoteRunInserter }

function TdxEndNoteRunInserter.CreateRunInsertedHistoryItem: TdxLayoutDependentTextRunInsertedHistoryItem;
begin
  Result := TdxEndNoteRunInsertedHistoryItem.Create(PieceTable);
end;

end.
