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

unit dxRichEdit.Actions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, ActnList, Classes, Controls, ImgList, Graphics, Generics.Defaults, Generics.Collections,
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  dxCore, dxCoreGraphics, dxActions, dxPrinting, cxGraphics, dxGallery,

  dxRichEdit.Control,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.NativeApi;

type
  TdxRichEditControlAction = class;

  { TdxRichEditControlAction }

  TdxRichEditControlAction = class abstract(TdxBasicAction)
  strict private
    function GetRichEditControl: TdxCustomRichEditControl;
    procedure ReadLargeImageIndex(Reader: TReader);
  protected
    function GetDefaultCaption: string; override;
    function GetDefaultHint: string; override;
    //
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoUpdateState; override;
    procedure DoResetState; override;
    procedure UpdateControl(Target: TObject); override;
    //
    procedure CopyFrom(const AState: IdxCommandUIState); virtual;
    procedure CopyTo(const AState: IdxCommandUIState); virtual;
    function CreateCommand(ATarget: TdxCustomRichEditControl): TdxCommand;
    function DoCreateCommand(ATarget: TdxCustomRichEditControl): TdxCommand; virtual;
    procedure DoExecute(ACommand: TdxCommand); virtual;
    function GetCommandClass: TdxControlCommandClass; virtual; abstract;
    function GetState(ACommand: TdxCommand): IdxCommandUIState;
    //
    property RichEditControl: TdxCustomRichEditControl read GetRichEditControl;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

  { TdxRichEditControlValueAction }

  TdxRichEditControlValueAction<T> = class abstract(TdxRichEditControlAction, IdxActionValue)
  strict private
    FValue: T;

    // IdxActionValue
    function IdxActionValue.GetValue = GetValueAsVariant;
    procedure IdxActionValue.SetValue = SetValueAsVariant;

    procedure SetValue(const AValue: T);
  protected
    procedure CopyFrom(const AState: IdxCommandUIState); override;
    procedure CopyTo(const AState: IdxCommandUIState); override;
    procedure DoExecute(ACommand: TdxCommand); override;
    function GetValueAsVariant: Variant; virtual; abstract;
    procedure SetValueAsVariant(const AValue: Variant); virtual; abstract;

    class function IsValueEquals(const AValue1, AValue2: T): Boolean; virtual;
  public
    property Value: T read FValue write SetValue;
  end;

  { TdxRichEditControlAlphaColorValueAction }

  TdxRichEditControlAlphaColorValueAction = class abstract(TdxRichEditControlValueAction<TdxAlphaColor>,
    IdxActionColorValue)
  protected
    function GetValueAsVariant: Variant; override;
    procedure SetValueAsVariant(const AValue: Variant); override;
    class function IsValueEquals(const AValue1, AValue2: TdxAlphaColor): Boolean; override;
  end;

  { TdxRichEditControlSingleValueAction }

  TdxRichEditControlSingleValueAction = class abstract(TdxRichEditControlValueAction<Single>)
  protected
    function GetValueAsVariant: Variant; override;
    procedure SetValueAsVariant(const AValue: Variant); override;
    class function IsValueEquals(const AValue1, AValue2: Single): Boolean; override;
  end;

  { TdxRichEditControlStringValueAction }

  TdxRichEditControlStringValueAction = class abstract(TdxRichEditControlValueAction<string>)
  protected
    function GetValueAsVariant: Variant; override;
    procedure SetValueAsVariant(const AValue: Variant); override;
    class function IsValueEquals(const AValue1, AValue2: string): Boolean; override;
  end;

  { TdxRichEditControlIntegerValueAction }

  TdxRichEditControlIntegerValueAction = class abstract(TdxRichEditControlValueAction<Integer>)
  protected
    function GetValueAsVariant: Variant; override;
    procedure SetValueAsVariant(const AValue: Variant); override;
    class function IsValueEquals(const AValue1, AValue2: Integer): Boolean; override;
  end;

  { TdxRichEditControlNewDocument }

  TdxRichEditControlNewDocument = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlLoadDocument }

  TdxRichEditControlLoadDocument = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSaveDocument }

  TdxRichEditControlSaveDocument = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSaveDocumentAs }

  TdxRichEditControlSaveDocumentAs = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleShowHorizontalRuler }

  TdxRichEditControlToggleShowHorizontalRuler = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleShowVerticalRuler }

  TdxRichEditControlToggleShowVerticalRuler = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlZoomIn }

  TdxRichEditControlZoomIn = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlZoomOut }

  TdxRichEditControlZoomOut = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlZoomPercent }

  TdxRichEditControlZoomPercent = class(TdxRichEditControlSingleValueAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlChangePageColor }

  TdxRichEditControlChangePageColor = class(TdxRichEditControlAlphaColorValueAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSwitchToDraftView }

  TdxRichEditControlSwitchToDraftView = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSwitchToSimpleView }

  TdxRichEditControlSwitchToSimpleView = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSwitchToPrintLayoutView }

  TdxRichEditControlSwitchToPrintLayoutView = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleFontBold }

  TdxRichEditControlToggleFontBold = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleFontItalic }

  TdxRichEditControlToggleFontItalic = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleFontUnderline }

  TdxRichEditControlToggleFontUnderline = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleFontDoubleUnderline }

  TdxRichEditControlToggleFontDoubleUnderline = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleFontStrikeout }

  TdxRichEditControlToggleFontStrikeout = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleFontDoubleStrikeout }

  TdxRichEditControlToggleFontDoubleStrikeout = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleParagraphAlignmentLeft }

  TdxRichEditControlToggleParagraphAlignmentLeft = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleParagraphAlignmentCenter }

  TdxRichEditControlToggleParagraphAlignmentCenter = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleParagraphAlignmentRight }

  TdxRichEditControlToggleParagraphAlignmentRight = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleParagraphAlignmentJustify }

  TdxRichEditControlToggleParagraphAlignmentJustify = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlCopySelection }

  TdxRichEditControlCopySelection = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlPasteSelection }

  TdxRichEditControlPasteSelection = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlCutSelection }

  TdxRichEditControlCutSelection = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetSingleParagraphSpacing }

  TdxRichEditControlSetSingleParagraphSpacing = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSetDoubleParagraphSpacing }

  TdxRichEditControlSetDoubleParagraphSpacing = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSetSesquialteralParagraphSpacing }

  TdxRichEditControlSetSesquialteralParagraphSpacing = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlUndo }

  TdxRichEditControlUndo = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlRedo }

  TdxRichEditControlRedo = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlIncreaseFontSize }

  TdxRichEditControlIncreaseFontSize = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlDecreaseFontSize }

  TdxRichEditControlDecreaseFontSize = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleFontSuperscript }

  TdxRichEditControlToggleFontSuperscript = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleFontSubscript }

  TdxRichEditControlToggleFontSubscript = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleBulletedList }

  TdxRichEditControlToggleBulletedList = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleSimpleNumberingList }

  TdxRichEditControlToggleSimpleNumberingList = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlToggleMultiLevelList }

  TdxRichEditControlToggleMultiLevelList = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlChangeSectionLineNumbering }

  TdxRichEditControlChangeSectionLineNumbering = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetSectionLineNumberingNone }

  TdxRichEditControlSetSectionLineNumberingNone = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSetSectionLineNumberingContinuous }

  TdxRichEditControlSetSectionLineNumberingContinuous = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSetSectionLineNumberingRestartNewPage }

  TdxRichEditControlSetSectionLineNumberingRestartNewPage = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
   public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
 end;

  { TdxRichEditControlSetSectionLineNumberingRestartNewSection }

  TdxRichEditControlSetSectionLineNumberingRestartNewSection = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSelectAll }

  TdxRichEditControlSelectAll = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleShowWhitespace }

  TdxRichEditControlToggleShowWhitespace = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlIncrementIndent }

  TdxRichEditControlIncrementIndent = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlDecrementIndent }

  TdxRichEditControlDecrementIndent = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowFontForm }

  TdxRichEditControlShowFontForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowParagraphForm }

  TdxRichEditControlShowParagraphForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowNumberingForm }

  TdxRichEditControlShowNumberingForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowInsertTableForm }

  TdxRichEditControlShowInsertTableForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlDeleteTable }

  TdxRichEditControlDeleteTable = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowSplitTableCellsForm }

  TdxRichEditControlShowSplitTableCellsForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowInsertTableCellsForm }

  TdxRichEditControlShowInsertTableCellsForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowDeleteTableCellsForm }

  TdxRichEditControlShowDeleteTableCellsForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowTablePropertiesForm }

  TdxRichEditControlShowTablePropertiesForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowSymbolForm }

  TdxRichEditControlShowSymbolForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowHyperlinkForm }

  TdxRichEditControlShowHyperlinkForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowColumnsSetupForm }

  TdxRichEditControlShowColumnsSetupForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowEditStyleForm }

  TdxRichEditControlShowEditStyleForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowTableStyleForm }

  TdxRichEditControlShowTableStyleForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlChangeFontName }

  TdxRichEditControlChangeFontName = class(TdxRichEditControlStringValueAction, IdxActionFontNameValue)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlChangeFontSize }

  TdxRichEditControlChangeFontSize = class(TdxRichEditControlSingleValueAction, IdxActionFontSizeValue)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlChangeFontColor }

  TdxRichEditControlChangeFontColor = class(TdxRichEditControlAlphaColorValueAction, IdxActionColorValue)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTextHighlight }

  TdxRichEditControlTextHighlight = class(TdxRichEditControlAlphaColorValueAction, IdxActionColorValue)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertPicture }

  TdxRichEditControlInsertPicture = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSearchFind }

  TdxRichEditControlSearchFind = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSearchReplace }

  TdxRichEditControlSearchReplace = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSearchFind }

  TdxRichEditControlSearchFindNext = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSplitTable }

  TdxRichEditControlSplitTable = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlMergeTableCells }

  TdxRichEditControlMergeTableCells = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableRowBelow }

  TdxRichEditControlInsertTableRowBelow = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableRowAbove }

  TdxRichEditControlInsertTableRowAbove = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableColumnToTheLeft }

  TdxRichEditControlInsertTableColumnToTheLeft = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableColumnToTheRight }

  TdxRichEditControlInsertTableColumnToTheRight = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlDeleteTableRows }

  TdxRichEditControlDeleteTableRows = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlDeleteTableColumns }

  TdxRichEditControlDeleteTableColumns = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsAllBorders }

  TdxRichEditControlToggleTableCellsAllBorders = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlResetTableCellsBorders }

  TdxRichEditControlResetTableCellsBorders = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsOutsideBorder }

  TdxRichEditControlToggleTableCellsOutsideBorder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsInsideBorder }

  TdxRichEditControlToggleTableCellsInsideBorder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsLeftBorder }

  TdxRichEditControlToggleTableCellsLeftBorder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsRightBorder }

  TdxRichEditControlToggleTableCellsRightBorder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsTopBorder }

  TdxRichEditControlToggleTableCellsTopBorder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsBottomBorder }

  TdxRichEditControlToggleTableCellsBottomBorder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsInsideHorizontalBorder }

  TdxRichEditControlToggleTableCellsInsideHorizontalBorder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsInsideVerticalBorder }

  TdxRichEditControlToggleTableCellsInsideVerticalBorder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsTopLeftAlignment}

  TdxRichEditControlToggleTableCellsTopLeftAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsTopCenterAlignment }

  TdxRichEditControlToggleTableCellsTopCenterAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsTopRightAlignment }

  TdxRichEditControlToggleTableCellsTopRightAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsMiddleLeftAlignment }

  TdxRichEditControlToggleTableCellsMiddleLeftAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsMiddleCenterAlignment }

  TdxRichEditControlToggleTableCellsMiddleCenterAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsMiddleRightAlignment }

  TdxRichEditControlToggleTableCellsMiddleRightAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsBottomLeftAlignment }

  TdxRichEditControlToggleTableCellsBottomLeftAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsBottomCenterAlignment }

  TdxRichEditControlToggleTableCellsBottomCenterAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableCellsBottomRightAlignment }

  TdxRichEditControlToggleTableCellsBottomRightAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableAutoFitContents }

  TdxRichEditControlToggleTableAutoFitContents = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableAutoFitWindow }

  TdxRichEditControlToggleTableAutoFitWindow = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTableFixedColumnWidth }

  TdxRichEditControlToggleTableFixedColumnWidth = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleShowTableGridLines }

  TdxRichEditControlToggleShowTableGridLines = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlInsertPageBreak }

  TdxRichEditControlInsertPageBreak = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertSectionBreakNextPage}

  TdxRichEditControlInsertSectionBreakNextPage = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertSectionBreakOddPage }

  TdxRichEditControlInsertSectionBreakOddPage = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertSectionBreakEvenPage }

  TdxRichEditControlInsertSectionBreakEvenPage = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertColumnBreak }

  TdxRichEditControlInsertColumnBreak = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTextUpperCase }

  TdxRichEditControlTextUpperCase = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTextLowerCase }

  TdxRichEditControlTextLowerCase = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleTextCase }

  TdxRichEditControlToggleTextCase = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetSectionOneColumn }

  TdxRichEditControlSetSectionOneColumn = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSetSectionTwoColumns }

  TdxRichEditControlSetSectionTwoColumns = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSetSectionThreeColumns }

  TdxRichEditControlSetSectionThreeColumns = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlInsertFloatingObjectPicture }

  TdxRichEditControlInsertFloatingObjectPicture = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTextBox }

  TdxRichEditControlInsertTextBox = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowParagraphForm }

  TdxRichEditControlShowFloatingObjectLayoutOptionsForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectSquareTextWrapType }

  TdxRichEditControlSetFloatingObjectSquareTextWrapType = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectBehindTextWrapType }

  TdxRichEditControlSetFloatingObjectBehindTextWrapType = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectInFrontOfTextWrapType }

  TdxRichEditControlSetFloatingObjectInFrontOfTextWrapType = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectThroughTextWrapType }

  TdxRichEditControlSetFloatingObjectThroughTextWrapType = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectTightTextWrapType }

  TdxRichEditControlSetFloatingObjectTightTextWrapType = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectTopAndBottomTextWrapType }

  TdxRichEditControlSetFloatingObjectTopAndBottomTextWrapType = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectTopLeftAlignment }

  TdxRichEditControlSetFloatingObjectTopLeftAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectTopCenterAlignment }

  TdxRichEditControlSetFloatingObjectTopCenterAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectTopRightAlignment }

  TdxRichEditControlSetFloatingObjectTopRightAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectMiddleLeftAlignment }

  TdxRichEditControlSetFloatingObjectMiddleLeftAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectMiddleCenterAlignment }

  TdxRichEditControlSetFloatingObjectMiddleCenterAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectMiddleRightAlignment }

  TdxRichEditControlSetFloatingObjectMiddleRightAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectBottomLeftAlignment }

  TdxRichEditControlSetFloatingObjectBottomLeftAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectBottomCenterAlignment }

  TdxRichEditControlSetFloatingObjectBottomCenterAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetFloatingObjectBottomRightAlignment }

  TdxRichEditControlSetFloatingObjectBottomRightAlignment = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlFloatingObjectBringForward }

  TdxRichEditControlFloatingObjectBringForward = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlFloatingObjectBringToFront }

  TdxRichEditControlFloatingObjectBringToFront = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlFloatingObjectBringInFrontOfText }

  TdxRichEditControlFloatingObjectBringInFrontOfText = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlFloatingObjectSendBackward }

  TdxRichEditControlFloatingObjectSendBackward = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlFloatingObjectSendToBack }

  TdxRichEditControlFloatingObjectSendToBack = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlFloatingObjectSendBehindText }

  TdxRichEditControlFloatingObjectSendBehindText = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlChangeFloatingObjectFillColor }

  TdxRichEditControlChangeFloatingObjectFillColor = class(
    TdxRichEditControlAlphaColorValueAction, IdxActionColorValue)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlChangeFloatingObjectOutlineColor }

  TdxRichEditControlChangeFloatingObjectOutlineColor = class(
    TdxRichEditControlAlphaColorValueAction, IdxActionColorValue)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlChangeFloatingObjectOutlineWidthCommand }

  TdxRichEditControlChangeFloatingObjectOutlineWidth = class(TdxRichEditControlIntegerValueAction)
  private
    function GetValueAsPoints: Single;
    procedure SetValueAsPoints(const AValue: Single);
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    property ValueAsPoints: Single read GetValueAsPoints write SetValueAsPoints;
  end;

  { TdxRichEditControlEditPageHeader }

  TdxRichEditControlEditPageHeader = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlEditPageFooter }

  TdxRichEditControlEditPageFooter = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlClosePageHeaderFooter }

  TdxRichEditControlClosePageHeaderFooter = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlGoToPageHeader }

  TdxRichEditControlGoToPageHeader = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlGoToPageFooter }

  TdxRichEditControlGoToPageFooter = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleHeaderFooterLinkToPrevious }

  TdxRichEditControlToggleHeaderFooterLinkToPrevious = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlGoToPreviousPageHeaderFooter }

  TdxRichEditControlGoToPreviousPageHeaderFooter = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlGoToNextPageHeaderFooter }

  TdxRichEditControlGoToNextPageHeaderFooter = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleDifferentFirstPage }

  TdxRichEditControlToggleDifferentFirstPage = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleDifferentOddAndEvenPages }

  TdxRichEditControlToggleDifferentOddAndEvenPages = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowLineNumberingForm }

  TdxRichEditControlShowLineNumberingForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowAllFieldResults }

  TdxRichEditControlShowAllFieldResults = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowAllFieldCodes }

  TdxRichEditControlShowAllFieldCodes = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleFieldCodes }

  TdxRichEditControlToggleFieldCodes = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlCreateField }

  TdxRichEditControlCreateField = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlUpdateField }

  TdxRichEditControlUpdateField = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlUpdateField }

  TdxRichEditControlUpdateFields = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowPageSetupForm }

  TdxRichEditControlShowPageSetupForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowPageMarginsSetupForm }

  TdxRichEditControlShowPageMarginsSetupForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowPagePaperSetupForm }

  TdxRichEditControlShowPagePaperSetupForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlSetPortraitPageOrientation }

  TdxRichEditControlSetPortraitPageOrientation = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlSetLandscapePageOrientation }

  TdxRichEditControlSetLandscapePageOrientation = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxRichEditControlOpenHyperlink }

  TdxRichEditControlOpenHyperlink = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlRemoveHyperlinkField }

  TdxRichEditControlRemoveHyperlinkField = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertPageCountField }

  TdxRichEditControlInsertPageCountField = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertPageNumberField }

  TdxRichEditControlInsertPageNumberField = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowInsertMergeFieldForm }

  TdxRichEditControlShowInsertMergeFieldForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowBookmarkForm }

  TdxRichEditControlShowBookmarkForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlToggleViewMergedData }

  TdxRichEditControlToggleViewMergedData = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowMergeDatabaseRecordsForm }

  TdxRichEditControlShowMergeDatabaseRecordsForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlCheckSpelling }

  TdxRichEditControlCheckSpelling = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableOfContents }

  TdxRichEditControlInsertTableOfContents = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlUpdateTableOfContents }

  TdxRichEditControlUpdateTableOfContents = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlAddParagraphsToTableOfContentsPlaceholder }

  TdxRichEditControlAddParagraphsToTableOfContentsPlaceholder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphBodyTextLevel }

  TdxRichEditControlTableOfContentsSetParagraphBodyTextLevel = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading1Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading1Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading2Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading2Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading3Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading3Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading4Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading4Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading5Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading5Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading6Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading6Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading7Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading7Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading8Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading8Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlTableOfContentsSetParagraphHeading9Level }

  TdxRichEditControlTableOfContentsSetParagraphHeading9Level = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowTableOfContentsForm }

  TdxRichEditControlShowTableOfContentsForm = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableOfFiguresPlaceholder }

  TdxRichEditControlInsertTableOfFiguresPlaceholder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableOfFigures }

  TdxRichEditControlInsertTableOfFigures = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlUpdateTableOfFigures }

  TdxRichEditControlUpdateTableOfFigures = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertCaptionPlaceholder }

  TdxRichEditControlInsertCaptionPlaceholder = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertFigureCaption }

  TdxRichEditControlInsertFigureCaption = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableOfTables }

  TdxRichEditControlInsertTableOfTables = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableCaption }

  TdxRichEditControlInsertTableCaption = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertTableOfEquations }

  TdxRichEditControlInsertTableOfEquations = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlInsertEquationCaption }

  TdxRichEditControlInsertEquationCaption = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlProtectDocument }

  TdxRichEditControlProtectDocument = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlUnprotectDocument }

  TdxRichEditControlUnprotectDocument = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlShowRangeEditingPermissions }

  TdxRichEditControlShowRangeEditingPermissions = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxRichEditControlEncryptDocument }

  TdxRichEditControlEncryptDocument = class(TdxRichEditControlAction)
  protected
    function GetCommandClass: TdxControlCommandClass; override;
  end;

  { TdxCustomRichEditControlPrintingAction }

  TdxCustomRichEditControlPrintingAction = class(TdxCustomPrintingAction)
  strict private
    function GetRichEditControl: TdxCustomRichEditControl;
    procedure ReadLargeImageIndex(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoUpdateState; override;
    function IsEnabled: Boolean; override;
    function GetControlClass: TWinControlClass; override;
    //
    property RichEditControl: TdxCustomRichEditControl read GetRichEditControl;
  end;

  { TdxRichEditControlShowPrintForm }

  TdxRichEditControlShowPrintForm = class(TdxCustomRichEditControlPrintingAction)
  protected
    function GetDefaultCaption: string; override;
    function GetDefaultHint: string; override;
    procedure DoExecute(AControl: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxRichEditControlShowPrintPreviewForm }

  TdxRichEditControlShowPrintPreviewForm = class(TdxCustomRichEditControlPrintingAction)
  protected
    function GetDefaultCaption: string; override;
    function GetDefaultHint: string; override;
    procedure DoExecute(AControl: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxRichEditControlCustomGalleryAction }

  TdxRichEditControlCustomGalleryAction = class(TdxRichEditControlAction, IdxActionGalleryClient)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // IdxActionGalleryClient
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const AValue: Variant); virtual; abstract;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); virtual; abstract;

    procedure CreateGalleryCommands; virtual; abstract;
    procedure DestroyGalleryCommands; virtual; abstract;
  public
    destructor Destroy; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { TdxRichEditControlPaperSizeGallery }

  TdxRichEditControlPaperSizeGallery = class(TdxRichEditControlCustomGalleryAction)
  protected const
    CDefaultPaperKindList: array [0 .. 9] of TdxPaperKind = (TdxPaperKind.Letter, TdxPaperKind.Tabloid,
      TdxPaperKind.Legal, TdxPaperKind.Statement, TdxPaperKind.Executive, TdxPaperKind.A3, TdxPaperKind.A4,
      TdxPaperKind.A5, TdxPaperKind.B4, TdxPaperKind.B5);
  strict private
    FCommand: TdxCommand;
  protected
    function GetCommandClass: TdxControlCommandClass; override;

    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;

    procedure CreateGalleryCommands; override;
    procedure DestroyGalleryCommands; override;
  end;

  { TdxRichEditControlPageMarginsGallery }

  TdxRichEditControlPageMarginsGallery = class(TdxRichEditControlCustomGalleryAction)
  protected type
    TMarginsPart = (mpTop, mpBottom, mpLeft, mpRight);
    TMarginsSet = array [TMarginsPart] of Single;
    TPredefinedMarginsSet = (pmsNormal, pmsNarrow, pmsModerate, pmsWide);
  protected const
    CPredefinedModerateMarginsSet: TMarginsSet = (1, 1, 0.75, 0.75);
    CPredefinedNarrowMarginsSet: TMarginsSet = (0.5, 0.5, 0.5, 0.5);
    CPredefinedNormalMarginsSet: TMarginsSet = (0.79, 0.79, 1.18, 0.59);
    CPredefinedWideMarginsSet: TMarginsSet = (1, 1, 2, 2);
  strict private
    FCommand: TdxCommand;
  protected
    function GetCommandClass: TdxControlCommandClass; override;

    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;

    procedure CreateGalleryCommands; override;
    procedure DestroyGalleryCommands; override;
  end;

  { TdxRichEditControlCustomStylesGalleryAction }

  TdxRichEditControlCustomStylesGalleryAction = class(TdxRichEditControlCustomGalleryAction)
  strict private
    procedure ReadGalleryGroup(Reader: TReader);
    procedure WriteGalleryGroup(Writer: TWriter);
  protected
    FGalleryGroup: TdxCustomGalleryGroup;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // IdxActionGalleryClient
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;

    procedure CreateGalleryCommands; override;
    procedure DestroyGalleryCommands; override;

    procedure AddGalleryGroupItem(AStyle: TdxStyleBase); virtual;
    function CanAddGalleryGroupItem(AStyle: TdxStyleBase): Boolean; virtual;
    function CanDeleteGalleryGroupItem(AGalleryGroupItem: TdxCustomGalleryItem; AStyleType: TdxStyleType): Boolean; virtual;
    function CreateStylePreview(AStyle: TdxStyleBase): TcxAlphaBitmap; virtual; abstract;
    function IsGalleryGroupItemForStyleExists(AStyle: TdxStyleBase): Boolean; virtual;
    function IsGalleryGroupUpdateNeeded: Boolean; virtual; abstract;
    function IsStyleOfGalleryGroupItemExists(AGalleryGroupItem: TdxCustomGalleryItem; AStyleType: TdxStyleType): Boolean; virtual;
    function IsStylesUpdateNeeded(AStyleType: TdxStyleType): Boolean; virtual;
    function GetStylesBy(AStyleType: TdxStyleType): TdxStyleCollectionBase; virtual;
    procedure PopulateStyles(AStyleType: TdxStyleType); virtual;
    procedure UpdateGalleryGroup; virtual; abstract;
  public
    procedure Assign(Source: TPersistent); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { TdxRichEditControlQuickStylesGallery }

  TdxRichEditControlQuickStylesGallery = class(TdxRichEditControlCustomStylesGalleryAction)
  strict private
    procedure ApplyStyleToFont(AStyle: TdxStyleBase; AFont: TFont);
    function GetCharacterProperties(AStyle: TdxStyleBase): TdxCharacterProperties;

    function GetActiveStyle: TdxStyleBase;
    function SetActiveStyle(AStyle: TdxStyleBase): Boolean;
  protected
    function GetCommandClass: TdxControlCommandClass; override;

    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;

    function CanAddGalleryGroupItem(AStyle: TdxStyleBase): Boolean; override;
    function CreateStylePreview(AStyle: TdxStyleBase): TcxAlphaBitmap; override;
    function IsGalleryGroupUpdateNeeded: Boolean; override;
    procedure UpdateGalleryGroup; override;
  end;

  { TdxRichEditControlTableStylesGallery }

  TdxRichEditControlTableStylesGallery = class(TdxRichEditControlCustomStylesGalleryAction)
  protected type
    TdxProcessSelectedTablesProcRef = reference to procedure (AFullySelectedTables: IdxRichEditReadOnlyTableCollection;
      AStartTable, AEndTable: IdxRichEditTable);
  strict private
    FTempTable: TdxTable;
    FTempRichEditControl: TdxRichEditControl;

    procedure DrawBordersCore(ACanvas: TCanvas; ACell: TdxTableCell;
      const ACellWidth, ACellHeight, ALeftImageLocation, ATopImageLocation: Integer);
    procedure DrawStyleBackgroundCore(ACanvas: TCanvas; ACell: TdxTableCell;
      const ACellWidth, ACellHeight, ALeftImageLocation, ATopImageLocation: Integer);

    procedure ForEachSelection(AProcessSelectedTablesProcRef: TdxProcessSelectedTablesProcRef);
    function GetActiveStyleName: string;
    procedure SetActiveStyleName(const AStyleName: string);
  protected
    function GetCommandClass: TdxControlCommandClass; override;

    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;

    function CreateStylePreview(AStyle: TdxStyleBase): TcxAlphaBitmap; override;
    function IsGalleryGroupUpdateNeeded: Boolean; override;
    procedure UpdateGalleryGroup; override;
  end;

implementation

uses
  Windows,
  Math, Variants, dxMeasurementUnits, dxPrintUtils, cxGeometry, cxControls,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Columns,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.Commands.FileOperations,
  dxRichEdit.Commands.Numbering,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.Dialogs,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.FindAndReplace,
  dxRichEdit.Commands.Tables,
  dxRichEdit.Commands.Tables.Cells,
  dxRichEdit.Commands.Hyperlink,
  dxRichEdit.Commands.FloatingObject,
  dxRichEdit.Commands.HeaderFooter,
  dxRichEdit.Commands.SpellChecker,
  dxRichEdit.Commands.Fields,
  dxRichEdit.Commands.Bookmarks,
  dxRichEdit.Commands.TableOfContents,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.Images,
  dxRichEdit.Dialogs.Utils,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.Utils.Types;

type
  TdxProcessPaperSizeGalleryCommandAccess = class(TdxProcessPaperSizeGalleryCommand);
  TdxProcessPageMarginsGalleryCommandAccess = class(TdxProcessPageMarginsGalleryCommand);
  TdxCustomGalleryItemAccess = class(TdxCustomGalleryItem);
  TdxSimpleDocumentModelAccess = class(TdxSimpleDocumentModel);
  TdxNativeDocumentAccess = class(TdxNativeDocument);

{ TdxRichEditControlAction }

procedure TdxRichEditControlAction.ExecuteTarget(Target: TObject);
var
  ACommand: TdxCommand;
begin
  UpdateControl(Target);
  if Control <> nil then
  begin
    ACommand := CreateCommand(RichEditControl);
    try
      DoExecute(ACommand);
    finally
      ACommand.Free;
    end;
  end;
end;

function TdxRichEditControlAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (inherited HandlesTarget(Target) or (Target is TdxCustomRichEditControl)) and
    (not NeedControlFocus or TdxCustomRichEditControl(Target).Focused);
end;

function TdxRichEditControlAction.GetDefaultCaption: string;
var
  ACommand: TdxCommand;
begin
  if GetCommandClass.NeedUseObjectLocalization and (RichEditControl <> nil) then
  begin
    ACommand := CreateCommand(RichEditControl);
    try
      Result := ACommand.GetObjectMenuCaption;
    finally
      ACommand.Free;
    end;
  end
  else
    Result := GetCommandClass.GetMenuCaption;
end;

function TdxRichEditControlAction.GetDefaultHint: string;
var
  ACommand: TdxCommand;
begin
  if GetCommandClass.NeedUseObjectLocalization and (RichEditControl <> nil) then
  begin
    ACommand := CreateCommand(RichEditControl);
    try
      Result := ACommand.GetObjectDescription;
    finally
      ACommand.Free;
    end;
  end
  else
    Result := GetCommandClass.GetDescription;
end;

procedure TdxRichEditControlAction.DoUpdateState;
var
  ACommand: TdxCommand;
  AState: IdxCommandUIState;
begin
  ACommand := CreateCommand(RichEditControl);
  try
    AState := GetState(ACommand);
    ACommand.UpdateUIState(AState);
  finally
    ACommand.Free;
  end;
  CopyFrom(AState);
end;

procedure TdxRichEditControlAction.DoResetState;
begin
  BeginUpdate;
  try
    Enabled := False;
    Checked := False;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditControlAction.UpdateControl(Target: TObject);
begin
  if Target is TdxCustomRichEditControl then
    Control := TdxCustomRichEditControl(Target);
end;

procedure TdxRichEditControlAction.CopyFrom(const AState: IdxCommandUIState);
begin
  BeginUpdate;
  try
    Enabled := AState.Enabled and (Control <> nil) and Control.Enabled;
    Checked := AState.Checked;
    Visible := AState.Visible;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditControlAction.CopyTo(const AState: IdxCommandUIState);
begin
  AState.Enabled := Enabled;
  AState.Checked := Checked;
  AState.Visible := Visible;
end;

constructor TdxRichEditControlAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultImageNameInIconLibrary := GetCommandClass.GetImageName;
end;

function TdxRichEditControlAction.CreateCommand(ATarget: TdxCustomRichEditControl): TdxCommand;
begin
  Result := DoCreateCommand(ATarget);
  Result.CommandSourceType := TdxCommandSourceType.Menu;
end;

procedure TdxRichEditControlAction.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('LargeImageIndex', ReadLargeImageIndex, nil, False);
end;

function TdxRichEditControlAction.DoCreateCommand(ATarget: TdxCustomRichEditControl): TdxCommand;
begin
  Result := GetCommandClass.Create(ATarget);
end;

procedure TdxRichEditControlAction.DoExecute(ACommand: TdxCommand);
begin
  ACommand.Execute;
end;

function TdxRichEditControlAction.GetState(ACommand: TdxCommand): IdxCommandUIState;
begin
  Result := ACommand.CreateDefaultCommandUIState;
  CopyTo(Result);
end;

function TdxRichEditControlAction.GetRichEditControl: TdxCustomRichEditControl;
begin
  Result := TdxCustomRichEditControl(Control);
end;

procedure TdxRichEditControlAction.ReadLargeImageIndex(Reader: TReader);
begin
  Reader.ReadInteger;
end;

{ TdxRichEditControlNewDocument }

function TdxRichEditControlNewDocument.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxCreateEmptyDocumentCommand;
end;

{ TdxRichEditControlLoadDocument }

function TdxRichEditControlLoadDocument.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxLoadDocumentCommand;
end;

{ TdxRichEditControlSaveDocument }

function TdxRichEditControlSaveDocument.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSaveDocumentCommand;
end;

{ TdxRichEditControlSaveDocumentAs }

function TdxRichEditControlSaveDocumentAs.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSaveDocumentAsCommand;
end;

{ TdxRichEditControlToggleShowHorizontalRuler }

constructor TdxRichEditControlToggleShowHorizontalRuler.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleShowHorizontalRuler.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleShowHorizontalRulerCommand;
end;

{ TdxRichEditControlToggleShowVerticalRuler }

constructor TdxRichEditControlToggleShowVerticalRuler.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleShowVerticalRuler.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleShowVerticalRulerCommand;
end;

{ TdxRichEditControlZoomIn }

function TdxRichEditControlZoomIn.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxZoomInCommand;
end;

{ TdxRichEditControlZoomOut }

function TdxRichEditControlZoomOut.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxZoomOutCommand;
end;

{ TdxRichEditControlZoom }

function TdxRichEditControlZoomPercent.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxZoomPercentCommand;
end;

{ TdxRichEditControlChangePageColor }

function TdxRichEditControlChangePageColor.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangePageColorCommand;
end;

{ TdxRichEditControlSwitchToDraftView }

constructor TdxRichEditControlSwitchToDraftView.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSwitchToDraftView.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSwitchToDraftViewCommand;
end;

{ TdxRichEditControlSwitchToSimpleView }

constructor TdxRichEditControlSwitchToSimpleView.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSwitchToSimpleView.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSwitchToSimpleViewCommand;
end;

{ TdxRichEditControlSwitchToPrintLayoutView }

constructor TdxRichEditControlSwitchToPrintLayoutView.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSwitchToPrintLayoutView.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSwitchToPrintLayoutViewCommand;
end;

{ TdxRichEditControlToggleFontBold }

constructor TdxRichEditControlToggleFontBold.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleFontBold.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFontBoldCommand;
end;

{ TdxRichEditControlToggleFontItalic }

constructor TdxRichEditControlToggleFontItalic.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleFontItalic.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFontItalicCommand;
end;

{ TdxRichEditControlToggleFontUnderline }

constructor TdxRichEditControlToggleFontUnderline.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleFontUnderline.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFontUnderlineCommand;
end;

{ TdxRichEditControlToggleFontDoubleUnderline }

constructor TdxRichEditControlToggleFontDoubleUnderline.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleFontDoubleUnderline.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFontDoubleUnderlineCommand;
end;

{ TdxRichEditControlToggleFontStrikeout }

constructor TdxRichEditControlToggleFontStrikeout.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleFontStrikeout.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFontStrikeoutCommand;
end;

{ TdxRichEditControlToggleFontDoubleStrikeout }

constructor TdxRichEditControlToggleFontDoubleStrikeout.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleFontDoubleStrikeout.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFontDoubleStrikeoutCommand;
end;

{ TdxRichEditControlToggleParagraphAlignmentLeft }

constructor TdxRichEditControlToggleParagraphAlignmentLeft.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleParagraphAlignmentLeft.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleParagraphAlignmentLeftCommand;
end;

{ TdxRichEditControlToggleParagraphAlignmentCenter }

constructor TdxRichEditControlToggleParagraphAlignmentCenter.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleParagraphAlignmentCenter.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleParagraphAlignmentCenterCommand;
end;

{ TdxRichEditControlToggleParagraphAlignmentRight }

constructor TdxRichEditControlToggleParagraphAlignmentRight.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleParagraphAlignmentRight.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleParagraphAlignmentRightCommand;
end;

{ TdxRichEditControlToggleParagraphAlignmentJustify }

constructor TdxRichEditControlToggleParagraphAlignmentJustify.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleParagraphAlignmentJustify.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleParagraphAlignmentJustifyCommand;
end;

{ TdxRichEditControlCopySelection }

function TdxRichEditControlCopySelection.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxCopySelectionCommand;
end;

{ TdxRichEditControlPasteSelection }

function TdxRichEditControlPasteSelection.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxPasteSelectionCommand;
end;

{ TdxRichEditControlCutSelection }

function TdxRichEditControlCutSelection.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxCutSelectionCommand;
end;

{ TdxRichEditControlSetSingleParagraphSpacing }

constructor TdxRichEditControlSetSingleParagraphSpacing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
end;

function TdxRichEditControlSetSingleParagraphSpacing.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSingleParagraphSpacingCommand;
end;

{ TdxRichEditControlSetDoubleParagraphSpacing }

constructor TdxRichEditControlSetDoubleParagraphSpacing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
end;

function TdxRichEditControlSetDoubleParagraphSpacing.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetDoubleParagraphSpacingCommand;
end;

{ TdxRichEditControlSetSesquialteralParagraphSpacing }

constructor TdxRichEditControlSetSesquialteralParagraphSpacing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
end;

function TdxRichEditControlSetSesquialteralParagraphSpacing.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSesquialteralParagraphSpacingCommand;
end;

{ TdxRichEditControlUndo }

function TdxRichEditControlUndo.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxUndoCommand;
end;

{ TdxRichEditControlRedo }

function TdxRichEditControlRedo.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxRedoCommand;
end;

{ TdxRichEditControlIncreaseFontSize }

function TdxRichEditControlIncreaseFontSize.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxIncreaseFontSizeCommand;
end;

{ TdxRichEditControlDecreaseFontSize }

function TdxRichEditControlDecreaseFontSize.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxDecreaseFontSizeCommand;
end;

{ TdxRichEditControlDecreaseToggleFontSuperscript }

constructor TdxRichEditControlToggleFontSuperscript.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleFontSuperscript.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFontSuperscriptCommand;
end;

{ TdxRichEditControlToggleFontSubscript }

constructor TdxRichEditControlToggleFontSubscript.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleFontSubscript.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFontSubscriptCommand;
end;

{ TdxRichEditControlInsertBulletList }

constructor TdxRichEditControlToggleBulletedList.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleBulletedList.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleBulletedListCommand;
end;

{ TdxRichEditControlToggleSimpleNumberingList }

constructor TdxRichEditControlToggleSimpleNumberingList.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleSimpleNumberingList.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleSimpleNumberingListCommand;
end;

{ TdxRichEditControlToggleMultiLevelList }

constructor TdxRichEditControlToggleMultiLevelList.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleMultiLevelList.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleMultiLevelListCommand;
end;

{ TdxRichEditControlChangeSectionLineNumbering }

function TdxRichEditControlChangeSectionLineNumbering.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangeSectionLineNumberingCommand;
end;

{ TdxRichEditControlSetSectionLineNumberingNone }

constructor TdxRichEditControlSetSectionLineNumberingNone.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetSectionLineNumberingNone.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSectionLineNumberingNoneCommand;
end;

{ TdxRichEditControlSetSectionLineNumberingContinuous }

constructor TdxRichEditControlSetSectionLineNumberingContinuous.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetSectionLineNumberingContinuous.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSectionLineNumberingContinuousCommand;
end;

{ TdxRichEditControlSetSectionLineNumberingRestartNewPage }

constructor TdxRichEditControlSetSectionLineNumberingRestartNewPage.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetSectionLineNumberingRestartNewPage.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSectionLineNumberingRestartNewPageCommand;
end;

{ TdxRichEditControlSetSectionLineNumberingRestartNewSection }

constructor TdxRichEditControlSetSectionLineNumberingRestartNewSection.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetSectionLineNumberingRestartNewSection.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSectionLineNumberingRestartNewSectionCommand;
end;

{ TdxRichEditControlSelectAll }

function TdxRichEditControlSelectAll.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSelectAllCommand;
end;

{ TdxRichEditControlToggleShowWhitespace }

constructor TdxRichEditControlToggleShowWhitespace.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlToggleShowWhitespace.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleShowWhitespaceCommand;
end;

{ TdxRichEditControlIncrementIndent }

function TdxRichEditControlIncrementIndent.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxIncrementIndentCommand;
end;

{ TdxRichEditControlDecrementIndent }

function TdxRichEditControlDecrementIndent.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxDecrementIndentCommand;
end;

{ TdxRichEditControlShowFontForm }

function TdxRichEditControlShowFontForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowFontFormCommand;
end;

{ TdxRichEditControlShowParagraphForm }

function TdxRichEditControlShowParagraphForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowParagraphFormCommand;
end;

{ TdxRichEditControlShowNumberingForm }

function TdxRichEditControlShowNumberingForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowNumberingListFormCommand;
end;

{ TdxRichEditControlShowInsertTableForm }

function TdxRichEditControlShowInsertTableForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableCommand;
end;

{ TdxRichEditControlDeleteTable }

function TdxRichEditControlDeleteTable.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxDeleteTableCommand;
end;

{ TdxRichEditControlShowSplitTableCellsForm }

function TdxRichEditControlShowSplitTableCellsForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowSplitTableCellsFormCommand;
end;

{ TdxRichEditControlShowInsertTableCellsForm }

function TdxRichEditControlShowInsertTableCellsForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowInsertTableCellsFormCommand;
end;

{ TdxRichEditControlShowDeleteTableCellsForm }

function TdxRichEditControlShowDeleteTableCellsForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowDeleteTableCellsFormCommand;
end;

{ TdxRichEditControlShowTablePropertiesForm }

function TdxRichEditControlShowTablePropertiesForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowTablePropertiesFormCommand;
end;

{ TdxRichEditControlShowSymbolForm }

function TdxRichEditControlShowSymbolForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowSymbolFormCommand;
end;

{ TdxRichEditControlShowHyperlinkForm }

function TdxRichEditControlShowHyperlinkForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowHyperlinkFormCommand;
end;

{ TdxRichEditControlShowColumnsSetupForm }

function TdxRichEditControlShowColumnsSetupForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowColumnsSetupFormCommand;
end;

{ TdxRichEditControlShowEditStyleForm }

function TdxRichEditControlShowEditStyleForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowEditStyleFormCommand;
end;

{ TdxRichEditControlShowTableStyleForm }

function TdxRichEditControlShowTableStyleForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowTableStyleFormCommand;
end;

{ TdxRichEditControlValueAction<T> }

procedure TdxRichEditControlValueAction<T>.CopyFrom(const AState: IdxCommandUIState);
var
  AValueState: IdxValueBasedCommandUIState<T>;
begin
  if IsLocked then
    Exit;

  inherited CopyFrom(AState);

  if Supports(AState, IdxValueBasedCommandUIState<T>, AValueState) then
  begin
    if not IsValueEquals(FValue, AValueState.Value) then
    begin
      FValue := AValueState.Value;
      DoActionValueChanged(GetValueAsVariant);
    end;
  end;
end;

procedure TdxRichEditControlValueAction<T>.CopyTo(const AState: IdxCommandUIState);
var
  AValueState: IdxValueBasedCommandUIState<T>;
begin
  inherited CopyTo(AState);
  if Supports(AState, IdxValueBasedCommandUIState<T>, AValueState) then
    AValueState.Value := FValue;
end;

procedure TdxRichEditControlValueAction<T>.DoExecute(ACommand: TdxCommand);
var
  AState: IdxCommandUIState;
begin
  AState := GetState(ACommand);
  ACommand.ForceExecute(AState);
end;

class function TdxRichEditControlValueAction<T>.IsValueEquals(const AValue1, AValue2: T): Boolean;
begin
  Result := False;
  dxAbstractError;
end;

procedure TdxRichEditControlValueAction<T>.SetValue(const AValue: T);
var
  ACommand: TdxCommand;
begin
  if not IsValueEquals(FValue, AValue) then
  begin
    FValue := AValue;
    if Enabled and not Execute then
    begin
      ACommand := CreateCommand(RichEditControl);
      try
        DoExecute(ACommand);
      finally
        ACommand.Free;
      end;
    end;
  end;
end;

{ TdxRichEditControlAlphaColorValueAction }

function TdxRichEditControlAlphaColorValueAction.GetValueAsVariant: Variant;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(Value) then
    Result := clDefault
  else
    Result := TdxAlphaColors.ToColor(Value);
end;

procedure TdxRichEditControlAlphaColorValueAction.SetValueAsVariant(const AValue: Variant);
begin
  if AValue = clDefault then
    Value := TdxAlphaColors.Empty
  else
    Value := TdxAlphaColors.FromColor(AValue);
end;

class function TdxRichEditControlAlphaColorValueAction.IsValueEquals(const AValue1, AValue2: TdxAlphaColor): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TdxRichEditControlSingleValueAction }

function TdxRichEditControlSingleValueAction.GetValueAsVariant: Variant;
begin
  if Value > 0 then
    Result := Value
  else
    Result := Null;
end;

procedure TdxRichEditControlSingleValueAction.SetValueAsVariant(const AValue: Variant);
begin
  if AValue <> Null then
    Value := AValue
  else
    Value := -1;
end;

class function TdxRichEditControlSingleValueAction.IsValueEquals(const AValue1, AValue2: Single): Boolean;
begin
  Result := SameValue(AValue1, AValue2);
end;

{ TdxRichEditControlStringValueAction }

function TdxRichEditControlStringValueAction.GetValueAsVariant: Variant;
begin
  Result := Value;
end;

procedure TdxRichEditControlStringValueAction.SetValueAsVariant(const AValue: Variant);
begin
  Value := AValue;
end;

class function TdxRichEditControlStringValueAction.IsValueEquals(const AValue1, AValue2: string): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TdxRichEditControlIntegerValueAction }

function TdxRichEditControlIntegerValueAction.GetValueAsVariant: Variant;
begin
  if Value >= 0 then
    Result := Value
  else
    Result := Null;
end;

procedure TdxRichEditControlIntegerValueAction.SetValueAsVariant(const AValue: Variant);
begin
  if AValue <> Null then
    Value := AValue
  else
    Value := -1;
end;

class function TdxRichEditControlIntegerValueAction.IsValueEquals(const AValue1, AValue2: Integer): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TdxRichEditControlChangeFontName }

function TdxRichEditControlChangeFontName.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangeFontNameCommand;
end;

{ TdxRichEditControlChangeFontSize }

function TdxRichEditControlChangeFontSize.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangeFontSizeCommand;
end;

{ TdxRichEditControlChangeFontColor }

function TdxRichEditControlChangeFontColor.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangeFontColorCommand;
end;

{ TdxRichEditControlInsertPicture }

function TdxRichEditControlInsertPicture.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertPictureCommand;
end;

{ TdxRichEditControlSearchFind }

function TdxRichEditControlSearchFind.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxFindCommand;
end;

{ TdxRichEditControlSearchReplace }

function TdxRichEditControlSearchReplace.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxReplaceCommand;
end;

{ TdxRichEditControlSearchFindNext }

function TdxRichEditControlSearchFindNext.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxFindAndSelectForwardCommand;
end;

{ TdxRichEditControlSplitTable }

function TdxRichEditControlSplitTable.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSplitTableCommand;
end;

{ TdxRichEditControlInsertTableRowBelow }

function TdxRichEditControlInsertTableRowBelow.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableRowBelowCommand;
end;

{ TdxRichEditControlInsertTableRowAbove }

function TdxRichEditControlInsertTableRowAbove.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableRowAboveCommand;
end;

{ TdxRichEditControlInsertTableColumnToTheLeft }

function TdxRichEditControlInsertTableColumnToTheLeft.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableColumnToTheLeftCommand;
end;

{ TdxRichEditControlInsertTableColumnToTheRight }

function TdxRichEditControlInsertTableColumnToTheRight.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableColumnToTheRightCommand;
end;

{ TdxRichEditControlDeleteTableRows }

function TdxRichEditControlDeleteTableRows.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxDeleteTableRowsCommand;
end;

{ TdxRichEditControlDeleteTableColumns }

function TdxRichEditControlDeleteTableColumns.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxDeleteTableColumnsCommand;
end;

{ TdxRichEditControlToggleTableCellsAllBorders }

function TdxRichEditControlToggleTableCellsAllBorders.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsAllBordersCommand;
end;

{ TdxRichEditControlResetTableCellsBorders }

function TdxRichEditControlResetTableCellsBorders.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxResetTableCellsBordersCommand;
end;

{ TdxRichEditControlToggleTableCellsOutsideBorder }

function TdxRichEditControlToggleTableCellsOutsideBorder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsOutsideBorderCommand;
end;

{ TdxRichEditControlToggleTableCellsInsideBorder }

function TdxRichEditControlToggleTableCellsInsideBorder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsInsideBorderCommand;
end;

{ TdxRichEditControlToggleTableCellsLeftBorder }

function TdxRichEditControlToggleTableCellsLeftBorder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsLeftBorderCommand;
end;

{ TdxRichEditControlToggleTableCellsRightBorder }

function TdxRichEditControlToggleTableCellsRightBorder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsRightBorderCommand;
end;

{ TdxRichEditControlToggleTableCellsTopBorder }

function TdxRichEditControlToggleTableCellsTopBorder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsTopBorderCommand;
end;

{ TdxRichEditControlToggleTableCellsBottomBorder }

function TdxRichEditControlToggleTableCellsBottomBorder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsBottomBorderCommand;
end;

{ TdxRichEditControlToggleTableCellsInsideHorizontalBorder }

function TdxRichEditControlToggleTableCellsInsideHorizontalBorder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsInsideHorizontalBorderCommand;
end;

{ TdxRichEditControlToggleTableCellsInsideVerticalBorder }

function TdxRichEditControlToggleTableCellsInsideVerticalBorder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsInsideVerticalBorderCommand;
end;

{ TdxRichEditControlToggleTableCellsTopLeftAlignment }

function TdxRichEditControlToggleTableCellsTopLeftAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsTopLeftAlignmentCommand;
end;

{ TdxRichEditControlToggleTableCellsTopCenterAlignment }

function TdxRichEditControlToggleTableCellsTopCenterAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsTopCenterAlignmentCommand;
end;

{ TdxRichEditControlToggleTableCellsTopRightAlignment }

function TdxRichEditControlToggleTableCellsTopRightAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsTopRightAlignmentCommand;
end;

{ TdxRichEditControlToggleTableCellsMiddleLeftAlignment }

function TdxRichEditControlToggleTableCellsMiddleLeftAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsMiddleLeftAlignmentCommand;
end;

{ TdxRichEditControlToggleTableCellsMiddleCenterAlignment }

function TdxRichEditControlToggleTableCellsMiddleCenterAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsMiddleCenterAlignmentCommand;
end;

{ TdxRichEditControlToggleTableCellsMiddleRightAlignment }

function TdxRichEditControlToggleTableCellsMiddleRightAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsMiddleRightAlignmentCommand;
end;

{ TdxRichEditControlToggleTableCellsBottomLeftAlignment }

function TdxRichEditControlToggleTableCellsBottomLeftAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsBottomLeftAlignmentCommand;
end;

{ TdxRichEditControlToggleTableCellsBottomCenterAlignment }

function TdxRichEditControlToggleTableCellsBottomCenterAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsBottomCenterAlignmentCommand;
end;

{ TdxRichEditControlToggleTableCellsBottomRightAlignment }

function TdxRichEditControlToggleTableCellsBottomRightAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableCellsBottomRightAlignmentCommand;
end;

{ TdxRichEditControlToggleTableAutoFitContents }

function TdxRichEditControlToggleTableAutoFitContents.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableAutoFitContentsCommand;
end;

{ TdxRichEditControlToggleTableAutoFitWindow }

function TdxRichEditControlToggleTableAutoFitWindow.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableAutoFitWindowCommand;
end;

{ TdxRichEditControlToggleTableFixedColumnWidth }

function TdxRichEditControlToggleTableFixedColumnWidth.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTableFixedColumnWidthCommand;
end;

{ TdxRichEditControlToggleShowTableGridLines }

constructor TdxRichEditControlToggleShowTableGridLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
end;

function TdxRichEditControlToggleShowTableGridLines.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleShowTableGridLinesCommand;
end;

{ TdxRichEditControlMergeTableCells }

function TdxRichEditControlMergeTableCells.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxMergeTableCellsCommand;
end;

{ TdxRichEditControlTextHighlight }

function TdxRichEditControlTextHighlight.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangeFontBackColorCommand;
end;

{ TdxRichEditControlInsertPageBreak }

function TdxRichEditControlInsertPageBreak.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertPageBreakCommand;
end;

{ TdxRichEditControlInsertSectionBreakNextPage }

function TdxRichEditControlInsertSectionBreakNextPage.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertSectionBreakNextPageCommand;
end;

{ TdxRichEditControlInsertSectionBreakOddPage }

function TdxRichEditControlInsertSectionBreakOddPage.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertSectionBreakOddPageCommand;
end;

{ TdxRichEditControlInsertSectionBreakEvenPage }

function TdxRichEditControlInsertSectionBreakEvenPage.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertSectionBreakEvenPageCommand;
end;

{ TdxRichEditControlInsertColumnBreak }

function TdxRichEditControlInsertColumnBreak.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertColumnBreakCommand;
end;

{ TdxRichEditControlTextUpperCase }

function TdxRichEditControlTextUpperCase.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxMakeTextUpperCaseCommand;
end;

{ TdxRichEditControlTextLowerCase }

function TdxRichEditControlTextLowerCase.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxMakeTextLowerCaseCommand;
end;

{ TdxRichEditControlToggleTextCase }

function TdxRichEditControlToggleTextCase.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleTextCaseCommand;
end;

{ TdxRichEditControlSetSectionOneColumn }

constructor TdxRichEditControlSetSectionOneColumn.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetSectionOneColumn.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSectionOneColumnCommand;
end;

{ TdxRichEditControlSetSectionTwoColumns }

constructor TdxRichEditControlSetSectionTwoColumns.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetSectionTwoColumns.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSectionTwoColumnsCommand;
end;

{ TdxRichEditControlSetSectionThreeColumns }

constructor TdxRichEditControlSetSectionThreeColumns.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetSectionThreeColumns.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetSectionThreeColumnsCommand;
end;

{ TdxRichEditControlInsertFloatingObjectPicture }

function TdxRichEditControlInsertFloatingObjectPicture.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertFloatingObjectPictureCommand;
end;

{ TdxRichEditControlInsertTextBox }

function TdxRichEditControlInsertTextBox.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTextBoxCommand;
end;

{ TdxRichEditControlShowFloatingObjectLayoutOptionsForm }

function TdxRichEditControlShowFloatingObjectLayoutOptionsForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowFloatingObjectLayoutOptionsFormCommand;
end;

{ TdxRichEditControlSetFloatingObjectSquareTextWrapType }

function TdxRichEditControlSetFloatingObjectSquareTextWrapType.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectSquareTextWrapTypeCommand;
end;

{ TdxRichEditControlSetFloatingObjectBehindTextWrapType }

function TdxRichEditControlSetFloatingObjectBehindTextWrapType.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectBehindTextWrapTypeCommand;
end;

{ TdxRichEditControlSetFloatingObjectInFrontOfTextWrapType }

function TdxRichEditControlSetFloatingObjectInFrontOfTextWrapType.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectInFrontOfTextWrapTypeCommand;
end;

{ TdxRichEditControlSetFloatingObjectThroughTextWrapType }

function TdxRichEditControlSetFloatingObjectThroughTextWrapType.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectThroughTextWrapTypeCommand;
end;

{ TdxRichEditControlSetFloatingObjectTightTextWrapType }

function TdxRichEditControlSetFloatingObjectTightTextWrapType.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectTightTextWrapTypeCommand;
end;

{ TdxRichEditControlSetFloatingObjectTopAndBottomTextWrapType }

function TdxRichEditControlSetFloatingObjectTopAndBottomTextWrapType.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand;
end;

{ TdxRichEditControlSetFloatingObjectTopLeftAlignment }

function TdxRichEditControlSetFloatingObjectTopLeftAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectTopLeftAlignmentCommand;
end;

{ TdxRichEditControlSetFloatingObjectTopCenterAlignment }

function TdxRichEditControlSetFloatingObjectTopCenterAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectTopCenterAlignmentCommand;
end;

{ TdxRichEditControlSetFloatingObjectTopRightAlignment }

function TdxRichEditControlSetFloatingObjectTopRightAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectTopRightAlignmentCommand;
end;

{ TdxRichEditControlSetFloatingObjectMiddleLeftAlignment }

function TdxRichEditControlSetFloatingObjectMiddleLeftAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectMiddleLeftAlignmentCommand;
end;

{ TdxRichEditControlSetFloatingObjectMiddleCenterAlignment }

function TdxRichEditControlSetFloatingObjectMiddleCenterAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectMiddleCenterAlignmentCommand;
end;

{ TdxRichEditControlSetFloatingObjectMiddleRightAlignment }

function TdxRichEditControlSetFloatingObjectMiddleRightAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectMiddleRightAlignmentCommand;
end;

{ TdxRichEditControlSetFloatingObjectBottomLeftAlignment }

function TdxRichEditControlSetFloatingObjectBottomLeftAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectBottomLeftAlignmentCommand;
end;

{ TdxRichEditControlSetFloatingObjectBottomCenterAlignment }

function TdxRichEditControlSetFloatingObjectBottomCenterAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectBottomCenterAlignmentCommand;
end;

{ TdxRichEditControlSetFloatingObjectBottomRightAlignment }

function TdxRichEditControlSetFloatingObjectBottomRightAlignment.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetFloatingObjectBottomRightAlignmentCommand;
end;

{ TdxRichEditControlFloatingObjectBringForward }

function TdxRichEditControlFloatingObjectBringForward.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxFloatingObjectBringForwardCommand;
end;

{ TdxRichEditControlFloatingObjectBringToFront }

function TdxRichEditControlFloatingObjectBringToFront.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxFloatingObjectBringToFrontCommand;
end;

{ TdxRichEditControlFloatingObjectBringInFrontOfText }

function TdxRichEditControlFloatingObjectBringInFrontOfText.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxFloatingObjectBringInFrontOfTextCommand;
end;

{ TdxRichEditControlFloatingObjectSendBackward }

function TdxRichEditControlFloatingObjectSendBackward.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxFloatingObjectSendBackwardCommand;
end;

{ TdxRichEditControlFloatingObjectSendToBack }

function TdxRichEditControlFloatingObjectSendToBack.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxFloatingObjectSendToBackCommand;
end;

{ TdxRichEditControlFloatingObjectSendBehindText }

function TdxRichEditControlFloatingObjectSendBehindText.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxFloatingObjectSendBehindTextCommand;
end;

{ TdxRichEditControlChangeFloatingObjectFillColor }

function TdxRichEditControlChangeFloatingObjectFillColor.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangeFloatingObjectFillColorCommand;
end;

{ TdxRichEditControlChangeFloatingObjectOutlineColor }

function TdxRichEditControlChangeFloatingObjectOutlineColor.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangeFloatingObjectOutlineColorCommand;
end;

{ TdxRichEditControlChangeFloatingObjectOutlineWidth }

function TdxRichEditControlChangeFloatingObjectOutlineWidth.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxChangeFloatingObjectOutlineWidthCommand;
end;

function TdxRichEditControlChangeFloatingObjectOutlineWidth.GetValueAsPoints: Single;
begin
  Result := RichEditControl.DocumentModel.UnitConverter.ModelUnitsToPointsF(Value);
end;

procedure TdxRichEditControlChangeFloatingObjectOutlineWidth.SetValueAsPoints(const AValue: Single);
begin
  Value := Trunc(RichEditControl.DocumentModel.UnitConverter.PointsToModelUnitsF(AValue));
end;

{ TdxRichEditControlEditPageHeader }

function TdxRichEditControlEditPageHeader.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxEditPageHeaderCommand;
end;

{ TdxRichEditControlEditPageFooter }

function TdxRichEditControlEditPageFooter.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxEditPageFooterCommand;
end;

{ TdxRichEditControlClosePageHeaderFooter }

function TdxRichEditControlClosePageHeaderFooter.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxClosePageHeaderFooterCommand;
end;

{ TdxRichEditControlGoToPageHeader }

function TdxRichEditControlGoToPageHeader.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxGoToPageHeaderCommand;
end;

{ TdxRichEditControlGoToPageFooter }

function TdxRichEditControlGoToPageFooter.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxGoToPageFooterCommand;
end;

{ TdxRichEditControlToggleHeaderFooterLinkToPrevious }

function TdxRichEditControlToggleHeaderFooterLinkToPrevious.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleHeaderFooterLinkToPreviousCommand;
end;

{ TdxRichEditControlGoToPreviousPageHeaderFooter }

function TdxRichEditControlGoToPreviousPageHeaderFooter.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxGoToPreviousPageHeaderFooterCommand;
end;

{ TdxRichEditControlGoToNextPageHeaderFooter }

function TdxRichEditControlGoToNextPageHeaderFooter.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxGoToNextPageHeaderFooterCommand;
end;

{ TdxRichEditControlToggleDifferentFirstPage }

function TdxRichEditControlToggleDifferentFirstPage.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleDifferentFirstPageCommand;
end;

{ TdxRichEditControlToggleDifferentOddAndEvenPages }

function TdxRichEditControlToggleDifferentOddAndEvenPages.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleDifferentOddAndEvenPagesCommand;
end;

{ TdxRichEditControlEditShowLineNumberingForm }

function TdxRichEditControlShowLineNumberingForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowLineNumberingFormCommand;
end;

{ TdxRichEditControlEditShowAllFieldResults }

function TdxRichEditControlShowAllFieldResults.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowAllFieldResultsCommand;
end;

{ TdxRichEditControlShowAllFieldCodes }

function TdxRichEditControlShowAllFieldCodes.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowAllFieldCodesCommand;
end;

{ TdxRichEditControlToggleFieldCodes }

function TdxRichEditControlToggleFieldCodes.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleFieldCodesCommand;
end;

{ TdxRichEditControlCreateField }

function TdxRichEditControlCreateField.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxCreateFieldCommand;
end;

{ TdxRichEditControlUpdateField }

function TdxRichEditControlUpdateField.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxUpdateFieldCommand;
end;

{ TdxRichEditControlUpdateFields }

function TdxRichEditControlUpdateFields.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxUpdateFieldsCommand;
end;

{ TdxRichEditControlShowPageSetupForm }

function TdxRichEditControlShowPageSetupForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowPageSetupFormCommand;
end;

{ TdxRichEditControlShowPageMarginsSetupForm }

function TdxRichEditControlShowPageMarginsSetupForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowPageMarginsSetupFormCommand;
end;

{ TdxRichEditControlShowPagePaperSetupForm }

function TdxRichEditControlShowPagePaperSetupForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowPagePaperSetupFormCommand;
end;

{ TdxRichEditControlSetPortraitPageOrientation }

constructor TdxRichEditControlSetPortraitPageOrientation.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetPortraitPageOrientation.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetPortraitPageOrientationCommand;
end;

{ TdxRichEditControlSetLandscapePageOrientation }

constructor TdxRichEditControlSetLandscapePageOrientation.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
end;

function TdxRichEditControlSetLandscapePageOrientation.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetLandscapePageOrientationCommand;
end;

{ TdxRichEditControlOpenHyperlink }

function TdxRichEditControlOpenHyperlink.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxOpenHyperlinkCommand;
end;

{ TdxRichEditControlRemoveHyperlinkField }

function TdxRichEditControlRemoveHyperlinkField.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxRemoveHyperlinkFieldCommand;
end;

{ TdxRichEditControlInsertPageCountField }

function TdxRichEditControlInsertPageCountField.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertPageCountFieldCommand;
end;

{ TdxRichEditControlInsertPageNumberField }

function TdxRichEditControlInsertPageNumberField.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertPageNumberFieldCommand;
end;

{ TdxRichEditControlShowInsertMergeFieldForm }

function TdxRichEditControlShowInsertMergeFieldForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowInsertMergeFieldFormCommand;
end;

{ TdxRichEditControlShowBookmarkForm }

function TdxRichEditControlShowBookmarkForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowBookmarkFormCommand;
end;

{ TdxRichEditControlToggleViewMergedData }

function TdxRichEditControlToggleViewMergedData.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxToggleViewMergedDataCommand;
end;

{ TdxRichEditControlShowMergeDatabaseRecordsForm }

function TdxRichEditControlShowMergeDatabaseRecordsForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowMergeDatabaseRecordsFormCommand;
end;

{ TdxRichEditControlCheckSpelling }

function TdxRichEditControlCheckSpelling.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxCheckSpellingCommand;
end;

{ TdxRichEditControlInsertTableOfContents }

function TdxRichEditControlInsertTableOfContents.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableOfContentsCommand;
end;

{ TdxRichEditControlUpdateTableOfContents }

function TdxRichEditControlUpdateTableOfContents.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxUpdateTableOfContentsCommand;
end;

{ TdxRichEditControlAddParagraphsToTableOfContentsPlaceholder }

function TdxRichEditControlAddParagraphsToTableOfContentsPlaceholder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxAddParagraphsToTableOfContentsCommand;
end;

{ TdxRichEditControlTableOfContentsSetParagraphBodyTextLevel }

function TdxRichEditControlTableOfContentsSetParagraphBodyTextLevel.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphBodyTextLevelCommand;
end;

{ TdxRichEditControlTableOfContentsSetParagraphHeading1Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading1Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading1LevelCommand;
end;

{ TdxRichEditTableOfContentsSetParagraphHeading2Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading2Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading2LevelCommand;
end;

{ TdxRichEditControlTableOfContentsSetParagraphHeading3Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading3Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading3LevelCommand;
end;

{ TdxRichEditControlTableOfContentsSetParagraphHeading4Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading4Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading4LevelCommand;
end;

{ TdxRichEditControlTableOfContentsSetParagraphHeading5Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading5Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading5LevelCommand;
end;

{ TdxRichEditControlTableOfContentsSetParagraphHeading6Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading6Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading6LevelCommand;
end;

{ TdxRichEditTableOfContentsSetParagraphHeading7Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading7Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading7LevelCommand;
end;

{ TdxRichEditControlTableOfContentsSetParagraphHeading8Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading8Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading8LevelCommand;
end;

{ TdxRichEditControlTableOfContentsSetParagraphHeading9Level }

function TdxRichEditControlTableOfContentsSetParagraphHeading9Level.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxSetParagraphHeading9LevelCommand;
end;

{ TdxRichEditControlShowTableOfContentsForm }

function TdxRichEditControlShowTableOfContentsForm.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowTOCFormCommand;
end;

{ TdxRichEditControlInsertTableOfFiguresPlaceholder }

function TdxRichEditControlInsertTableOfFiguresPlaceholder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableOfFiguresPlaceholderCommand;
end;

{ TdxRichEditControlInsertTableOfFigures }

function TdxRichEditControlInsertTableOfFigures.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableOfFiguresCommand;
end;

{ TdxRichEditControlUpdateTableOfFigures }

function TdxRichEditControlUpdateTableOfFigures.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxUpdateTableOfFiguresCommand;
end;

{ TdxRichEditControlInsertCaptionPlaceholder }

function TdxRichEditControlInsertCaptionPlaceholder.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertCaptionPlaceholderCommand;
end;

{ TdxRichEditControlInsertFigureCaption }

function TdxRichEditControlInsertFigureCaption.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertFigureCaptionCommand;
end;

{ TdxRichEditControlInsertTableOfTables }

function TdxRichEditControlInsertTableOfTables.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableOfTablesCommand;
end;

{ TdxRichEditControlInsertTableCaption }

function TdxRichEditControlInsertTableCaption.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableCaptionCommand;
end;

{ TdxRichEditControlInsertTableOfEquations }

function TdxRichEditControlInsertTableOfEquations.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertTableOfEquationsCommand;
end;

{ TdxRichEditControlInsertEquationCaption }

function TdxRichEditControlInsertEquationCaption.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxInsertEquationCaptionCommand;
end;

{ TdxRichEditControlProtectDocument }

function TdxRichEditControlProtectDocument.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxProtectDocumentCommand;
end;

{ TdxRichEditControlUnprotectDocumentCommand }

function TdxRichEditControlUnprotectDocument.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxUnprotectDocumentCommand;
end;

{ TdxRichEditControlShowRangeEditingPermissions }

function TdxRichEditControlShowRangeEditingPermissions.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxShowRangeEditingPermissionsFormCommand;
end;

{ TdxRichEditControlEncryptDocument }

function TdxRichEditControlEncryptDocument.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxEncryptDocumentCommand;
end;

{ TdxCustomRichEditControlPrintingAction }

procedure TdxCustomRichEditControlPrintingAction.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('LargeImageIndex', ReadLargeImageIndex, nil, False);
end;

procedure TdxCustomRichEditControlPrintingAction.DoUpdateState;
begin
  RichEditControl.InnerControl.ActiveView.CheckExecutedAtUIThread;
  inherited DoUpdateState;
end;

function TdxCustomRichEditControlPrintingAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and RichEditControl.InnerControl.Enabled;
end;

function TdxCustomRichEditControlPrintingAction.GetControlClass: TWinControlClass;
begin
  Result := TdxCustomRichEditControl;
end;

function TdxCustomRichEditControlPrintingAction.GetRichEditControl: TdxCustomRichEditControl;
begin
  Result := TdxCustomRichEditControl(Control);
end;

procedure TdxCustomRichEditControlPrintingAction.ReadLargeImageIndex(Reader: TReader);
begin
  Reader.ReadInteger;
end;

{ TdxRichEditControlShowPrintForm }

constructor TdxRichEditControlShowPrintForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultImageNameInIconLibrary := TdxRichEditControlCommandsImages.ShowPrintForm;
end;

function TdxRichEditControlShowPrintForm.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPrintMenuCaption);
end;

function TdxRichEditControlShowPrintForm.GetDefaultHint: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPrintDescription);
end;

procedure TdxRichEditControlShowPrintForm.DoExecute(AControl: TWinControl);
begin
  dxPrintingRepository.PrintReport(AControl);
end;

{ TdxRichEditControlShowPrintPreviewForm }

constructor TdxRichEditControlShowPrintPreviewForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultImageNameInIconLibrary := TdxRichEditControlCommandsImages.ShowPrintPreviewForm;
end;

function TdxRichEditControlShowPrintPreviewForm.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPrintPreviewMenuCaption);
end;

function TdxRichEditControlShowPrintPreviewForm.GetDefaultHint: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPrintPreviewDescription);
end;

procedure TdxRichEditControlShowPrintPreviewForm.DoExecute(AControl: TWinControl);
begin
  dxPrintingRepository.PreviewReport(AControl);
end;

{ TdxRichEditControlCustomGalleryAction }

destructor TdxRichEditControlCustomGalleryAction.Destroy;
begin
  DestroyGalleryCommands;
  inherited Destroy;
end;

procedure TdxRichEditControlCustomGalleryAction.UpdateTarget(Target: TObject);
var
  ARecreateGalleryCommands: Boolean;
begin
  ARecreateGalleryCommands := (Target is TdxCustomRichEditControl) and (Target <> RichEditControl);
  inherited UpdateTarget(Target);
  if ARecreateGalleryCommands then
  begin
    DestroyGalleryCommands;
    CreateGalleryCommands;
  end;
end;

procedure TdxRichEditControlCustomGalleryAction.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Control) then
    DestroyGalleryCommands;
  inherited Notification(AComponent, Operation);
end;

{ TdxRichEditControlPaperSizeGallery }

function TdxRichEditControlPaperSizeGallery.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxProcessPaperSizeGalleryCommand;
end;

function TdxRichEditControlPaperSizeGallery.GetValue: Variant;
var
  APaperKind: TdxPaperKind;
begin
  Result := Null;
  if FCommand is TdxProcessPaperSizeGalleryCommand then
  begin
    APaperKind := TdxProcessPaperSizeGalleryCommandAccess(FCommand).GetPaperKind;
    if APaperKind <> TdxPaperKind.Custom then
      Result := APaperKind;
  end;
end;

procedure TdxRichEditControlPaperSizeGallery.SetValue(const AValue: Variant);
begin
  if FCommand is TdxProcessPaperSizeGalleryCommand then
    TdxProcessPaperSizeGalleryCommandAccess(FCommand).SetPaperKind(AValue);
end;

procedure TdxRichEditControlPaperSizeGallery.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);

  function GetPaperIndexInPrintingDefaults(const APaperKind: TdxPaperKind): Integer;
  begin
    case APaperKind of
      TdxPaperKind.Letter:
        Result := 0;
      TdxPaperKind.Tabloid:
        Result := 2;
      TdxPaperKind.Legal:
        Result := 4;
      TdxPaperKind.Statement:
        Result := 5;
      TdxPaperKind.Executive:
        Result := 6;
      TdxPaperKind.A3:
        Result := 7;
      TdxPaperKind.A4:
        Result := 8;
      TdxPaperKind.A5:
        Result := 10;
      TdxPaperKind.B4:
        Result := 11;
      TdxPaperKind.B5:
        Result := 12;
    else
      Result := -1;
    end;
  end;

  function GetDescription(const APaperKind: TdxPaperKind): string;
  var
    APaper: TdxPaper;
    AUnitsConverter: TdxUnitsConverterClass;
    AUnitsName: string;
  begin
    APaper := TdxPrintingDefaults.Papers[GetPaperIndexInPrintingDefaults(APaperKind)];
    if dxGetDefaultMeasurementUnits = muInches then
    begin
      AUnitsName := cxGetResourceString(@sdxRichEditCommandPaperSizeGalleryUnitsInchesCaption);
      AUnitsConverter := TdxInchesUnits;
    end
    else
    begin
      AUnitsName := cxGetResourceString(@sdxRichEditCommandPaperSizeGalleryUnitsMillimetersCaption);
      AUnitsConverter := TdxMillimetersUnits;
    end;
    Result := Format('%f %s x %f %s', [AUnitsConverter.FromLoMetric(APaper.Width), AUnitsName,
      AUnitsConverter.FromLoMetric(APaper.Height), AUnitsName]);
  end;

  function GetImageName(const APaperKind: TdxPaperKind): string;
  begin
    case APaperKind of
      TdxPaperKind.Letter:
        Result := 'Pages\PaperKind_Letter.png';
      TdxPaperKind.Tabloid:
        Result := 'Pages\PaperKind_Tabloid.png';
      TdxPaperKind.Legal:
        Result := 'Pages\PaperKind_Legal.png';
      TdxPaperKind.Statement:
        Result := 'Miscellaneous\EmptySpace.png';
      TdxPaperKind.Executive:
        Result := 'Pages\PaperKind_Executive.png';
      TdxPaperKind.A3:
        Result := 'Pages\PaperKind_A3.png';
      TdxPaperKind.A4:
        Result := 'Pages\PaperKind_A4.png';
      TdxPaperKind.A5:
        Result := 'Pages\PaperKind_A5.png';
      TdxPaperKind.B4:
        Result := 'Miscellaneous\EmptySpace.png';
      TdxPaperKind.B5:
        Result := 'Miscellaneous\EmptySpace.png';
    else
      Result := '';
    end;
  end;

var
  APaperKind: TdxPaperKind;
  I: Integer;
begin
  for I := 0 to Length(CDefaultPaperKindList) - 1 do
  begin
    APaperKind := CDefaultPaperKindList[I];
    AInfo.Add(APaperKind, '',
      cxGetResourceString(dxPaperKindNames[APaperKind]),
      GetDescription(APaperKind), GetImageName(APaperKind));
  end;
end;

procedure TdxRichEditControlPaperSizeGallery.CreateGalleryCommands;
begin
  FCommand := CreateCommand(RichEditControl);
end;

procedure TdxRichEditControlPaperSizeGallery.DestroyGalleryCommands;
begin
  FreeAndNil(FCommand);
end;

{ TdxRichEditControlPageMarginsGallery }

function TdxRichEditControlPageMarginsGallery.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxProcessPageMarginsGalleryCommand;
end;

function TdxRichEditControlPageMarginsGallery.GetValue: Variant;

  function AreSameMargins(const ATop, ABottom, ALeft, ARight: Single; const AMarginsSet: TMarginsSet): Boolean;
  begin
    Result := (ATop = AMarginsSet[TMarginsPart.mpTop]) and (ABottom = AMarginsSet[TMarginsPart.mpBottom]) and
      (ALeft = AMarginsSet[TMarginsPart.mpLeft]) and (ARight = AMarginsSet[TMarginsPart.mpRight]);
  end;

var
  ATop, ABottom, ALeft, ARight: Single;
begin
  Result := Null;
  if FCommand is TdxProcessPageMarginsGalleryCommand then
  begin
    TdxProcessPageMarginsGalleryCommandAccess(FCommand).GetPageMargins(ATop, ABottom, ALeft, ARight);
    if AreSameMargins(ATop, ABottom, ALeft, ARight, CPredefinedNormalMarginsSet) then
      Result := TPredefinedMarginsSet.pmsNormal
    else if AreSameMargins(ATop, ABottom, ALeft, ARight, CPredefinedNarrowMarginsSet) then
      Result := TPredefinedMarginsSet.pmsNarrow
    else if AreSameMargins(ATop, ABottom, ALeft, ARight, CPredefinedModerateMarginsSet) then
      Result := TPredefinedMarginsSet.pmsModerate
    else if AreSameMargins(ATop, ABottom, ALeft, ARight, CPredefinedWideMarginsSet) then
      Result := TPredefinedMarginsSet.pmsWide
  end;
end;

procedure TdxRichEditControlPageMarginsGallery.SetValue(const AValue: Variant);

  procedure SetMarginsFromMarginsSet(const AMarginsSet: TMarginsSet);
  begin
    TdxProcessPageMarginsGalleryCommandAccess(FCommand).SetPageMargins(AMarginsSet[TMarginsPart.mpTop],
      AMarginsSet[TMarginsPart.mpBottom], AMarginsSet[TMarginsPart.mpLeft], AMarginsSet[TMarginsPart.mpRight]);
  end;

begin
  if FCommand is TdxProcessPageMarginsGalleryCommand then
    case TPredefinedMarginsSet(AValue) of
      TPredefinedMarginsSet.pmsNormal:
        SetMarginsFromMarginsSet(CPredefinedNormalMarginsSet);
      TPredefinedMarginsSet.pmsNarrow:
        SetMarginsFromMarginsSet(CPredefinedNarrowMarginsSet);
      TPredefinedMarginsSet.pmsModerate:
        SetMarginsFromMarginsSet(CPredefinedModerateMarginsSet);
      TPredefinedMarginsSet.pmsWide:
        SetMarginsFromMarginsSet(CPredefinedWideMarginsSet);
    end;
end;

procedure TdxRichEditControlPageMarginsGallery.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);

  function GetMarginsDescription(const AMarginsSet: TMarginsSet): string;
  var
    AUnitsName: string;
  begin
    AUnitsName := cxGetResourceString(@sdxRichEditCommandPaperSizeGalleryUnitsInchesCaption);
    Result :=
      Format('%s:'#9'%2.2f %s    %s:'#9'%2.2f %s', [
        cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryTopPartCaption),
        AMarginsSet[TMarginsPart.mpTop], AUnitsName,
        cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryBottomPartCaption),
        AMarginsSet[TMarginsPart.mpBottom], AUnitsName]) +
      #13#10 +
      Format('%s:'#9'%2.2f %s    %s:'#9'%2.2f %s', [
        cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryLeftPartCaption),
        AMarginsSet[TMarginsPart.mpLeft], AUnitsName,
        cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryRightPartCaption),
        AMarginsSet[TMarginsPart.mpRight], AUnitsName]);
  end;

begin
  AInfo.Add(TPredefinedMarginsSet.pmsNormal, '',
    cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryNormalMarginsCaption),
    GetMarginsDescription(CPredefinedNormalMarginsSet), 'Pages\PageMarginsNormal.png');
  AInfo.Add(TPredefinedMarginsSet.pmsNarrow, '',
    cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryNarrowMarginsCaption),
    GetMarginsDescription(CPredefinedNarrowMarginsSet), 'Pages\PageMarginsNarrow.png');
  AInfo.Add(TPredefinedMarginsSet.pmsModerate, '',
    cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryModerateMarginsCaption),
    GetMarginsDescription(CPredefinedModerateMarginsSet), 'Pages\PageMarginsNormal.png');
  AInfo.Add(TPredefinedMarginsSet.pmsWide, '',
    cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryWideMarginsCaption),
    GetMarginsDescription(CPredefinedWideMarginsSet), 'Pages\PageMarginsWide.png');
end;

procedure TdxRichEditControlPageMarginsGallery.CreateGalleryCommands;
begin
  FCommand := CreateCommand(RichEditControl);
end;

procedure TdxRichEditControlPageMarginsGallery.DestroyGalleryCommands;
begin
  FreeAndNil(FCommand);
end;

{ TdxRichEditControlCustomStylesGalleryAction }

procedure TdxRichEditControlCustomStylesGalleryAction.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxRichEditControlQuickStylesGallery then
    FGalleryGroup := TdxRichEditControlQuickStylesGallery(Source).FGalleryGroup as TdxCustomGalleryGroup;
end;

procedure TdxRichEditControlCustomStylesGalleryAction.UpdateTarget(Target: TObject);
begin
  inherited;
  if (RichEditControl <> nil) and (FGalleryGroup is TdxCustomGalleryGroup) then
    if IsGalleryGroupUpdateNeeded then
    begin
      ShowHourglassCursor;
      FGalleryGroup.Collection.BeginUpdate;
      try
        UpdateGalleryGroup;
      finally
        FGalleryGroup.Collection.EndUpdate;
        HideHourglassCursor;
      end;
    end;
end;

procedure TdxRichEditControlCustomStylesGalleryAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('GalleryGroup', ReadGalleryGroup, WriteGalleryGroup, FGalleryGroup <> nil);
end;

procedure TdxRichEditControlCustomStylesGalleryAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FGalleryGroup) then
    FGalleryGroup := nil;
end;

procedure TdxRichEditControlCustomStylesGalleryAction.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);
begin
  if AInfo.GetGroups is TdxCustomGalleryGroups then
    FGalleryGroup := TdxCustomGalleryGroups(AInfo.GetGroups).Add
  else
    FGalleryGroup := nil;
end;

procedure TdxRichEditControlCustomStylesGalleryAction.CreateGalleryCommands;
begin
  // do nothing
end;

procedure TdxRichEditControlCustomStylesGalleryAction.DestroyGalleryCommands;
begin
  // do nothing
end;

procedure TdxRichEditControlCustomStylesGalleryAction.AddGalleryGroupItem(AStyle: TdxStyleBase);
var
  ABitmap: TcxAlphaBitmap;
  AGalleryGroupItem: TdxCustomGalleryItem;
begin
  AGalleryGroupItem := FGalleryGroup.Items.Add;
  AGalleryGroupItem.Caption := AStyle.StyleName;
  AGalleryGroupItem.Description := '';
  ABitmap := CreateStylePreview(AStyle);
  try
    AGalleryGroupItem.Glyph.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
  AGalleryGroupItem.Tag := NativeInt(AStyle.StyleType);
  TdxCustomGalleryItemAccess(AGalleryGroupItem).FActionIndex := AStyle.StyleName;
end;

function TdxRichEditControlCustomStylesGalleryAction.CanAddGalleryGroupItem(AStyle: TdxStyleBase): Boolean;
begin
  Result := not IsGalleryGroupItemForStyleExists(AStyle);
end;

function TdxRichEditControlCustomStylesGalleryAction.CanDeleteGalleryGroupItem(AGalleryGroupItem: TdxCustomGalleryItem;
  AStyleType: TdxStyleType): Boolean;
begin
  Result := (TdxStyleType(AGalleryGroupItem.Tag) = AStyleType) and
    not IsStyleOfGalleryGroupItemExists(AGalleryGroupItem, AStyleType)
end;

function TdxRichEditControlCustomStylesGalleryAction.IsGalleryGroupItemForStyleExists(AStyle: TdxStyleBase): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FGalleryGroup.Items.Count - 1 do
    if (FGalleryGroup.Items[I].Caption = AStyle.StyleName) and
      (TdxStyleType(FGalleryGroup.Items[I].Tag) = AStyle.StyleType) then
    begin
      Result := True;
      Break;
    end;
end;

function TdxRichEditControlCustomStylesGalleryAction.IsStyleOfGalleryGroupItemExists(AGalleryGroupItem: TdxCustomGalleryItem;
  AStyleType: TdxStyleType): Boolean;
var
  AStyles: TdxStyleCollectionBase;
  I: Integer;
begin
  Result := False;
  AStyles := GetStylesBy(AStyleType);
  if AStyles <> nil then
    for I := 0 to AStyles.Count - 1 do
      if (AStyles[I].StyleName = AGalleryGroupItem.Caption) and
        (AStyles[I].StyleType = TdxStyleType(AGalleryGroupItem.Tag)) then
      begin
        Result := True;
        Break;
      end;
end;

function TdxRichEditControlCustomStylesGalleryAction.IsStylesUpdateNeeded(AStyleType: TdxStyleType): Boolean;
var
  AStyles: TdxStyleCollectionBase;
  I: Integer;
begin
  Result := False;
  AStyles := GetStylesBy(AStyleType);
  if AStyles <> nil then
  begin
    for I := 0 to AStyles.Count - 1 do
      if CanAddGalleryGroupItem(AStyles[I]) then
        Exit(True);

    for I := FGalleryGroup.Items.Count - 1 downto 0 do
      if CanDeleteGalleryGroupItem(FGalleryGroup.Items[I], AStyleType) then
        Exit(True);
  end;
end;

function TdxRichEditControlCustomStylesGalleryAction.GetStylesBy(AStyleType: TdxStyleType): TdxStyleCollectionBase;
begin
  case AStyleType of
    TdxStyleType.CharacterStyle:
      Result := RichEditControl.InnerControl.DocumentModel.CharacterStyles;
    TdxStyleType.ParagraphStyle:
      Result := RichEditControl.InnerControl.DocumentModel.ParagraphStyles;
    TdxStyleType.TableStyle:
      Result := RichEditControl.InnerControl.DocumentModel.TableStyles;
  else
    Result := nil;
  end;
end;

procedure TdxRichEditControlCustomStylesGalleryAction.PopulateStyles(AStyleType: TdxStyleType);
var
  AStyles: TdxStyleCollectionBase;
  I: Integer;
begin
  AStyles := GetStylesBy(AStyleType);
  if AStyles <> nil then
  begin
    for I := 0 to AStyles.Count - 1 do
      if CanAddGalleryGroupItem(AStyles[I]) then
        AddGalleryGroupItem(AStyles[I]);

    for I := FGalleryGroup.Items.Count - 1 downto 0 do
      if CanDeleteGalleryGroupItem(FGalleryGroup.Items[I], AStyleType) then
        FGalleryGroup.Items.Delete(I);
  end;
end;

procedure TdxRichEditControlCustomStylesGalleryAction.ReadGalleryGroup(Reader: TReader);
var
  AGalleryGroupIdent: string;
  AOwner: TComponent;
begin
  AGalleryGroupIdent := Reader.ReadIdent;
  AOwner := Owner;
  while AOwner.Owner is TWinControl do
    AOwner := AOwner.Owner;
  FGalleryGroup := AOwner.FindComponent(AGalleryGroupIdent) as TdxCustomGalleryGroup;
end;

procedure TdxRichEditControlCustomStylesGalleryAction.WriteGalleryGroup(Writer: TWriter);
begin
  Writer.WriteIdent(TComponent(FGalleryGroup).Name);
end;

{ TdxRichEditControlQuickStylesGallery }

function TdxRichEditControlQuickStylesGallery.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxQuickStylesGalleryCommand;
end;

function TdxRichEditControlQuickStylesGallery.GetValue: Variant;
var
  AStyle: TdxStyleBase;
begin
  Result := Null;
  if RichEditControl <> nil then
  begin
    AStyle := GetActiveStyle;
    if AStyle <> nil then
      Result := AStyle.StyleName;
  end;
end;

procedure TdxRichEditControlQuickStylesGallery.SetValue(const AValue: Variant);
var
  AModel: TdxDocumentModel;
  AStyleName: string;
begin
  if RichEditControl <> nil then
  begin
    AStyleName := VarToStr(AValue);
    AModel := RichEditControl.InnerControl.DocumentModel;
    if not SetActiveStyle(AModel.ParagraphStyles.GetStyleByName(AStyleName)) then
      SetActiveStyle(AModel.CharacterStyles.GetStyleByName(AStyleName));
  end;
end;

function TdxRichEditControlQuickStylesGallery.CanAddGalleryGroupItem(AStyle: TdxStyleBase): Boolean;
begin
  Result := inherited CanAddGalleryGroupItem(AStyle) and AStyle.Primary;
end;

function TdxRichEditControlQuickStylesGallery.CreateStylePreview(AStyle: TdxStyleBase): TcxAlphaBitmap;
var
  AContentRect, ARect: TRect;
  AStyleName: string;
begin
  Result := TcxAlphaBitmap.CreateSize(cxGetValueCurrentDPI(64), cxGetValueCurrentDPI(48));
  AContentRect := cxRectInflate(Result.ClientRect, -cxGetValueCurrentDPI(3));

  ARect := cxRectSetBottom(AContentRect, AContentRect.Bottom, cxTextHeight(Result.Canvas.Font));
  AStyleName := AStyle.LocalizedStyleName;
  if AStyle.StyleType = TdxStyleType.ParagraphStyle then
    AStyleName := #182 + ' ' + AStyleName;
  cxDrawText(Result.Canvas, AStyleName, ARect, DT_CENTER or DT_END_ELLIPSIS);

  ARect.Bottom := ARect.Top - cxGetValueCurrentDPI(3);
  ARect.Top := AContentRect.Top;
  ApplyStyleToFont(AStyle, Result.Canvas.Font);
  cxDrawText(Result.Canvas, 'AaBbCcDd', ARect, DT_VCENTER or DT_SINGLELINE);

  Result.RecoverTransparency(clNone);
end;

function TdxRichEditControlQuickStylesGallery.IsGalleryGroupUpdateNeeded: Boolean;
begin
  Result := IsStylesUpdateNeeded(TdxStyleType.ParagraphStyle) or IsStylesUpdateNeeded(TdxStyleType.CharacterStyle);
end;

procedure TdxRichEditControlQuickStylesGallery.UpdateGalleryGroup;
begin
  PopulateStyles(TdxStyleType.ParagraphStyle);
  PopulateStyles(TdxStyleType.CharacterStyle);
end;

procedure TdxRichEditControlQuickStylesGallery.ApplyStyleToFont(AStyle: TdxStyleBase; AFont: TFont);
var
  ACharacterProperties: TdxCharacterProperties;
begin
  ACharacterProperties := GetCharacterProperties(AStyle);
  if ACharacterProperties <> nil then
  begin
    AFont.Name := ACharacterProperties.FontName;
    AFont.Size := ACharacterProperties.DoubleFontSize div 2;
    if ACharacterProperties.FontBold then
      AFont.Style := AFont.Style + [fsBold];
    if ACharacterProperties.FontItalic then
      AFont.Style := AFont.Style + [fsItalic];
    if ACharacterProperties.FontUnderlineType <> TdxRichEditUnderlineType.None then
      AFont.Style := AFont.Style + [fsUnderline];
    AFont.Color := dxAlphaColorToColor(ACharacterProperties.ForeColor);
  end;
end;

function TdxRichEditControlQuickStylesGallery.GetCharacterProperties(AStyle: TdxStyleBase): TdxCharacterProperties;
begin
  case AStyle.StyleType of
    TdxStyleType.ParagraphStyle:
      Result := TdxParagraphStyle(AStyle).CharacterProperties;
    TdxStyleType.CharacterStyle:
      Result := TdxCharacterStyle(AStyle).CharacterProperties;
  else
    Result := nil;
  end;
end;

function TdxRichEditControlQuickStylesGallery.GetActiveStyle: TdxStyleBase;
var
  ACharacterStyle, AParagraphStyle: TdxStyleBase;
  AIsSingleCharacterStyle, AIsSingleParagraphStyle: Boolean;
  AModel: TdxDocumentModel;
  ARun: TdxTextRunBase;
  AStartIndex, AEndIndex, I: TdxRunIndex;
  ASum: Integer;
begin
  AModel := RichEditControl.InnerControl.DocumentModel;

  AStartIndex := AModel.Selection.Interval.NormalizedStart.RunIndex;
  AEndIndex := AModel.Selection.Interval.NormalizedEnd.RunIndex;

  ACharacterStyle := AModel.ActivePieceTable.Runs[AStartIndex].CharacterStyle;
  AParagraphStyle := AModel.ActivePieceTable.Runs[AStartIndex].Paragraph.ParagraphStyle;

  AIsSingleCharacterStyle := True;
  AIsSingleParagraphStyle := True;
  ASum := AModel.ActivePieceTable.Runs[AStartIndex].CharacterStyleIndex;
  for I := AStartIndex + 1 to AEndIndex do
  begin
    ARun := AModel.ActivePieceTable.Runs[I];
    AIsSingleCharacterStyle := AIsSingleCharacterStyle and (ARun.CharacterStyle = ACharacterStyle);
    AIsSingleParagraphStyle := AIsSingleParagraphStyle and (ARun.Paragraph.ParagraphStyle = AParagraphStyle);
    ASum := ASum + ARun.CharacterStyleIndex;
  end;

  if AIsSingleParagraphStyle and (ASum = 0) then
    Result := AParagraphStyle
  else
    if AIsSingleCharacterStyle then
      Result := ACharacterStyle
    else
      Result := nil;
end;

function TdxRichEditControlQuickStylesGallery.SetActiveStyle(AStyle: TdxStyleBase): Boolean;
var
  AModel: TdxDocumentModel;
  I: TdxRunIndex;
begin
  Result := AStyle <> nil;
  if Result then
  begin
    AModel := RichEditControl.InnerControl.DocumentModel;
    for I := AModel.Selection.Interval.NormalizedStart.RunIndex to AModel.Selection.Interval.NormalizedEnd.RunIndex do
      case AStyle.StyleType of
        TdxStyleType.ParagraphStyle:
        begin
          AModel.ActivePieceTable.Runs[I].CharacterStyleIndex := 0;
          AModel.ActivePieceTable.Runs[I].Paragraph.ParagraphStyleIndex := AModel.ParagraphStyles.IndexOf(AStyle);
        end;
        TdxStyleType.CharacterStyle:
        begin
          AModel.ActivePieceTable.Runs[I].Paragraph.ParagraphStyleIndex := 0;
          AModel.ActivePieceTable.Runs[I].CharacterStyleIndex := AModel.CharacterStyles.IndexOf(AStyle);
        end;
      end;
  end;
end;

{ TdxRichEditControlTableStylesGallery }

function TdxRichEditControlTableStylesGallery.GetCommandClass: TdxControlCommandClass;
begin
  Result := TdxTableStylesGalleryCommand;
end;

function TdxRichEditControlTableStylesGallery.GetValue: Variant;
var
  AStyleName: string;
begin
  Result := Null;
  if RichEditControl <> nil then
  begin
    AStyleName := GetActiveStyleName;
    if AStyleName <> '' then
      Result := AStyleName;
  end;
end;

procedure TdxRichEditControlTableStylesGallery.SetValue(const AValue: Variant);
begin
  if RichEditControl <> nil then
    SetActiveStyleName(VarToStr(AValue));
end;

function TdxRichEditControlTableStylesGallery.CreateStylePreview(AStyle: TdxStyleBase): TcxAlphaBitmap;

  procedure DrawBorders(ACanvas: TCanvas; ATable: TdxTable;
    const ACellWidth, ACellHeight, ALeftImageLocation, ATopImageLocation: Integer);
  begin
    ATable.ForEachCell(
      procedure(ACell: TdxTableCell)
      begin
        DrawBordersCore(ACanvas, ACell, ACellWidth, ACellHeight, ALeftImageLocation, ATopImageLocation);
      end);
  end;

  procedure DrawStyleBackground(ACanvas: TCanvas; ATable: TdxTable;
    const ACellWidth, ACellHeight, ALeftImageLocation, ATopImageLocation: Integer);
  begin
    ATable.ForEachCell(
      procedure(ACell: TdxTableCell)
      begin
        DrawStyleBackgroundCore(ACanvas, ACell, ACellWidth, ACellHeight, ALeftImageLocation, ATopImageLocation);
      end);
  end;

var
  ADocumentModel: TdxDocumentModel;
begin
  ADocumentModel := FTempRichEditControl.DocumentModel;
  AStyle.Copy(ADocumentModel);
  FTempTable.StyleIndex := ADocumentModel.TableStyles.GetStyleIndexByName(AStyle.StyleName);
  Result := TcxAlphaBitmap.CreateSize(Rect(0, 0, 65, 46));
  DrawStyleBackground(Result.Canvas, FTempTable, 12, 8, 2, 2);
  DrawBorders(Result.Canvas, FTempTable, 12, 8, 2, 2);
  Result.RecoverTransparency(clNone);
end;

function TdxRichEditControlTableStylesGallery.IsGalleryGroupUpdateNeeded: Boolean;
begin
  Result := IsStylesUpdateNeeded(TdxStyleType.TableStyle);
end;

procedure TdxRichEditControlTableStylesGallery.UpdateGalleryGroup;
var
  APieceTable: TdxPieceTable;
begin
  FTempRichEditControl := TdxRichEditControl.Create(nil);
  try
    FTempRichEditControl.DocumentModel.BeginUpdate;
    try
      APieceTable := FTempRichEditControl.DocumentModel.ActivePieceTable;
      FTempTable := APieceTable.InsertTable(APieceTable.DocumentStartLogPosition, 5, 5);

      PopulateStyles(TdxStyleType.TableStyle);
    finally
      FTempRichEditControl.DocumentModel.CancelUpdate;
    end;
  finally
    FTempRichEditControl.Free;
  end;
end;

procedure TdxRichEditControlTableStylesGallery.DrawBordersCore(ACanvas: TCanvas; ACell: TdxTableCell;
  const ACellWidth, ACellHeight, ALeftImageLocation, ATopImageLocation: Integer);

  procedure DrawBorder(ACanvas: TCanvas; ABorder: TdxBorderBase;
    const AStart, AFinish, AStartBold, AFinishBold: TPoint);
  var
    AColor: TColor;
  begin
    if (ABorder.Style = TdxBorderLineStyle.Nil) or (ABorder.Style = TdxBorderLineStyle.None) then
			Exit;
    AColor := ACanvas.Pen.Color;
    try
      ACanvas.Pen.Color := dxAlphaColorToColor(ABorder.Color);

      if ABorder.Width > 20 then
      begin
        ACanvas.MoveTo(AStartBold.X - 1, AStartBold.Y - 1);
        ACanvas.LineTo(AFinishBold.X - 1, AStartBold.Y - 1);

        ACanvas.MoveTo(AStartBold.X - 1, AFinishBold.Y);
        ACanvas.LineTo(AFinishBold.X - 1, AFinishBold.Y);
      end
      else
      begin
        ACanvas.MoveTo(AStart.X, AStart.Y);
        ACanvas.LineTo(AFinish.X, AFinish.Y);
      end;
    finally
      ACanvas.Pen.Color := AColor;
    end;
  end;

  procedure DrawCharacterLine(ACanvas: TCanvas; ACell: TdxTableCell;
    const AHorizontalCellLocation, AVerticalCellLocation: Integer);
  var
    AColor: TColor;
    AParagraphIndex: TdxParagraphIndex;
    APieceTable: TdxSimplePieceTable;
    ARunIndex: TdxRunIndex;
  begin
    APieceTable := TdxSimpleDocumentModelAccess(ACell.DocumentModel).PieceTable;
    AParagraphIndex := ACell.StartParagraphIndex;
		ARunIndex := APieceTable.Paragraphs[AParagraphIndex].FirstRunIndex;
    AColor := ACanvas.Pen.Color;
    try
      ACanvas.Pen.Color := dxAlphaColorToColor(APieceTable.Runs[ARunIndex].ForeColor);

      ACanvas.MoveTo(AHorizontalCellLocation + 4, AVerticalCellLocation + 5);
      ACanvas.LineTo(AHorizontalCellLocation + 9, AVerticalCellLocation + 5);
    finally
      ACanvas.Pen.Color := AColor;
    end;
  end;

var
  ALeftCellLocation, ATopCellLocation, ARightCellLocation, ABottomCellLocation: Integer;
  ALeftTopCell, ALeftBottomCell, ARightTopCell, ARightBottomCell: TPoint;
  ALeftTopCellBold, ALeftBottomCellBold, ARightTopCellBold, ARightBottomCellBold: TPoint;
begin
  ALeftCellLocation := ALeftImageLocation + ACellWidth * ACell.IndexInRow;
  ATopCellLocation := ATopImageLocation + ACellHeight * ACell.RowIndex;
  ARightCellLocation := ALeftCellLocation + ACellWidth;
  ABottomCellLocation := ATopCellLocation + ACellHeight;

  ALeftTopCell := Point(ALeftCellLocation, ATopCellLocation);
  ALeftBottomCell := Point(ALeftCellLocation, ABottomCellLocation);
  ARightTopCell := Point(ARightCellLocation, ATopCellLocation);
  ARightBottomCell := Point(ARightCellLocation, ABottomCellLocation);

  ALeftTopCellBold := Point(ALeftCellLocation + 1, ATopCellLocation + 1);
  ALeftBottomCellBold := Point(ALeftCellLocation + 1, ABottomCellLocation + 1);
  ARightTopCellBold := Point(ARightCellLocation + 1, ATopCellLocation + 1);
  ARightBottomCellBold := Point(ARightCellLocation + 1, ABottomCellLocation + 1);

  DrawBorder(ACanvas, ACell.GetActualLeftCellBorder,
    ALeftTopCell, ALeftBottomCell, ALeftTopCellBold, ALeftBottomCellBold);
  DrawBorder(ACanvas, ACell.GetActualRightCellBorder,
    ARightTopCell, ARightBottomCell, ARightTopCellBold, ARightBottomCellBold);
  DrawBorder(ACanvas, ACell.GetActualTopCellBorder,
    ALeftTopCell, ARightTopCell, ALeftTopCellBold, ARightTopCellBold);
  DrawBorder(ACanvas, ACell.GetActualBottomCellBorder,
    ALeftBottomCell, ARightBottomCell, ALeftBottomCellBold, ARightBottomCellBold);
  DrawCharacterLine(ACanvas, ACell, ALeftCellLocation, ATopCellLocation);
end;

procedure TdxRichEditControlTableStylesGallery.DrawStyleBackgroundCore(ACanvas: TCanvas; ACell: TdxTableCell;
  const ACellWidth, ACellHeight, ALeftImageLocation, ATopImageLocation: Integer);
var
  ALeftCellLocation, ATopCellLocation: Integer;
begin
  ALeftCellLocation := ALeftImageLocation + ACellWidth * ACell.IndexInRow;
  ATopCellLocation := ATopImageLocation + ACellHeight * ACell.RowIndex;
  FillRectByColor(ACanvas.Handle,
    Rect(ALeftCellLocation, ATopCellLocation, ALeftCellLocation + ACellWidth, ATopCellLocation + ACellHeight),
    dxAlphaColorToColor(ACell.GetActualBackgroundColor));
end;

procedure TdxRichEditControlTableStylesGallery.ForEachSelection(
  AProcessSelectedTablesProcRef: TdxProcessSelectedTablesProcRef);
var
  ACell: IdxRichEditTableCell;
  ADocument: IdxRichEditDocument;
  ASubDocument: IdxRichEditSubDocument;
  AFullySelectedTables: IdxRichEditReadOnlyTableCollection;
  AStartTable, AEndTable: IdxRichEditTable;
  I: Integer;
begin
  ADocument := RichEditControl.Document;
  for I := 0 to ADocument.Selections.Count - 1 do
  begin
    ASubDocument := ADocument.ActiveSubDocument;
    AFullySelectedTables := ASubDocument.Tables.Get(ADocument.Selections[I]);

    ACell := ASubDocument.Tables.GetTableCell(ADocument.Selections[I].Start);
    AStartTable := nil;
    if (ACell <> nil) and ((AFullySelectedTables.Count = 0) or
      (ACell.Table.Range.Start.CompareTo(AFullySelectedTables[0].Range.Start) <> 0)) then
      AStartTable := ACell.Table;

    ACell := ASubDocument.Tables.GetTableCell(ADocument.Selections[I].&End);
    AEndTable := nil;
    if (ACell <> nil) and ((AFullySelectedTables.Count = 0) or
      (ACell.Table.Range.Start.CompareTo(AFullySelectedTables[AFullySelectedTables.Count - 1].Range.Start) <> 0)) then
      AEndTable := ACell.Table;

    if AStartTable = AEndTable then
      AEndTable := nil;

    AProcessSelectedTablesProcRef(AFullySelectedTables, AStartTable, AEndTable);
  end;
end;

function TdxRichEditControlTableStylesGallery.GetActiveStyleName: string;
var
  AIsSingleTableStyle: Boolean;
  ATableStyleName: string;
begin
  AIsSingleTableStyle := True;
  ATableStyleName := '';

  ForEachSelection(
    procedure (AFullySelectedTables: IdxRichEditReadOnlyTableCollection; AStartTable, AEndTable: IdxRichEditTable)
    var
      I: Integer;
    begin
      if AIsSingleTableStyle and (AStartTable <> nil) and (AStartTable.Style <> nil) then
      begin
        if ATableStyleName <> '' then
          AIsSingleTableStyle := AStartTable.Style.Name = ATableStyleName
        else
          ATableStyleName := AStartTable.Style.Name;
      end;

      if AIsSingleTableStyle and (AFullySelectedTables.Count > 0) then
      begin
        if (ATableStyleName = '') and (AFullySelectedTables[0].Style <> nil) then
          ATableStyleName := AFullySelectedTables[0].Style.Name;
        for I := 0 to AFullySelectedTables.Count - 1 do
          if AFullySelectedTables[I].Style <> nil then
            AIsSingleTableStyle := AIsSingleTableStyle and (AFullySelectedTables[I].Style.Name = ATableStyleName);
      end;

      if AIsSingleTableStyle and (AEndTable <> nil) and (AEndTable.Style <> nil) then
      begin
        if ATableStyleName <> '' then
          AIsSingleTableStyle := AEndTable.Style.Name = ATableStyleName
        else
          ATableStyleName := AEndTable.Style.Name;
      end;
    end);

  if AIsSingleTableStyle then
    Result := ATableStyleName
  else
    Result := '';
end;

procedure TdxRichEditControlTableStylesGallery.SetActiveStyleName(const AStyleName: string);
var
  ATableStyle: IdxRichEditTableStyle;
begin
  if AStyleName <> '' then
  begin
    ATableStyle := RichEditControl.Document.TableStyles[AStyleName];
    ForEachSelection(
      procedure (AFullySelectedTables: IdxRichEditReadOnlyTableCollection; AStartTable, AEndTable: IdxRichEditTable)
      var
        I: Integer;
      begin
        if AStartTable <> nil then
          AStartTable.Style := ATableStyle;

        for I := 0 to AFullySelectedTables.Count - 1 do
          AFullySelectedTables[I].Style := ATableStyle;

        if AEndTable <> nil then
          AEndTable.Style := ATableStyle;
      end);
  end;
end;

end.
