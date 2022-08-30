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

unit dxRichEdit.Commands.ChangeProperties;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Controls, Graphics,
  dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.View.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.PieceTableModifiers.Simple,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.Options,
  dxGenerics;

type
  { IdxValueBasedCommandUIState }

  IdxValueBasedCommandUIState<T> = interface(IdxCommandUIState)
  ['{AF8E926A-6AFD-413B-B89D-0687A4D7CC78}']
    function GetValue: T;
    procedure SetValue(const Value: T);
    property Value: T read GetValue write SetValue;
  end;

  { TdxChangeCharacterPropertiesCommandBase }

  TdxChangeCharacterPropertiesCommandBase = class abstract (TdxSelectionBasedPropertyChangeCommandBase)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function ChangeCharacterFormatting(ALogPositionFrom, ALogPositionTo: TdxDocumentLogPosition;
      AModifier: TdxRunPropertyModifierBase): TdxDocumentModelChangeActions; virtual;
    procedure ChangeInputPositionCharacterFormatting(AModifier: TdxRunPropertyModifierBase); virtual; abstract;
  end;

  { TdxChangeParagraphFormattingCommandBase }

  TdxChangeParagraphFormattingCommandBase<T> = class abstract(TdxSelectionBasedPropertyChangeCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    procedure ChangeParagraphFormatting(ALogPositionFrom: TdxDocumentLogPosition;
      ALogPositionTo: TdxDocumentLogPosition; AModifier: TdxParagraphPropertyModifier<T>);
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<T>; virtual; abstract;
  end;

  { TdxChangeParagraphFirstLineIndentCommand }

  TdxChangeParagraphFirstLineIndentCommand = class(TdxChangeParagraphFormattingCommandBase<Integer>)
  strict private
    FNewIndent: Integer;
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;
  public
    constructor Create(const AControl: IdxRichEditControl; ANewIndent: Integer); reintroduce; virtual;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeParagraphLeftIndentCommand }

  TdxChangeParagraphLeftIndentCommand = class(TdxChangeParagraphFormattingCommandBase<Integer>)
  strict private
    FNewIndent: Integer;
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;
  public
    constructor Create(const AControl: IdxRichEditControl; ANewIndent: Integer); reintroduce; virtual;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeParagraphRightIndentCommand }

  TdxChangeParagraphRightIndentCommand = class(TdxChangeParagraphFormattingCommandBase<Integer>)
  strict private
    FNewIndent: Integer;
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;
  public
    constructor Create(const AControl: IdxRichEditControl; ANewIndent: Integer); reintroduce; virtual;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleChangeParagraphFormattingCommandBase }

  TdxToggleChangeParagraphFormattingCommandBase<T> = class abstract(TdxChangeParagraphFormattingCommandBase<T>)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function IsChecked: Boolean;
    function IsCheckedCore(ALogPositionFrom, ALogPositionTo: TdxDocumentLogPosition; AModifier: TdxParagraphPropertyModifier<T>): Boolean;
    function IsCheckedValue(AValue: T): Boolean; virtual; abstract;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); reintroduce;
  end;

  { TdxToggleParagraphAlignmentLeftCommand }

  TdxToggleParagraphAlignmentLeftCommand = class(TdxToggleChangeParagraphFormattingCommandBase<TdxParagraphAlignment>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>; override;
    function IsCheckedValue(AValue: TdxParagraphAlignment): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleParagraphAlignmentCenterCommand }

  TdxToggleParagraphAlignmentCenterCommand = class(TdxToggleChangeParagraphFormattingCommandBase<TdxParagraphAlignment>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>; override;
    function IsCheckedValue(AValue: TdxParagraphAlignment): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleParagraphAlignmentRightCommand }

  TdxToggleParagraphAlignmentRightCommand = class(TdxToggleChangeParagraphFormattingCommandBase<TdxParagraphAlignment>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>; override;
    function IsCheckedValue(AValue: TdxParagraphAlignment): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleParagraphAlignmentJustifyCommand }

  TdxToggleParagraphAlignmentJustifyCommand = class(TdxToggleChangeParagraphFormattingCommandBase<TdxParagraphAlignment>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>; override;
    function IsCheckedValue(AValue: TdxParagraphAlignment): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetSingleParagraphSpacingCommand }

  TdxSetSingleParagraphSpacingCommand = class(TdxToggleChangeParagraphFormattingCommandBase<TdxParagraphLineSpacing>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphLineSpacing>; override;
    function IsCheckedValue(AValue: TdxParagraphLineSpacing): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetSesquialteralParagraphSpacingCommand }

  TdxSetSesquialteralParagraphSpacingCommand = class(TdxToggleChangeParagraphFormattingCommandBase<TdxParagraphLineSpacing>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphLineSpacing>; override;
    function IsCheckedValue(AValue: TdxParagraphLineSpacing): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetDoubleParagraphSpacingCommand }

  TdxSetDoubleParagraphSpacingCommand = class(TdxToggleChangeParagraphFormattingCommandBase<TdxParagraphLineSpacing>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphLineSpacing>; override;
    function IsCheckedValue(AValue: TdxParagraphLineSpacing): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeCharacterFormattingCommandBase }

  TdxChangeCharacterFormattingCommandBase<T> = class abstract(TdxChangeCharacterPropertiesCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure ChangeInputPositionCharacterFormatting(AModifier: TdxRunPropertyModifierBase); override;

    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<T>; virtual; abstract;
    function GetCurrentPropertyValue(out AValue: T): Boolean;
    function ObtainRunsPropertyValue(const AStart: TdxDocumentModelPosition; ALength: Integer;
      AModifier: TdxRunPropertyModifier<T>; out AValue: T): Boolean; virtual;
  end;

  { TdxDefaultValueBasedCommandUIState<T> }

  TdxDefaultValueBasedCommandUIState<T> = class(TdxDefaultCommandUIState,
    IdxValueBasedCommandUIState<T>, IdxCommandUIState)
  private
    FValue: T;
    //IdxValueBasedCommandUIState<T>
    function GetValue: T;
  protected
    procedure SetValue(const Value: T); virtual;
  public
    property Value: T read FValue write FValue;
  end;

  { TdxDefaultObjectValueBasedCommandUIState<T> }

  TdxDefaultObjectValueBasedCommandUIState<T: class> = class(TdxDefaultValueBasedCommandUIState<T>)
  protected
    procedure SetValue(const Value: T); override;
  public
    destructor Destroy; override;
  end;

  { TdxChangeFontNameCommand }

  TdxChangeFontNameCommand = class(TdxChangeCharacterFormattingCommandBase<string>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<string>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeFontSizeCommand }

  TdxChangeFontSizeCommand = class(TdxChangeCharacterFormattingCommandBase<Single>)
  public const
    InvalidValue = -1;
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Single>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleChangeCharacterFormattingCommandBase }

  TdxToggleChangeCharacterFormattingCommandBase<T> = class abstract(TdxChangeCharacterFormattingCommandBase<T>)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function IsChecked: Boolean;
    function IsCheckedValue(AValue: T): Boolean; virtual; abstract;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); reintroduce;
  end;

  { TdxToggleCharacterFormattingBoolPropertyCommand }

  TdxToggleCharacterFormattingBoolPropertyCommand = class abstract(TdxToggleChangeCharacterFormattingCommandBase<Boolean>)
  protected
    function IsCheckedValue(AValue: Boolean): Boolean; override;
  end;

  { TdxToggleFontBoldCommand }

  TdxToggleFontBoldCommand = class(TdxToggleCharacterFormattingBoolPropertyCommand)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Boolean>; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleFontItalicCommand }

  TdxToggleFontItalicCommand = class(TdxToggleCharacterFormattingBoolPropertyCommand)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Boolean>; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleFontUnderlineCommandBase }

  TdxToggleFontUnderlineCommandBase = class abstract(TdxToggleChangeCharacterFormattingCommandBase<TdxUnderlineType>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxUnderlineType>; override;
    function IsCheckedValue(AValue: TdxUnderlineType): Boolean; override;

    function GetUnderlineType: TdxUnderlineType; virtual; abstract;
  end;

  { TdxToggleFontUnderlineCommand }

  TdxToggleFontUnderlineCommand = class(TdxToggleFontUnderlineCommandBase)
  protected
    function GetUnderlineType: TdxUnderlineType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleFontDoubleUnderlineCommand }

  TdxToggleFontDoubleUnderlineCommand = class(TdxToggleFontUnderlineCommandBase)
  protected
    function GetUnderlineType: TdxUnderlineType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleFontStrikeoutCommandBase }

  TdxToggleFontStrikeoutCommandBase = class(TdxToggleChangeCharacterFormattingCommandBase<TdxStrikeoutType>)
  protected
    function GetStrikeoutType: TdxStrikeoutType; virtual; abstract;
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxStrikeoutType>; override;
    function IsCheckedValue(AValue: TdxStrikeoutType): Boolean; override;
  end;

  { TdxToggleFontStrikeoutCommand }

  TdxToggleFontStrikeoutCommand = class(TdxToggleFontStrikeoutCommandBase)
  protected
    function GetStrikeoutType: TdxStrikeoutType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleFontDoubleStrikeoutCommand }

  TdxToggleFontDoubleStrikeoutCommand = class(TdxToggleFontStrikeoutCommandBase)
  protected
    function GetStrikeoutType: TdxStrikeoutType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeFontColorCommand }

  TdxChangeFontColorCommand = class(TdxChangeCharacterFormattingCommandBase<TdxAlphaColor>)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxAlphaColor>; override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeFontBackColorCommand }

  TdxChangeFontBackColorCommand = class(TdxChangeCharacterFormattingCommandBase<TdxAlphaColor>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxAlphaColor>; override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

  { TdxChangeFontBackColorByMouseCommand }

  TdxChangeFontBackColorByMouseCommand = class(TdxRichEditCommand)
  private
    FInternalCommand: TdxChangeFontBackColorCommand;
  protected
    procedure ExecuteCore; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property InternalCommand: TdxChangeFontBackColorCommand read FInternalCommand;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); override;
    destructor Destroy; override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function IsChangeByMouse: Boolean; virtual;
  end;

  { TdxIncrementFontSizeCommand }

  TdxIncrementFontSizeCommand = class(TdxChangeCharacterFormattingCommandBase<Integer>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Integer>; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDecrementFontSizeCommand }

  TdxDecrementFontSizeCommand = class(TdxChangeCharacterFormattingCommandBase<Integer>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Integer>; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxIncreaseFontSizeCommand }

  TdxIncreaseFontSizeCommand = class(TdxChangeCharacterFormattingCommandBase<Integer>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Integer>; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDecreaseFontSizeCommand }

  TdxDecreaseFontSizeCommand = class(TdxChangeCharacterFormattingCommandBase<Integer>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Integer>; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleFontSuperscriptCommand }

  TdxToggleFontSuperscriptCommand = class(TdxToggleChangeCharacterFormattingCommandBase<TdxCharacterFormattingScript>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxCharacterFormattingScript>; override;
    function IsCheckedValue(AValue: TdxCharacterFormattingScript): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleFontSubscriptCommand }

  TdxToggleFontSubscriptCommand = class(TdxToggleChangeCharacterFormattingCommandBase<TdxCharacterFormattingScript>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxCharacterFormattingScript>; override;
    function IsCheckedValue(AValue: TdxCharacterFormattingScript): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeParagraphIndentCommandBase<T> }

  TdxChangeParagraphIndentCommandBase<T> = class abstract(TdxChangeParagraphFormattingCommandBase<T>)
  private
    FTabsList: TdxIntegerList;
    function GetStartParagraphIndex: TdxParagraphIndex;
    function GetEndParagraphIndex: TdxParagraphIndex;
    function GetDefaultTabWidth: Integer;
  protected
    procedure FillTabsList; virtual;
    procedure AddParagraphTabs(AParagraph: TdxSimpleParagraph); virtual;
    function GetNearRightDefaultTab(ALeftIndent: Integer): Integer;
    function GetNearRightTab(ALeftIndent: Integer): Integer;
    function GetNearLeftDefaultTab(ALeftIndent: Integer): Integer;
    function GetNearLeftTab(ALeftIndent: Integer): Integer;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property TabsList: TdxIntegerList read FTabsList write FTabsList;
    property StartParagraphIndex: TdxParagraphIndex read GetStartParagraphIndex;
    property EndParagraphIndex: TdxParagraphIndex read GetEndParagraphIndex;
    property DefaultTabWidth: Integer read GetDefaultTabWidth;
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce;
    destructor Destroy; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxIncrementParagraphLeftIndentCommand }

  TdxIncrementParagraphLeftIndentCommand = class(TdxChangeParagraphIndentCommandBase<Integer>)
  private
    function GetCaretPosition: TdxCaretPosition;
  protected
    procedure ModifyDocumentModel(const AState: IdxCommandUIState); override;
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;

    property CaretPosition: TdxCaretPosition read GetCaretPosition;
  end;

  { TdxIncrementParagraphFirstLineIndentCommand }

  TdxIncrementParagraphFirstLineIndentCommand = class(TdxChangeParagraphIndentCommandBase<Integer>)
  private
    function GetCaretPosition: TdxCaretPosition;
  protected
    procedure ModifyDocumentModel(const AState: IdxCommandUIState); override;
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;

    property CaretPosition: TdxCaretPosition read GetCaretPosition;
  end;

  { TdxIncrementParagraphIndentCommand }

  TdxIncrementParagraphIndentCommand = class(TdxChangeIndentCommand)
  protected
    procedure ExecuteCore; override;
    function CreateIncrementParagraphFirstLineIndentCommand: TdxIncrementParagraphFirstLineIndentCommand; virtual;
    function CreateIncrementParagraphLeftIndentCommand: TdxIncrementParagraphLeftIndentCommand; virtual;
    function IsFirstLineIndentLessDefaultTabSize: Boolean; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxIncrementIndentByTheTabCommand }

  TdxIncrementIndentByTheTabCommand = class(TdxIncrementIndentCommand)
  protected
    procedure IncrementParagraphIndent; override;
    function CreateIncrementParagraphIndentCommand: TdxIncrementParagraphIndentCommand; virtual;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDecrementParagraphLeftIndentCommand }

  TdxDecrementParagraphLeftIndentCommand = class(TdxChangeParagraphIndentCommandBase<Integer>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDecrementParagraphFirstLineIndentCommand }

  TdxDecrementParagraphFirstLineIndentCommand = class(TdxChangeParagraphIndentCommandBase<Integer>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;
  end;

  { TdxDecrementParagraphIndentCommand }

  TdxDecrementParagraphIndentCommand = class(TdxChangeIndentCommand)
  protected
    procedure ExecuteCore; override;
    function FirstLineIndentIsPositive: Boolean; virtual;
    function CreateDecrementParagraphLeftIndentCommand: TdxDecrementParagraphLeftIndentCommand; virtual;
    procedure DecrementParagraphFirstLineIndent; virtual;
    function CreateDecrementParagraphFirstLineIndentCommand: TdxDecrementParagraphFirstLineIndentCommand; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDecrementIndentByTheTabCommand }

  TdxDecrementIndentByTheTabCommand = class(TdxDecrementIndentCommand)
  protected
    procedure DecrementParagraphIndent; override;
    function CreateDecrementParagraphIndentCommand: TdxDecrementParagraphIndentCommand; virtual;
  end;

  { TdxChangeCaseCommandBase }

  TdxChangeCaseCommandBase = class abstract(TdxChangeCharacterPropertiesCommandBase)
  protected
    procedure ChangeInputPositionCharacterFormatting(AModifier: TdxRunPropertyModifierBase); override;
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    function CreateModifier: TdxRunChangeCaseModifierBase; virtual; abstract;
  end;

  { TdxChangeTextCasePlaceholderCommand }

  TdxChangeTextCasePlaceholderCommand = class(TdxChangeCaseCommandBase)
  protected
    function CreateModifier: TdxRunChangeCaseModifierBase; override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
    class function Id: TdxRichEditCommandId; override;

    procedure ForceExecute(const AState: IdxCommandUIState); override;
  end;

  { TdxMakeTextUpperCaseCommand }

  TdxMakeTextUpperCaseCommand = class(TdxChangeCaseCommandBase)
  protected
    function CreateModifier: TdxRunChangeCaseModifierBase; override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxMakeTextLowerCaseCommand }

  TdxMakeTextLowerCaseCommand = class(TdxChangeCaseCommandBase)
  protected
    function CreateModifier: TdxRunChangeCaseModifierBase; override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxToggleTextCaseCommand }

  TdxToggleTextCaseCommand = class(TdxChangeCaseCommandBase)
  protected
    function CreateModifier: TdxRunChangeCaseModifierBase; override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;


  { TdxChangeParagraphStyleCommandBase }

  TdxChangeParagraphStyleCommandBase = class abstract(TdxSelectionBasedPropertyChangeCommandBase)
  protected
    function CalculateStartPosition(AItem: TdxSelectionItem; AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition; override;
    function CalculateEndPosition(AItem: TdxSelectionItem; AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition; override;
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure ChangeParagraphProperty(AParagraph: TdxParagraph); virtual;
    function CalculateParagraphStyleIndex(AParagraph: TdxParagraph): Integer; virtual; abstract;
  end;

  { TdxChangeParagraphStyleCommand }

  TdxChangeParagraphStyleCommand = class(TdxChangeParagraphStyleCommandBase)
  strict private
    FParagraphStyle: TdxParagraphStyle;
  protected
    function CalculateParagraphStyleIndex(AParagraph: TdxParagraph): Integer; override;
  public
    constructor Create(const AControl: IdxRichEditControl; AParagraphStyle: TdxParagraphStyle); reintroduce;

    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxChangeSectionFormattingCommandBase }

  TdxChangeSectionFormattingCommandBase<T> = class abstract(TdxSelectionBasedPropertyChangeCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition; const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure ChangeSectionFormatting(ALogPositionFrom: TdxDocumentLogPosition; ALogPositionTo: TdxDocumentLogPosition; AModifier: TdxSectionPropertyModifier<T>); virtual;
    function CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<T>; virtual; abstract;
  end;

  { TdxToggleChangeSectionFormattingCommandBase }

  TdxToggleChangeSectionFormattingCommandBase<T> = class abstract(TdxChangeSectionFormattingCommandBase<T>)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function IsChecked: Boolean; virtual;
    function IsCheckedCore(ALogPositionFrom: TdxDocumentLogPosition; ALogPositionTo: TdxDocumentLogPosition;
      AModifier: TdxSectionPropertyModifier<T>): Boolean; virtual;
    function IsCheckedValue(AValue: T): Boolean; virtual; abstract;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); reintroduce;
  end;

  { TdxQuickStylesGalleryCommand }

  TdxQuickStylesGalleryCommand = class(TdxRichEditCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    class function GetDescription: string; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Math, Character, dxCore, dxTypeHelpers,

  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs;

{ TdxDefaultValueBasedCommandUIState<T> }

function TdxDefaultValueBasedCommandUIState<T>.GetValue: T;
begin
  Result := FValue;
end;

procedure TdxDefaultValueBasedCommandUIState<T>.SetValue(const Value: T);
begin
  FValue := Value;
end;

{ TdxDefaultObjectValueBasedCommandUIState<T> }

destructor TdxDefaultObjectValueBasedCommandUIState<T>.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

procedure TdxDefaultObjectValueBasedCommandUIState<T>.SetValue(const Value: T);
begin
  if FValue <> Value then
    FreeAndNil(FValue);
  inherited SetValue(Value);
end;

{ TdxChangeCharacterPropertiesCommandBase }

function TdxChangeCharacterPropertiesCommandBase.ChangeCharacterFormatting(
  ALogPositionFrom, ALogPositionTo: TdxDocumentLogPosition;
  AModifier: TdxRunPropertyModifierBase): TdxDocumentModelChangeActions;
var
  ALength: Integer;
  AParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
  AResetInputPosition: Boolean;
begin
  ALength := ALogPositionTo - ALogPositionFrom;
  if ALength <= 0 then
  begin
    AParagraphIndex := ActivePieceTable.FindParagraphIndex(ALogPositionFrom);
    AParagraph := ActivePieceTable.Paragraphs[AParagraphIndex];
    if AParagraph.LogPosition + AParagraph.Length - 1 = ALogPositionFrom then
    begin
      AResetInputPosition := TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting in
        DocumentModel.DeferredChanges.ChangeActions;
      ActivePieceTable.ApplyCharacterFormatting(AParagraph.LogPosition + AParagraph.Length - 1, 1, AModifier);
      if not AResetInputPosition then
        DocumentModel.DeferredChanges.ChangeActions := DocumentModel.DeferredChanges.ChangeActions -
          [TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting];
    end;
    ChangeInputPositionCharacterFormatting(AModifier);
    Result := [];
  end
  else
  begin
    ActivePieceTable.ApplyCharacterFormatting(ALogPositionFrom, ALength, AModifier);
    Result := [TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting];
  end;
end;

procedure TdxChangeCharacterPropertiesCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
	ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.CharacterFormatting);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxChangeCharacterFormattingCommandBase<T> }

procedure TdxChangeCharacterFormattingCommandBase<T>.ChangeInputPositionCharacterFormatting(
  AModifier: TdxRunPropertyModifierBase);
var
  ATypedModifier: TdxRunPropertyModifier<T>;
  APos: TdxInputPosition;
begin
  ATypedModifier := TdxRunPropertyModifier<T>(AModifier);
  APos := ActiveView.CaretPosition.GetInputPosition;
  ATypedModifier.ModifyInputPosition(APos);
end;

function TdxChangeCharacterFormattingCommandBase<T>.ChangeProperty(const AStart,
  AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AModifier: TdxRunPropertyModifier<T>;
begin
  AModifier := CreateModifier(AState);
  try
    Result := ChangeCharacterFormatting(AStart.LogPosition, AEnd.LogPosition, AModifier);
  finally
    AModifier.Free;
  end;
end;

function TdxChangeCharacterFormattingCommandBase<T>.GetCurrentPropertyValue(
  out AValue: T): Boolean;
var
  AModifier: TdxRunPropertyModifier<T>;
  AItems: TdxSelectionItemList;
  AItem: TdxSelectionItem;
  I, ALength: Integer;
  AStart, AEnd: TdxDocumentModelPosition;
  APos: TdxInputPosition;
  ARunValue: T;
  AObtainValueResult: Boolean;
begin
  AValue := Default(T);
  AModifier := CreateModifier(CreateDefaultCommandUIState);
  try
    Result := True;
    AItems := DocumentModel.Selection.Items;
    Assert(AItems.Count > 0);
    for I := 0 to AItems.Count -1 do
    begin
      AItem := AItems[I];
      AStart := CalculateStartPosition(AItem, False);
      AEnd := CalculateEndPosition(AItem, False);
      ALength := AEnd.LogPosition - AStart.LogPosition;
      if (AItems.Count = 1) and (ALength <= 0) then
      begin
        APos := ActiveView.CaretPosition.GetInputPosition;
        AValue := AModifier.GetInputPositionPropertyValue(APos);
        Exit;
      end;
      AObtainValueResult := not ObtainRunsPropertyValue(AStart, ALength, AModifier, ARunValue);
      try
        if I = 0 then
          AValue := ARunValue;
        if AObtainValueResult then
          Exit(False);
        if (I > 0) and not AModifier.IsValueEquals(AValue, ARunValue) then
          Exit(False);
      finally
        if I > 0 then
          AModifier.CleanupValue(ARunValue);
      end;
    end;
  finally
    AModifier.Free;
  end;
end;

function TdxChangeCharacterFormattingCommandBase<T>.ObtainRunsPropertyValue(
  const AStart: TdxDocumentModelPosition; ALength: Integer;
  AModifier: TdxRunPropertyModifier<T>; out AValue: T): Boolean;
begin
  Result := ActivePieceTableObtainRunsPropertyValue<T>(AStart.LogPosition,
    ALength, AModifier, AValue);
end;

{ TdxChangeFontNameCommand }

function TdxChangeFontNameCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AState: IdxValueBasedCommandUIState<string>;
begin
  AState := TdxDefaultValueBasedCommandUIState<string>.Create;
  AState.Value := '';
  Result := AState;
end;

class function TdxChangeFontNameCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFontName;
end;

class function TdxChangeFontNameCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ChangeFontStyle;
end;

function TdxChangeFontNameCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<string>;
var
  AValueBasedState: IdxValueBasedCommandUIState<string>;
begin
  if not Supports(AState, IdxValueBasedCommandUIState<string>, AValueBasedState) then
    TdxRichEditExceptions.ThrowInternalException;

  Result := TdxRunFontNamePropertyModifier.Create(AValueBasedState.Value);
end;

class function TdxChangeFontNameCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFontNameDescription);
end;

class function TdxChangeFontNameCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFontNameMenuCaption);
end;

procedure TdxChangeFontNameCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValue: string;
  AValueBasedState: IdxValueBasedCommandUIState<string>;
begin
  inherited UpdateUIStateCore(AState);

  if Supports(AState, IdxValueBasedCommandUIState<string>, AValueBasedState) then
  begin
    if GetCurrentPropertyValue(AValue) then
      AValueBasedState.Value := AValue
    else
      AValueBasedState.Value := '';
  end;
end;

{ TdxChangeFontSizeCommand }

function TdxChangeFontSizeCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<Single>.Create;
end;

class function TdxChangeFontSizeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFontSize;
end;

class function TdxChangeFontSizeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.FontSize;
end;

function TdxChangeFontSizeCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Single>;
var
  AValueBasedState: IdxValueBasedCommandUIState<Single>;
begin
  if not Supports(AState, IdxValueBasedCommandUIState<Single>, AValueBasedState) then
    TdxRichEditExceptions.ThrowInternalException;
  if AValueBasedState.Value <> InvalidValue then
    Result := TdxRunFontSizePropertyModifier.Create(AValueBasedState.Value)
  else
    Result := TdxRunFontSizePropertyModifier.Create(0);
end;

class function TdxChangeFontSizeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFontSizeDescription);
end;

class function TdxChangeFontSizeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFontSizeMenuCaption);
end;

procedure TdxChangeFontSizeCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValue: Single;
  AValueBasedState: IdxValueBasedCommandUIState<Single>;
begin
  inherited UpdateUIStateCore(AState);

  if Supports(AState, IdxValueBasedCommandUIState<Single>, AValueBasedState) then
  begin
    if GetCurrentPropertyValue(AValue) then
      AValueBasedState.Value := AValue
    else
      AValueBasedState.Value := InvalidValue;
  end;
end;

{ TdxChangeFontColorCommand }

function TdxChangeFontColorCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<TdxAlphaColor>.Create;
end;

function TdxChangeFontColorCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxAlphaColor>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxAlphaColor>;
begin
  if not Supports(AState, IdxValueBasedCommandUIState<TdxAlphaColor>, AValueBasedState) then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxRunFontColorPropertyModifier.Create(AValueBasedState.Value);
end;

class function TdxChangeFontColorCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFontColorDescription);
end;

class function TdxChangeFontColorCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFontColorMenuCaption);
end;

class function TdxChangeFontColorCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFontForeColor;
end;

class function TdxChangeFontColorCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ChangeFontColor;
end;

procedure TdxChangeFontColorCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValue: TdxAlphaColor;
  AValueBasedState: IdxValueBasedCommandUIState<TdxAlphaColor>;
begin
  inherited UpdateUIStateCore(AState);

  if Supports(AState, IdxValueBasedCommandUIState<TdxAlphaColor>, AValueBasedState) then
  begin
    GetCurrentPropertyValue(AValue);
    AValueBasedState.Value := AValue;
  end;
end;

{ TdxChangeFontBackColorByMouseCommand }

constructor TdxChangeFontBackColorByMouseCommand.Create(
  const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  FInternalCommand := TdxChangeFontBackColorCommand.Create(ARichEditControl);
end;

function TdxChangeFontBackColorByMouseCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<TdxAlphaColor>.Create;
end;

destructor TdxChangeFontBackColorByMouseCommand.Destroy;
begin
  FreeAndNil(FInternalCommand);
  inherited Destroy;
end;

procedure TdxChangeFontBackColorByMouseCommand.ExecuteCore;
begin
Assert(False);

end;

procedure TdxChangeFontBackColorByMouseCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  NotifyBeginCommandExecution(AState);
  try
    ExecuteCore;
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

class function TdxChangeFontBackColorByMouseCommand.GetDescription: string;
begin
  Result := '';
end;

class function TdxChangeFontBackColorByMouseCommand.GetMenuCaption: string;
begin
  Result := '';
end;

class function TdxChangeFontBackColorByMouseCommand.IsChangeByMouse: Boolean;
begin
  Result := False;
end;

procedure TdxChangeFontBackColorByMouseCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  InternalCommand.UpdateUIState(AState);
end;

{ TdxChangeParagraphFormattingCommandBase<T> }

procedure TdxChangeParagraphFormattingCommandBase<T>.ChangeParagraphFormatting(
  ALogPositionFrom, ALogPositionTo: TdxDocumentLogPosition;
  AModifier: TdxParagraphPropertyModifier<T>);
var
  ALength: Integer;
begin
  ALength := Max(1, ALogPositionTo - ALogPositionFrom);
  ActivePieceTable.ApplyParagraphFormatting(ALogPositionFrom, ALength, AModifier);
end;

function TdxChangeParagraphFormattingCommandBase<T>.ChangeProperty(const AStart,
  AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AModifier: TdxParagraphPropertyModifier<T>;
begin
  AModifier := CreateModifier(AState);
  try
    ChangeParagraphFormatting(AStart.LogPosition, AEnd.LogPosition, AModifier);
    Result := [];
  finally
    AModifier.Free;
  end;
end;

procedure TdxChangeParagraphFormattingCommandBase<T>.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.ParagraphFormatting);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxChangeParagraphFirstLineIndentCommand }

constructor TdxChangeParagraphFirstLineIndentCommand.Create(const AControl: IdxRichEditControl; ANewIndent: Integer);
begin
  inherited Create(AControl);
  FNewIndent := ANewIndent;
end;

class function TdxChangeParagraphFirstLineIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeParagraphFirstLineIndentMenuCaption);
end;


class function TdxChangeParagraphFirstLineIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeParagraphFirstLineIndentDescription);
end;

function TdxChangeParagraphFirstLineIndentCommand.CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
begin
  Result := TdxParagraphFirstLineIndentModifier.Create(FNewIndent);
end;

{ TdxChangeParagraphLeftIndentCommand }

constructor TdxChangeParagraphLeftIndentCommand.Create(const AControl: IdxRichEditControl; ANewIndent: Integer);
begin
  inherited Create(AControl);
  FNewIndent := ANewIndent;
end;

class function TdxChangeParagraphLeftIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeParagraphLeftIndentMenuCaption);
end;

class function TdxChangeParagraphLeftIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeParagraphLeftIndentDescription);
end;

function TdxChangeParagraphLeftIndentCommand.CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
begin
  Result := TdxParagraphLeftIndentModifier.Create(FNewIndent);
end;

{ TdxChangeParagraphRightIndentCommand }

constructor TdxChangeParagraphRightIndentCommand.Create(const AControl: IdxRichEditControl; ANewIndent: Integer);
begin
  inherited Create(AControl);
  FNewIndent := ANewIndent;
end;

class function TdxChangeParagraphRightIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeParagraphRightIndentMenuCaption);
end;

class function TdxChangeParagraphRightIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeParagraphRightIndentDescription);
end;

function TdxChangeParagraphRightIndentCommand.CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
begin
  Result := TdxParagraphRightIndentModifier.Create(FNewIndent);
end;

{ TdxToggleChangeParagraphFormattingCommandBase<T> }

constructor TdxToggleChangeParagraphFormattingCommandBase<T>.Create(
  const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
end;

function TdxToggleChangeParagraphFormattingCommandBase<T>.IsChecked: Boolean;
var
  AItems: TdxSelectionItemList;
  AItem: TdxSelectionItem;
  I: Integer;
  AStart, AEnd: TdxDocumentModelPosition;
  AModifier: TdxParagraphPropertyModifier<T>;
begin
  AItems := GetSelectionItems;
  Result := True;
  for I := 0 to AItems.Count - 1 do
  begin
    AItem := AItems[I];
    AStart := CalculateStartPosition(AItem, False);
    AEnd := CalculateEndPosition(AItem, False);
    AModifier := CreateModifier(CreateDefaultCommandUIState);
    try
      Result := IsCheckedCore(AStart.LogPosition, AEnd.LogPosition, AModifier);
    finally
      AModifier.Free;
    end;
    if not Result then
      Break;
  end;
end;

function TdxToggleChangeParagraphFormattingCommandBase<T>.IsCheckedCore(
  ALogPositionFrom, ALogPositionTo: TdxDocumentLogPosition;
  AModifier: TdxParagraphPropertyModifier<T>): Boolean;
var
  ALength: Integer;
  AValue: T;
begin
  ALength := Max(1, ALogPositionTo - ALogPositionFrom);
  Result := ActivePieceTableObtainParagraphsPropertyValue<T>(ALogPositionFrom, ALength,
    AModifier, AValue) and IsCheckedValue(AValue);
end;

procedure TdxToggleChangeParagraphFormattingCommandBase<T>.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Checked := IsChecked;
end;

{ TdxToggleParagraphAlignmentLeftCommand }

function TdxToggleParagraphAlignmentLeftCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>;
var
  Alignment: TdxParagraphAlignment;
begin
  if AState.Checked then
    Alignment := TdxParagraphAlignment.Justify
  else
    Alignment := TdxParagraphAlignment.Left;
  Result := TdxParagraphAlignmentModifier.Create(Alignment);
end;

class function TdxToggleParagraphAlignmentLeftCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleParagraphAlignmentLeft;
end;

class function TdxToggleParagraphAlignmentLeftCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandParagraphAlignmentLeftDescription);
end;

class function TdxToggleParagraphAlignmentLeftCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandParagraphAlignmentLeftMenuCaption);
end;

class function TdxToggleParagraphAlignmentLeftCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleParagraphAlignmentLeft;
end;

function TdxToggleParagraphAlignmentLeftCommand.IsCheckedValue(
  AValue: TdxParagraphAlignment): Boolean;
begin
  Result := AValue = TdxParagraphAlignment.Left;
end;

{ TdxToggleParagraphAlignmentCenterCommand }

function TdxToggleParagraphAlignmentCenterCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>;
var
  Alignment: TdxParagraphAlignment;
begin
  if AState.Checked then
    Alignment := TdxParagraphAlignment.Left
  else
    Alignment := TdxParagraphAlignment.Center;
  Result := TdxParagraphAlignmentModifier.Create(Alignment);
end;

class function TdxToggleParagraphAlignmentCenterCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandParagraphAlignmentCenterDescription);
end;

class function TdxToggleParagraphAlignmentCenterCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandParagraphAlignmentCenterMenuCaption);
end;

class function TdxToggleParagraphAlignmentCenterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleParagraphAlignmentCenter;
end;

class function TdxToggleParagraphAlignmentCenterCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleParagraphAlignmentCenter;
end;

function TdxToggleParagraphAlignmentCenterCommand.IsCheckedValue(
  AValue: TdxParagraphAlignment): Boolean;
begin
  Result := AValue = TdxParagraphAlignment.Center;
end;

{ TdxToggleParagraphAlignmentRightCommand }

function TdxToggleParagraphAlignmentRightCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>;
var
  Alignment: TdxParagraphAlignment;
begin
  if AState.Checked then
    Alignment := TdxParagraphAlignment.Left
  else
    Alignment := TdxParagraphAlignment.Right;
  Result := TdxParagraphAlignmentModifier.Create(Alignment);
end;

class function TdxToggleParagraphAlignmentRightCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandParagraphAlignmentRightDescription);
end;

class function TdxToggleParagraphAlignmentRightCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandParagraphAlignmentRightMenuCaption);
end;

class function TdxToggleParagraphAlignmentRightCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleParagraphAlignmentRight;
end;

class function TdxToggleParagraphAlignmentRightCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleParagraphAlignmentRight;
end;

function TdxToggleParagraphAlignmentRightCommand.IsCheckedValue(
  AValue: TdxParagraphAlignment): Boolean;
begin
  Result := AValue = TdxParagraphAlignment.Right;
end;

{ TdxToggleParagraphAlignmentJustifyCommand }

function TdxToggleParagraphAlignmentJustifyCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>;
var
  Alignment: TdxParagraphAlignment;
begin
  if AState.Checked then
    Alignment := TdxParagraphAlignment.Left
  else
    Alignment := TdxParagraphAlignment.Justify;
  Result := TdxParagraphAlignmentModifier.Create(Alignment);
end;

class function TdxToggleParagraphAlignmentJustifyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandParagraphAlignmentJustifyDescription);
end;

class function TdxToggleParagraphAlignmentJustifyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandParagraphAlignmentJustifyMenuCaption);
end;

class function TdxToggleParagraphAlignmentJustifyCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleParagraphAlignmentJustify;
end;

class function TdxToggleParagraphAlignmentJustifyCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleParagraphAlignmentJustify;
end;

function TdxToggleParagraphAlignmentJustifyCommand.IsCheckedValue(
  AValue: TdxParagraphAlignment): Boolean;
begin
  Result := AValue = TdxParagraphAlignment.Justify;
end;

{ TdxToggleChangeCharacterFormattingCommandBase<T> }

constructor TdxToggleChangeCharacterFormattingCommandBase<T>.Create(
  const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
end;

function TdxToggleChangeCharacterFormattingCommandBase<T>.IsChecked: Boolean;
var
  AValue: T;
begin
  Result := GetCurrentPropertyValue(AValue) and IsCheckedValue(AValue);
end;

procedure TdxToggleChangeCharacterFormattingCommandBase<T>.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Checked := IsChecked;
end;

{ TdxToggleCharacterFormattingBoolPropertyCommand }

function TdxToggleCharacterFormattingBoolPropertyCommand.IsCheckedValue(
  AValue: Boolean): Boolean;
begin
  Result := AValue;
end;

{ TdxToggleFontBoldCommand }

function TdxToggleFontBoldCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Boolean>;
begin
  Result := TdxRunFontBoldModifier.Create(not AState.Checked);
end;

class function TdxToggleFontBoldCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFontBold;
end;

class function TdxToggleFontBoldCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFontBold;
end;

class function TdxToggleFontBoldCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontBoldDescription);
end;

class function TdxToggleFontBoldCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontBoldMenuCaption);
end;

{ TdxToggleFontItalicCommand }

function TdxToggleFontItalicCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Boolean>;
begin
  Result := TdxRunFontItalicModifier.Create(not AState.Checked);
end;

class function TdxToggleFontItalicCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFontItalic;
end;

class function TdxToggleFontItalicCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontItalicDescription);
end;

class function TdxToggleFontItalicCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontItalicMenuCaption);
end;

class function TdxToggleFontItalicCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFontItalic;
end;

{ TdxToggleFontUnderlineCommandBase }

function TdxToggleFontUnderlineCommandBase.CreateModifier(
  const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxUnderlineType>;
var
  AUnderlineType: TdxUnderlineType;
begin
  if AState.Checked then
    AUnderlineType := TdxUnderlineType.None
  else
    AUnderlineType := GetUnderlineType;
  Result := TdxRunFontUnderlineTypeModifier.Create(AUnderlineType);
end;

function TdxToggleFontUnderlineCommandBase.IsCheckedValue(
  AValue: TdxUnderlineType): Boolean;
begin
  Result := GetUnderlineType = AValue;
end;

{ TdxToggleFontUnderlineCommand }

class function TdxToggleFontUnderlineCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontUnderlineDescription);
end;

class function TdxToggleFontUnderlineCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontUnderlineMenuCaption);
end;

function TdxToggleFontUnderlineCommand.GetUnderlineType: TdxUnderlineType;
begin
  Result := TdxUnderlineType.Single;
end;

class function TdxToggleFontUnderlineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFontUnderline;
end;

class function TdxToggleFontUnderlineCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFontUnderline;
end;

{ TdxToggleFontDoubleUnderlineCommand }

class function TdxToggleFontDoubleUnderlineCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontDoubleUnderlineDescription);
end;

class function TdxToggleFontDoubleUnderlineCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontDoubleUnderlineMenuCaption);
end;

function TdxToggleFontDoubleUnderlineCommand.GetUnderlineType: TdxUnderlineType;
begin
  Result := TdxUnderlineType.Double;
end;

class function TdxToggleFontDoubleUnderlineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFontDoubleUnderline;
end;

class function TdxToggleFontDoubleUnderlineCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFontDoubleUnderline;
end;

{ TdxToggleFontStrikeoutCommandBase }

function TdxToggleFontStrikeoutCommandBase.CreateModifier(
  const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxStrikeoutType>;
var
  AStrikeoutType: TdxStrikeoutType;
begin
  if AState.Checked then
    AStrikeoutType := TdxStrikeoutType.None
  else
    AStrikeoutType := GetStrikeoutType;

  Result := TdxRunFontStrikeoutTypeModifier.Create(AStrikeoutType);
end;

function TdxToggleFontStrikeoutCommandBase.IsCheckedValue(AValue: TdxStrikeoutType): Boolean;
begin
  Result := AValue = GetStrikeoutType;
end;

{ TdxToggleFontStrikeoutCommand }

class function TdxToggleFontStrikeoutCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFontStrikeout;
end;

class function TdxToggleFontStrikeoutCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontStrikeoutDescription);
end;

class function TdxToggleFontStrikeoutCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontStrikeoutMenuCaption);
end;

function TdxToggleFontStrikeoutCommand.GetStrikeoutType: TdxStrikeoutType;
begin
  Result := TdxStrikeoutType.Single;
end;

class function TdxToggleFontStrikeoutCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFontStrikeout;
end;

{ TdxToggleFontDoubleStrikeoutCommand }

class function TdxToggleFontDoubleStrikeoutCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFontDoubleStrikeout;
end;

class function TdxToggleFontDoubleStrikeoutCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontDoubleStrikeoutDescription);
end;

class function TdxToggleFontDoubleStrikeoutCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleFontDoubleStrikeoutMenuCaption);
end;

function TdxToggleFontDoubleStrikeoutCommand.GetStrikeoutType: TdxStrikeoutType;
begin
  Result := TdxStrikeoutType.Double;
end;

class function TdxToggleFontDoubleStrikeoutCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFontDoubleStrikeout;
end;

{ TdxIncrementFontSizeCommand }

function TdxIncrementFontSizeCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Integer>;
begin
  Result := TdxRunIncrementFontSizeModifier.Create;
end;

class function TdxIncrementFontSizeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementFontSizeDescription);
end;

class function TdxIncrementFontSizeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementFontSizeMenuCaption);
end;

class function TdxIncrementFontSizeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.IncrementFontSize;
end;

{ TdxDecrementFontSizeCommand }

function TdxDecrementFontSizeCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Integer>;
begin
  Result := TdxRunDecrementFontSizeModifier.Create;
end;

class function TdxDecrementFontSizeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementFontSizeDescription);
end;

class function TdxDecrementFontSizeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementFontSizeMenuCaption);
end;

class function TdxDecrementFontSizeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DecrementFontSize;
end;

{ TdxToggleFontSuperscriptCommand }

function TdxToggleFontSuperscriptCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxCharacterFormattingScript>;
var
  AScript: TdxCharacterFormattingScript;
begin
  if AState.Checked then
    AScript := TdxCharacterFormattingScript.Normal
  else
    AScript := TdxCharacterFormattingScript.Superscript;
  Result := TdxRunScriptModifier.Create(AScript);
end;

class function TdxToggleFontSuperscriptCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFontSuperscript;
end;

class function TdxToggleFontSuperscriptCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFontSuperscriptDescription);
end;

class function TdxToggleFontSuperscriptCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFontSuperscriptMenuCaption);
end;

class function TdxToggleFontSuperscriptCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFontSuperscript;
end;

function TdxToggleFontSuperscriptCommand.IsCheckedValue(
  AValue: TdxCharacterFormattingScript): Boolean;
begin
  Result := AValue = TdxCharacterFormattingScript.Superscript;
end;

{ TdxToggleFontSubscriptCommand }

function TdxToggleFontSubscriptCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxCharacterFormattingScript>;
var
  AScript: TdxCharacterFormattingScript;
begin
  if AState.Checked then
    AScript := TdxCharacterFormattingScript.Normal
  else
    AScript := TdxCharacterFormattingScript.Subscript;
  Result := TdxRunScriptModifier.Create(AScript);
end;

class function TdxToggleFontSubscriptCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleFontSubscript;
end;

class function TdxToggleFontSubscriptCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFontSubscriptDescription);
end;

class function TdxToggleFontSubscriptCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFontSubscriptMenuCaption);
end;

class function TdxToggleFontSubscriptCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleFontSubscript;
end;

function TdxToggleFontSubscriptCommand.IsCheckedValue(
  AValue: TdxCharacterFormattingScript): Boolean;
begin
  Result := AValue = TdxCharacterFormattingScript.Subscript;
end;

{ TdxIncreaseFontSizeCommand }

function TdxIncreaseFontSizeCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Integer>;
begin
  Result := TdxRunIncreaseFontSizeModifier.Create(InnerControl.PredefinedFontSizeCollection)
end;

class function TdxIncreaseFontSizeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncreaseFontSizeDescription);
end;

class function TdxIncreaseFontSizeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncreaseFontSizeMenuCaption);
end;

class function TdxIncreaseFontSizeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.IncreaseFontSize;
end;

class function TdxIncreaseFontSizeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.IncreaseFontSize;
end;

{ TdxDecreaseFontSizeCommand }

function TdxDecreaseFontSizeCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<Integer>;
begin
  Result := TdxRunDecreaseFontSizeModifier.Create(InnerControl.PredefinedFontSizeCollection);
end;

class function TdxDecreaseFontSizeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecreaseFontSizeDescription);
end;

class function TdxDecreaseFontSizeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecreaseFontSizeMenuCaption);
end;

class function TdxDecreaseFontSizeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DecreaseFontSize;
end;

class function TdxDecreaseFontSizeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DecreaseFontSize;
end;

{ TdxChangeFontBackColorCommand }

function TdxChangeFontBackColorCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<TdxAlphaColor>.Create;
end;

class function TdxChangeFontBackColorCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFontBackColor;
end;

class function TdxChangeFontBackColorCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.TextHighlight;
end;

procedure TdxChangeFontBackColorCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxAlphaColor>;
  AValue: TdxAlphaColor;
begin
  inherited UpdateUIStateCore(AState);
  if Supports(AState, IdxValueBasedCommandUIState<TdxAlphaColor>, AValueBasedState) then
  begin
    GetCurrentPropertyValue(AValue);
    AValueBasedState.Value := AValue;
  end;
end;

function TdxChangeFontBackColorCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxAlphaColor>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxAlphaColor>;
begin
  if not Supports(AState, IdxValueBasedCommandUIState<TdxAlphaColor>, AValueBasedState) then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxRunBackColorModifier.Create(AValueBasedState.Value);
end;

class function TdxChangeFontBackColorCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandHighlightTextDescription);
end;

class function TdxChangeFontBackColorCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandHighlightTextMenuCaption);
end;

{ TdxSetSingleParagraphSpacingCommand }

class function TdxSetSingleParagraphSpacingCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSingleParagraphSpacingDescription);
end;

class function TdxSetSingleParagraphSpacingCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSingleParagraphSpacingMenuCaption);
end;

class function TdxSetSingleParagraphSpacingCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSingleParagraphSpacing;
end;

function TdxSetSingleParagraphSpacingCommand.CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphLineSpacing>;
begin
  Result := TdxParagraphSpacingModifier.Create(TdxParagraphLineSpacing.Single);
end;

function TdxSetSingleParagraphSpacingCommand.IsCheckedValue(AValue: TdxParagraphLineSpacing): Boolean;
begin
  Result := AValue = TdxParagraphLineSpacing.Single;
end;

{ TdxSetSesquialteralParagraphSpacingCommand }

class function TdxSetSesquialteralParagraphSpacingCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSesquialteralParagraphSpacingDescription);
end;

class function TdxSetSesquialteralParagraphSpacingCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSesquialteralParagraphSpacingMenuCaption);
end;

class function TdxSetSesquialteralParagraphSpacingCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSesquialteralParagraphSpacing;
end;

function TdxSetSesquialteralParagraphSpacingCommand.CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphLineSpacing>;
begin
  Result := TdxParagraphSpacingModifier.Create(TdxParagraphLineSpacing.Sesquialteral);
end;

function TdxSetSesquialteralParagraphSpacingCommand.IsCheckedValue(AValue: TdxParagraphLineSpacing): Boolean;
begin
  Result := AValue = TdxParagraphLineSpacing.Sesquialteral;
end;

{ TdxSetDoubleParagraphSpacingCommand }

class function TdxSetDoubleParagraphSpacingCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetDoubleParagraphSpacingDescription);
end;

class function TdxSetDoubleParagraphSpacingCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetDoubleParagraphSpacingMenuCaption);
end;

class function TdxSetDoubleParagraphSpacingCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetDoubleParagraphSpacing;
end;

function TdxSetDoubleParagraphSpacingCommand.CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphLineSpacing>;
begin
  Result := TdxParagraphSpacingModifier.Create(TdxParagraphLineSpacing.Double);
end;

function TdxSetDoubleParagraphSpacingCommand.IsCheckedValue(AValue: TdxParagraphLineSpacing): Boolean;
begin
  Result := AValue = TdxParagraphLineSpacing.Double;
end;

{ TdxChangeParagraphIndentCommandBase<T> }

constructor TdxChangeParagraphIndentCommandBase<T>.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FTabsList := TdxIntegerList.Create;
end;

destructor TdxChangeParagraphIndentCommandBase<T>.Destroy;
begin
  FTabsList.Free;
  inherited Destroy;
end;

procedure TdxChangeParagraphIndentCommandBase<T>.AddParagraphTabs(AParagraph: TdxSimpleParagraph);
begin
  TabsList.Add(AParagraph.LeftIndent);
  if AParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
    TabsList.Add(AParagraph.LeftIndent - AParagraph.FirstLineIndent)
  else
    if AParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Indented then
      TabsList.Add(AParagraph.LeftIndent + AParagraph.FirstLineIndent);
end;

procedure TdxChangeParagraphIndentCommandBase<T>.FillTabsList;
var
  AParagraphs: TdxParagraphCollection;
  ATabFormattingInfo: TdxTabFormattingInfo;
  I: Integer;
begin
  AParagraphs := ActivePieceTable.Paragraphs;
  ATabFormattingInfo := AParagraphs[StartParagraphIndex].GetTabs;
  try
    for I := 0 to ATabFormattingInfo.Count - 1 do
      TabsList.Add(ATabFormattingInfo[I].Position);
  finally
    ATabFormattingInfo.Free;
  end;

  if (StartParagraphIndex = 0) and (StartParagraphIndex = EndParagraphIndex) then
    AddParagraphTabs(AParagraphs[StartParagraphIndex])
  else
  begin
    if StartParagraphIndex > 0 then
        AddParagraphTabs(AParagraphs[StartParagraphIndex - 1]);
    if EndParagraphIndex < AParagraphs.Count - 1 then
        AddParagraphTabs(AParagraphs[EndParagraphIndex + 1]);
  end;
  TabsList.Sort;
end;

function TdxChangeParagraphIndentCommandBase<T>.GetDefaultTabWidth: Integer;
begin
  Result := DocumentModel.DocumentProperties.DefaultTabWidth;
end;

class function TdxChangeParagraphIndentCommandBase<T>.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementParagraphLeftIndentDescription);
end;

function TdxChangeParagraphIndentCommandBase<T>.GetEndParagraphIndex: TdxParagraphIndex;
begin
  Result := DocumentModel.Selection.Interval.NormalizedEnd.ParagraphIndex;
end;

class function TdxChangeParagraphIndentCommandBase<T>.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementParagraphLeftIndentMenuCaption);
end;

function TdxChangeParagraphIndentCommandBase<T>.GetNearLeftDefaultTab(ALeftIndent: Integer): Integer;
var
  ANearestLeftDefaultTab: Integer;
begin
  ANearestLeftDefaultTab := ALeftIndent div DefaultTabWidth;
  if ANearestLeftDefaultTab > 0 then
  begin
    if ALeftIndent mod DefaultTabWidth <> 0 then
      Exit(ANearestLeftDefaultTab * DefaultTabWidth)
    else
      Exit((ANearestLeftDefaultTab - 1) * DefaultTabWidth);
  end;
  Result := ANearestLeftDefaultTab;
end;

function TdxChangeParagraphIndentCommandBase<T>.GetNearLeftTab(ALeftIndent: Integer): Integer;
var
  I: Integer;
begin
  for I := TabsList.Count - 1 downto 0 do
    if ALeftIndent > TabsList[I] then
        Exit(TabsList[I]);
  Result := ALeftIndent;
end;

function TdxChangeParagraphIndentCommandBase<T>.GetNearRightDefaultTab(ALeftIndent: Integer): Integer;
begin
  Result := ((ALeftIndent div DefaultTabWidth) + 1) * DefaultTabWidth;
end;

function TdxChangeParagraphIndentCommandBase<T>.GetNearRightTab(ALeftIndent: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to TabsList.Count - 1 do
    if ALeftIndent < TabsList[I] then
      Exit(TabsList[I]);
  Result := ALeftIndent;
end;

function TdxChangeParagraphIndentCommandBase<T>.GetStartParagraphIndex: TdxParagraphIndex;
begin
  Result := DocumentModel.Selection.Interval.NormalizedStart.ParagraphIndex;
end;

procedure TdxChangeParagraphIndentCommandBase<T>.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxIncrementParagraphLeftIndentCommand }

function TdxIncrementParagraphLeftIndentCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
var
  AParagraph: TdxParagraph;
  AMaxValue, ANearestRightDefaultTab, ANearestRightTab: Integer;
begin
  AMaxValue := MaxInt;
  if (CaretPosition <> nil) and CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.Column) then
    AMaxValue := DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(CaretPosition.LayoutPosition.Column.Bounds.Width);

  FillTabsList;
  AParagraph := ActivePieceTable.Paragraphs[StartParagraphIndex];
  ANearestRightDefaultTab := GetNearRightDefaultTab(AParagraph.LeftIndent);
  ANearestRightTab := GetNearRightTab(AParagraph.LeftIndent);
  if (ANearestRightDefaultTab < ANearestRightTab) or (ANearestRightTab = AParagraph.LeftIndent) then
    Result := TdxAssignParagraphLeftIndentModifier.Create(ANearestRightDefaultTab - AParagraph.LeftIndent, AMaxValue)
  else
    Result := TdxAssignParagraphLeftIndentModifier.Create(ANearestRightTab - AParagraph.LeftIndent, AMaxValue);
end;

function TdxIncrementParagraphLeftIndentCommand.GetCaretPosition: TdxCaretPosition;
begin
  Result := ActiveView.CaretPosition;
end;

procedure TdxIncrementParagraphLeftIndentCommand.ModifyDocumentModel(const AState: IdxCommandUIState);
begin
  CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column);
  inherited ModifyDocumentModel(AState);
end;

{ TdxIncrementParagraphFirstLineIndentCommand }

function TdxIncrementParagraphFirstLineIndentCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
var
  AParagraph: TdxParagraph;
  ADocumentModel: TdxDocumentModel;
  AMaxValue, ANearestRightTab, ADefaultTabWidth: Integer;
begin
  AMaxValue := MaxInt div 4;
  if (CaretPosition <> nil) and CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.Column) then
    AMaxValue := DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(CaretPosition.LayoutPosition.Column.Bounds.Width);

  ADocumentModel := DocumentModel;
  AParagraph := ActivePieceTable.Paragraphs[StartParagraphIndex];
  FillTabsList;
  ANearestRightTab := GetNearRightTab(AParagraph.FirstLineIndent);
  ADefaultTabWidth := ADocumentModel.DocumentProperties.DefaultTabWidth;
  if (ADefaultTabWidth < ANearestRightTab) or (ANearestRightTab = AParagraph.FirstLineIndent) then
    Result := TdxParagraphFirstLineIndentModifier.Create(ADefaultTabWidth, AMaxValue)
  else
    Result := TdxParagraphFirstLineIndentModifier.Create(ANearestRightTab, AMaxValue);
end;

function TdxIncrementParagraphFirstLineIndentCommand.GetCaretPosition: TdxCaretPosition;
begin
  Result := ActiveView.CaretPosition;
end;

procedure TdxIncrementParagraphFirstLineIndentCommand.ModifyDocumentModel(const AState: IdxCommandUIState);
begin
  CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column);
  inherited ModifyDocumentModel(AState);
end;

{ TdxIncrementParagraphIndentCommand }

function TdxIncrementParagraphIndentCommand.CreateIncrementParagraphFirstLineIndentCommand: TdxIncrementParagraphFirstLineIndentCommand;
begin
  Result := TdxIncrementParagraphFirstLineIndentCommand.Create(RichEditControl);
end;

function TdxIncrementParagraphIndentCommand.CreateIncrementParagraphLeftIndentCommand: TdxIncrementParagraphLeftIndentCommand;
begin
  Result := TdxIncrementParagraphLeftIndentCommand.Create(RichEditControl);
end;

procedure TdxIncrementParagraphIndentCommand.ExecuteCore;
var
  AIncrementLeftIndent: TdxIncrementParagraphLeftIndentCommand;
  AIncrementFirstLineIndent: TdxIncrementParagraphFirstLineIndentCommand;
begin
  if SelectionBeginFirstRowStartPos and IsFirstLineIndentLessDefaultTabSize then
  begin
    AIncrementFirstLineIndent := CreateIncrementParagraphFirstLineIndentCommand;
    try
      AIncrementFirstLineIndent.ForceExecute(CreateDefaultCommandUIState);
    finally
      AIncrementFirstLineIndent.Free;
    end;
  end
  else
  begin
    AIncrementLeftIndent := CreateIncrementParagraphLeftIndentCommand;
    try
      AIncrementLeftIndent.ForceExecute(CreateDefaultCommandUIState);
    finally
      AIncrementLeftIndent.Free;
    end;
  end;
end;

class function TdxIncrementParagraphIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementIndentDescription);
end;

class function TdxIncrementParagraphIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementIndentMenuCaption);
end;

function TdxIncrementParagraphIndentCommand.IsFirstLineIndentLessDefaultTabSize: Boolean;
var
  ADocumentModel: TdxDocumentModel;
  AStartParagraph: TdxParagraph;
begin
  ADocumentModel := DocumentModel;
  AStartParagraph := ActivePieceTable.Paragraphs[StartIndex];
  Result := AStartParagraph.FirstLineIndent < ADocumentModel.DocumentProperties.DefaultTabWidth;
end;

procedure TdxIncrementParagraphIndentCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxIncrementIndentByTheTabCommand }

function TdxIncrementIndentByTheTabCommand.CreateIncrementParagraphIndentCommand: TdxIncrementParagraphIndentCommand;
begin
  Result := TdxIncrementParagraphIndentCommand.Create(RichEditControl);
end;

class function TdxIncrementIndentByTheTabCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementIndentDescription);
end;

class function TdxIncrementIndentByTheTabCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementIndentMenuCaption);
end;

procedure TdxIncrementIndentByTheTabCommand.IncrementParagraphIndent;
var
  ACommand: TdxIncrementParagraphIndentCommand;
begin
  ACommand := CreateIncrementParagraphIndentCommand;
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

{ TdxDecrementParagraphLeftIndentCommand }

function TdxDecrementParagraphLeftIndentCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
var
  AParagraph: TdxParagraph;
  ANearLeftDefaultTab, ANearLeftTab: Integer;
begin
  FillTabsList;
  AParagraph := ActivePieceTable.Paragraphs[StartParagraphIndex];
  ANearLeftDefaultTab := GetNearLeftDefaultTab(AParagraph.LeftIndent);
  ANearLeftTab := GetNearLeftTab(AParagraph.LeftIndent);
  if (ANearLeftDefaultTab > ANearLeftTab) or (ANearLeftTab = AParagraph.LeftIndent) then
    Result := TdxAssignParagraphLeftIndentModifier.Create(ANearLeftDefaultTab - AParagraph.LeftIndent, MaxInt)
  else
    Result := TdxAssignParagraphLeftIndentModifier.Create(ANearLeftTab - AParagraph.LeftIndent, MaxInt);
end;

class function TdxDecrementParagraphLeftIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementParagraphLeftIndentDescription);
end;

class function TdxDecrementParagraphLeftIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementParagraphLeftIndentMenuCaption);
end;

{ TdxDecrementParagraphFirstLineIndentCommand }

function TdxDecrementParagraphFirstLineIndentCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
var
  AParagraph: TdxParagraph;
  ANearLeftDefaultTab, ANearLeftTab: Integer;
begin
  AParagraph := ActivePieceTable.Paragraphs[StartParagraphIndex];
  FillTabsList;
  ANearLeftDefaultTab := GetNearLeftDefaultTab(AParagraph.LeftIndent);
  ANearLeftTab := GetNearLeftTab(AParagraph.FirstLineIndent);
  if ((ANearLeftDefaultTab > ANearLeftTab) and (ANearLeftDefaultTab >= AParagraph.LeftIndent)) or (ANearLeftTab = AParagraph.LeftIndent) then
    Result := TdxParagraphFirstLineIndentModifier.Create(ANearLeftDefaultTab)
  else
    if ANearLeftTab >= AParagraph.LeftIndent then
      Result := TdxParagraphFirstLineIndentModifier.Create(ANearLeftTab)
    else
      Result := TdxParagraphFirstLineIndentModifier.Create(0);
end;

{ TdxDecrementParagraphIndentCommand }

function TdxDecrementParagraphIndentCommand.CreateDecrementParagraphFirstLineIndentCommand: TdxDecrementParagraphFirstLineIndentCommand;
begin
  Result := TdxDecrementParagraphFirstLineIndentCommand.Create(RichEditControl);
end;

function TdxDecrementParagraphIndentCommand.CreateDecrementParagraphLeftIndentCommand: TdxDecrementParagraphLeftIndentCommand;
begin
  Result := TdxDecrementParagraphLeftIndentCommand.Create(RichEditControl);
end;

procedure TdxDecrementParagraphIndentCommand.DecrementParagraphFirstLineIndent;
var
  ACommand: TdxDecrementParagraphFirstLineIndentCommand;
begin
  ACommand := CreateDecrementParagraphFirstLineIndentCommand;
  try
    ACommand.ForceExecute(CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDecrementParagraphIndentCommand.ExecuteCore;
var
  AStartParagraph: TdxParagraph;
  ACommand: TdxDecrementParagraphLeftIndentCommand;
begin
  AStartParagraph := ActivePieceTable.Paragraphs[StartIndex];
  if (AStartParagraph.LeftIndent = 0) and (AStartParagraph.FirstLineIndent <> 0) then
    DecrementParagraphFirstLineIndent;
  if SelectionBeginFirstRowStartPos and FirstLineIndentIsPositive then
    DecrementParagraphFirstLineIndent
  else
  begin
    ACommand := CreateDecrementParagraphLeftIndentCommand;
    try
      ACommand.ForceExecute(CreateDefaultCommandUIState());
    finally
      ACommand.Free;
    end;
  end;
end;

function TdxDecrementParagraphIndentCommand.FirstLineIndentIsPositive: Boolean;
var
  AStartParagraph: TdxParagraph;
begin
  AStartParagraph := ActivePieceTable.Paragraphs[StartIndex];
  Result := AStartParagraph.FirstLineIndent > 0;
end;

class function TdxDecrementParagraphIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementIndentDescription);
end;

class function TdxDecrementParagraphIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementIndentMenuCaption);
end;

procedure TdxDecrementParagraphIndentCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxDecrementIndentByTheTabCommand }

function TdxDecrementIndentByTheTabCommand.CreateDecrementParagraphIndentCommand: TdxDecrementParagraphIndentCommand;
begin
  Result := TdxDecrementParagraphIndentCommand.Create(RichEditControl);
end;

procedure TdxDecrementIndentByTheTabCommand.DecrementParagraphIndent;
var
  ACommand: TdxDecrementParagraphIndentCommand;
begin
  ACommand := CreateDecrementParagraphIndentCommand;
  try
    ACommand.ForceExecute(CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

{ TdxChangeCaseCommandBase }

procedure TdxChangeCaseCommandBase.ChangeInputPositionCharacterFormatting(
  AModifier: TdxRunPropertyModifierBase);
begin
// do nothing
end;

function TdxChangeCaseCommandBase.ChangeProperty(const AStart,
  AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AModifier: TdxRunChangeCaseModifierBase;
begin
  AModifier := CreateModifier;
  try
    Result := ChangeCharacterFormatting(AStart.LogPosition, AEnd.LogPosition, AModifier);
  finally
    AModifier.Free;
  end;
end;

{ TdxChangeTextCasePlaceholderCommand }

function TdxChangeTextCasePlaceholderCommand.CreateModifier: TdxRunChangeCaseModifierBase;
begin
  Result := nil;
end;

procedure TdxChangeTextCasePlaceholderCommand.ForceExecute(
  const AState: IdxCommandUIState);
begin
// do nothing
end;

class function TdxChangeTextCasePlaceholderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeTextCaseDescription);
end;

class function TdxChangeTextCasePlaceholderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeTextCaseMenuCaption);
end;

class function TdxChangeTextCasePlaceholderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeTextCasePlaceholder;
end;

{ TdxMakeTextUpperCaseCommand }

function TdxMakeTextUpperCaseCommand.CreateModifier: TdxRunChangeCaseModifierBase;
begin
  Result := TdxRunMakeUpperCaseModifier.Create;
end;

class function TdxMakeTextUpperCaseCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMakeTextUpperCaseDescription);
end;

class function TdxMakeTextUpperCaseCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMakeTextUpperCaseMenuCaption);
end;

class function TdxMakeTextUpperCaseCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.MakeTextUpperCase;
end;

{ TdxMakeTextLowerCaseCommand }

function TdxMakeTextLowerCaseCommand.CreateModifier: TdxRunChangeCaseModifierBase;
begin
  Result := TdxRunMakeLowerCaseModifier.Create;
end;

class function TdxMakeTextLowerCaseCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMakeTextLowerCaseDescription);
end;

class function TdxMakeTextLowerCaseCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMakeTextLowerCaseMenuCaption);
end;

class function TdxMakeTextLowerCaseCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.MakeTextLowerCase;
end;

{ TdxToggleTextCaseCommand }

function TdxToggleTextCaseCommand.CreateModifier: TdxRunChangeCaseModifierBase;
begin
  Result := TdxRunToggleCaseModifier.Create;
end;

class function TdxToggleTextCaseCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTextCaseDescription);
end;

class function TdxToggleTextCaseCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTextCaseMenuCaption);
end;

class function TdxToggleTextCaseCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTextCase;
end;


{ TdxChangeParagraphStyleCommandBase }

function TdxChangeParagraphStyleCommandBase.CalculateStartPosition(AItem: TdxSelectionItem; AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
var
  AIterator: TdxParagraphsDocumentModelIterator;
begin
  Result := AItem.Interval.NormalizedStart^;
  AIterator := TdxParagraphsDocumentModelIterator.Create(ActivePieceTable);
  try
    if AIterator.IsNewElement(Result) then
      Exit
    else
      Result := AIterator.MoveBack(Result);
  finally
    AIterator.Free;
  end;
end;

function TdxChangeParagraphStyleCommandBase.CalculateEndPosition(AItem: TdxSelectionItem; AAllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
var
  AIterator: TdxParagraphsDocumentModelIterator;
begin
  Result := AItem.Interval.NormalizedEnd^;
  if Result.LogPosition > ActivePieceTable.DocumentEndLogPosition then
  begin
    Result.ParagraphIndex := ActivePieceTable.Paragraphs.Last.Index + 1;
    Exit;
  end;
  AIterator := TdxParagraphsDocumentModelIterator.Create(ActivePieceTable);
  try
    if AIterator.IsNewElement(Result) then
      Exit
    else
      Result := AIterator.MoveForward(Result);
  finally
    AIterator.Free;
  end;
end;

function TdxChangeParagraphStyleCommandBase.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AStartIndex, AEndIndex, AIndex: TdxParagraphIndex;
begin
  AStartIndex := AStart.ParagraphIndex;
  AEndIndex := AEnd.ParagraphIndex;
  if AStartIndex = AEndIndex then
    ChangeParagraphProperty(ActivePieceTable.Paragraphs[AStartIndex])
  else
    for AIndex := AStartIndex to AEndIndex - 1 do
      ChangeParagraphProperty(ActivePieceTable.Paragraphs[AIndex]);
  Result := [];
end;

procedure TdxChangeParagraphStyleCommandBase.ChangeParagraphProperty(AParagraph: TdxParagraph);
begin
  AParagraph.ParagraphProperties.ResetAllUse;
  AParagraph.ParagraphStyleIndex := CalculateParagraphStyleIndex(AParagraph);
end;

procedure TdxChangeParagraphStyleCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.ParagraphStyle);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxChangeParagraphStyleCommand }

constructor TdxChangeParagraphStyleCommand.Create(const AControl: IdxRichEditControl; AParagraphStyle: TdxParagraphStyle);
begin
  inherited Create(AControl);
  FParagraphStyle := AParagraphStyle;
end;

class function TdxChangeParagraphStyleCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeParagraphStyleMenuCaption);
end;

class function TdxChangeParagraphStyleCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeParagraphStyleDescription);
end;

function TdxChangeParagraphStyleCommand.CalculateParagraphStyleIndex(AParagraph: TdxParagraph): Integer;
begin
  if FParagraphStyle = nil then
    Result := DocumentModel.ParagraphStyles.DefaultItemIndex
  else
    Result := DocumentModel.ParagraphStyles.IndexOf(FParagraphStyle);
end;

{ TdxChangeSectionFormattingCommandBase }

function TdxChangeSectionFormattingCommandBase<T>.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AModifier: TdxSectionPropertyModifier<T>;
begin
  AModifier := CreateModifier(AState);
  try
    ChangeSectionFormatting(AStart.LogPosition, AEnd.LogPosition, AModifier);
    Result := [];
  finally
    FreeAndNil(AModifier);
  end;
end;

procedure TdxChangeSectionFormattingCommandBase<T>.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Sections);
  ApplyDocumentProtectionToSelectedSections(AState);
end;

procedure TdxChangeSectionFormattingCommandBase<T>.ChangeSectionFormatting(ALogPositionFrom: TdxDocumentLogPosition; ALogPositionTo: TdxDocumentLogPosition; AModifier: TdxSectionPropertyModifier<T>);
var
  ALength: Integer;
begin
  ALength := Max(1, ALogPositionTo - ALogPositionFrom);
  ActivePieceTable.ApplySectionFormatting(ALogPositionFrom, ALength, AModifier);
end;

{ TdxToggleChangeSectionFormattingCommandBase }

constructor TdxToggleChangeSectionFormattingCommandBase<T>.Create(
  const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
end;

procedure TdxToggleChangeSectionFormattingCommandBase<T>.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if ActivePieceTable.IsMain then
    AState.Checked := IsChecked;
end;

function TdxToggleChangeSectionFormattingCommandBase<T>.IsChecked: Boolean;
var
  AItems: TdxSelectionItemList;
  ACount, I: Integer;
  AItem: TdxSelectionItem;
  AStart, AEnd: TdxDocumentModelPosition;
  AModifier: TdxSectionPropertyModifier<T>;
begin
  AItems := DocumentModel.Selection.Items;
  ACount := AItems.Count;
  Result := True;
  for I := 0 to ACount - 1 do
  begin
    AItem := AItems[I];
    AStart := CalculateStartPosition(AItem, False);
    AEnd := CalculateEndPosition(AItem, False);
    AModifier := CreateModifier(CreateDefaultCommandUIState);
    try
      Result := Result and IsCheckedCore(AStart.LogPosition, AEnd.LogPosition, AModifier);
    finally
      AModifier.Free;
    end;
  end;
end;

function TdxToggleChangeSectionFormattingCommandBase<T>.IsCheckedCore(ALogPositionFrom: TdxDocumentLogPosition;
  ALogPositionTo: TdxDocumentLogPosition; AModifier: TdxSectionPropertyModifier<T>): Boolean;
var
  ALength: Integer;
  AValue: TdxNullableValue<T>;
begin
  ALength := Math.Max(1, ALogPositionTo - ALogPositionFrom);

  AValue := AModifier.ObtainSectionsPropertyValue(ActivePieceTable.DocumentModel, ALogPositionFrom, ALength);
  if AValue.IsNull then
    Result := False
  else
    Result := IsCheckedValue(AValue.Value);
end;

{ TdxQuickStylesGalleryCommand }

procedure TdxQuickStylesGalleryCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  // do nothing
end;

class function TdxQuickStylesGalleryCommand.GetDescription: string;
begin
  Result := '';
end;

class function TdxQuickStylesGalleryCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ChangeFontStyle;
end;

class function TdxQuickStylesGalleryCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandQuickStylesGalleryCaption);
end;

procedure TdxQuickStylesGalleryCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  // do nothing
end;

end.
