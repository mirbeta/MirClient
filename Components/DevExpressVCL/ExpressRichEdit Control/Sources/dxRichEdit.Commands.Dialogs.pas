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

unit dxRichEdit.Commands.Dialogs;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Windows, Generics.Defaults, Generics.Collections, Controls, Graphics,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.View.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.FindAndReplace,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.Tables,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.FindAndReplace,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableModifiers.Simple,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Export.Core,
  dxRichEdit.Options,
  dxRichEdit.Types,
  dxRichEdit.NativeApi;

type

  { TdxShowParagraphFormCommand }

  TdxShowParagraphFormCommand = class(TdxChangeParagraphFormattingCommandBase<TdxMergedParagraphProperties>)
  protected
    procedure ShowParagraphFormCallback(AProperties: TdxMergedParagraphProperties; ACallbackData: TObject); virtual;
    procedure ShowParagraphForm(AParagraphProperties: TdxMergedParagraphProperties;
      const ACallback: TdxShowParagraphFormCallback; ACallbackData: TObject); virtual;
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxMergedParagraphProperties>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetCurrentPropertyValue: TdxMergedParagraphProperties;
    function GetShowsModalDialog: Boolean; override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxTabsFormCommandUIState }

  TdxTabsFormCommandUIState = class(TdxDefaultObjectValueBasedCommandUIState<TdxTabFormattingInfo>)
  strict private
    FDefaultTabWidth: Integer;
    FTabsFormOwner: IdxFormOwner;
  public
    property DefaultTabWidth: Integer read FDefaultTabWidth write FDefaultTabWidth;
    property TabsFormOwner: IdxFormOwner read FTabsFormOwner write FTabsFormOwner;
  end;

  { TdxShowTabsFormCommand }

  TdxShowTabsFormCommand = class(TdxChangeParagraphFormattingCommandBase<TdxTabFormattingInfo>)
  strict private
    FTabFormOwner: IdxFormOwner;
  protected
    procedure ShowTabsFormCallback(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer; ACallbackData: TObject); virtual;
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxTabFormattingInfo>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetCurrentPropertyValue: TdxTabFormattingInfo;
  public
    constructor Create(const AControl: IdxRichEditControl); overload; override;
    constructor Create(const AControl: IdxRichEditControl; const ATabFormOwner: IdxFormOwner); reintroduce; overload;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    procedure ShowTabsForm(ACommandState: TdxTabsFormCommandUIState);
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    property TabFormOwner: IdxFormOwner read FTabFormOwner write FTabFormOwner;
  end;

  { TdxShowNumberingListFormCommand }

  TdxShowNumberingListFormCommand = class(TdxSelectionBasedCommandBase)
  protected
    procedure ShowNumberingListFormCallback(AParagraphs: TdxParagraphList; AData: TObject); virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowTablePropertiesFormCommand }

  TdxShowTablePropertiesFormCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowTablePropertiesFormMenuCommand }

  TdxShowTablePropertiesFormMenuCommand = class(TdxShowTablePropertiesFormCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowPageSetupFormCommandBase }

  TdxShowPageSetupFormCommandBase = class abstract(TdxChangeSectionFormattingCommandBase<TdxPageSetupInfo>)
  protected
    function GetInitialTabPage: TdxPageSetupFormInitialTabPage; virtual; abstract;
    function GetShowsModalDialog: Boolean; override;
    procedure ShowPageSetupFormCallback(AProperties: TdxPageSetupInfo; ACallbackData: TObject); virtual;
    procedure ModifyDocumentModelCore(const AState: IdxCommandUIState); override;
    procedure ShowPageSetupForm(AProperties: TdxPageSetupInfo; const ACallback: TdxShowPageSetupFormCallback;
      ACallbackData: TObject); virtual;
    function CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxPageSetupInfo>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetCurrentPropertyValue: TdxPageSetupInfo;
    function CalculateApplyType(AMinSectionIndex: TdxSectionIndex; AMaxSectionIndex: TdxSectionIndex): TdxSectionPropertiesApplyType;
    function CalculateAvailableApplyTypes(AMinSectionIndex, AMaxSectionIndex: TdxSectionIndex): TdxSectionPropertiesApplyTypes;

    property InitialTabPage: TdxPageSetupFormInitialTabPage read GetInitialTabPage;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
  end;

  { TdxShowPageSetupFormCommand }

  TdxShowPageSetupFormCommand = class(TdxShowPageSetupFormCommandBase)
  protected
    function GetInitialTabPage: TdxPageSetupFormInitialTabPage; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxShowPageMarginsSetupFormCommand }

  TdxShowPageMarginsSetupFormCommand = class(TdxShowPageSetupFormCommandBase)
  protected
    function GetInitialTabPage: TdxPageSetupFormInitialTabPage; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowPagePaperSetupFormCommand }

  TdxShowPagePaperSetupFormCommand = class(TdxShowPageSetupFormCommandBase)
  protected
    function GetInitialTabPage: TdxPageSetupFormInitialTabPage; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeSectionPaperKindCommand }

  TdxChangeSectionPaperKindCommand = class(TdxToggleChangeSectionFormattingCommandBase<TdxPaperKind>)
  strict private
    class var
      FDefaultPaperKindList: TdxPaperKindList;
      FFullPaperKindList: TdxPaperKindList;
  strict private
    FPaperKind: TdxPaperKind;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxPaperKind>; override;
    function IsCheckedValue(AValue: TdxPaperKind): Boolean; override;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); reintroduce;
    class function Id: TdxRichEditCommandId; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;

    class function CreateDefaultPaperKindList: TdxPaperKindList; static;
    class function CreateFullPaperKindList: TdxPaperKindList; static;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class property DefaultPaperKindList: TdxPaperKindList read FDefaultPaperKindList;
    class property FullPaperKindList: TdxPaperKindList read FFullPaperKindList;
    property PaperKind: TdxPaperKind read FPaperKind write FPaperKind;
  end;

  { TdxSetPortraitPageOrientationCommand }

  TdxSetPortraitPageOrientationCommand = class(TdxToggleChangeSectionFormattingCommandBase<Boolean>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<Boolean>; override;
    function IsCheckedValue(AValue: Boolean): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
    class function GetImageName: string; override;
  end;

  { TdxSetLandscapePageOrientationCommand }

  TdxSetLandscapePageOrientationCommand = class(TdxToggleChangeSectionFormattingCommandBase<Boolean>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<Boolean>; override;
    function IsCheckedValue(AValue: Boolean): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
    class function GetImageName: string; override;
  end;

  { TdxFindAndReplaceStringCommand }

  TdxFindAndReplaceStringCommand = class abstract(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure ExecuteCore; override;
    procedure ShowForm(const ASearchString: string); virtual; abstract;
  end;

 { TdxFindCommand }

  TdxFindCommand = class(TdxFindAndReplaceStringCommand)
  protected
    procedure ShowForm(const ASearchString: string); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxReplaceCommand }

  TdxReplaceCommand = class(TdxFindAndReplaceStringCommand)
  protected
    procedure ShowForm(const ASearchString: string); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxShowFontFormCommand }

  TdxShowFontFormCommand = class(TdxChangeCharacterFormattingCommandBase<TdxMergedCharacterProperties>)
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure ShowFontFormCallback(AProperties: TdxMergedCharacterProperties; ACallbackData: TObject); virtual;
    procedure ShowFontForm(ACharacterProperties: TdxMergedCharacterProperties; const ACallback: TdxShowFontFormCallback; ACallbackData: TObject); virtual;
    function CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxMergedCharacterProperties>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function ObtainRunsPropertyValue(const AStart: TdxDocumentModelPosition; ALength: Integer;
      AModifier: TdxRunPropertyModifier<TdxMergedCharacterProperties>; out AValue: TdxMergedCharacterProperties): Boolean; override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowSymbolFormCommand }

  TdxShowSymbolFormCommand = class(TdxRichEditCommand)
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxShowSplitTableCellsFormCommand }

  TdxShowSplitTableCellsFormCommand = class(TdxRichEditCommand)
  protected
    function GetShowsModalDialog: Boolean; override;
    function GetSplitTableCellsParameters: TdxSplitTableCellsParameters;
    function CalculateRowCountAfterMerge(ASelectedCellsCollection: TdxSelectedCellsCollection): Integer;
    function ShouldIgnoreRow(ASelectedCellsInRow: TdxSelectedCellsIntervalInRow): Boolean; virtual;
    function GetActualRowsCount(ASelectedCellsCollection: TdxSelectedCellsCollection; AIsSelectedCellsSquare: Boolean): Integer;
    procedure ShowSplitTableCellsFormCallback(const AParameters: TdxSplitTableCellsParameters; ACallbackData: TObject);
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowSplitTableCellsFormMenuCommand }

  TdxShowSplitTableCellsFormMenuCommand = class(TdxShowSplitTableCellsFormCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowInsertDeleteTableCellsFormCommandBase }

  TdxShowInsertDeleteTableCellsFormCommandBase = class abstract(TdxRichEditCommand)
  strict private
    FInsertDeleteTableCellsCommand: TdxInsertDeleteTableCellsDispatcherCommandBase;
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function CreateTableCellsCommand: TdxInsertDeleteTableCellsDispatcherCommandBase;
    class function GetTableCellsCommandClass: TdxRichEditCommandClass; virtual; {$IFDEF DELPHIXE2}abstract;{$ENDIF}
    function GetTableCellsOperation: TdxTableCellOperation; virtual; abstract;
    procedure ShowInsertDeleteTableCellsForm(const AParameters: TdxTableCellsParameters; const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject); virtual; abstract;
    procedure ShowInsertDeleteTableCellsFormCallback(const AParameters: TdxTableCellsParameters; ACallbackData: TObject);

    property InsertDeleteTableCellsCommand: TdxInsertDeleteTableCellsDispatcherCommandBase read FInsertDeleteTableCellsCommand;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); override;
    destructor Destroy; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
  end;

  { TdxShowInsertTableCellsFormCommand }

  TdxShowInsertTableCellsFormCommand = class(TdxShowInsertDeleteTableCellsFormCommandBase)
  protected
    class function GetTableCellsCommandClass: TdxRichEditCommandClass; override;
    function GetTableCellsOperation: TdxTableCellOperation; override;
    procedure ShowInsertDeleteTableCellsForm(const AParameters: TdxTableCellsParameters;
      const ACallback: TdxShowInsertDeleteTableCellsFormCallback;
      ACallbackData: TObject); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
  end;

  { TdxShowDeleteTableCellsFormCommand }

  TdxShowDeleteTableCellsFormCommand = class(TdxShowInsertDeleteTableCellsFormCommandBase)
  protected
    class function GetTableCellsCommandClass: TdxRichEditCommandClass; override;
    function GetTableCellsOperation: TdxTableCellOperation; override;
    procedure ShowInsertDeleteTableCellsForm(const AParameters: TdxTableCellsParameters;
      const ACallback: TdxShowInsertDeleteTableCellsFormCallback;
      ACallbackData: TObject); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
  end;

  { TdxShowDeleteTableCellsFormMenuCommand }

  TdxShowDeleteTableCellsFormMenuCommand = class(TdxShowDeleteTableCellsFormCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxShowColumnsSetupFormCommand }

  TdxShowColumnsSetupFormCommand = class(TdxChangeSectionFormattingCommandBase<TdxColumnsInfoUI>)
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure ShowColumnsSetupFormCallback(const AProperties: TdxColumnsInfoUI; ACallbackData: TObject);
    procedure ModifyDocumentModelCore(const AState: IdxCommandUIState); override;
    procedure ShowColumnsSetupForm(AProperties: TdxColumnsInfoUI; const ACallback: TdxShowColumnsSetupFormCallback;
      ACallbackData: TObject);
    function CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxColumnsInfoUI>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetCurrentPropertyValue: TdxColumnsInfoUI;
    function CalculateApplyType(AMinSectionIndex: TdxSectionIndex; AMaxSectionIndex: TdxSectionIndex): TdxSectionPropertiesApplyType;
    function CalculateAvailableApplyTypes(AMinSectionIndex: TdxSectionIndex; AMaxSectionIndex: TdxSectionIndex): TdxSectionPropertiesApplyTypes;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowEditStyleFormCommand }

  TdxShowEditStyleFormCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    UseParagraphStyle: Boolean;
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure ShowEditStyleFormCallback(ASourceStyle, ATargetStyle: TdxStyleBase);
    procedure ShowEditStyleForm(ASourceStyle: TdxParagraphStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload;
    procedure ShowEditStyleForm(ASourceStyle: TdxCharacterStyle; AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback); overload;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    procedure FindStyleAndShowForm;
    procedure ModifyParagraphStyle(AParagraphSourceStyle: TdxParagraphStyle; AParagraphStyle: TdxParagraphStyle);
    procedure ModifyCharacterStyle(ACharacterSourceStyle: TdxCharacterStyle; ACharacterStyle: TdxCharacterStyle);

    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowFloatingObjectLayoutOptionsFormCommand }

  TdxShowFloatingObjectLayoutOptionsFormCommand = class(TdxSelectionBasedCommandBase)
  private type
    TdxObjectType = (
      FloatingObject,
      InlineObject,
      None
    );
  private
    FIsFloatingObject: TdxObjectType;
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure ShowFloatingInlineObjectLayoutOptionsFormCallback(const AParameters: TdxFloatingInlineObjectParameters; AData: TObject); virtual;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowLineNumberingFormCommand }

  TdxShowLineNumberingFormCommand = class(TdxChangeSectionFormattingCommandBase<TdxLineNumberingInfo>)
  protected
    procedure ShowLineNumberingFormCallback(AProperties: TdxLineNumberingInfo; ACallbackData: TObject);
    procedure ShowLineNumberingForm(AProperties: TdxLineNumberingInfo;
      const ACallback: TdxShowLineNumberingFormCallback; ACallbackData: TObject);
    function CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxLineNumberingInfo>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetCurrentPropertyValue: TdxLineNumberingInfo;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowMergeFormCommandBase }

  TdxShowMergeFormCommandBase = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure GetDefaultExt(AExporters: TdxExporterList; out AExtension: string; out AIndex: Integer);
    function GetDocumentFormat(const AFileName: string; AExporters: TdxExporterList): TdxRichEditDocumentFormat;
    function CreateExportFilters(AExporters: TdxExporterList): string;
    procedure CalculateMailMergeOptions(const AMergeRecordsParameters: TdxMergeRecordsParameters;
      const AOptions: IdxRichEditMailMergeOptions); virtual;
    procedure MailMergeToNewControl(const AOptions: IdxRichEditMailMergeOptions);
    procedure MailMergeToNewFile(const AOptions: IdxRichEditMailMergeOptions);
    procedure MergeToNewDocument(const AMergeRecordsParameters: TdxMergeRecordsParameters); virtual;
  end;

  { TdxShowMergeDatabaseRecordsFormCommand }

  TdxShowMergeDatabaseRecordsFormCommand = class(TdxShowMergeFormCommandBase)
  protected
    procedure ShowMergeDatabaseRecordsFormCallBack(const AMergeRecordsParameters: TdxMergeRecordsParameters; AData: TObject);
    procedure CalculateMailMergeOptions(const AMergeRecordsParameters: TdxMergeRecordsParameters;
      const AOptions: IdxRichEditMailMergeOptions); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowTOCFormCommand }

  TdxShowTOCFormCommand = class(TdxRichEditSelectionCommand)
  strict private
    FField: TdxField;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
  public
    procedure ExecuteCore; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowTableStyleFormCommand }

  TdxShowTableStyleFormCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FStyle: TdxTableStyle;
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; AStyle: TdxTableStyle = nil); reintroduce;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    procedure FindStyleAndShowForm;
    procedure ShowTableStyleForm(AStyle: TdxTableStyle);

    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property Style: TdxTableStyle read FStyle write FStyle;
  end;

  { TdxProtectDocumentCommand }

  TdxProtectDocumentCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure ShowPasswordFormCallback(const APassword: string);
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxUnprotectDocumentCommand }

  TdxUnprotectDocumentCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure ShowPasswordFormCallback(const APassword: string);
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShowRangeEditingPermissionsFormCommand }

  TdxShowRangeEditingPermissionsFormCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxEncryptDocumentCommand }

  TdxEncryptDocumentCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure ExecuteCore; override;
    procedure ShowPasswordFormCallback(const APassword: string);
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxProcessPaperSizeGalleryCommand }

  TdxProcessPaperSizeGalleryCommand = class(TdxShowPageSetupFormCommandBase)
  strict private
    FPageSetupInfo: IdxValueBasedCommandUIState<TdxPageSetupInfo>;
  protected
    function GetPaperKind: TdxPaperKind; virtual;
    procedure SetPaperKind(const APaperKind: TdxPaperKind); virtual;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    class function GetDescription: string; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxProcessPageMarginsGalleryCommand }

  TdxProcessPageMarginsGalleryCommand = class(TdxShowPageSetupFormCommandBase)
  strict private
    FPageSetupInfo: IdxValueBasedCommandUIState<TdxPageSetupInfo>;

    procedure ConvertToLandscapeMargins(var ATop, ABottom, ALeft, ARight: Single);
    procedure ConvertToPortraitMargins(var ATop, ABottom, ALeft, ARight: Single);
  protected
    procedure GetPageMargins(out ATop, ABottom, ALeft, ARight: Single); virtual;
    procedure SetPageMargins(ATop, ABottom, ALeft, ARight: Single); virtual;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    class function GetDescription: string; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Math, Dialogs, IOUtils, DB,
  dxCore, dxTypeHelpers,

  dxRichEdit.Commands.Images,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Commands.Strs,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.Fields.TocField,
  dxRichEdit.DocumentModel.FieldController,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Commands.Tables.Cells,
  dxRichEdit.Utils.FileDialogFilter,
  dxStringHelper;

type
  TdxSelectionAccess = class(TdxSelection);

{ TdxShowParagraphFormCommand }

function TdxShowParagraphFormCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultObjectValueBasedCommandUIState<TdxMergedParagraphProperties>.Create;
end;

function TdxShowParagraphFormCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxMergedParagraphProperties>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxMergedParagraphProperties>;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxMergedParagraphProperties>;
  Result := TdxParagraphPropertiesModifier.Create(AValueBasedState.Value);
end;

procedure TdxShowParagraphFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxMergedParagraphProperties>;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    AValueBasedState := AState as IdxValueBasedCommandUIState<TdxMergedParagraphProperties>;
    ShowParagraphForm(AValueBasedState.Value, ShowParagraphFormCallback, TObject(AState));
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

function TdxShowParagraphFormCommand.GetCurrentPropertyValue: TdxMergedParagraphProperties;
var
  I, ACount: Integer;
  AItems: TdxSelectionItemList;
  AModifier: TdxMergedParagraphPropertyModifier<TdxMergedParagraphProperties>;
  AProperties, AOldValue: TdxMergedParagraphProperties;
  AStart, AEnd: TdxDocumentModelPosition;
  AItem: TdxSelectionItem;
begin
  AModifier := TdxMergedParagraphPropertyModifier<TdxMergedParagraphProperties>(CreateModifier(CreateDefaultCommandUIState));
  try
    AItems := DocumentModel.Selection.Items;
    ACount := AItems.Count;
    Result := nil;
    Assert(ACount > 0);
    for I := 0 to ACount - 1  do
    begin
      AItem := AItems[I];
      AStart := CalculateStartPosition(AItem, False);
      AEnd := CalculateEndPosition(AItem, False);
      AProperties := ActivePieceTable.ObtainMergedParagraphsProperties(AStart.LogPosition,
        Max(1, AEnd.LogPosition - AStart.LogPosition), AModifier);
      try
        if Result <> nil then
        begin
          AOldValue := Result;
          Result := AModifier.Merge(Result, AProperties);
          AOldValue.Free;
        end
        else
          Result := AProperties;
      finally
        if Result <> AProperties then
          AProperties.Free;
      end;
    end;
  finally
    AModifier.Free;
  end;
end;

class function TdxShowParagraphFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowParagraphFormDescription);
end;

class function TdxShowParagraphFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowParagraphFormMenuCaption);
end;

class function TdxShowParagraphFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowParagraphForm;
end;

class function TdxShowParagraphFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowParagraphForm;
end;

procedure TdxShowParagraphFormCommand.ShowParagraphForm(AParagraphProperties: TdxMergedParagraphProperties;
  const ACallback: TdxShowParagraphFormCallback; ACallbackData: TObject);
begin
  RichEditControl.ShowParagraphForm(AParagraphProperties, ACallback, ACallbackData);
end;

procedure TdxShowParagraphFormCommand.ShowParagraphFormCallback(AProperties: TdxMergedParagraphProperties;
  ACallbackData: TObject);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxMergedParagraphProperties>;
begin
  if not Supports(ACallbackData, IdxValueBasedCommandUIState<TdxMergedParagraphProperties>, AValueBasedState) then
    TdxRichEditExceptions.ThrowInternalException;
  AValueBasedState.Value := AProperties;
  inherited ForceExecute(AValueBasedState);
end;

function TdxShowParagraphFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

procedure TdxShowParagraphFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxMergedParagraphProperties>;
begin
  CheckExecutedAtUIThread;

  inherited UpdateUIStateCore(AState);

  if Supports(AState, IdxValueBasedCommandUIState<TdxMergedParagraphProperties>, AValueBasedState) then
    AValueBasedState.Value := GetCurrentPropertyValue;
end;

{ TdxShowTabsFormCommand }

constructor TdxShowTabsFormCommand.Create(const AControl: IdxRichEditControl; const ATabFormOwner: IdxFormOwner);
begin
  inherited Create(AControl);
  FTabFormOwner := ATabFormOwner;
end;

constructor TdxShowTabsFormCommand.Create(const AControl: IdxRichEditControl);
begin
  Create(AControl, nil);
end;

class function TdxShowTabsFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowTabsFormMenuCaption);
end;

class function TdxShowTabsFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowTabsFormDescription);
end;

procedure TdxShowTabsFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  ACommandState: TdxTabsFormCommandUIState;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    ACommandState := TdxTabsFormCommandUIState(AState);
    ShowTabsForm(ACommandState);
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

procedure TdxShowTabsFormCommand.ShowTabsForm(ACommandState: TdxTabsFormCommandUIState);
begin
  RichEditControl.ShowTabsForm(ACommandState.Value, ACommandState.DefaultTabWidth, ShowTabsFormCallback, ACommandState);
end;

procedure TdxShowTabsFormCommand.ShowTabsFormCallback(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer; ACallbackData: TObject);
var
  ACommandUIState: TdxTabsFormCommandUIState;
begin
  ACommandUIState := TdxTabsFormCommandUIState(ACallbackData);
  ACommandUIState.Value := ATabInfo;
  ACommandUIState.DefaultTabWidth := ADefaultTabWidth;
  inherited ForceExecute(ACommandUIState);
end;

function TdxShowTabsFormCommand.CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxTabFormattingInfo>;
var
  ACommandState: TdxTabsFormCommandUIState;
begin
  ACommandState := TdxTabsFormCommandUIState(AState);
  Result := TdxTabFormattingInfoModifier.Create(ACommandState.Value, ACommandState.DefaultTabWidth);
end;

procedure TdxShowTabsFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ACommandState: TdxTabsFormCommandUIState;
begin
  CheckExecutedAtUIThread;

  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.ParagraphTabs);

  ACommandState := Safe<TdxTabsFormCommandUIState>.Cast(TObject(AState));
  if ACommandState = nil then
    Exit;
  ACommandState.Value := GetCurrentPropertyValue;
  ACommandState.DefaultTabWidth := DocumentModel.DocumentProperties.DefaultTabWidth;
end;

function TdxShowTabsFormCommand.GetCurrentPropertyValue: TdxTabFormattingInfo;
var
  AModifier: TdxMergedParagraphPropertyModifier<TdxTabFormattingInfo>;
  AItems: TdxSelectionItemList;
  ACount, I: Integer;
  AProperties: TdxTabFormattingInfo;
  AItem: TdxSelectionItem;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AModifier := TdxMergedParagraphPropertyModifier<TdxTabFormattingInfo>(CreateModifier(CreateDefaultCommandUIState));
  try
    AItems := DocumentModel.Selection.Items;
    ACount := AItems.Count;
    Result := nil;
    Assert(ACount > 0);
    for I := 0 to ACount - 1 do
    begin
      AItem := AItems[I];
      AStart := CalculateStartPosition(AItem, False);
      AEnd := CalculateEndPosition(AItem, False);
      AProperties := ActivePieceTable.ObtainMergedParagraphsTabFormattingInfo(
        AStart.LogPosition, Max(1, AEnd.LogPosition - AStart.LogPosition), AModifier);
      if Result <> nil then
        AModifier.Merge(Result, AProperties)
      else
        Result := AProperties;
    end;
  finally
    FreeAndNil(AModifier);
  end;
end;

function TdxShowTabsFormCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AState: TdxTabsFormCommandUIState;
begin
  AState := TdxTabsFormCommandUIState.Create;
  AState.TabsFormOwner := TabFormOwner;
  Result := AState;
end;

{ TdxShowNumberingListFormCommand }

class function TdxShowNumberingListFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowNumberingListDescription);
end;

class function TdxShowNumberingListFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowNumberingListMenuCaption);
end;

class function TdxShowNumberingListFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowNumberingListForm;
end;

class function TdxShowNumberingListFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleBulletedList;
end;

procedure TdxShowNumberingListFormCommand.ShowNumberingListFormCallback(AParagraphs: TdxParagraphList; AData: TObject);
begin
end;

procedure TdxShowNumberingListFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AOptions: TdxNumberingOptions;
  AEnabled: Boolean;
begin
  CheckExecutedAtUIThread;
  AOptions := DocumentModel.DocumentCapabilities.Numbering;
  AEnabled := AOptions.MultiLevelAllowed or AOptions.BulletedAllowed or AOptions.SimpleAllowed;
  AState.Enabled := AEnabled and IsContentEditable;
  AState.Visible := RichEditControl.CanShowNumberingListForm and
    ((AOptions.Bulleted <> TdxDocumentCapability.Hidden) or
     (AOptions.MultiLevel <> TdxDocumentCapability.Hidden) or (AOptions.Simple <> TdxDocumentCapability.Hidden));
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

procedure TdxShowNumberingListFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AParagraphs: TdxParagraphList;
begin
  CheckExecutedAtUIThread;
  AParagraphs := DocumentModel.Selection.GetSelectedParagraphs;
  try
    NotifyBeginCommandExecution(AState);
    try
      RichEditControl.ShowNumberingListForm(AParagraphs, ShowNumberingListFormCallback, nil);
    finally
      NotifyEndCommandExecution(AState);
    end;
  finally
    AParagraphs.Free;
  end;
end;

{ TdxShowTablePropertiesFormCommand }

procedure TdxShowTablePropertiesFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  ASelectedCells: TdxSelectedCellsCollection;
begin
  ASelectedCells := GetSelectedCellsCollection;
  if ASelectedCells <> nil then
  try
    RichEditControl.ShowTablePropertiesForm(ASelectedCells);
  finally
    ASelectedCells.Free;
  end;
end;

class function TdxShowTablePropertiesFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowTablePropertiesFormDescription);
end;

class function TdxShowTablePropertiesFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowTablePropertiesFormMenuItemMenuCaption);
end;

class function TdxShowTablePropertiesFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowTablePropertiesForm;
end;

class function TdxShowTablePropertiesFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.TableProperties;
end;

procedure TdxShowTablePropertiesFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := DocumentModel.Selection.IsWholeSelectionInOneTable;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxShowTablePropertiesFormMenuCommand }

class function TdxShowTablePropertiesFormMenuCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowTablePropertiesFormDescriptionMenuItemMenuCaption);
end;

class function TdxShowTablePropertiesFormMenuCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowTablePropertiesFormMenuItemMenuCaption);
end;

class function TdxShowTablePropertiesFormMenuCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowTablePropertiesFormMenuItem;
end;

procedure TdxShowTablePropertiesFormMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Visible := AState.Enabled;
end;

{ TdxShowPageSetupFormCommandBase }

function TdxShowPageSetupFormCommandBase.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

procedure TdxShowPageSetupFormCommandBase.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxPageSetupInfo>;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    AValueBasedState := IdxValueBasedCommandUIState<TdxPageSetupInfo>(AState);
    ShowPageSetupForm(AValueBasedState.Value, ShowPageSetupFormCallback, TObject(AState));
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

procedure TdxShowPageSetupFormCommandBase.ShowPageSetupFormCallback(AProperties: TdxPageSetupInfo; ACallbackData: TObject);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxPageSetupInfo>;
begin
  Supports(ACallbackData, IdxValueBasedCommandUIState<TdxPageSetupInfo>, AValueBasedState);
  AValueBasedState.Value := AProperties;
  inherited ForceExecute(AValueBasedState);
end;

procedure TdxShowPageSetupFormCommandBase.ModifyDocumentModelCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxPageSetupInfo>;
  APieceTable: TdxPieceTable;
  AStart, AEnd: TdxDocumentModelPosition;
  AActions: TdxDocumentModelChangeActions;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxPageSetupInfo>;
  if AValueBasedState.Value.ApplyType = TdxSectionPropertiesApplyType.WholeDocument then
  begin
    APieceTable := ActivePieceTable;
    AStart := TdxDocumentModelPosition.FromParagraphStart(APieceTable, 0);
    AEnd := TdxDocumentModelPosition.FromParagraphEnd(APieceTable, APieceTable.Paragraphs.Count - 1);
    AActions := ChangeProperty(AStart, AEnd, AState);
    APieceTable.ApplyChangesCore(AActions, dxRunIndexDontCare, dxRunIndexDontCare);
  end
  else
    inherited ModifyDocumentModelCore(AState);
end;

procedure TdxShowPageSetupFormCommandBase.ShowPageSetupForm(AProperties: TdxPageSetupInfo;
  const ACallback: TdxShowPageSetupFormCallback; ACallbackData: TObject);
begin
  RichEditControl.ShowPageSetupForm(AProperties, ACallback, ACallbackData, InitialTabPage);
end;

function TdxShowPageSetupFormCommandBase.CreateModifier(
  const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxPageSetupInfo>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxPageSetupInfo>;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxPageSetupInfo>;
  Result := TdxSectionPageSetupModifier.Create(AValueBasedState.Value);
end;

procedure TdxShowPageSetupFormCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxPageSetupInfo>;
begin
  CheckExecutedAtUIThread;

  inherited UpdateUIStateCore(AState);

  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxPageSetupInfo>;
  if AValueBasedState = nil then
    Exit;
  AValueBasedState.Value := GetCurrentPropertyValue;
end;

function TdxShowPageSetupFormCommandBase.GetCurrentPropertyValue: TdxPageSetupInfo;
var
  AModifier: TdxMergedSectionPropertyModifier<TdxPageSetupInfo>;
  AItems: TdxSelectionItemList;
  ACount, I: Integer;
  AMergedResult, AProperties: TdxPageSetupInfo;
  AMinSectionIndex, AMaxSectionIndex: TdxSectionIndex;
  AItem: TdxSelectionItem;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AModifier := TdxMergedSectionPropertyModifier<TdxPageSetupInfo>(CreateModifier(CreateDefaultCommandUIState));
  try
    AItems := DocumentModel.Selection.Items;
    ACount := AItems.Count;
    Result := nil;
    Assert(ACount > 0);
    AMinSectionIndex := dxSectionIndexMinValue;
    AMaxSectionIndex := dxSectionIndexMinValue;
    for I := 0 to ACount - 1 do
    begin
      AItem := AItems[I];
      AStart := CalculateStartPosition(AItem, False);
      AEnd := CalculateEndPosition(AItem, False);
      AProperties := AModifier.ObtainMergedSectionsPropertyValue(DocumentModel, AStart.LogPosition,
        Math.Max(1, AEnd.LogPosition - AStart.LogPosition));
      if Result <> nil then
        try
          AMergedResult := AModifier.Merge(Result, AProperties);
          Result.Free;
          Result := AMergedResult;
        finally
          AProperties.Free;
        end
      else
        Result := AProperties;

      AMinSectionIndex := DocumentModel.FindSectionIndex(AStart.LogPosition);
      AMaxSectionIndex := DocumentModel.FindSectionIndex(AEnd.LogPosition);
    end;
    Result.ApplyType := CalculateApplyType(AMinSectionIndex, AMaxSectionIndex);
    Result.AvailableApplyType := CalculateAvailableApplyTypes(AMinSectionIndex, AMaxSectionIndex);
  finally
    AModifier.Free;
  end;
end;

function TdxShowPageSetupFormCommandBase.CalculateApplyType(AMinSectionIndex: TdxSectionIndex; AMaxSectionIndex: TdxSectionIndex): TdxSectionPropertiesApplyType;
begin
  if DocumentModel.Sections.Count = 1 then
    Exit(TdxSectionPropertiesApplyType.WholeDocument);

  if AMinSectionIndex = AMaxSectionIndex then
    Exit(TdxSectionPropertiesApplyType.CurrentSection);

  Result := TdxSectionPropertiesApplyType.SelectedSections;
end;

function TdxShowPageSetupFormCommandBase.CalculateAvailableApplyTypes(
  AMinSectionIndex, AMaxSectionIndex: TdxSectionIndex): TdxSectionPropertiesApplyTypes;
begin
  if DocumentModel.Sections.Count = 1 then
    Exit([TdxSectionPropertiesApplyType.WholeDocument]);

  if AMinSectionIndex = AMaxSectionIndex then
    Exit([TdxSectionPropertiesApplyType.CurrentSection, TdxSectionPropertiesApplyType.WholeDocument]);

  Result := [TdxSectionPropertiesApplyType.SelectedSections, TdxSectionPropertiesApplyType.WholeDocument];
end;

function TdxShowPageSetupFormCommandBase.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultObjectValueBasedCommandUIState<TdxPageSetupInfo>.Create;
end;

{ TdxShowPageSetupFormCommand }

class function TdxShowPageSetupFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowPageSetupForm;
end;

class function TdxShowPageSetupFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowPageSetupForm;
end;

class function TdxShowPageSetupFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowPageSetupFormMenuCaption);
end;

class function TdxShowPageSetupFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowPageSetupFormDescription);
end;

function TdxShowPageSetupFormCommand.GetInitialTabPage: TdxPageSetupFormInitialTabPage;
begin
  Result := TdxPageSetupFormInitialTabPage.Margins;
end;

{ TdxShowPageMarginsSetupFormCommand }

class function TdxShowPageMarginsSetupFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Margins;
end;

class function TdxShowPageMarginsSetupFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowPageMarginsSetupFormDescription);
end;

function TdxShowPageMarginsSetupFormCommand.GetInitialTabPage: TdxPageSetupFormInitialTabPage;
begin
  Result := TdxPageSetupFormInitialTabPage.Margins;
end;

class function TdxShowPageMarginsSetupFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowPageMarginsSetupFormMenuCaption);
end;

class function TdxShowPageMarginsSetupFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowPageMarginsSetupForm;
end;

{ TdxShowPagePaperSetupFormCommand }

class function TdxShowPagePaperSetupFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowPagePaperSetupFormDescription);
end;

function TdxShowPagePaperSetupFormCommand.GetInitialTabPage: TdxPageSetupFormInitialTabPage;
begin
  Result := TdxPageSetupFormInitialTabPage.Paper;
end;

class function TdxShowPagePaperSetupFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowPagePaperSetupFormMenuCaption);
end;

class function TdxShowPagePaperSetupFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowPagePaperSetupForm;
end;

class function TdxShowPagePaperSetupFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.PaperSize;
end;

{ TdxChangeSectionPaperKindCommand }

constructor TdxChangeSectionPaperKindCommand.Create(const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  FPaperKind := TdxPaperKind.Letter;
end;

function TdxChangeSectionPaperKindCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AState: TdxDefaultValueBasedCommandUIState<TdxPaperKind>;
begin
  AState := TdxDefaultValueBasedCommandUIState<TdxPaperKind>.Create;
  AState.Value := PaperKind;
  Result := AState;
end;

class function TdxChangeSectionPaperKindCommand.CreateDefaultPaperKindList: TdxPaperKindList;
begin
  Result := TdxPaperKindList.Create;
  Result.Add(TdxPaperKind.Letter);
  Result.Add(TdxPaperKind.Legal);

  Result.Add(TdxPaperKind.Folio);
  Result.Add(TdxPaperKind.A4);
  Result.Add(TdxPaperKind.B5);

  Result.Add(TdxPaperKind.Executive);
  Result.Add(TdxPaperKind.A5);
  Result.Add(TdxPaperKind.A6);
end;

class function TdxChangeSectionPaperKindCommand.CreateFullPaperKindList: TdxPaperKindList;
var
  APaperKind: TdxPaperKind;
begin
  Result := TdxPaperKindList.Create;
  for APaperKind := Low(TdxPaperKind) to High(TdxPaperKind) do
    if not (APaperKind in [TdxPaperKind.Custom, TdxPaperKind.Reserved_48, TdxPaperKind.Reserved_49]) then
      Result.Add(APaperKind);
end;

function TdxChangeSectionPaperKindCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxPaperKind>;
begin
  Result := NotImplemented;
end;

class destructor TdxChangeSectionPaperKindCommand.Finalize;
begin
  FreeAndNil(FDefaultPaperKindList);
  FreeAndNil(FFullPaperKindList);
end;

procedure TdxChangeSectionPaperKindCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxPaperKind>;
begin
  NotImplemented;
  if AValueBasedState <> nil then
  begin
    if AValueBasedState.Value <> TdxPaperKind.Custom then
      PaperKind := AValueBasedState.Value;
  end;

  inherited ForceExecute(AState);
end;

class function TdxChangeSectionPaperKindCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeSectionPagePaperKindDescription);
end;

class function TdxChangeSectionPaperKindCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeSectionPagePaperKindMenuCaption);
end;

class function TdxChangeSectionPaperKindCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeSectionPaperKind;
end;

class constructor TdxChangeSectionPaperKindCommand.Initialize;
begin
  FDefaultPaperKindList := CreateDefaultPaperKindList;
  FFullPaperKindList := CreateFullPaperKindList;
end;

function TdxChangeSectionPaperKindCommand.IsCheckedValue(AValue: TdxPaperKind): Boolean;
begin
  Result := AValue = PaperKind;
end;

{ TdxSetPortraitPageOrientationCommand }

function TdxSetPortraitPageOrientationCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxSectionPropertyModifier<Boolean>;
begin
  Result := TdxSectionPageOrientationLandscapeModifier.Create(False);
end;

class function TdxSetPortraitPageOrientationCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetPortraitPageOrientationDescription);
end;

class function TdxSetPortraitPageOrientationCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.PageOrientationPortrait;
end;

class function TdxSetPortraitPageOrientationCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetPortraitPageOrientationMenuCaption);
end;

class function TdxSetPortraitPageOrientationCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetPortraitPageOrientation;
end;

function TdxSetPortraitPageOrientationCommand.IsCheckedValue(AValue: Boolean): Boolean;
begin
  Result := not AValue;
end;

{ TdxSetLandscapePageOrientationCommand }

function TdxSetLandscapePageOrientationCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxSectionPropertyModifier<Boolean>;
begin
  Result := TdxSectionPageOrientationLandscapeModifier.Create(True);
end;

class function TdxSetLandscapePageOrientationCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetLandscapePageOrientationDescription);
end;

class function TdxSetLandscapePageOrientationCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.PageOrientationLandscape;
end;

class function TdxSetLandscapePageOrientationCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetLandscapePageOrientationMenuCaption);
end;

class function TdxSetLandscapePageOrientationCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetLandscapePageOrientation;
end;

function TdxSetLandscapePageOrientationCommand.IsCheckedValue(AValue: Boolean): Boolean;
begin
  Result := AValue;
end;

{ TdxFindAndReplaceStringCommand }

procedure TdxFindAndReplaceStringCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := True;
  AState.Visible := True;
end;

procedure TdxFindAndReplaceStringCommand.ExecuteCore;
begin
  CheckExecutedAtUIThread;

  ShowForm('');
end;

{ TdxFindCommand }

class function TdxFindCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindDescription);
end;

class function TdxFindCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFindMenuCaption);
end;

class function TdxFindCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.Find;
end;

class function TdxFindCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SearchFind;
end;

procedure TdxFindCommand.ShowForm(const ASearchString: string);
begin
  RichEditControl.ShowSearchForm;
end;

{ TdxReplaceCommand }

class function TdxReplaceCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceDescription);
end;

class function TdxReplaceCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandReplaceMenuCaption);
end;

class function TdxReplaceCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.Replace;
end;

class function TdxReplaceCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SearchReplace;
end;

procedure TdxReplaceCommand.ShowForm(const ASearchString: string);
begin
  RichEditControl.ShowReplaceForm;
end;

procedure TdxReplaceCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
end;

{ TdxShowFontFormCommand }

class function TdxShowFontFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Font;
end;

class function TdxShowFontFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowFontFormDescription);
end;

class function TdxShowFontFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowFontFormMenuCaption);
end;

function TdxShowFontFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

class function TdxShowFontFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowFontForm;
end;

procedure TdxShowFontFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxMergedCharacterProperties>;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    AValueBasedState := AState as IdxValueBasedCommandUIState<TdxMergedCharacterProperties> ;
    ShowFontForm(AValueBasedState.Value, ShowFontFormCallback, AState as TdxDefaultCommandUIState);
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

procedure TdxShowFontFormCommand.ShowFontFormCallback(AProperties: TdxMergedCharacterProperties; ACallbackData: TObject);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxMergedCharacterProperties>;
begin
  Supports(ACallbackData, IdxValueBasedCommandUIState<TdxMergedCharacterProperties>, AValueBasedState);
  AValueBasedState.Value := AProperties;
  inherited ForceExecute(AValueBasedState);
end;

procedure TdxShowFontFormCommand.ShowFontForm(ACharacterProperties: TdxMergedCharacterProperties;
  const ACallback: TdxShowFontFormCallback; ACallbackData: TObject);
begin
  RichEditControl.ShowFontForm(ACharacterProperties, ACallback, ACallbackData);
end;

function TdxShowFontFormCommand.CreateModifier(const AState: IdxCommandUIState): TdxRunPropertyModifier<TdxMergedCharacterProperties>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxMergedCharacterProperties>;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxMergedCharacterProperties>;
  Result := TdxFontPropertiesModifier.Create(AValueBasedState.Value);
end;

procedure TdxShowFontFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxMergedCharacterProperties>;
  AValue: TdxMergedCharacterProperties;
begin
  CheckExecutedAtUIThread;

  inherited UpdateUIStateCore(AState);

  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxMergedCharacterProperties>;
  if AValueBasedState = nil then
    Exit;
  GetCurrentPropertyValue(AValue);
  AValueBasedState.Value := AValue;
end;

function TdxShowFontFormCommand.ObtainRunsPropertyValue(const AStart: TdxDocumentModelPosition; ALength: Integer;
  AModifier: TdxRunPropertyModifier<TdxMergedCharacterProperties>; out AValue: TdxMergedCharacterProperties): Boolean;
begin
  AValue := AModifier.ObtainMergedRunsPropertyValue(ActivePieceTable, AStart.LogPosition, ALength);
  Result := True;
end;

function TdxShowFontFormCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultObjectValueBasedCommandUIState<TdxMergedCharacterProperties>.Create;
end;

{ TdxShowSymbolFormCommand }

function TdxShowSymbolFormCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AState: TdxInsertSymbolCommandUIState;
begin
  AState := TdxInsertSymbolCommandUIState.Create;
  AState.Value := TdxSymbolProperties.Create;
  Result := AState;
end;

procedure TdxShowSymbolFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueState: TdxInsertSymbolCommandUIState;
  AModifier: TdxRunFontNamePropertyModifier;
  ARun: TdxTextRunBase;
  ASymbolProperties: TdxSymbolProperties;
begin
  AValueState := AState as TdxInsertSymbolCommandUIState;
  if AValueState = nil then
    Exit;

  if AValueState.Value.FontName = '' then
  begin
    AModifier := TdxRunFontNamePropertyModifier.Create(AValueState.Value.FontName);
    try
      ARun := ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedEnd.RunIndex];
      AValueState.Value.SetFontName(AModifier.GetRunPropertyValue(ARun));
    finally
      AModifier.Free;
    end;
  end;

  ActiveView.CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    ASymbolProperties := TdxSymbolProperties.Create(AValueState.Value.UnicodeChar, AValueState.Value.FontName);
    RichEditControl.ShowSymbolForm(ASymbolProperties, nil, nil);
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

class function TdxShowSymbolFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowSymbolDescription);
end;

class function TdxShowSymbolFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowSymbolForm;
end;

class function TdxShowSymbolFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowSymbolMenuCaption);
end;

function TdxShowSymbolFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

class function TdxShowSymbolFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowSymbolForm;
end;

procedure TdxShowSymbolFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ActiveView.CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxShowSplitTableCellsFormCommand }

class function TdxShowSplitTableCellsFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSplitTableCellsDescription);
end;

class function TdxShowSplitTableCellsFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSplitTableCellsMenuCaption);
end;

class function TdxShowSplitTableCellsFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowSplitTableCellsForm;
end;

class function TdxShowSplitTableCellsFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SplitCells;
end;

function TdxShowSplitTableCellsFormCommand.CalculateRowCountAfterMerge(
  ASelectedCellsCollection: TdxSelectedCellsCollection): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ASelectedCellsCollection.RowsCount - 1 do
  begin
    if not ShouldIgnoreRow(ASelectedCellsCollection[I]) then
      Inc(Result);
  end;
  Result := Max(1, Result);
end;

function TdxShowSplitTableCellsFormCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<TdxSplitTableCellsParameters>.Create;
end;

procedure TdxShowSplitTableCellsFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxSplitTableCellsParameters>;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    AValueBasedState := AState as IdxValueBasedCommandUIState<TdxSplitTableCellsParameters>;
    AValueBasedState.Value := GetSplitTableCellsParameters;

    RichEditControl.ShowSplitTableCellsForm(AValueBasedState.Value, ShowSplitTableCellsFormCallback, AState as TObject);
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

function TdxShowSplitTableCellsFormCommand.GetActualRowsCount(ASelectedCellsCollection: TdxSelectedCellsCollection;
  AIsSelectedCellsSquare: Boolean): Integer;
var
  AColumnIndex: Integer;
begin
  if AIsSelectedCellsSquare then
  begin
    AColumnIndex := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ASelectedCellsCollection.NormalizedFirst.NormalizedStartCell, False);
    Result := ASelectedCellsCollection.GetSelectedRowsCount(AColumnIndex);
  end
  else
    Result := 0;
end;

function TdxShowSplitTableCellsFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

function TdxShowSplitTableCellsFormCommand.GetSplitTableCellsParameters: TdxSplitTableCellsParameters;
var
  ASelectedCellsCollection: TdxSelectedCellsCollection;
  ASelectedCellsInRowCount, AColumnsCount, ARowCountAfterMerge: Integer;
  AIsSelectedCellsSquare, AIsMergeCellsBeforeSplit: Boolean;
begin
  ASelectedCellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  ASelectedCellsInRowCount := ASelectedCellsCollection.NormalizedFirst.NormalizedLength;
  AIsSelectedCellsSquare := ASelectedCellsCollection.IsSquare;
  AIsMergeCellsBeforeSplit := AIsSelectedCellsSquare and ((ASelectedCellsCollection.RowsCount > 1) or (ASelectedCellsInRowCount > 0));
  AColumnsCount := (ASelectedCellsInRowCount + 1) * 2;
  if AIsMergeCellsBeforeSplit then
    ARowCountAfterMerge := CalculateRowCountAfterMerge(ASelectedCellsCollection)
  else
    ARowCountAfterMerge := GetActualRowsCount(ASelectedCellsCollection, AIsSelectedCellsSquare);

  Result := TdxSplitTableCellsParameters.Create(AColumnsCount, ASelectedCellsCollection.RowsCount, AIsMergeCellsBeforeSplit, ARowCountAfterMerge);
  Result.IsSelectedCellsSquare := AIsSelectedCellsSquare;
end;

function TdxShowSplitTableCellsFormCommand.ShouldIgnoreRow(ASelectedCellsInRow: TdxSelectedCellsIntervalInRow): Boolean;
var
  AStartCellIndex, AEndCellIndex, ACellCount, I: Integer;
  ACells: TdxTableCellCollection;
begin
  AStartCellIndex := ASelectedCellsInRow.NormalizedStartCellIndex;
  AEndCellIndex := ASelectedCellsInRow.NormalizedEndCellIndex;
  ACells := ASelectedCellsInRow.Row.Cells;
  ACellCount := ACells.Count;
  for I := 0 to ACellCount - 1 do
  begin
    if (I >= AStartCellIndex) and (I <= AEndCellIndex) then
      Continue;
    if ACells[I].VerticalMerging <> TdxMergingState.Continue then
      Exit(False);
  end;
  Result := True;
end;

procedure TdxShowSplitTableCellsFormCommand.ShowSplitTableCellsFormCallback(const AParameters: TdxSplitTableCellsParameters;
  ACallbackData: TObject);
var
  ATransaction: TdxHistoryTransaction;
  ACommand: TdxSplitTableCellsCommand;
  AState: IdxValueBasedCommandUIState<TdxSplitTableCellsParameters>;
begin
  RichEditControl.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      ACommand := TdxSplitTableCellsCommand.Create(RichEditControl);
      try
        if ACommand.CanExecute then
        begin
          AState := TdxDefaultValueBasedCommandUIState<TdxSplitTableCellsParameters>.Create;
          AState.Value := AParameters;
          ACommand.ForceExecute(AState);
        end;
      finally
        ACommand.Free;
      end;
    finally
      ATransaction.Free;
    end;
  finally
    RichEditControl.EndUpdate;
  end;
end;

procedure TdxShowSplitTableCellsFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := DocumentModel.Selection.IsWholeSelectionInOneTable and
    TdxSelectionAccess(DocumentModel.Selection).IsValidSelectedCells;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;


{ TdxShowSplitTableCellsFormMenuCommand }

class function TdxShowSplitTableCellsFormMenuCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSplitTableCellsMenuItemMenuCaption);
end;

class function TdxShowSplitTableCellsFormMenuCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowSplitTableCellsFormMenuItem;
end;

procedure TdxShowSplitTableCellsFormMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AIsSelectionInTable: Boolean;
begin
  AIsSelectionInTable := DocumentModel.Selection.IsWholeSelectionInOneTable;
  AState.Enabled := AIsSelectionInTable and DocumentModel.Selection.SelectedCells.SelectedOnlyOneCell;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
  AState.Visible := AState.Enabled;
end;

{ TdxShowInsertDeleteTableCellsFormCommandBase }

constructor TdxShowInsertDeleteTableCellsFormCommandBase.Create(const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  FInsertDeleteTableCellsCommand := CreateTableCellsCommand;
end;

function TdxShowInsertDeleteTableCellsFormCommandBase.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<TdxTableCellsParameters>.Create;
end;

destructor TdxShowInsertDeleteTableCellsFormCommandBase.Destroy;
begin
  FreeAndNil(FInsertDeleteTableCellsCommand);
  inherited Destroy;
end;

class function TdxShowInsertDeleteTableCellsFormCommandBase.GetDescription: string;
begin
  Result := GetTableCellsCommandClass.GetDescription;
end;

{$IFNDEF DELPHIXE2}
class function TdxShowInsertDeleteTableCellsFormCommandBase.GetTableCellsCommandClass: TdxRichEditCommandClass;
begin
  raise Exception.Create('for C++Builder XE');
end;
{$ENDIF}

class function TdxShowInsertDeleteTableCellsFormCommandBase.GetMenuCaption: string;
begin
  Result := GetTableCellsCommandClass.GetMenuCaption;
end;

procedure TdxShowInsertDeleteTableCellsFormCommandBase.ForceExecute(
  const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxTableCellsParameters>;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    AValueBasedState := AState as IdxValueBasedCommandUIState<TdxTableCellsParameters>;
    AValueBasedState.Value := TdxTableCellsParameters.Create(GetTableCellsOperation);
    ShowInsertDeleteTableCellsForm(AValueBasedState.Value, ShowInsertDeleteTableCellsFormCallback, TObject(AState));
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

function TdxShowInsertDeleteTableCellsFormCommandBase.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

function TdxShowInsertDeleteTableCellsFormCommandBase.CreateTableCellsCommand: TdxInsertDeleteTableCellsDispatcherCommandBase;
begin
  Result := TdxInsertDeleteTableCellsDispatcherCommandBase(GetTableCellsCommandClass.Create(RichEditControl));
end;

procedure TdxShowInsertDeleteTableCellsFormCommandBase.ShowInsertDeleteTableCellsFormCallback(
  const AParameters: TdxTableCellsParameters; ACallbackData: TObject);
var
  ATransaction: TdxHistoryTransaction;
  AState: IdxValueBasedCommandUIState<TdxTableCellsParameters>;
begin
  RichEditControl.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      if InsertDeleteTableCellsCommand.CanExecute then
      begin
        AState := TdxDefaultValueBasedCommandUIState<TdxTableCellsParameters>.Create;
        AState.Value := AParameters;
        InsertDeleteTableCellsCommand.ForceExecute(AState);
      end;
    finally
      ATransaction.Free;
    end;
  finally
    RichEditControl.EndUpdate;
  end;
end;

procedure TdxShowInsertDeleteTableCellsFormCommandBase.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  AState.Enabled := DocumentModel.Selection.IsWholeSelectionInOneTable;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxShowInsertTableCellsFormCommand }

class function TdxShowInsertTableCellsFormCommand.GetTableCellsCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTableCellsDispatcherCommand;
end;

function TdxShowInsertTableCellsFormCommand.GetTableCellsOperation: TdxTableCellOperation;
begin
  Result := TdxTableCellOperation.ShiftToTheVertically;
end;

class function TdxShowInsertTableCellsFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowInsertTableCellsForm;
end;

class function TdxShowInsertTableCellsFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertTableCells;
end;

procedure TdxShowInsertTableCellsFormCommand.ShowInsertDeleteTableCellsForm(
  const AParameters: TdxTableCellsParameters;
  const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject);
begin
  RichEditControl.ShowInsertTableCellsForm(AParameters, ACallback, ACallbackData);
end;

{ TdxShowDeleteTableCellsFormCommand }

class function TdxShowDeleteTableCellsFormCommand.GetTableCellsCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxDeleteTableCellsDispatcherCommand;
end;

function TdxShowDeleteTableCellsFormCommand.GetTableCellsOperation: TdxTableCellOperation;
begin
  Result := TdxTableCellOperation.ShiftToTheHorizontally;
end;

class function TdxShowDeleteTableCellsFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowDeleteTableCellsForm;
end;

class function TdxShowDeleteTableCellsFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DeleteCells;
end;

procedure TdxShowDeleteTableCellsFormCommand.ShowInsertDeleteTableCellsForm(
  const AParameters: TdxTableCellsParameters;
  const ACallback: TdxShowInsertDeleteTableCellsFormCallback; ACallbackData: TObject);
begin
  RichEditControl.ShowDeleteTableCellsForm(AParameters, ACallback, ACallbackData);
end;

{ TdxShowDeleteTableCellsFormMenuCommand }

class function TdxShowDeleteTableCellsFormMenuCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableCellsMenuItem);
end;

class function TdxShowDeleteTableCellsFormMenuCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowDeleteTableCellsFormMenuItem;
end;

procedure TdxShowDeleteTableCellsFormMenuCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
var
  ACellsCollection: TdxSelectedCellsCollection;
begin
  inherited UpdateUIStateCore(AState);
  if not AState.Enabled then
  begin
    AState.Visible := False;
    Exit;
  end;
  ACellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  AState.Enabled := AState.Enabled and not ACellsCollection.IsSelectedEntireTableRows and
    not ACellsCollection.IsSelectedEntireTableColumns;
  AState.Visible := AState.Enabled;
end;

{ TdxShowColumnsSetupFormCommand }

class function TdxShowColumnsSetupFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowColumnsSetupFormDescription);
end;

class function TdxShowColumnsSetupFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowColumnsSetupFormMenuCaption);
end;

class function TdxShowColumnsSetupFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowColumnsSetupForm;
end;

class function TdxShowColumnsSetupFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowColumnsSetupForm;
end;

procedure TdxShowColumnsSetupFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    AValueBasedState := AState as IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
    ShowColumnsSetupForm(AValueBasedState.Value, ShowColumnsSetupFormCallback, TObject(AState));
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

function TdxShowColumnsSetupFormCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AState: TdxDefaultValueBasedCommandUIState<TdxColumnsInfoUI>;
begin
  AState := TdxDefaultObjectValueBasedCommandUIState<TdxColumnsInfoUI>.Create;
  Result := AState;
end;

function TdxShowColumnsSetupFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

procedure TdxShowColumnsSetupFormCommand.ShowColumnsSetupFormCallback(const AProperties: TdxColumnsInfoUI; ACallbackData: TObject);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
begin
  if Supports(ACallbackData, IdxValueBasedCommandUIState<TdxColumnsInfoUI>, AValueBasedState) then
    AValueBasedState.Value := AProperties
  else
    TdxRichEditExceptions.ThrowInternalException;
  inherited ForceExecute(AValueBasedState);
end;

procedure TdxShowColumnsSetupFormCommand.ModifyDocumentModelCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
  APieceTable: TdxPieceTable;
  AStart, AEnd: TdxDocumentModelPosition;
  AActions: TdxDocumentModelChangeActions;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
  if AValueBasedState.Value.ApplyType = TdxSectionPropertiesApplyType.WholeDocument then
  begin
    APieceTable := ActivePieceTable;
    AStart := TdxDocumentModelPosition.FromParagraphStart(APieceTable, 0);
    AEnd := TdxDocumentModelPosition.FromParagraphEnd(APieceTable, APieceTable.Paragraphs.Count - 1);
    AActions := ChangeProperty(AStart, AEnd, AState);
    APieceTable.ApplyChangesCore(AActions, dxRunIndexDontCare, dxRunIndexDontCare);
  end
  else
    inherited ModifyDocumentModelCore(AState);
end;

procedure TdxShowColumnsSetupFormCommand.ShowColumnsSetupForm(AProperties: TdxColumnsInfoUI;
  const ACallback: TdxShowColumnsSetupFormCallback; ACallbackData: TObject);
begin
  RichEditControl.ShowColumnsSetupForm(AProperties, ACallback, ACallbackData);
end;

function TdxShowColumnsSetupFormCommand.CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxColumnsInfoUI>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
  Result := TdxSectionColumnsSetupModifier.Create(AValueBasedState.Value);
end;

procedure TdxShowColumnsSetupFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
begin
  CheckExecutedAtUIThread;

  inherited UpdateUIStateCore(AState);

  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxColumnsInfoUI>;
  if AValueBasedState = nil then
    Exit;
  AValueBasedState.Value := GetCurrentPropertyValue;
end;

function TdxShowColumnsSetupFormCommand.GetCurrentPropertyValue: TdxColumnsInfoUI;
var
  AState: IdxCommandUIState;
  AModifier: TdxMergedSectionPropertyModifier<TdxColumnsInfoUI>;
  AItems: TdxSelectionItemList;
  ACount, I: Integer;
  AValue, AProperties: TdxColumnsInfoUI;
  AMinSectionIndex, AMaxSectionIndex: TdxSectionIndex;
  AItem: TdxSelectionItem;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AState := CreateDefaultCommandUIState;
  try
    AModifier := TdxMergedSectionPropertyModifier<TdxColumnsInfoUI>(CreateModifier(AState));
    try
      AItems := DocumentModel.Selection.Items;
      ACount := AItems.Count;
      Result := nil;
      Assert(ACount > 0);
      AMinSectionIndex := dxSectionIndexMinValue;
      AMaxSectionIndex := dxSectionIndexMinValue;
      for I := 0 to ACount - 1 do
      begin
        AItem := AItems[I];
        AStart := CalculateStartPosition(AItem, False);
        AEnd := CalculateEndPosition(AItem, False);
        AProperties := AModifier.ObtainMergedSectionsPropertyValue(DocumentModel, AStart.LogPosition, Max(1, AEnd.LogPosition - AStart.LogPosition));
        if Result <> nil then
        try
          AValue := AModifier.Merge(Result, AProperties);
          AValue.Free;
        finally
          AProperties.Free;
        end
        else
          Result := AProperties;

        AMinSectionIndex := DocumentModel.FindSectionIndex(AStart.LogPosition);
        AMaxSectionIndex := DocumentModel.FindSectionIndex(AEnd.LogPosition);
      end;
      Result.ApplyType := CalculateApplyType(AMinSectionIndex, AMaxSectionIndex);
      Result.AvailableApplyType := CalculateAvailableApplyTypes(AMinSectionIndex, AMaxSectionIndex);
    finally
      AModifier.Free;
    end;
  finally
    AState := nil;
  end;
end;

function TdxShowColumnsSetupFormCommand.CalculateApplyType(AMinSectionIndex: TdxSectionIndex; AMaxSectionIndex: TdxSectionIndex): TdxSectionPropertiesApplyType;
begin
  if DocumentModel.Sections.Count = 1 then
    Result := TdxSectionPropertiesApplyType.WholeDocument
  else
    if AMinSectionIndex = AMaxSectionIndex then
      Result := TdxSectionPropertiesApplyType.CurrentSection
    else
      Result := TdxSectionPropertiesApplyType.SelectedSections;
end;

function TdxShowColumnsSetupFormCommand.CalculateAvailableApplyTypes(AMinSectionIndex: TdxSectionIndex; AMaxSectionIndex: TdxSectionIndex): TdxSectionPropertiesApplyTypes;
begin
  if DocumentModel.Sections.Count = 1 then
    Result := [TdxSectionPropertiesApplyType.WholeDocument]
  else
    if AMinSectionIndex = AMaxSectionIndex then
      Result := [TdxSectionPropertiesApplyType.CurrentSection, TdxSectionPropertiesApplyType.WholeDocument]
    else
      Result := [TdxSectionPropertiesApplyType.SelectedSections, TdxSectionPropertiesApplyType.WholeDocument];
end;

{ TdxShowEditStyleFormCommand }

constructor TdxShowEditStyleFormCommand.Create(const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  UseParagraphStyle := False;
end;

class function TdxShowEditStyleFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowEditStyleFormDescription);
end;

class function TdxShowEditStyleFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowEditStyleFormMenuCaption);
end;

function TdxShowEditStyleFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

class function TdxShowEditStyleFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowEditStyleForm;
end;

procedure TdxShowEditStyleFormCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    FindStyleAndShowForm;
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

procedure TdxShowEditStyleFormCommand.FindStyleAndShowForm;
var
  AModel: TdxDocumentModel;
  AStartIndex, AEndIndex, I: TdxRunIndex;
  AFirstRun, ARun: TdxTextRunBase;
  ACharacterSourceStyle: TdxCharacterStyle;
  AParagraphSourceStyle: TdxParagraphStyle;
  AOnlyOneParagraphStyle: Boolean;
  AIndex: TdxParagraphIndex;
begin
  AModel := RichEditControl.InnerControl.DocumentModel;
  AStartIndex := AModel.Selection.Interval.NormalizedStart.RunIndex;
  AEndIndex := AModel.Selection.Interval.NormalizedEnd.RunIndex;
  AFirstRun := AModel.ActivePieceTable.Runs[AStartIndex];
  ACharacterSourceStyle := AFirstRun.CharacterStyle;
  AParagraphSourceStyle := AFirstRun.Paragraph.ParagraphStyle;
  AOnlyOneParagraphStyle := True;

  for I := AStartIndex to AEndIndex do
  begin
    ARun := AModel.ActivePieceTable.Runs[I];
    if ARun.Paragraph.ParagraphStyle <> AParagraphSourceStyle then
    begin
      AOnlyOneParagraphStyle := False;
      Continue;
    end;
    if ARun.CharacterStyle <> ACharacterSourceStyle then
    begin
      UseParagraphStyle := True;
      Continue;
    end;
    if ARun.CharacterStyleIndex = 0 then
      UseParagraphStyle := True;
  end;

  if AOnlyOneParagraphStyle then
  begin
    AIndex := AFirstRun.Paragraph.Index;
    if UseParagraphStyle then
      ShowEditStyleForm(AParagraphSourceStyle, AIndex, ShowEditStyleFormCallback)
    else
      ShowEditStyleForm(ACharacterSourceStyle, AIndex, ShowEditStyleFormCallback);
  end;
end;

procedure TdxShowEditStyleFormCommand.ModifyParagraphStyle(AParagraphSourceStyle: TdxParagraphStyle; AParagraphStyle: TdxParagraphStyle);
begin
  AParagraphSourceStyle.BeginUpdate;
  try
    AParagraphSourceStyle.CopyProperties(AParagraphStyle);
    AParagraphSourceStyle.StyleName := AParagraphStyle.StyleName;
    AParagraphSourceStyle.Tabs.SetTabs(AParagraphStyle.Tabs.GetTabs);
    AParagraphSourceStyle.Parent := AParagraphStyle.Parent;
    AParagraphSourceStyle.NextParagraphStyle := AParagraphStyle.NextParagraphStyle;
  finally
    AParagraphSourceStyle.EndUpdate;
  end;
end;

procedure TdxShowEditStyleFormCommand.ModifyCharacterStyle(ACharacterSourceStyle: TdxCharacterStyle; ACharacterStyle: TdxCharacterStyle);
begin
  ACharacterSourceStyle.BeginUpdate;
  try
    ACharacterSourceStyle.CopyProperties(ACharacterStyle);
    ACharacterSourceStyle.StyleName := ACharacterStyle.StyleName;
    ACharacterSourceStyle.Parent := ACharacterStyle.Parent;
  finally
    ACharacterSourceStyle.EndUpdate;
  end;
end;

procedure TdxShowEditStyleFormCommand.ShowEditStyleFormCallback(ASourceStyle, ATargetStyle: TdxStyleBase);
var
 AParagraphStyleTo, AParagraphStyleFrom: TdxParagraphStyle;
 ACharacterStyleTo, ACharacterStyleFrom: TdxCharacterStyle;
begin
  DocumentModel.BeginUpdate;
  try
    AParagraphStyleTo := Safe<TdxParagraphStyle>.Cast(ASourceStyle);
    AParagraphStyleFrom := Safe<TdxParagraphStyle>.Cast(ATargetStyle);
    if (AParagraphStyleTo <> nil) and (AParagraphStyleFrom <> nil) then
    begin
      ModifyParagraphStyle(AParagraphStyleTo, AParagraphStyleFrom);
      Exit;
    end;
    ACharacterStyleTo := Safe<TdxCharacterStyle>.Cast(ASourceStyle);
    ACharacterStyleFrom := Safe<TdxCharacterStyle>.Cast(ATargetStyle);
    if (ACharacterStyleTo <> nil) and (ACharacterStyleFrom <> nil) then
      ModifyCharacterStyle(ACharacterStyleTo, ACharacterStyleFrom);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxShowEditStyleFormCommand.ShowEditStyleForm(ASourceStyle: TdxParagraphStyle;
  AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback);
begin
  RichEditControl.ShowEditStyleForm(ASourceStyle, AIndex, ACallback);
end;

procedure TdxShowEditStyleFormCommand.ShowEditStyleForm(ASourceStyle: TdxCharacterStyle;
  AIndex: TdxParagraphIndex; const ACallback: TdxShowEditStyleFormCallback);
begin
  RichEditControl.ShowEditStyleForm(ASourceStyle, AIndex, ACallback);
end;

procedure TdxShowEditStyleFormCommand.ExecuteCore;
begin
end;

procedure TdxShowEditStyleFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;

  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.CharacterStyle);
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.ParagraphStyle);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxShowFloatingObjectLayoutOptionsFormCommand }

constructor TdxShowFloatingObjectLayoutOptionsFormCommand.Create(const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  FIsFloatingObject := TdxObjectType.None;
end;

procedure TdxShowFloatingObjectLayoutOptionsFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AFloatingObjectAnchorRun: TdxFloatingObjectAnchorRun;
  AInlinePictureRun: TdxInlinePictureRun;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    if FIsFloatingObject = TdxObjectType.FloatingObject then
    begin
      AFloatingObjectAnchorRun := Safe<TdxFloatingObjectAnchorRun>.Cast(ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedStart.RunIndex]);
      RichEditControl.ShowFloatingInlineObjectLayoutOptionsForm(TdxFloatingInlineObjectParameters.Create(AFloatingObjectAnchorRun), ShowFloatingInlineObjectLayoutOptionsFormCallback, nil);
    end
    else
      if FIsFloatingObject = TdxObjectType.InlineObject then
      begin
        AInlinePictureRun := Safe<TdxInlinePictureRun>.Cast(ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedStart.RunIndex]);
        RichEditControl.ShowFloatingInlineObjectLayoutOptionsForm(TdxFloatingInlineObjectParameters.Create(AInlinePictureRun), ShowFloatingInlineObjectLayoutOptionsFormCallback, nil);
      end;
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

class function TdxShowFloatingObjectLayoutOptionsFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowFloatingObjectLayoutOptionsFormDescription);
end;

class function TdxShowFloatingObjectLayoutOptionsFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowFloatingObjectLayoutOptionsFormMenuCaption);
end;

function TdxShowFloatingObjectLayoutOptionsFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

class function TdxShowFloatingObjectLayoutOptionsFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowFloatingObjectLayoutOptionsForm;
end;

class function TdxShowFloatingObjectLayoutOptionsFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.FloatingObjectLayoutOptions;
end;

procedure TdxShowFloatingObjectLayoutOptionsFormCommand.ShowFloatingInlineObjectLayoutOptionsFormCallback(
  const AParameters: TdxFloatingInlineObjectParameters; AData: TObject);
begin
end;

procedure TdxShowFloatingObjectLayoutOptionsFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ANeedEnable: Boolean;
begin
  if DocumentModel.Selection.IsFloatingObjectSelected then
    FIsFloatingObject := TdxObjectType.FloatingObject
  else
    if DocumentModel.Selection.IsInlinePictureSelected then
      FIsFloatingObject := TdxObjectType.InlineObject
    else
      FIsFloatingObject := TdxObjectType.None;

  ANeedEnable := IsContentEditable and (FIsFloatingObject <> TdxObjectType.None);
  AState.Enabled := ANeedEnable;
  AState.Visible := ANeedEnable;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxShowLineNumberingFormCommand }

procedure TdxShowLineNumberingFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxLineNumberingInfo>;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    AValueBasedState := AState as IdxValueBasedCommandUIState<TdxLineNumberingInfo>;
    ShowLineNumberingForm(AValueBasedState.Value, ShowLineNumberingFormCallback, AState as TObject);
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

procedure TdxShowLineNumberingFormCommand.ShowLineNumberingFormCallback(AProperties: TdxLineNumberingInfo; ACallbackData: TObject);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxLineNumberingInfo>;
begin
  if Supports(ACallbackData, IdxValueBasedCommandUIState<TdxLineNumberingInfo>, AValueBasedState) then
    AValueBasedState.Value := AProperties
  else
    TdxRichEditExceptions.ThrowInternalException;
  inherited ForceExecute(AValueBasedState);
end;

procedure TdxShowLineNumberingFormCommand.ShowLineNumberingForm(AProperties: TdxLineNumberingInfo;
  const ACallback: TdxShowLineNumberingFormCallback; ACallbackData: TObject);
begin
  RichEditControl.ShowLineNumberingForm(AProperties, ACallback, ACallbackData);
end;

function TdxShowLineNumberingFormCommand.CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxLineNumberingInfo>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxLineNumberingInfo>;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxLineNumberingInfo>;
  Result := TdxSectionLineNumberingModifier.Create(AValueBasedState.Value);
end;

procedure TdxShowLineNumberingFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxLineNumberingInfo>;
begin
  CheckExecutedAtUIThread;

  inherited UpdateUIStateCore(AState);

  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxLineNumberingInfo>;
  if AValueBasedState = nil then
    Exit;
  AValueBasedState.Value := GetCurrentPropertyValue;
end;

function TdxShowLineNumberingFormCommand.GetCurrentPropertyValue: TdxLineNumberingInfo;
var
  AModifier: TdxMergedSectionPropertyModifier<TdxLineNumberingInfo>;
  AItems: TdxSelectionItemList;
  ACount, I: Integer;
  AValue, AProperties: TdxLineNumberingInfo;
  AItem: TdxSelectionItem;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AModifier := TdxMergedSectionPropertyModifier<TdxLineNumberingInfo>(CreateModifier(CreateDefaultCommandUIState));
  try
    AItems := DocumentModel.Selection.Items;
    ACount := AItems.Count;
    Result := nil;
    Assert(ACount > 0);
    for I := 0 to ACount - 1 do
    begin
      AItem := AItems[I];
      AStart := CalculateStartPosition(AItem, False);
      AEnd := CalculateEndPosition(AItem, False);
      AProperties := AModifier.ObtainMergedSectionsPropertyValue(DocumentModel, AStart.LogPosition,
        Max(1, AEnd.LogPosition - AStart.LogPosition));
      if Result <> nil then
      try
        AValue := AModifier.Merge(Result, AProperties);
        AValue.Free;
      finally
        AProperties.Free;
      end
      else
        Result := AProperties;
    end;
  finally
    AModifier.Free;
  end;
end;

class function TdxShowLineNumberingFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowLineNumberingFormDescription);
end;

class function TdxShowLineNumberingFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowLineNumberingFormMenuCaption);
end;

class function TdxShowLineNumberingFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowLineNumberingForm;
end;

function TdxShowLineNumberingFormCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultObjectValueBasedCommandUIState<TdxLineNumberingInfo>.Create;
end;

{ TdxShowMergeFormCommandBase }

procedure TdxShowMergeFormCommandBase.CalculateMailMergeOptions(const AMergeRecordsParameters: TdxMergeRecordsParameters;
  const AOptions: IdxRichEditMailMergeOptions);
begin
end;

function TdxShowMergeFormCommandBase.CreateExportFilters(AExporters: TdxExporterList): string;
var
  AStringBuilder: TStringBuilder;
  ACount, I: Integer;
  AFilter: TdxFileDialogFilter;
begin
  AStringBuilder := TStringBuilder.Create;
  try
    ACount := AExporters.Count;
    for I := 0 to ACount - 1 do
    begin
      AFilter := AExporters[I].Filter;
      if AFilter.Extensions.Count > 0 then
      begin
        if I > 0 then
          AStringBuilder.Append('|');
        AStringBuilder.Append(AFilter.ToString);
      end;
    end;
    Result := AStringBuilder.ToString;
  finally
    AStringBuilder.Free;
  end;
end;

procedure TdxShowMergeFormCommandBase.ExecuteCore;
begin
end;

procedure TdxShowMergeFormCommandBase.GetDefaultExt(AExporters: TdxExporterList;
  out AExtension: string; out AIndex: Integer);
var
  I: Integer;
  AExporter: IdxExporter;
  AFilter: TdxFileDialogFilter;
begin
  AIndex := -1;
  AFilter := AExporters.First.Filter;
  for I := 0 to AExporters.Count - 1 do
  begin
    AExporter := AExporters[I];
    if AExporter.Format = DocumentModel.DocumentSaveOptions.DefaultFormat then
    begin
      AFilter := AExporter.Filter;
      AIndex := I;
      Break;
    end;
  end;
  if AFilter.Extensions.Count > 0 then
    AExtension := AFilter.Extensions[0]
  else
    AExtension := '';
end;

function TdxShowMergeFormCommandBase.GetDocumentFormat(const AFileName: string;
  AExporters: TdxExporterList): TdxRichEditDocumentFormat;
var
  I: Integer;
  AExtension: string;
  AExporter: IdxExporter;
  AExtensions: TStrings;
begin
  AExtension := LowerCase(TdxStringHelper.TrimStart(TPath.GetExtension(AFileName), ['.']));
  if AExtension = '' then
    Exit(TdxRichEditDocumentFormat.Rtf);
  for I := 0 to AExporters.Count - 1 do
  begin
    AExporter := AExporters[I];
    AExtensions := AExporter.Filter.Extensions;
    if AExtensions.IndexOf(AExtension) >= 0 then
      Exit(AExporter.Format);
  end;
  Result := TdxRichEditDocumentFormat.Rtf;
end;

procedure TdxShowMergeFormCommandBase.MailMergeToNewControl(const AOptions: IdxRichEditMailMergeOptions);
var
  AArgs: TdxMailMergeGetTargetDocumentEventArgs;
begin
  if RichEditControl.DocumentModel.MailMergeGetTargetDocument.Empty then
    Exit;
  AArgs := TdxMailMergeGetTargetDocumentEventArgs.Create;
  try
    RichEditControl.DocumentModel.RaiseMailMergeGetTargetDocument(AArgs);
    if AArgs.TargetDocument <> nil then
      RichEditControl.Document.MailMerge(AOptions, AArgs.TargetDocument);
  finally
    AArgs.Free
  end;
end;

procedure TdxShowMergeFormCommandBase.MailMergeToNewFile(const AOptions: IdxRichEditMailMergeOptions);
var
  AExportManagerService: IdxExportManagerService;
  AExporters: TdxExporterList;
  AFileSaveDialog: TSaveDialog;
  AFileName: string;
  ADocumentFormat: TdxRichEditDocumentFormat;
  AExtension: string;
  AIndex: Integer;
begin
  AExportManagerService := RichEditControl.InnerControl.DocumentModel.GetService<IdxDocumentExportManagerService>;
  AExporters := AExportManagerService.GetExporters;
  try
    AFileSaveDialog := TSaveDialog.Create(nil);
    try
      AFileSaveDialog.Filter := CreateExportFilters(AExporters);
      GetDefaultExt(AExporters, AExtension, AIndex);
      AFileSaveDialog.DefaultExt := AExtension;
      AFileSaveDialog.FilterIndex := AIndex + 1;
      AFileSaveDialog.Options := AFileSaveDialog.Options + [TOpenOption.ofOverwritePrompt,
        TOpenOption.ofFileMustExist, TOpenOption.ofPathMustExist];
      if not AFileSaveDialog.Execute then
        Exit;
      AFileName := AFileSaveDialog.FileName;
      ADocumentFormat := GetDocumentFormat(AFileName, AExporters);
      RichEditControl.Document.MailMerge(AOptions, AFileName, ADocumentFormat);
    finally
      AFileSaveDialog.Free;
    end;
  finally
    AExporters.Free;
  end;
end;

procedure TdxShowMergeFormCommandBase.MergeToNewDocument(const AMergeRecordsParameters: TdxMergeRecordsParameters);
var
  AOptions: IdxRichEditMailMergeOptions;
begin
  AOptions := RichEditControl.Document.CreateMailMergeOptions;
  CalculateMailMergeOptions(AMergeRecordsParameters, AOptions);
  AOptions.MergeMode := TdxRichEditMergeMode.NewSection;
  AOptions.MergeRecords := AMergeRecordsParameters.MergeRecords;
  if AMergeRecordsParameters.MergeDestination = TdxMergeDestination.NewTab then
    MailMergeToNewControl(AOptions)
  else
    MailMergeToNewFile(AOptions);
end;

{ TdxShowMergeDatabaseRecordsFormCommand }

procedure TdxShowMergeDatabaseRecordsFormCommand.CalculateMailMergeOptions(
  const AMergeRecordsParameters: TdxMergeRecordsParameters; const AOptions: IdxRichEditMailMergeOptions);
begin
  AOptions.DataSource := RichEditControl.InnerControl.Options.MailMerge.DataSource;
end;

procedure TdxShowMergeDatabaseRecordsFormCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AMergeRecordsParameters: TdxMergeRecordsParameters;
begin
  AMergeRecordsParameters.MergeRecords := TdxMergeRecords.All;
  AMergeRecordsParameters.MergeDestination := TdxMergeDestination.NewTab;
  RichEditControl.ShowMergeDatabaseRecordsForm(AMergeRecordsParameters, ShowMergeDatabaseRecordsFormCallBack);
end;

class function TdxShowMergeDatabaseRecordsFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowMergeDatabaseRecordsFormDescription);
end;

class function TdxShowMergeDatabaseRecordsFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowMergeDatabaseRecordsFormMenuCaption);
end;

class function TdxShowMergeDatabaseRecordsFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowMergeDatabaseRecordsForm;
end;

class function TdxShowMergeDatabaseRecordsFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.MailMerge;
end;

procedure TdxShowMergeDatabaseRecordsFormCommand.ShowMergeDatabaseRecordsFormCallBack(
  const AMergeRecordsParameters: TdxMergeRecordsParameters; AData: TObject);
begin
  MergeToNewDocument(AMergeRecordsParameters);
end;

procedure TdxShowMergeDatabaseRecordsFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ADataSource: TDataSource;
begin
  ADataSource := RichEditControl.InnerControl.Options.MailMerge.DataSource;
  AState.Enabled := (ADataSource <> nil) and (ADataSource.DataSet <> nil) and not ADataSource.DataSet.IsEmpty;
end;

{ TdxShowTOCFormCommand }

procedure TdxShowTOCFormCommand.ExecuteCore;
begin
  RichEditControl.ShowTOCForm(FField);
end;

procedure TdxShowTOCFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ATocField: TdxTocField;
begin
  inherited UpdateUIStateCore(AState);

  FField := TdxFieldController.FindFieldBySelection<TdxTocField>(DocumentModel.Selection, ATocField);
  ATocField.Free;
  AState.Enabled := (FField <> nil) and IsContentEditable;
  AState.Visible := FField <> nil;
end;

function TdxShowTOCFormCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := False;
end;

function TdxShowTOCFormCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxShowTOCFormCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

class function TdxShowTOCFormCommand.GetDescription: string;
begin
  Result := '';
end;

function TdxShowTOCFormCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxShowTOCFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditTOCMenuCaption);
end;

function TdxShowTOCFormCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxShowTOCFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowTOCForm;
end;

function TdxShowTOCFormCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

{ TdxShowTableStyleFormCommand }

constructor TdxShowTableStyleFormCommand.Create(const AControl: IdxRichEditControl; AStyle: TdxTableStyle);
begin
  inherited Create(AControl);
  FStyle := AStyle;
end;

class function TdxShowTableStyleFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowEditStyleFormDescription);
end;


class function TdxShowTableStyleFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowEditStyleFormMenuCaption);
end;

function TdxShowTableStyleFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

class function TdxShowTableStyleFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowEditStyleForm;
end;

procedure TdxShowTableStyleFormCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    if Style <> nil then
      ShowTableStyleForm(Style)
    else
      FindStyleAndShowForm;
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

procedure TdxShowTableStyleFormCommand.FindStyleAndShowForm;
var
  AModel: TdxDocumentModel;
  AStartIndex, AEndIndex, I: TdxRunIndex;
  AFirstRun, ARun: TdxTextRunBase;
  AIndex: TdxParagraphIndex;
  ASourceCell: TdxTableCell;
  ATargetTable: TdxTable;
  AOnlyOneTableStyle: Boolean;
begin
  AModel := RichEditControl.InnerControl.DocumentModel;
  AStartIndex := AModel.Selection.Interval.NormalizedStart.RunIndex;
  AEndIndex := AModel.Selection.Interval.NormalizedEnd.RunIndex;
  AFirstRun := AModel.ActivePieceTable.Runs[AStartIndex];
  AIndex := AFirstRun.Paragraph.Index;
  ASourceCell := TdxTableCell(AFirstRun.PieceTable.Paragraphs[AIndex].GetCellCore);
  if ASourceCell = nil then
    Exit;
  ATargetTable := ASourceCell.Table;
  AOnlyOneTableStyle := True;

  for I := AStartIndex to AEndIndex do
  begin
    ARun := AModel.ActivePieceTable.Runs[I];
    if TdxTableCell(ARun.PieceTable.Paragraphs[AIndex].GetCellCore).Table.TableStyle <> ATargetTable.TableStyle then
    begin
      AOnlyOneTableStyle := False;
      Continue;
    end;
  end;

  if AOnlyOneTableStyle and (ASourceCell <> nil) then
    ShowTableStyleForm(ATargetTable.TableStyle);
end;

procedure TdxShowTableStyleFormCommand.ShowTableStyleForm(AStyle: TdxTableStyle);
begin
  RichEditControl.ShowTableStyleForm(AStyle);
end;

procedure TdxShowTableStyleFormCommand.ExecuteCore;
begin
end;

procedure TdxShowTableStyleFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.TableStyle);
end;

{ TdxUnprotectDocumentCommand }

procedure TdxUnprotectDocumentCommand.ExecuteCore;
begin
  CheckExecutedAtUIThread;

  if DocumentModel.CheckDocumentProtectionPassword('') then
    DocumentModel.RemoveDocumentProtection('')
  else
  begin
    RichEditControl.ShowDocumentProtectionQueryPasswordForm('', ShowPasswordFormCallback);
  end;
end;

class function TdxUnprotectDocumentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUnprotectDocumentDescription);
end;

class function TdxUnprotectDocumentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.UnprotectDocument;
end;

class function TdxUnprotectDocumentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUnprotectDocumentMenuCaption);
end;

class function TdxUnprotectDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.UnprotectDocument;
end;

procedure TdxUnprotectDocumentCommand.ShowPasswordFormCallback(const APassword: string);
begin
  if not DocumentModel.RemoveDocumentProtection(APassword) then
    RichEditControl.ShowWarningMessage(cxGetResourceString(@sdxRichEditExceptionDocumentProtectionInvalidPassword));
end;

procedure TdxUnprotectDocumentCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := DocumentModel.ProtectionProperties.EnforceProtection;
  ApplyCommandRestrictionOnReadOnlyControl(AState);
end;

{ TdxProtectDocumentCommand }

procedure TdxProtectDocumentCommand.ExecuteCore;
begin
  CheckExecutedAtUIThread;

  RichEditControl.ShowDocumentProtectionQueryNewPasswordForm('', ShowPasswordFormCallback);
end;

class function TdxProtectDocumentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandProtectDocumentDescription);
end;

class function TdxProtectDocumentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ProtectDocument;
end;

class function TdxProtectDocumentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandProtectDocumentMenuCaption);
end;

class function TdxProtectDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ProtectDocument;
end;

procedure TdxProtectDocumentCommand.ShowPasswordFormCallback(const APassword: string);
begin
  DocumentModel.EnforceDocumentProtection(APassword);
end;

procedure TdxProtectDocumentCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := not DocumentModel.ProtectionProperties.EnforceProtection;

  ApplyCommandRestrictionOnReadOnlyControl(AState);
end;

{ TdxShowRangeEditingPermissionsFormCommand }

class function TdxShowRangeEditingPermissionsFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowRangeEditingPermissionsFormDescription);
end;

class function TdxShowRangeEditingPermissionsFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.EditRangePermission;
end;

class function TdxShowRangeEditingPermissionsFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowRangeEditingPermissionsFormMenuCaption);
end;

function TdxShowRangeEditingPermissionsFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

class function TdxShowRangeEditingPermissionsFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowRangeEditingPermissionsForm;
end;

procedure TdxShowRangeEditingPermissionsFormCommand.ExecuteCore;
begin
  RichEditControl.ShowRangeEditingPermissionsForm;
end;

procedure TdxShowRangeEditingPermissionsFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := not DocumentModel.ProtectionProperties.EnforceProtection;

  ApplyCommandRestrictionOnReadOnlyControl(AState);
end;

{ TdxEncryptDocumentCommand }

procedure TdxEncryptDocumentCommand.ExecuteCore;
begin
  CheckExecutedAtUIThread;

  RichEditControl.ShowDocumentEncryptQueryNewPasswordForm(DocumentModel.EncryptionProperties.Password,
    ShowPasswordFormCallback);
end;

class function TdxEncryptDocumentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEncryptDocumentDescription);
end;

class function TdxEncryptDocumentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.EncryptDocument;
end;

class function TdxEncryptDocumentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEncryptDocumentMenuCaption);
end;

function TdxEncryptDocumentCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

class function TdxEncryptDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.EncryptDocument;
end;

procedure TdxEncryptDocumentCommand.ShowPasswordFormCallback(const APassword: string);
begin
  DocumentModel.EncryptionProperties.Password := APassword;
end;

procedure TdxEncryptDocumentCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;

end;

{ TdxProcessPaperSizeGalleryCommand }

procedure TdxProcessPaperSizeGalleryCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  // do nothing
end;

class function TdxProcessPaperSizeGalleryCommand.GetDescription: string;
begin
  Result := '';
end;

class function TdxProcessPaperSizeGalleryCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.PaperSize;
end;

class function TdxProcessPaperSizeGalleryCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPaperSizeGalleryCaption);
end;

function TdxProcessPaperSizeGalleryCommand.GetPaperKind: TdxPaperKind;
var
  AState: IdxCommandUIState;
begin
  Result := TdxPaperKind.Custom;
  FPageSetupInfo := nil;
  AState := CreateDefaultCommandUIState;
  UpdateUIState(AState);
  if AState.Visible and AState.Enabled then
  begin
    CheckExecutedAtUIThread;
    if ValidateUIState(AState) then
    begin
      FPageSetupInfo := IdxValueBasedCommandUIState<TdxPageSetupInfo>(AState);
      if (FPageSetupInfo.Value <> nil) and FPageSetupInfo.Value.PaperKind.HasValue then
        Result := FPageSetupInfo.Value.PaperKind;
    end;
  end;
end;

procedure TdxProcessPaperSizeGalleryCommand.SetPaperKind(const APaperKind: TdxPaperKind);
var
  ASize: TSize;
  ATemp: Integer;
  APageSetupInfo: TdxPageSetupInfo;
begin
  NotifyBeginCommandExecution(FPageSetupInfo);
  try
    if (FPageSetupInfo <> nil) and (FPageSetupInfo.Value <> nil) then
    begin
      APageSetupInfo := FPageSetupInfo.Value;
      APageSetupInfo.PaperKind := APaperKind;
      if APaperKind <> TdxPaperKind.Custom then
      begin
        ASize := DocumentModel.UnitConverter.TwipsToModelUnits(TdxPaperSizeCalculator.CalculatePaperSize(APaperKind));
        if APageSetupInfo.Landscape.HasValue and APageSetupInfo.Landscape then
        begin
          ATemp := ASize.Width;
          ASize.Width := ASize.Height;
          ASize.Height := ATemp;
        end;
        APageSetupInfo.PaperWidth := ASize.Width;
        APageSetupInfo.PaperHeight := ASize.Height;
      end;
      ModifyDocumentModel(FPageSetupInfo);
    end;
  finally
    NotifyEndCommandExecution(FPageSetupInfo);
  end;
end;

{ TdxProcessPageMarginsGalleryCommand }

procedure TdxProcessPageMarginsGalleryCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  // do nothing
end;

class function TdxProcessPageMarginsGalleryCommand.GetDescription: string;
begin
  Result := '';
end;

class function TdxProcessPageMarginsGalleryCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Margins;
end;

class function TdxProcessPageMarginsGalleryCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPageMarginsGalleryCaption);
end;

procedure TdxProcessPageMarginsGalleryCommand.GetPageMargins(out ATop, ABottom, ALeft, ARight: Single);
var
  APageSetupInfo: TdxPageSetupInfo;
  AState: IdxCommandUIState;
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  ATop := 0;
  ABottom := 0;
  ALeft := 0;
  ARight := 0;
  FPageSetupInfo := nil;
  AState := CreateDefaultCommandUIState;
  UpdateUIState(AState);
  if AState.Visible and AState.Enabled then
  begin
    CheckExecutedAtUIThread;
    if ValidateUIState(AState) then
    begin
      FPageSetupInfo := IdxValueBasedCommandUIState<TdxPageSetupInfo>(AState);
      if FPageSetupInfo.Value <> nil then
      begin
        APageSetupInfo := FPageSetupInfo.Value;
        AUnitConverter := DocumentModel.UnitConverter;
        if APageSetupInfo.TopMargin.HasValue then
          ATop := SimpleRoundTo(AUnitConverter.ModelUnitsToInchesF(APageSetupInfo.TopMargin));
        if APageSetupInfo.BottomMargin.HasValue then
          ABottom := SimpleRoundTo(AUnitConverter.ModelUnitsToInchesF(APageSetupInfo.BottomMargin));
        if APageSetupInfo.LeftMargin.HasValue then
          ALeft := SimpleRoundTo(AUnitConverter.ModelUnitsToInchesF(APageSetupInfo.LeftMargin));
        if APageSetupInfo.RightMargin.HasValue then
          ARight := SimpleRoundTo(AUnitConverter.ModelUnitsToInchesF(APageSetupInfo.RightMargin));
        if APageSetupInfo.Landscape.HasValue and APageSetupInfo.Landscape then
          ConvertToPortraitMargins(ATop, ABottom, ALeft, ARight);
      end;
    end;
  end;
end;

procedure TdxProcessPageMarginsGalleryCommand.SetPageMargins(ATop, ABottom, ALeft, ARight: Single);
var
  APageSetupInfo: TdxPageSetupInfo;
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  NotifyBeginCommandExecution(FPageSetupInfo);
  try
    if (FPageSetupInfo <> nil) and (FPageSetupInfo.Value <> nil) then
    begin
      APageSetupInfo := FPageSetupInfo.Value;
      AUnitConverter := DocumentModel.UnitConverter;
      if APageSetupInfo.Landscape.HasValue and APageSetupInfo.Landscape then
        ConvertToLandscapeMargins(ATop, ABottom, ALeft, ARight);
      APageSetupInfo.TopMargin := Trunc(AUnitConverter.InchesToModelUnitsF(ATop));
      APageSetupInfo.BottomMargin := Trunc(AUnitConverter.InchesToModelUnitsF(ABottom));
      APageSetupInfo.LeftMargin := Trunc(AUnitConverter.InchesToModelUnitsF(ALeft));
      APageSetupInfo.RightMargin := Trunc(AUnitConverter.InchesToModelUnitsF(ARight));
      ModifyDocumentModel(FPageSetupInfo);
    end;
  finally
    NotifyEndCommandExecution(FPageSetupInfo);
  end;
end;

procedure TdxProcessPageMarginsGalleryCommand.ConvertToLandscapeMargins(var ATop, ABottom, ALeft, ARight: Single);
var
  ATempTop, ATempBottom, ATempLeft, ATempRight: Single;
begin
  ATempTop := ATop;
  ATempBottom := ABottom;
  ATempLeft := ALeft;
  ATempRight := ARight;

  ATop := ATempRight;
  ABottom := ATempLeft;
  ALeft := ATempTop;
  ARight := ATempBottom;
end;

procedure TdxProcessPageMarginsGalleryCommand.ConvertToPortraitMargins(var ATop, ABottom, ALeft, ARight: Single);
var
  ATempTop, ATempBottom, ATempLeft, ATempRight: Single;
begin
  ATempTop := ATop;
  ATempBottom := ABottom;
  ATempLeft := ALeft;
  ATempRight := ARight;

  ATop := ATempLeft;
  ABottom := ATempRight;
  ALeft := ATempBottom;
  ARight := ATempTop;
end;

end.
