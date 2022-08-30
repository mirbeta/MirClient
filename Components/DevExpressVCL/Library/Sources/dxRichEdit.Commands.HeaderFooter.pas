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

unit dxRichEdit.Commands.HeaderFooter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Graphics, Classes, Controls, Generics.Defaults, Generics.Collections, ImgList,
  dxCore, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Types,
  dxRichEdit.Options,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.FastComparer,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.MultiCommand;

type

  IdxPieceTableProvider = interface
  ['{F4689300-112D-4C44-89FF-754C11C7000F}']
    function GetPieceTable: TdxPieceTable;
    function GetSection: TdxSection;
    function GetPreferredPageIndex: Integer;

    property PieceTable: TdxPieceTable read GetPieceTable;
    property Section: TdxSection read GetSection;
    property PreferredPageIndex: Integer read GetPreferredPageIndex;
  end;

  { TdxExplicitPieceTableProvider }

  TdxExplicitPieceTableProvider = class(TInterfacedObject, IdxPieceTableProvider)
  strict private
    FPieceTable: TdxPieceTable;
    FSection: TdxSection;
    FPreferredPageIndex: Integer;
    function GetPieceTable: TdxPieceTable;
    function GetSection: TdxSection;
    function GetPreferredPageIndex: Integer;
  public
    constructor Create(APieceTable: TdxPieceTable; ASection: TdxSection; APreferredPageIndex: Integer);

    property PieceTable: TdxPieceTable read FPieceTable;
    property Section: TdxSection read FSection;
    property PreferredPageIndex: Integer read FPreferredPageIndex;
  end;

  { TdxFakePieceTableProvider }

  TdxFakePieceTableProvider = class(TInterfacedObject, IdxPieceTableProvider)
  private
    FGetPieceTable: TdxFunc<TdxPieceTable>;
    FGetSection: TdxFunc<TdxSection>;
    FGetPreferredPageIndex: TdxFunc<Integer>;
  protected
    constructor Create(const AGetPieceTable: TdxFunc<TdxPieceTable>; AGetSection: TdxFunc<TdxSection>;
      const AGetPreferredPageIndex: TdxFunc<Integer>);

    function GetPieceTable: TdxPieceTable;
    function GetSection: TdxSection;
    function GetPreferredPageIndex: Integer;
  end;

  { TdxMakeHeaderFooterActiveCommand }

  TdxMakeHeaderFooterActiveCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FPieceTableProvider: IdxPieceTableProvider;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; const APieceTableProvider: IdxPieceTableProvider); reintroduce; virtual;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxHeaderFooterRelatedMultiCommandBase }

  TdxHeaderFooterRelatedMultiCommandBase = class abstract(TdxTransactedMultiCommand)
  protected
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
    function GetCanExecuteOnlyInMainPieceTable: Boolean; virtual;
    procedure CreateCommands; override;
    function UpdateLayoutPositionToPageArea: Boolean; virtual;
    procedure GoToExistingHeaderFooter(const AWhere: IdxPieceTableProvider); overload; virtual;
    procedure GoToExistingHeaderFooter(AHeaderFooter: TdxSectionHeaderFooterBase; APreferredPageIndex: Integer; ASection: TdxSection); overload; virtual;
    function IsFirstSectionPage(ALayoutPosition: TdxDocumentLayoutPosition): Boolean;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property CanExecuteOnlyInMainPieceTable: Boolean read GetCanExecuteOnlyInMainPieceTable;
  public
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
  end;

  { TdxInsertPageHeaderFooterCoreCommandBase }

  TdxInsertPageHeaderFooterCoreCommandBase = class abstract(TdxInsertObjectCommandBase)
  protected
    function GetPieceTableProvider: IdxPieceTableProvider; virtual; abstract;
    function ModifyModelCore(ASection: TdxSection): TdxSectionHeaderFooterBase; virtual; abstract;
  end;

  { TdxInsertPageHeaderFooterCoreCommand }

  TdxInsertPageHeaderFooterCoreCommand<T: TdxSectionHeaderFooterBase> = class abstract (
    TdxInsertPageHeaderFooterCoreCommandBase)
  strict private
    FType: TdxHeaderFooterType;
    FInsertedHeaderFooter: TdxSectionHeaderFooterBase;
    FSection: TdxSection;
    function GetPieceTable: TdxPieceTable;
    function GetPreferredPageIndex: Integer;
    function GetSection: TdxSection;
  protected
    procedure ModifyModel; override;
    function ModifyModelCore(ASection: TdxSection): TdxSectionHeaderFooterBase; override;
    function GetHeaderFooterContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; virtual; abstract;
    function CreateHeaderFooter(ASection: TdxSection): TdxSectionHeaderFooterBase; virtual;
    function GetPieceTableProvider: IdxPieceTableProvider; override;

    property PieceTable: TdxPieceTable read GetPieceTable;
    property Section: TdxSection read FSection;
    property PreferredPageIndex: Integer read GetPreferredPageIndex;
  public
    constructor Create(const AControl: IdxRichEditControl; AType: TdxHeaderFooterType); reintroduce; virtual;

    property &Type: TdxHeaderFooterType read FType;
  end;

  { TdxEditPageHeaderFooterCommand }

  TdxEditPageHeaderFooterCommand<T: TdxSectionHeaderFooterBase > = class abstract(TdxHeaderFooterRelatedMultiCommandBase)
  strict private
    FForceCreateNewHeader: Boolean;
  protected
    function GetCanExecuteOnlyInMainPieceTable: Boolean; override;
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    function CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommand<T>; virtual; abstract;
    function GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; virtual; abstract;
  public
    property ForceCreateNewHeader: Boolean read FForceCreateNewHeader write FForceCreateNewHeader;
  end;

  { TdxInsertPageHeaderCoreCommand }

  TdxInsertPageHeaderCoreCommand = class(TdxInsertPageHeaderFooterCoreCommand<TdxSectionHeader>)
  protected
    function GetHeaderFooterContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxEditPageHeaderCommand }

  TdxEditPageHeaderCommand = class(TdxEditPageHeaderFooterCommand<TdxSectionHeader>)
  protected
    function CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommand<TdxSectionHeader>; override;
    function GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertPageFooterCoreCommand }

  TdxInsertPageFooterCoreCommand = class(TdxInsertPageHeaderFooterCoreCommand<TdxSectionFooter>)
  protected
    function GetHeaderFooterContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxEditPageFooterCommand }

  TdxEditPageFooterCommand = class(TdxEditPageHeaderFooterCommand<TdxSectionFooter>)
  protected
    function CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommand<TdxSectionFooter>; override;
    function GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxNextHeaderFooterInfo }

  TdxNextHeaderFooterInfo = class
  strict private
    FHeaderFooter: TdxSectionHeaderFooterBase;
    FPage: TdxPage;
    FSection: TdxSection;
    FType: TdxHeaderFooterType;
    function GetCanGoOrCreate: Boolean;
    function GetCanGo: Boolean;
  public
    constructor Create(AHeaderFooter: TdxSectionHeaderFooterBase; APage: TdxPage; ASection: TdxSection; AType: TdxHeaderFooterType);
    class function CreateNoJump: TdxNextHeaderFooterInfo; static;
    class function CreateJumpToExisting(AHeaderFooter: TdxSectionHeaderFooterBase; APage: TdxPage; ASection: TdxSection): TdxNextHeaderFooterInfo; static;
    class function CreateJumpToNonExisting(APage: TdxPage; ASection: TdxSection; AType: TdxHeaderFooterType): TdxNextHeaderFooterInfo; static;

    property HeaderFooter: TdxSectionHeaderFooterBase read FHeaderFooter;
    property Page: TdxPage read FPage;
    property Section: TdxSection read FSection;
    property &Type: TdxHeaderFooterType read FType;
    property CanGoOrCreate: Boolean read GetCanGoOrCreate;
    property CanGo: Boolean read GetCanGo;
  end;

  { TdxPrevNextHeaderFooterInfoCalculatorBase }

  TdxPrevNextHeaderFooterInfoCalculatorBase = class abstract(TdxRichEditCaretBasedCommand)
  protected
    function GetNextHeaderFooterInfo: TdxNextHeaderFooterInfo; virtual; abstract;
  public
    function CreateNewHeaderFooter(AInfo: TdxNextHeaderFooterInfo): TdxSectionHeaderFooterBase; virtual; abstract;

    property NextHeaderFooterInfo: TdxNextHeaderFooterInfo read GetNextHeaderFooterInfo;
  end;

  { TdxPrevNextHeaderFooterInfoCalculator }

  TdxPrevNextHeaderFooterInfoCalculator<T: TdxSectionHeaderFooterBase> = class abstract(TdxPrevNextHeaderFooterInfoCalculatorBase)
  strict private
    FNextHeaderFooterInfo: TdxNextHeaderFooterInfo;
  protected
    function GetNextHeaderFooterInfo: TdxNextHeaderFooterInfo; override;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function CalculateTargetHeaderFooterInfo(ACurrentSectionHeaderFooter: T; ACurrentPage: TdxPage): TdxNextHeaderFooterInfo; virtual;
    function CreateSearchOrder(ASection: TdxSection): TdxOrdinalList<TdxHeaderFooterType>; virtual;
    function CalculateNextPageHeaderFooterInfo(ACurrentSectionHeaderFooter: T; ACurrentPage: TdxPage; AType: TdxHeaderFooterType): TdxNextHeaderFooterInfo; virtual;
    function GetPageHeaderFooterInfo(ANextPage: TdxPage): TdxNextHeaderFooterInfo; virtual;
    function ShouldGoToNextSection(ACurrentOrder: Integer; AOrderCount: Integer): Boolean; virtual; abstract;
    function ObtainNextPage(ACurrentPage: TdxPage): TdxPage; virtual; abstract;
    function GetSection(APage: TdxPage): TdxSection; virtual; abstract;
    function GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea; virtual; abstract;
    function CalculateNextSectionHeaderFooterInfo(ACurrentSectionHeaderFooter: T; ACurrentPage: TdxPage): TdxNextHeaderFooterInfo; virtual; abstract;
    function CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase; virtual; abstract;
    function CalculateSectionNewHeaderFooterType(APage: TdxPage; ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>): TdxHeaderFooterType; virtual; abstract;
    function GetNextHeaderFooterType(ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>; ACurrentIndex: Integer): TdxHeaderFooterType; virtual; abstract;
  public
    destructor Destroy; override;
    function CalculateNextPageHeaderFooterInfoForHeaderFooter(AExpectedHeaderFooter: T; ACurrentPage: TdxPage): TdxNextHeaderFooterInfo;
    function CreateNewHeaderFooter(AInfo: TdxNextHeaderFooterInfo): TdxSectionHeaderFooterBase; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxPrevHeaderFooterInfoCalculator }

  TdxPrevHeaderFooterInfoCalculator<T: TdxSectionHeaderFooterBase> = class abstract(TdxPrevNextHeaderFooterInfoCalculator<T>)
  protected
    function ShouldGoToNextSection(ACurrentOrder: Integer; AOrderCount: Integer): Boolean; override;
    function ObtainNextPage(ACurrentPage: TdxPage): TdxPage; override;
    function CalculateNextSectionHeaderFooterInfo(ACurrentSectionHeaderFooter: T; ACurrentPage: TdxPage): TdxNextHeaderFooterInfo; override;
    function GetNextHeaderFooterType(ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>; ACurrentIndex: Integer): TdxHeaderFooterType; override;
    function CalculateSectionNewHeaderFooterType(APage: TdxPage; ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>): TdxHeaderFooterType; override;
  end;

  { TdxPrevHeaderInfoCalculator }

  TdxPrevHeaderInfoCalculator = class(TdxPrevHeaderFooterInfoCalculator<TdxSectionHeader>)
  protected
    function GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea; override;
    function GetSection(APage: TdxPage): TdxSection; override;
    function CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase; override;
  end;

  { TdxPrevFooterInfoCalculator }

  TdxPrevFooterInfoCalculator = class(TdxPrevHeaderFooterInfoCalculator<TdxSectionFooter>)
  protected
    function GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea; override;
    function GetSection(APage: TdxPage): TdxSection; override;
    function CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase; override;
  end;

  { TdxNextHeaderFooterInfoCalculator }

  TdxNextHeaderFooterInfoCalculator<T: TdxSectionHeaderFooterBase> = class abstract(TdxPrevNextHeaderFooterInfoCalculator<T>)
  protected
    function ShouldGoToNextSection(ACurrentOrder: Integer; AOrderCount: Integer): Boolean; override;
    function ObtainNextPage(ACurrentPage: TdxPage): TdxPage; override;
    function CalculateNextSectionHeaderFooterInfo(ACurrentSectionHeaderFooter: T; ACurrentPage: TdxPage): TdxNextHeaderFooterInfo; override;
    function GetNextHeaderFooterType(ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>; ACurrentIndex: Integer): TdxHeaderFooterType; override;
    function CalculateSectionNewHeaderFooterType(APage: TdxPage; ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>): TdxHeaderFooterType; override;
  end;

  { TdxNextHeaderInfoCalculator }

  TdxNextHeaderInfoCalculator = class(TdxNextHeaderFooterInfoCalculator<TdxSectionHeader>)
  protected
    function GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea; override;
    function GetSection(APage: TdxPage): TdxSection; override;
    function CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase; override;
  end;

  { TdxNextFooterInfoCalculator }

  TdxNextFooterInfoCalculator = class(TdxNextHeaderFooterInfoCalculator<TdxSectionFooter>)
  protected
    function GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea; override;
    function GetSection(APage: TdxPage): TdxSection; override;
    function CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase; override;
  end;

  { TdxMakeNearestHeaderFooterActiveCommand }

  TdxMakeNearestHeaderFooterActiveCommand<T: TdxSectionHeaderFooterBase> = class abstract(TdxRichEditCaretBasedCommand)
  strict private
    FTargetTable: T;
  protected
    procedure ExecuteCore; override;
    procedure ChangeActivePieceTable(ALayoutPosition: TdxDocumentLayoutPosition); virtual;
    function TryMakePrevHeaderFooterActive(ACurrentPage: TdxPage): Boolean; virtual;
    procedure MakeNextHeaderFooterActive(ACurrentPage: TdxPage); virtual;
    function CreatePrevHeaderFooterCalculator: TdxPrevHeaderFooterInfoCalculator<T>; virtual; abstract;
    function CreateNextHeaderFooterCalculator: TdxNextHeaderFooterInfoCalculator<T>; virtual; abstract;
    function GetPageHeaderFooter(APage: TdxPage): TdxPieceTable; virtual; abstract;
    procedure MakeHeaderFooterActive(APieceTable: TdxPieceTable; ASection: TdxSection; APageIndex: Integer); virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property TargetTable: T read FTargetTable;
  public
    constructor Create(const AControl: IdxRichEditControl; ATargetTable: T); reintroduce;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxMakeNearestHeaderActiveCommand }

  TdxMakeNearestHeaderActiveCommand = class(TdxMakeNearestHeaderFooterActiveCommand<TdxSectionHeader>)
  protected
    function CreatePrevHeaderFooterCalculator: TdxPrevHeaderFooterInfoCalculator<TdxSectionHeader>; override;
    function CreateNextHeaderFooterCalculator: TdxNextHeaderFooterInfoCalculator<TdxSectionHeader>; override;
    function GetPageHeaderFooter(APage: TdxPage): TdxPieceTable; override;
  end;

  { TdxMakeNearestFooterActiveCommand }

  TdxMakeNearestFooterActiveCommand = class(TdxMakeNearestHeaderFooterActiveCommand<TdxSectionFooter>)
  protected
    function CreatePrevHeaderFooterCalculator: TdxPrevHeaderFooterInfoCalculator<TdxSectionFooter>; override;
    function CreateNextHeaderFooterCalculator: TdxNextHeaderFooterInfoCalculator<TdxSectionFooter>; override;
    function GetPageHeaderFooter(APage: TdxPage): TdxPieceTable; override;
  end;

  { TdxClosePageHeaderFooterCommand }

  TdxClosePageHeaderFooterCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxGoToPageHeaderFooterCommand }

  TdxGoToPageHeaderFooterCommand<T: TdxSectionHeaderFooterBase> = class abstract(TdxTransactedInsertObjectCommand)
  protected
    function GetInsertObjectCommand: TdxRichEditCommand; override;
    procedure CreateCommands; override;
    function CanGoFromCurrentSelection: Boolean; virtual;
    function GetCorrespondingHeaderFooter: T; virtual; abstract;
  public
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
  end;

  { TdxGoToPageHeaderCommand }

  TdxGoToPageHeaderCommand = class(TdxGoToPageHeaderFooterCommand<TdxSectionHeader>)
  protected
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
    function GetCorrespondingHeaderFooter: TdxSectionHeader; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxGoToPageFooterCommand }

  TdxGoToPageFooterCommand = class(TdxGoToPageHeaderFooterCommand<TdxSectionFooter>)
  protected
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
    function GetCorrespondingHeaderFooter: TdxSectionFooter; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleHeaderFooterLinkToPreviousCoreCommand }

  TdxToggleHeaderFooterLinkToPreviousCoreCommand = class(TdxInsertObjectCommandBase)
  strict private
    FNewActiveHeaderFooter: TdxSectionHeaderFooterBase;
    FSection: TdxSection;
    function GetPieceTable: TdxPieceTable;
    function GetSection: TdxSection;
    function GetPreferredPageIndex: Integer;
  protected
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure ModifyModel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetCurrentSectionFromCaretLayoutPosition: TdxSection;
    function GetPieceTableProvider: IdxPieceTableProvider;

    property PieceTable: TdxPieceTable read GetPieceTable;
    property Section: TdxSection read GetSection;
    property PreferredPageIndex: Integer read GetPreferredPageIndex;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleHeaderFooterLinkToPreviousCommand }

  TdxToggleHeaderFooterLinkToPreviousCommand = class(TdxHeaderFooterRelatedMultiCommandBase)
  protected
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxGoToPrevNextPageHeaderFooterCommand }

  TdxGoToPrevNextPageHeaderFooterCommand = class abstract(TdxHeaderFooterRelatedMultiCommandBase)
  protected
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    function CreateCalculateNextHeaderFooterInfoCommand: TdxPrevNextHeaderFooterInfoCalculatorBase; virtual; abstract;
  end;

  { TdxGoToNextPageHeaderFooterCommand }

  TdxGoToNextPageHeaderFooterCommand = class(TdxGoToPrevNextPageHeaderFooterCommand)
  protected
    function CreateCalculateNextHeaderFooterInfoCommand: TdxPrevNextHeaderFooterInfoCalculatorBase; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxGoToPreviousPageHeaderFooterCommand }

  TdxGoToPreviousPageHeaderFooterCommand = class(TdxGoToPrevNextPageHeaderFooterCommand)
  protected
    function CreateCalculateNextHeaderFooterInfoCommand: TdxPrevNextHeaderFooterInfoCalculatorBase; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleHeaderFooterCommandBase }

  TdxToggleHeaderFooterCommandBase = class abstract(TdxHeaderFooterRelatedMultiCommandBase)
  protected
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    procedure ChangeValue(ASection: TdxSection; APreferredPageIndex: Integer); virtual;
    function CreateNewHeaderFooter(AHeaderFooter: TdxSectionHeaderFooterBase; AIsFirstSectionPage: Boolean;
      AIsEvenPage: Boolean; AContainer: TdxSectionHeadersFootersBase): TdxSectionHeaderFooterBase;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetValue(ASection: TdxSection): Boolean; virtual; abstract;
    procedure SetValue(ASection: TdxSection; AValue: Boolean); virtual; abstract;
  end;

  { TdxToggleDifferentFirstPageCommand }

  TdxToggleDifferentFirstPageCommand = class(TdxToggleHeaderFooterCommandBase)
  protected
    function GetValue(ASection: TdxSection): Boolean; override;
    procedure SetValue(ASection: TdxSection; AValue: Boolean); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleDifferentOddAndEvenPagesCommand }

  TdxToggleDifferentOddAndEvenPagesCommand = class(TdxToggleHeaderFooterCommandBase)
  protected
    function GetValue(ASection: TdxSection): Boolean; override;
    procedure SetValue(ASection: TdxSection; AValue: Boolean); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs;

{ TdxExplicitPieceTableProvider }

constructor TdxExplicitPieceTableProvider.Create(APieceTable: TdxPieceTable; ASection: TdxSection;
  APreferredPageIndex: Integer);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
  FSection := ASection;
  FPreferredPageIndex := APreferredPageIndex;
end;

function TdxExplicitPieceTableProvider.GetPieceTable: TdxPieceTable;
begin
  Result := FPieceTable;
end;

function TdxExplicitPieceTableProvider.GetPreferredPageIndex: Integer;
begin
  Result := FPreferredPageIndex;
end;

function TdxExplicitPieceTableProvider.GetSection: TdxSection;
begin
  Result := FSection;
end;

{ TdxFakePieceTableProvider }

constructor TdxFakePieceTableProvider.Create(const AGetPieceTable: TdxFunc<TdxPieceTable>; AGetSection: TdxFunc<TdxSection>;
  const AGetPreferredPageIndex: TdxFunc<Integer>);
begin
  inherited Create;
  FGetPieceTable := AGetPieceTable;
  FGetSection := AGetSection;
  FGetPreferredPageIndex := AGetPreferredPageIndex;
end;

function TdxFakePieceTableProvider.GetPieceTable: TdxPieceTable;
begin
  Result := FGetPieceTable();
end;

function TdxFakePieceTableProvider.GetSection: TdxSection;
begin
  Result := FGetSection();
end;

function TdxFakePieceTableProvider.GetPreferredPageIndex: Integer;
begin
  Result := FGetPreferredPageIndex();
end;

{ TdxMakeHeaderFooterActiveCommand }

constructor TdxMakeHeaderFooterActiveCommand.Create(const AControl: IdxRichEditControl;
  const APieceTableProvider: IdxPieceTableProvider);
begin
  inherited Create(AControl);
  Assert(APieceTableProvider <> nil);
  FPieceTableProvider := APieceTableProvider;
end;

procedure TdxMakeHeaderFooterActiveCommand.ExecuteCore;
var
  APieceTable: TdxPieceTable;
  ACommand: TdxChangeActivePieceTableCommand;
begin
  APieceTable := FPieceTableProvider.PieceTable;
  if APieceTable <> nil then
  begin
    ACommand := TdxChangeActivePieceTableCommand.Create(RichEditControl, APieceTable,
      FPieceTableProvider.Section, FPieceTableProvider.PreferredPageIndex);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

class function TdxMakeHeaderFooterActiveCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

class function TdxMakeHeaderFooterActiveCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

procedure TdxMakeHeaderFooterActiveCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
end;

{ TdxHeaderFooterRelatedMultiCommandBase }

function TdxHeaderFooterRelatedMultiCommandBase.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteAllAvailable;
end;

function TdxHeaderFooterRelatedMultiCommandBase.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAllAvailable;
end;

function TdxHeaderFooterRelatedMultiCommandBase.GetCanExecuteOnlyInMainPieceTable: Boolean;
begin
  Result := False;
end;

procedure TdxHeaderFooterRelatedMultiCommandBase.CreateCommands;
begin
end;

function TdxHeaderFooterRelatedMultiCommandBase.UpdateLayoutPositionToPageArea: Boolean;
var
  ACaretPosition: TdxCaretPosition;
begin
  ACaretPosition := ActiveView.CaretPosition;
  ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.PageArea);
  Result := ACaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.PageArea);
end;

procedure TdxHeaderFooterRelatedMultiCommandBase.GoToExistingHeaderFooter(const AWhere: IdxPieceTableProvider);
var
  ACommand: TdxMakeHeaderFooterActiveCommand;
begin
  ACommand := TdxMakeHeaderFooterActiveCommand.Create(RichEditControl, AWhere);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxHeaderFooterRelatedMultiCommandBase.GoToExistingHeaderFooter(AHeaderFooter: TdxSectionHeaderFooterBase;
  APreferredPageIndex: Integer; ASection: TdxSection);
var
  AProvider: IdxPieceTableProvider;
begin
  AProvider := TdxExplicitPieceTableProvider.Create(TdxPieceTable(AHeaderFooter.PieceTable), ASection, APreferredPageIndex);
  GoToExistingHeaderFooter(AProvider);
end;

function TdxHeaderFooterRelatedMultiCommandBase.IsFirstSectionPage(ALayoutPosition: TdxDocumentLayoutPosition): Boolean;
var
  ACurrentPageIndex: Integer;
  APreviousPage: TdxPage;
begin
  ACurrentPageIndex := ALayoutPosition.Page.PageIndex;
  if ACurrentPageIndex <= 0 then
    Exit(True);

  APreviousPage := ActiveView.DocumentLayout.Pages[ACurrentPageIndex - 1];
  Result := APreviousPage.Areas.First.Section <> ALayoutPosition.PageArea.Section;
end;

procedure TdxHeaderFooterRelatedMultiCommandBase.UpdateUIState(const AState: IdxCommandUIState);
begin
  UpdateUIStateCore(AState);
  UpdateUIStateViaService(AState);
end;

procedure TdxHeaderFooterRelatedMultiCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.HeadersFooters, AState.Enabled);
  ApplyDocumentProtectionToSelectedSections(AState);
  if AState.Enabled then
    AState.Enabled := (ActivePieceTable.IsMain = CanExecuteOnlyInMainPieceTable) and (ActiveViewType = TdxRichEditViewType.PrintLayout);
end;

{ TdxInsertPageHeaderFooterCoreCommand }

constructor TdxInsertPageHeaderFooterCoreCommand<T>.Create(const AControl: IdxRichEditControl; AType: TdxHeaderFooterType);
begin
  inherited Create(AControl);
  FType := AType;
end;

procedure TdxInsertPageHeaderFooterCoreCommand<T>.ModifyModel;
begin
  ModifyModelCore(DocumentModel.GetActiveSectionBySelectionEnd);
end;

function TdxInsertPageHeaderFooterCoreCommand<T>.ModifyModelCore(ASection: TdxSection): TdxSectionHeaderFooterBase;
begin
  FSection := ASection;
  if ASection <> nil then
    FInsertedHeaderFooter := CreateHeaderFooter(ASection);

  Result := FInsertedHeaderFooter;
end;

function TdxInsertPageHeaderFooterCoreCommand<T>.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(FInsertedHeaderFooter.PieceTable);
end;

function TdxInsertPageHeaderFooterCoreCommand<T>.GetPreferredPageIndex: Integer;
begin
  Result := -1;
end;

function TdxInsertPageHeaderFooterCoreCommand<T>.GetSection: TdxSection;
begin
  Result := FSection;
end;

function TdxInsertPageHeaderFooterCoreCommand<T>.CreateHeaderFooter(ASection: TdxSection): TdxSectionHeaderFooterBase;
var
  AContainer: TdxSectionHeadersFootersBase;
  ARelinkNextSection, ARelinkPrevSection: Boolean;
begin
  AContainer := GetHeaderFooterContainer(ASection);

  ARelinkNextSection := AContainer.ShouldRelinkNextSection(&Type);
  ARelinkPrevSection := AContainer.ShouldRelinkPreviousSection(&Type);

  AContainer.Add(&Type);
  if ARelinkPrevSection then
    GetHeaderFooterContainer(ASection.Previous).LinkToNext(&Type);
  if ARelinkNextSection then
    GetHeaderFooterContainer(ASection.Next).LinkToPrevious(&Type);
  Result := AContainer.GetObjectCore(&Type);
end;

function TdxInsertPageHeaderFooterCoreCommand<T>.GetPieceTableProvider: IdxPieceTableProvider;
begin
  Result := TdxFakePieceTableProvider.Create(GetPieceTable, GetSection, GetPreferredPageIndex);
end;

{ TdxEditPageHeaderFooterCommand }

function TdxEditPageHeaderFooterCommand<T>.GetCanExecuteOnlyInMainPieceTable: Boolean;
begin
  Result := True;
end;

procedure TdxEditPageHeaderFooterCommand<T>.ForceExecuteCore(const AState: IdxCommandUIState);
var
  ALayoutPosition: TdxDocumentLayoutPosition;
  ASection: TdxSection;
  AIsFirstSectionPage, AIsEvenPage: Boolean;
  AContainer: TdxSectionHeadersFootersBase;
  AHeaderFooter: TdxSectionHeaderFooterBase;
  AType: TdxHeaderFooterType;
  ACommand: TdxInsertPageHeaderFooterCoreCommand<T>;
begin
  if not UpdateLayoutPositionToPageArea then
    Exit;

  ALayoutPosition := ActiveView.CaretPosition.LayoutPosition;
  ASection := ALayoutPosition.PageArea.Section;
  if not DocumentModel.CanEditSection(ASection) then
    Exit;

  AIsFirstSectionPage := IsFirstSectionPage(ALayoutPosition);
  AIsEvenPage := ALayoutPosition.Page.IsEven;
  AContainer := GetContainer(ASection);

  AHeaderFooter := AContainer.CalculateActualObjectCore(AIsFirstSectionPage, AIsEvenPage);
  if (AHeaderFooter = nil) or (ForceCreateNewHeader) then
  begin
    AType := AContainer.CalculateActualObjectType(AIsFirstSectionPage, AIsEvenPage);
    ACommand := CreateInsertObjectCommand(AType);
    try
      ACommand.Execute;
      GoToExistingHeaderFooter(ACommand.GetPieceTableProvider);
    finally
      ACommand.Free;
    end;
  end
  else
    GoToExistingHeaderFooter(AHeaderFooter, ALayoutPosition.Page.PageIndex, ASection);
end;

{ TdxInsertPageHeaderCoreCommand }

class function TdxInsertPageHeaderCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditPageHeaderDescription);
end;

function TdxInsertPageHeaderCoreCommand.GetHeaderFooterContainer(ASection: TdxSection): TdxSectionHeadersFootersBase;
begin
  Result := ASection.Headers;
end;

class function TdxInsertPageHeaderCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditPageHeaderMenuCaption);
end;

{ TdxEditPageHeaderCommand }

function TdxEditPageHeaderCommand.CreateInsertObjectCommand(
  AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommand<TdxSectionHeader>;
begin
  Result := TdxInsertPageHeaderCoreCommand.Create(RichEditControl, AType);
end;

function TdxEditPageHeaderCommand.GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase;
begin
  Result := ASection.Headers;
end;

class function TdxEditPageHeaderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditPageHeaderDescription);
end;

class function TdxEditPageHeaderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditPageHeaderMenuCaption);
end;

class function TdxEditPageHeaderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.EditPageHeader;
end;

class function TdxEditPageHeaderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Header;
end;

{ TdxInsertPageFooterCoreCommand }

class function TdxInsertPageFooterCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditPageFooterDescription);
end;

function TdxInsertPageFooterCoreCommand.GetHeaderFooterContainer(ASection: TdxSection): TdxSectionHeadersFootersBase;
begin
  Result := ASection.Footers;
end;

class function TdxInsertPageFooterCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditPageFooterMenuCaption);
end;

{ TdxEditPageFooterCommand }

function TdxEditPageFooterCommand.CreateInsertObjectCommand(
  AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommand<TdxSectionFooter>;
begin
  Result := TdxInsertPageFooterCoreCommand.Create(RichEditControl, AType);
end;

function TdxEditPageFooterCommand.GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase;
begin
  Result := ASection.Footers;
end;

class function TdxEditPageFooterCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditPageFooterDescription);
end;

class function TdxEditPageFooterCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEditPageFooterMenuCaption);
end;

class function TdxEditPageFooterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.EditPageFooter;
end;

class function TdxEditPageFooterCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Footer;
end;

{ TdxNextHeaderFooterInfo }

constructor TdxNextHeaderFooterInfo.Create(AHeaderFooter: TdxSectionHeaderFooterBase; APage: TdxPage; ASection: TdxSection; AType: TdxHeaderFooterType);
begin
  inherited Create;
  FHeaderFooter := AHeaderFooter;
  FPage := APage;
  FSection := ASection;
  FType := AType;
end;

function TdxNextHeaderFooterInfo.GetCanGoOrCreate: Boolean;
begin
  Result := (Page <> nil) and (Section <> nil);
end;

function TdxNextHeaderFooterInfo.GetCanGo: Boolean;
begin
  Result := (HeaderFooter <> nil) and (Page <> nil) and (Section <> nil);
end;

class function TdxNextHeaderFooterInfo.CreateNoJump: TdxNextHeaderFooterInfo;
begin
  Result := TdxNextHeaderFooterInfo.Create(nil, nil, nil, TdxHeaderFooterType.Odd);
end;

class function TdxNextHeaderFooterInfo.CreateJumpToExisting(AHeaderFooter: TdxSectionHeaderFooterBase; APage: TdxPage;
  ASection: TdxSection): TdxNextHeaderFooterInfo;
begin
  Result := TdxNextHeaderFooterInfo.Create(AHeaderFooter, APage, ASection, AHeaderFooter.&Type);
end;

class function TdxNextHeaderFooterInfo.CreateJumpToNonExisting(APage: TdxPage; ASection: TdxSection;
  AType: TdxHeaderFooterType): TdxNextHeaderFooterInfo;
begin
  Result := TdxNextHeaderFooterInfo.Create(nil, APage, ASection, AType);
end;

{ TdxPrevNextHeaderFooterInfoCalculator }

destructor TdxPrevNextHeaderFooterInfoCalculator<T>.Destroy;
begin
  FreeAndNil(FNextHeaderFooterInfo);
  inherited Destroy;
end;

class function TdxPrevNextHeaderFooterInfoCalculator<T>.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

class function TdxPrevNextHeaderFooterInfoCalculator<T>.GetDescription: string;
begin
  Result := 'InternalError';
end;

function TdxPrevNextHeaderFooterInfoCalculator<T>.GetNextHeaderFooterInfo: TdxNextHeaderFooterInfo;
begin
  Result := FNextHeaderFooterInfo;
end;

procedure TdxPrevNextHeaderFooterInfoCalculator<T>.ExecuteCore;
begin
  UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.PageArea);
  if not CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
    Exit;
  FNextHeaderFooterInfo.Free;
  FNextHeaderFooterInfo := CalculateTargetHeaderFooterInfo(T(DocumentModel.ActivePieceTable.ContentType),
    CaretPosition.LayoutPosition.Page);
end;

procedure TdxPrevNextHeaderFooterInfoCalculator<T>.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.HeadersFooters, AState.Enabled);
  if AState.Enabled then
    AState.Enabled := ActivePieceTable.IsHeaderFooter and (ActiveViewType = TdxRichEditViewType.PrintLayout);
end;

function TdxPrevNextHeaderFooterInfoCalculator<T>.CalculateTargetHeaderFooterInfo(ACurrentSectionHeaderFooter: T;
  ACurrentPage: TdxPage): TdxNextHeaderFooterInfo;
var
  ACurrentIndex: Integer;
  AType: TdxHeaderFooterType;
  ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>;
begin
  ASearchOrder := CreateSearchOrder(GetSection(ACurrentPage));
  try
    ACurrentIndex := ASearchOrder.IndexOf(ACurrentSectionHeaderFooter.&Type);
    if ShouldGoToNextSection(ACurrentIndex, ASearchOrder.Count) then
      Result := CalculateNextSectionHeaderFooterInfo(ACurrentSectionHeaderFooter, ACurrentPage)
    else
    begin
      AType := GetNextHeaderFooterType(ASearchOrder, ACurrentIndex);
      Result := CalculateNextPageHeaderFooterInfo(ACurrentSectionHeaderFooter, ACurrentPage, AType);
    end;
  finally
    ASearchOrder.Free;
  end;
end;

function TdxPrevNextHeaderFooterInfoCalculator<T>.CreateSearchOrder(ASection: TdxSection): TdxOrdinalList<TdxHeaderFooterType>;
begin
  Result := TdxOrdinalList<TdxHeaderFooterType>.Create;
  if ASection.GeneralSettings.DifferentFirstPage then
  begin
    Result.Add(TdxHeaderFooterType.First);
    if TdxDocumentModel(ASection.DocumentModel).DocumentProperties.DifferentOddAndEvenPages then
    begin
      Result.Add(TdxHeaderFooterType.Even);
      Result.Add(TdxHeaderFooterType.Odd);
    end
    else
      Result.Add(TdxHeaderFooterType.Odd);
  end
  else
  begin
    if TdxDocumentModel(ASection.DocumentModel).DocumentProperties.DifferentOddAndEvenPages then
    begin
      Result.Add(TdxHeaderFooterType.Odd);
      Result.Add(TdxHeaderFooterType.Even);
    end
    else
      Result.Add(TdxHeaderFooterType.Odd);
  end;
end;

function TdxPrevNextHeaderFooterInfoCalculator<T>.CalculateNextPageHeaderFooterInfoForHeaderFooter(
  AExpectedHeaderFooter: T; ACurrentPage: TdxPage): TdxNextHeaderFooterInfo;
var
  AArea: TdxPageArea;
begin
  while True do
  begin
    ACurrentPage := ObtainNextPage(ACurrentPage);
    if ACurrentPage = nil then
      Break;
    AArea := GetHeaderFooterPageArea(ACurrentPage);
    if AArea = nil then
      Continue;
    if TdxSectionHeaderFooterBase(AExpectedHeaderFooter) = AArea.PieceTable.ContentType then
      Exit(NextHeaderFooterInfo.CreateJumpToExisting(AExpectedHeaderFooter, ACurrentPage, GetSection(ACurrentPage)));
  end;
  Result := NextHeaderFooterInfo.CreateNoJump;
end;

function TdxPrevNextHeaderFooterInfoCalculator<T>.CalculateNextPageHeaderFooterInfo(ACurrentSectionHeaderFooter: T;
  ACurrentPage: TdxPage; AType: TdxHeaderFooterType): TdxNextHeaderFooterInfo;
var
  ANextPage, APage: TdxPage;
  ASection: TdxSection;
  ANextHeaderFooterArea: TdxPageArea;
  AHeaderFooter: T;
begin
  ANextPage := ObtainNextPage(ACurrentPage);
  if ANextPage = nil then
    Exit(NextHeaderFooterInfo.CreateNoJump);

  ASection := GetSection(ACurrentPage);
  if GetSection(ANextPage) <> ASection then
    Exit(CalculateNextSectionHeaderFooterInfo(ACurrentSectionHeaderFooter, ACurrentPage));

  ANextHeaderFooterArea := GetHeaderFooterPageArea(ANextPage);
  if ANextHeaderFooterArea = nil then
    Result := NextHeaderFooterInfo.CreateJumpToNonExisting(ANextPage, ASection, AType)
  else
  begin
    while True do
    begin
      AHeaderFooter := T(ANextHeaderFooterArea.PieceTable.ContentType);
      if AHeaderFooter.&Type = AType then
        Break;
      APage := ObtainNextPage(ANextPage);
      if (APage = nil) or (GetSection(APage) <> ASection) then
      begin
        ANextHeaderFooterArea := nil;
        Break;
      end;
      ANextPage := APage;
      ANextHeaderFooterArea := GetHeaderFooterPageArea(ANextPage);
      if ANextHeaderFooterArea = nil then
        Exit(NextHeaderFooterInfo.CreateNoJump);
    end;
    if ANextHeaderFooterArea <> nil then
      Exit(NextHeaderFooterInfo.CreateJumpToExisting(T(ANextHeaderFooterArea.PieceTable.ContentType), ANextPage,
        GetSection(ANextPage)))
    else
      Exit(NextHeaderFooterInfo.CreateJumpToNonExisting(ANextPage, GetSection(ANextPage), AType));
  end;
end;

function TdxPrevNextHeaderFooterInfoCalculator<T>.GetPageHeaderFooterInfo(ANextPage: TdxPage): TdxNextHeaderFooterInfo;
var
  ASection: TdxSection;
  ANextHeaderFooterArea: TdxPageArea;
  ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>;
  AType: TdxHeaderFooterType;
begin
  ASection := GetSection(ANextPage);
  ANextHeaderFooterArea := GetHeaderFooterPageArea(ANextPage);
  if ANextHeaderFooterArea <> nil then
    Result := NextHeaderFooterInfo.CreateJumpToExisting(T(ANextHeaderFooterArea.PieceTable.ContentType), ANextPage, ASection)
  else
  begin
    ASearchOrder := CreateSearchOrder(ASection);
    try
      AType := CalculateSectionNewHeaderFooterType(ANextPage, ASearchOrder);
    finally
      ASearchOrder.Free;
    end;
    Result := NextHeaderFooterInfo.CreateJumpToNonExisting(ANextPage, ASection, AType);
  end;
end;

function TdxPrevNextHeaderFooterInfoCalculator<T>.CreateNewHeaderFooter(AInfo: TdxNextHeaderFooterInfo): TdxSectionHeaderFooterBase;
var
  ACommand: TdxInsertPageHeaderFooterCoreCommandBase;
begin
  ACommand := CreateInsertObjectCommand(AInfo.&Type);
  try
    DocumentModel.BeginUpdate;
    try
      Result := ACommand.ModifyModelCore(GetSection(AInfo.Page));
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    ACommand.Free;
  end;

  ActiveView.EnforceFormattingCompleteForVisibleArea;
end;

{ TdxPrevHeaderInfoCalculator }

function TdxPrevHeaderInfoCalculator.GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea;
begin
  Result := APage.Header;
end;

function TdxPrevHeaderInfoCalculator.GetSection(APage: TdxPage): TdxSection;
begin
  Result := APage.Areas.First.Section;
end;

function TdxPrevHeaderInfoCalculator.CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase;
begin
  Result := TdxInsertPageHeaderCoreCommand.Create(RichEditControl, AType);
end;

{ TdxPrevFooterInfoCalculator }

function TdxPrevFooterInfoCalculator.GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea;
begin
  Result := APage.Footer;
end;

function TdxPrevFooterInfoCalculator.GetSection(APage: TdxPage): TdxSection;
begin
  Result := APage.Areas.Last.Section;
end;

function TdxPrevFooterInfoCalculator.CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase;
begin
  Result := TdxInsertPageFooterCoreCommand.Create(RichEditControl, AType);
end;

{ TdxPrevHeaderFooterInfoCalculator }

function TdxPrevHeaderFooterInfoCalculator<T>.ShouldGoToNextSection(ACurrentOrder: Integer; AOrderCount: Integer): Boolean;
begin
  Result := ACurrentOrder <= 0;
end;

function TdxPrevHeaderFooterInfoCalculator<T>.ObtainNextPage(ACurrentPage: TdxPage): TdxPage;
begin
  if ACurrentPage.PageIndex <= 0 then
    Result := nil
  else
    Result := ActiveView.DocumentLayout.Pages[ACurrentPage.PageIndex - 1];
end;

function TdxPrevHeaderFooterInfoCalculator<T>.CalculateNextSectionHeaderFooterInfo(ACurrentSectionHeaderFooter: T; ACurrentPage: TdxPage): TdxNextHeaderFooterInfo;
var
  ASection: TdxSection;
  ASectionIndex: TdxSectionIndex;
  APages: TdxPageCollection;
  I: Integer;
  ANextPage: TdxPage;
begin
  ASection := GetSection(ACurrentPage);
  ASectionIndex := DocumentModel.Sections.IndexOf(ASection);
  if ASectionIndex <= 0 then
    Exit(NextHeaderFooterInfo.CreateNoJump);

  APages := ActiveView.DocumentLayout.Pages;
  for I := ACurrentPage.PageIndex - 1 downto 0 do
  begin
    ANextPage := APages[I];
    if GetSection(ANextPage) <> ASection then
      Exit(GetPageHeaderFooterInfo(ANextPage));
  end;

  Result := NextHeaderFooterInfo.CreateNoJump;
end;

function TdxPrevHeaderFooterInfoCalculator<T>.GetNextHeaderFooterType(ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>; ACurrentIndex: Integer): TdxHeaderFooterType;
begin
  Result := ASearchOrder[ACurrentIndex - 1];
end;

function TdxPrevHeaderFooterInfoCalculator<T>.CalculateSectionNewHeaderFooterType(APage: TdxPage; ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>): TdxHeaderFooterType;
var
  ASection: TdxSection;
  APageCount, I: Integer;
  APages: TdxPageCollection;
  ANextPage: TdxPage;
begin
  ASection := GetSection(APage);
  APageCount := 1;
  APages := ActiveView.DocumentLayout.Pages;
  I := APage.PageIndex - 1;
  while (I >= 0) and (APageCount < ASearchOrder.Count) do
  begin
    ANextPage := APages[I];
    if GetSection(ANextPage) <> ASection then
      Break
    else
      Inc(APageCount);
    Dec(I);
  end;

  Result := ASearchOrder[APageCount - 1];
end;

{ TdxNextHeaderInfoCalculator }

function TdxNextHeaderInfoCalculator.GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea;
begin
  Result := APage.Header;
end;

function TdxNextHeaderInfoCalculator.GetSection(APage: TdxPage): TdxSection;
begin
  Result := APage.Areas.First.Section;
end;

function TdxNextHeaderInfoCalculator.CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase;
begin
  Result := TdxInsertPageHeaderCoreCommand.Create(RichEditControl, AType);
end;

{ TdxNextFooterInfoCalculator }

function TdxNextFooterInfoCalculator.GetHeaderFooterPageArea(APage: TdxPage): TdxPageArea;
begin
  Result := APage.Footer;
end;

function TdxNextFooterInfoCalculator.GetSection(APage: TdxPage): TdxSection;
begin
  Result := APage.Areas.Last.Section;
end;

function TdxNextFooterInfoCalculator.CreateInsertObjectCommand(AType: TdxHeaderFooterType): TdxInsertPageHeaderFooterCoreCommandBase;
begin
  Result := TdxInsertPageFooterCoreCommand.Create(RichEditControl, AType);
end;

{ TdxNextHeaderFooterInfoCalculator }

function TdxNextHeaderFooterInfoCalculator<T>.ShouldGoToNextSection(ACurrentOrder: Integer; AOrderCount: Integer): Boolean;
begin
  Result := ACurrentOrder + 1 >= AOrderCount;
end;

function TdxNextHeaderFooterInfoCalculator<T>.ObtainNextPage(ACurrentPage: TdxPage): TdxPage;
var
  ANextPageIndex: Integer;
begin
  ANextPageIndex := ACurrentPage.PageIndex + 1;
  ActiveView.EnsureFormattingCompleteForPreferredPage(ANextPageIndex);

  if ANextPageIndex >= ActiveView.DocumentLayout.Pages.Count then
    Exit(nil)
  else
    Exit(ActiveView.DocumentLayout.Pages[ANextPageIndex]);
end;

function TdxNextHeaderFooterInfoCalculator<T>.CalculateNextSectionHeaderFooterInfo(ACurrentSectionHeaderFooter: T; ACurrentPage: TdxPage): TdxNextHeaderFooterInfo;
var
  ASection: TdxSection;
  ASectionIndex: TdxSectionIndex;
  ANextPage: TdxPage;
begin
  ASection := GetSection(ACurrentPage);
  ASectionIndex := DocumentModel.Sections.IndexOf(ASection);
  if ASectionIndex + 1 >= DocumentModel.Sections.Count then
    Exit(NextHeaderFooterInfo.CreateNoJump);

  while True do
  begin
    ANextPage := ObtainNextPage(ACurrentPage);
    if ANextPage = nil then
      Exit(NextHeaderFooterInfo.CreateNoJump);

    if GetSection(ANextPage) <> ASection then
      Exit(GetPageHeaderFooterInfo(ANextPage))
    else
      ACurrentPage := ANextPage;
  end;
end;

function TdxNextHeaderFooterInfoCalculator<T>.GetNextHeaderFooterType(ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>; ACurrentIndex: Integer): TdxHeaderFooterType;
begin
  Result := ASearchOrder[ACurrentIndex + 1];
end;

function TdxNextHeaderFooterInfoCalculator<T>.CalculateSectionNewHeaderFooterType(APage: TdxPage; ASearchOrder: TdxOrdinalList<TdxHeaderFooterType>): TdxHeaderFooterType;
begin
  Result := ASearchOrder[0];
end;

{ TdxMakeNearestHeaderFooterActiveCommand }

constructor TdxMakeNearestHeaderFooterActiveCommand<T>.Create(const AControl: IdxRichEditControl; ATargetTable: T);
begin
  inherited Create(AControl);
  FTargetTable := ATargetTable;
end;

class function TdxMakeNearestHeaderFooterActiveCommand<T>.GetDescription: string;
begin
  Result := 'InternalError';
end;

class function TdxMakeNearestHeaderFooterActiveCommand<T>.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

procedure TdxMakeNearestHeaderFooterActiveCommand<T>.ExecuteCore;
begin
  UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.PageArea);
  if not CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
    Exit;
  ChangeActivePieceTable(CaretPosition.LayoutPosition);
end;

procedure TdxMakeNearestHeaderFooterActiveCommand<T>.ChangeActivePieceTable(ALayoutPosition: TdxDocumentLayoutPosition);
var
  ACurrentPage: TdxPage;
  AHeaderFooter: TdxPieceTable;
begin
  ACurrentPage := ALayoutPosition.Page;
  AHeaderFooter := GetPageHeaderFooter(ACurrentPage);
  if (AHeaderFooter <> nil) and (TdxSectionHeaderFooterBase(FTargetTable) = AHeaderFooter.ContentType) then
    MakeHeaderFooterActive(AHeaderFooter, ALayoutPosition.PageArea.Section, ACurrentPage.PageIndex)
  else
    if not TryMakePrevHeaderFooterActive(ACurrentPage) then
      MakeNextHeaderFooterActive(ACurrentPage);
end;

function TdxMakeNearestHeaderFooterActiveCommand<T>.TryMakePrevHeaderFooterActive(ACurrentPage: TdxPage): Boolean;
var
  AInfo: TdxNextHeaderFooterInfo;
  ACalculator: TdxPrevHeaderFooterInfoCalculator<T>;
begin
  ACalculator := CreatePrevHeaderFooterCalculator;
  try
    AInfo := ACalculator.CalculateNextPageHeaderFooterInfoForHeaderFooter(FTargetTable, ACurrentPage);
    try
      if (AInfo <> nil) and AInfo.CanGo then
      begin
        MakeHeaderFooterActive(TdxPieceTable(AInfo.HeaderFooter.PieceTable), AInfo.Section, AInfo.Page.PageIndex);
        Result := True;
      end
      else
        Result := False;
    finally
      AInfo.Free;
    end;
  finally
    ACalculator.Free;
  end;
end;

procedure TdxMakeNearestHeaderFooterActiveCommand<T>.MakeNextHeaderFooterActive(ACurrentPage: TdxPage);
var
  AInfo: TdxNextHeaderFooterInfo;
  ACalculator: TdxNextHeaderFooterInfoCalculator<T>;
begin
  ACalculator := CreateNextHeaderFooterCalculator;
  try
    AInfo := ACalculator.CalculateNextPageHeaderFooterInfoForHeaderFooter(FTargetTable, ACurrentPage);
    try
      if (AInfo <> nil) and AInfo.CanGo then
        MakeHeaderFooterActive(TdxPieceTable(AInfo.HeaderFooter.PieceTable), AInfo.Section, AInfo.Page.PageIndex);
    finally
      AInfo.Free;
    end;
  finally
    ACalculator.Free;
  end;
end;

procedure TdxMakeNearestHeaderFooterActiveCommand<T>.MakeHeaderFooterActive(APieceTable: TdxPieceTable; ASection: TdxSection; APageIndex: Integer);
var
  AProvider: IdxPieceTableProvider;
  ACommand: TdxMakeHeaderFooterActiveCommand;
begin
  AProvider := TdxExplicitPieceTableProvider.Create(APieceTable, ASection, APageIndex);
  ACommand := TdxMakeHeaderFooterActiveCommand.Create(RichEditControl, AProvider);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxMakeNearestHeaderFooterActiveCommand<T>.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.HeadersFooters, AState.Enabled);
  if AState.Enabled then
    AState.Enabled := ActiveViewType = TdxRichEditViewType.PrintLayout;
end;

{ TdxMakeNearestHeaderActiveCommand }

function TdxMakeNearestHeaderActiveCommand.CreatePrevHeaderFooterCalculator: TdxPrevHeaderFooterInfoCalculator<TdxSectionHeader>;
begin
  Result := TdxPrevHeaderInfoCalculator.Create(RichEditControl);
end;

function TdxMakeNearestHeaderActiveCommand.CreateNextHeaderFooterCalculator: TdxNextHeaderFooterInfoCalculator<TdxSectionHeader>;
begin
  Result := TdxNextHeaderInfoCalculator.Create(RichEditControl);
end;

function TdxMakeNearestHeaderActiveCommand.GetPageHeaderFooter(APage: TdxPage): TdxPieceTable;
begin
  if APage.Header <> nil then
    Result := TdxPieceTable(APage.Header.PieceTable)
  else
    Result := nil;
end;

{ TdxMakeNearestFooterActiveCommand }

function TdxMakeNearestFooterActiveCommand.CreatePrevHeaderFooterCalculator: TdxPrevHeaderFooterInfoCalculator<TdxSectionFooter>;
begin
  Result := TdxPrevFooterInfoCalculator.Create(RichEditControl);
end;

function TdxMakeNearestFooterActiveCommand.CreateNextHeaderFooterCalculator: TdxNextHeaderFooterInfoCalculator<TdxSectionFooter>;
begin
  Result := TdxNextFooterInfoCalculator.Create(RichEditControl);
end;

function TdxMakeNearestFooterActiveCommand.GetPageHeaderFooter(APage: TdxPage): TdxPieceTable;
begin
  if APage.Footer <> nil then
    Result := TdxPieceTable(APage.Footer.PieceTable)
  else
    Result := nil;
end;

{ TdxClosePageHeaderFooterCommand }

procedure TdxClosePageHeaderFooterCommand.ExecuteCore;
var
  ACommand: TdxChangeActivePieceTableCommand;
begin
  ACommand := TdxChangeActivePieceTableCommand.Create(RichEditControl, DocumentModel.MainPieceTable, nil, -1);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

class function TdxClosePageHeaderFooterCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandClosePageHeaderFooterDescription);
end;

class function TdxClosePageHeaderFooterCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandClosePageHeaderFooterMenuCaption);
end;

class function TdxClosePageHeaderFooterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ClosePageHeaderFooter;
end;

class function TdxClosePageHeaderFooterCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Close;
end;

procedure TdxClosePageHeaderFooterCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := ActivePieceTable.IsHeaderFooter and (ActiveViewType = TdxRichEditViewType.PrintLayout);
  AState.Visible := True;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.HeadersFooters, AState.Enabled);
end;

{ TdxGoToPageHeaderFooterCommand }

function TdxGoToPageHeaderFooterCommand<T>.GetInsertObjectCommand: TdxRichEditCommand;
begin
  if Commands.Count > 0 then
    Result := TdxRichEditCommand(Commands[0])
  else
    Result := nil;
end;

procedure TdxGoToPageHeaderFooterCommand<T>.CreateCommands;
var
  ACorrespondingHeaderFooter: T;
  ACommand: TdxInsertPageHeaderFooterCoreCommand<T>;
  AProvider: IdxPieceTableProvider;
begin
  ACorrespondingHeaderFooter := GetCorrespondingHeaderFooter;
  if ACorrespondingHeaderFooter = nil then
  begin
    ACommand := TdxInsertPageHeaderFooterCoreCommand<T>(CreateInsertObjectCommand);
    if ACommand <> nil then
    begin
      Commands.Add(ACommand);
      Commands.Add(TdxMakeHeaderFooterActiveCommand.Create(RichEditControl, ACommand.GetPieceTableProvider));
    end;
  end
  else
  begin
    AProvider := TdxExplicitPieceTableProvider.Create(TdxPieceTable(ACorrespondingHeaderFooter.PieceTable), DocumentModel.GetActiveSection, -1);
    Commands.Add(TdxMakeHeaderFooterActiveCommand.Create(RichEditControl, AProvider));
  end;
end;

procedure TdxGoToPageHeaderFooterCommand<T>.UpdateUIState(const AState: IdxCommandUIState);
begin
  inherited UpdateUIState(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.HeadersFooters, AState.Enabled);
  if AState.Enabled then
    AState.Enabled := (ActivePieceTable.IsHeaderFooter and (ActiveViewType = TdxRichEditViewType.PrintLayout)) and
      CanGoFromCurrentSelection;
end;

function TdxGoToPageHeaderFooterCommand<T>.CanGoFromCurrentSelection: Boolean;
begin
  Result := not (ActivePieceTable.ContentType is T);
end;

{ TdxGoToPageHeaderCommand }

function TdxGoToPageHeaderCommand.CreateInsertObjectCommand: TdxRichEditCommand;
var
  AFooter: TdxSectionFooter;
begin
  AFooter := Safe<TdxSectionFooter>.Cast(ActivePieceTable.ContentType);
  if AFooter <> nil then
    Result := TdxInsertPageHeaderCoreCommand.Create(RichEditControl, AFooter.&Type)
  else
    Result := nil;
end;

function TdxGoToPageHeaderCommand.GetCorrespondingHeaderFooter: TdxSectionHeader;
var
  AFooter: TdxSectionFooter;
  ASection: TdxSection;
begin
  AFooter := Safe<TdxSectionFooter>.Cast(ActivePieceTable.ContentType);
  if AFooter <> nil then
  begin
    ASection := DocumentModel.GetActiveSectionBySelectionEnd;
    Result := ASection.GetCorrespondingHeader(AFooter);
  end
  else
    Result := nil;
end;

class function TdxGoToPageHeaderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandGoToPageHeaderDescription);
end;

class function TdxGoToPageHeaderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandGoToPageHeaderMenuCaption);
end;

class function TdxGoToPageHeaderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.GoToPageHeader;
end;

class function TdxGoToPageHeaderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.GoToHeader;
end;

{ TdxGoToPageFooterCommand }

function TdxGoToPageFooterCommand.CreateInsertObjectCommand: TdxRichEditCommand;
var
  AHeader: TdxSectionHeader;
begin
  AHeader := Safe<TdxSectionHeader>.Cast(ActivePieceTable.ContentType);
  if AHeader <> nil then
    Result := TdxInsertPageFooterCoreCommand.Create(RichEditControl, AHeader.&Type)
  else
    Result := nil;
end;

function TdxGoToPageFooterCommand.GetCorrespondingHeaderFooter: TdxSectionFooter;
var
  AHeader: TdxSectionHeader;
  ASection: TdxSection;
begin
  AHeader := Safe<TdxSectionHeader>.Cast(ActivePieceTable.ContentType);
  if AHeader <> nil then
  begin
    ASection := DocumentModel.GetActiveSectionBySelectionEnd;
    Result := ASection.GetCorrespondingFooter(AHeader);
  end
  else
    Result := nil;
end;

class function TdxGoToPageFooterCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandGoToPageFooterDescription);
end;

class function TdxGoToPageFooterCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandGoToPageFooterMenuCaption);
end;

class function TdxGoToPageFooterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.GoToPageFooter;
end;

class function TdxGoToPageFooterCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.GoToFooter;
end;

{ TdxToggleHeaderFooterLinkToPreviousCoreCommand }

function TdxToggleHeaderFooterLinkToPreviousCoreCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Page;
end;

procedure TdxToggleHeaderFooterLinkToPreviousCoreCommand.ModifyModel;
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
  AContainer: TdxSectionHeadersFootersBase;
  AActualActiveHeaderFooterPieceTable: TdxPieceTable;
  ACommand: TdxChangeActivePieceTableCommand;
begin
  AHeaderFooter := Safe<TdxSectionHeaderFooterBase>.Cast(ActivePieceTable.ContentType);
  if AHeaderFooter = nil then
    Exit;

  FSection := GetCurrentSectionFromCaretLayoutPosition;
  if FSection = nil then
    Exit;

  AContainer := AHeaderFooter.GetContainer(FSection);
  if AContainer.IsLinkedToPrevious(AHeaderFooter.&Type) then
    AContainer.UnlinkFromPrevious(AHeaderFooter.&Type)
  else
    AContainer.LinkToPrevious(AHeaderFooter.&Type);

  FNewActiveHeaderFooter := AContainer.GetObjectCore(AHeaderFooter.&Type);

  if FNewActiveHeaderFooter <> nil then
    AActualActiveHeaderFooterPieceTable := TdxPieceTable(FNewActiveHeaderFooter.PieceTable)
  else
    AActualActiveHeaderFooterPieceTable := DocumentModel.MainPieceTable;
  ACommand := TdxChangeActivePieceTableCommand.Create(RichEditControl, AActualActiveHeaderFooterPieceTable, FSection, -1);
  ACommand.ActivatePieceTable(AActualActiveHeaderFooterPieceTable, FSection);
end;

procedure TdxToggleHeaderFooterLinkToPreviousCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
  ASection: TdxSection;
  AContainer: TdxSectionHeadersFootersBase;
begin
  AHeaderFooter := Safe<TdxSectionHeaderFooterBase>.Cast(ActivePieceTable.ContentType);
  if AHeaderFooter = nil then
  begin
    AState.Enabled := False;
    Exit;
  end;

  ASection := GetCurrentSectionFromCaretLayoutPosition;
  if ASection = nil then
    Exit;

  AContainer := AHeaderFooter.GetContainer(ASection);
  if AState.Enabled then
  begin
    AState.Enabled := AContainer.CanLinkToPrevious(AHeaderFooter.&Type);
    if AState.Enabled then
      AState.Checked := AContainer.IsLinkedToPrevious(AHeaderFooter.&Type);
  end;
end;

class function TdxToggleHeaderFooterLinkToPreviousCoreCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

class function TdxToggleHeaderFooterLinkToPreviousCoreCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

function TdxToggleHeaderFooterLinkToPreviousCoreCommand.GetPieceTable: TdxPieceTable;
begin
  if FNewActiveHeaderFooter <> nil then
    Result := TdxPieceTable(FNewActiveHeaderFooter.PieceTable)
  else
    Result := DocumentModel.MainPieceTable;
end;

function TdxToggleHeaderFooterLinkToPreviousCoreCommand.GetSection: TdxSection;
begin
  if FNewActiveHeaderFooter <> nil then
    Result := FSection
  else
    Result := nil;
end;

function TdxToggleHeaderFooterLinkToPreviousCoreCommand.GetPreferredPageIndex: Integer;
begin
  Result := -1;
end;

function TdxToggleHeaderFooterLinkToPreviousCoreCommand.GetCurrentSectionFromCaretLayoutPosition: TdxSection;
begin
  UpdateCaretPosition;
  if not CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
    Result := nil
  else
    Result := CaretPosition.LayoutPosition.PageArea.Section;
end;

function TdxToggleHeaderFooterLinkToPreviousCoreCommand.GetPieceTableProvider: IdxPieceTableProvider;
begin
  Result := TdxFakePieceTableProvider.Create(GetPieceTable, GetSection, GetPreferredPageIndex);
end;

{ TdxToggleHeaderFooterLinkToPreviousCommand }

procedure TdxToggleHeaderFooterLinkToPreviousCommand.ForceExecuteCore(const AState: IdxCommandUIState);
var
  ACommand: TdxToggleHeaderFooterLinkToPreviousCoreCommand;
begin
  ACommand := TdxToggleHeaderFooterLinkToPreviousCoreCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

class function TdxToggleHeaderFooterLinkToPreviousCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleHeaderFooterLinkToPreviousDescription);
end;

class function TdxToggleHeaderFooterLinkToPreviousCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleHeaderFooterLinkToPreviousMenuCaption);
end;

class function TdxToggleHeaderFooterLinkToPreviousCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleHeaderFooterLinkToPrevious;
end;

class function TdxToggleHeaderFooterLinkToPreviousCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.LinkToPrevious;
end;

procedure TdxToggleHeaderFooterLinkToPreviousCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ACommand: TdxToggleHeaderFooterLinkToPreviousCoreCommand;
  ANewState: IdxCommandUIState;
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
  begin
    ACommand := TdxToggleHeaderFooterLinkToPreviousCoreCommand.Create(RichEditControl);
    try
      ANewState := ACommand.CreateDefaultCommandUIState;
      ACommand.UpdateUIState(ANewState);
      AState.Enabled := ANewState.Enabled;
      AState.Checked := ANewState.Checked;
    finally
      ACommand.Free;
    end;
  end;
end;

{ TdxGoToPrevNextPageHeaderFooterCommand }

procedure TdxGoToPrevNextPageHeaderFooterCommand.ForceExecuteCore(const AState: IdxCommandUIState);
var
  ACalculateCommand: TdxPrevNextHeaderFooterInfoCalculatorBase;
  AInfo: TdxNextHeaderFooterInfo;
  ANewHeaderFooter: TdxSectionHeaderFooterBase;
begin
  ACalculateCommand := CreateCalculateNextHeaderFooterInfoCommand;
  try
    ACalculateCommand.Execute;
    AInfo := ACalculateCommand.NextHeaderFooterInfo;
    if (AInfo = nil) or (not AInfo.CanGoOrCreate) then
      Exit;

    if AInfo.CanGo then
      GoToExistingHeaderFooter(AInfo.HeaderFooter, AInfo.Page.PageIndex, AInfo.Section)
    else
    begin

      ANewHeaderFooter := ACalculateCommand.CreateNewHeaderFooter(AInfo);
      if ANewHeaderFooter <> nil then
        GoToExistingHeaderFooter(ANewHeaderFooter, AInfo.Page.PageIndex, AInfo.Section);
    end;
  finally
    ACalculateCommand.Free;
  end;
end;

{ TdxGoToNextPageHeaderFooterCommand }

function TdxGoToNextPageHeaderFooterCommand.CreateCalculateNextHeaderFooterInfoCommand: TdxPrevNextHeaderFooterInfoCalculatorBase;
begin
  if ActivePieceTable.ContentType is TdxSectionHeader then
    Result := TdxNextHeaderInfoCalculator.Create(RichEditControl)
  else
    Result := TdxNextFooterInfoCalculator.Create(RichEditControl);
end;

class function TdxGoToNextPageHeaderFooterCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandGoToNextHeaderFooterDescription);
end;

class function TdxGoToNextPageHeaderFooterCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandGoToNextHeaderFooterMenuCaption);
end;

class function TdxGoToNextPageHeaderFooterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.GoToNextHeaderFooter;
end;

class function TdxGoToNextPageHeaderFooterCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowNextHeaderFooter;
end;

procedure TdxGoToNextPageHeaderFooterCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if not RichEditControl.InnerControl.DocumentModel.ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False;
end;

{ TdxGoToPreviousPageHeaderFooterCommand }

function TdxGoToPreviousPageHeaderFooterCommand.CreateCalculateNextHeaderFooterInfoCommand: TdxPrevNextHeaderFooterInfoCalculatorBase;
begin
  if ActivePieceTable.ContentType is TdxSectionHeader then
    Result := TdxPrevHeaderInfoCalculator.Create(RichEditControl)
  else
    Result := TdxPrevFooterInfoCalculator.Create(RichEditControl);
end;

class function TdxGoToPreviousPageHeaderFooterCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandGoToPreviousHeaderFooterDescription);
end;

class function TdxGoToPreviousPageHeaderFooterCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandGoToPreviousHeaderFooterMenuCaption);
end;

class function TdxGoToPreviousPageHeaderFooterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.GoToPreviousHeaderFooter;
end;

class function TdxGoToPreviousPageHeaderFooterCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowPreviousHeaderFooter;
end;

procedure TdxGoToPreviousPageHeaderFooterCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if not RichEditControl.InnerControl.DocumentModel.ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False;
end;

{ TdxToggleHeaderFooterCommandBase }

procedure TdxToggleHeaderFooterCommandBase.ForceExecuteCore(const AState: IdxCommandUIState);
var
  ALayoutPosition: TdxDocumentLayoutPosition;
  ASection: TdxSection;
  AHeaderFooter, ANewHeaderFooter: TdxSectionHeaderFooterBase;
  AIsFirstSectionPage, AIsEvenPage: Boolean;
  APreferredPageIndex: Integer;
  AContainer: TdxSectionHeadersFootersBase;
  AType: TdxHeaderFooterType;
begin
  if not UpdateLayoutPositionToPageArea then
    Exit;

  ALayoutPosition := ActiveView.CaretPosition.LayoutPosition;
  ASection := ALayoutPosition.PageArea.Section;
  AHeaderFooter := TdxSectionHeaderFooterBase(ALayoutPosition.PageArea.PieceTable.ContentType);

  AIsFirstSectionPage := IsFirstSectionPage(ALayoutPosition);
  AIsEvenPage := ALayoutPosition.Page.IsEven;
  APreferredPageIndex := ALayoutPosition.Page.PageIndex;
  AContainer := AHeaderFooter.GetContainer(ASection);

  ChangeValue(ASection, APreferredPageIndex);

  AType := AContainer.CalculateActualObjectType(AIsFirstSectionPage, AIsEvenPage);
  ANewHeaderFooter := AContainer.GetObjectCore(AType);
  if ANewHeaderFooter = nil then
    ANewHeaderFooter := CreateNewHeaderFooter(AHeaderFooter, AIsFirstSectionPage, AIsEvenPage, AContainer);
  GoToExistingHeaderFooter(ANewHeaderFooter, APreferredPageIndex, ASection);
end;

procedure TdxToggleHeaderFooterCommandBase.ChangeValue(ASection: TdxSection; APreferredPageIndex: Integer);
var
  ACommand: TdxChangeActivePieceTableCommand;
begin
  DocumentModel.BeginUpdate;
  try
    SetValue(ASection, not GetValue(ASection));

    ACommand := TdxChangeActivePieceTableCommand.Create(RichEditControl, DocumentModel.MainPieceTable,
      ASection, APreferredPageIndex);
    try
      ACommand.ActivatePieceTable(DocumentModel.MainPieceTable, nil);
    finally
      ACommand.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxToggleHeaderFooterCommandBase.CreateNewHeaderFooter(AHeaderFooter: TdxSectionHeaderFooterBase;
  AIsFirstSectionPage: Boolean; AIsEvenPage: Boolean; AContainer: TdxSectionHeadersFootersBase): TdxSectionHeaderFooterBase;
var
  AType: TdxHeaderFooterType;
  ACommand: TdxInsertPageHeaderFooterCoreCommandBase;
  AProvider: IdxPieceTableProvider;
begin
  AType := AContainer.CalculateActualObjectType(AIsFirstSectionPage, AIsEvenPage);
  if AHeaderFooter is TdxSectionHeader then
    ACommand := TdxInsertPageHeaderCoreCommand.Create(RichEditControl, AType)
  else
    ACommand := TdxInsertPageFooterCoreCommand.Create(RichEditControl, AType);
  try
    ACommand.Execute;
    AProvider := ACommand.GetPieceTableProvider;
    Result := TdxSectionHeaderFooterBase(AProvider.PieceTable.ContentType);
  finally
    ACommand.Free;
  end;
end;

procedure TdxToggleHeaderFooterCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ACaretPosition: TdxCaretPosition;
  ASection: TdxSection;
begin
  inherited UpdateUIStateCore(AState);

  ACaretPosition := ActiveView.CaretPosition;
  ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.PageArea);
  if ACaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
  begin
    ASection := ACaretPosition.LayoutPosition.PageArea.Section;
    AState.Checked := GetValue(ASection);
  end;
end;

{ TdxToggleDifferentFirstPageCommand }

class function TdxToggleDifferentFirstPageCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleDifferentFirstPageDescription);
end;

class function TdxToggleDifferentFirstPageCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleDifferentFirstPageMenuCaption);
end;

function TdxToggleDifferentFirstPageCommand.GetValue(ASection: TdxSection): Boolean;
begin
  Result := ASection.GeneralSettings.DifferentFirstPage;
end;

class function TdxToggleDifferentFirstPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleDifferentFirstPage;
end;

class function TdxToggleDifferentFirstPageCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DifferentFirstPage;
end;

procedure TdxToggleDifferentFirstPageCommand.SetValue(ASection: TdxSection; AValue: Boolean);
begin
  ASection.GeneralSettings.DifferentFirstPage := AValue;
end;

procedure TdxToggleDifferentFirstPageCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if not RichEditControl.InnerControl.DocumentModel.ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False;
end;

{ TdxToggleDifferentOddAndEvenPagesCommand }

class function TdxToggleDifferentOddAndEvenPagesCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleDifferentOddAndEvenPagesDescription);
end;

class function TdxToggleDifferentOddAndEvenPagesCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleDifferentOddAndEvenPagesMenuCaption);
end;

function TdxToggleDifferentOddAndEvenPagesCommand.GetValue(ASection: TdxSection): Boolean;
begin
  Result := TdxDocumentModel(ASection.DocumentModel).DocumentProperties.DifferentOddAndEvenPages;
end;

class function TdxToggleDifferentOddAndEvenPagesCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleDifferentOddAndEvenPages;
end;

class function TdxToggleDifferentOddAndEvenPagesCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DifferentOddAndEvenPages;
end;

procedure TdxToggleDifferentOddAndEvenPagesCommand.SetValue(ASection: TdxSection; AValue: Boolean);
begin
  TdxDocumentModel(ASection.DocumentModel).DocumentProperties.DifferentOddAndEvenPages := AValue;
end;

procedure TdxToggleDifferentOddAndEvenPagesCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if not RichEditControl.InnerControl.DocumentModel.ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False;
end;

end.
