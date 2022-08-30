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

unit dxRichEdit.Commands.FloatingObject;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, cxGeometry, dxTypeHelpers, dxCoreGraphics, dxCoreClasses,
  dxRichEdit.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Utils.Types,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.Insert,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Import.Core,
  dxRichEdit.Commands.IDs,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.Commands;

type

  { TdxMoveSelectionToBeginOfParagraph }

  TdxMoveSelectionToBeginOfParagraph = class(TdxRichEditSelectionCommand)
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxInsertFloatingObjectCommandBase }

  TdxInsertFloatingObjectCommandBase = class abstract(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function CreateDeleteCommand: TdxCommand; override;
  end;

  { TdxInsertTextBoxCommand }

  TdxInsertTextBoxCommand = class(TdxInsertFloatingObjectCommandBase)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertFloatingObjectPictureCommand }

  TdxInsertFloatingObjectPictureCommand = class(TdxInsertFloatingObjectCommandBase)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxFloatingObjectCommandBase }

  TdxFloatingObjectCommandBase = class abstract(TdxSelectionBasedPropertyChangeCommandBase)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function ChangeFloatingObjectProperty(AModifier: TdxFloatingObjectRunPropertyModifierBase;
      const AStart, AEnd: TdxDocumentModelPosition; const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
  end;

  { TdxChangeFloatingObjectPropertyCommandBase }

  TdxChangeFloatingObjectPropertyCommandBase<T> = class abstract(TdxFloatingObjectCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    function GetCurrentPropertyValue(out AValue: T): Boolean; virtual;
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<T>; virtual; abstract;
  end;

  { TdxChangeFloatingObjectTextWrapTypeCommandBase }

  TdxChangeFloatingObjectTextWrapTypeCommandBase = class abstract(TdxChangeFloatingObjectPropertyCommandBase<TdxFloatingObjectTextWrapType>)
  protected
    function GetTextWrapType: TdxFloatingObjectTextWrapType; virtual; abstract;
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<TdxFloatingObjectTextWrapType>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function IsChecked(const AState: IdxCommandUIState): Boolean; virtual;

    property TextWrapType: TdxFloatingObjectTextWrapType read GetTextWrapType;
  end;

  { TdxSetFloatingObjectSquareTextWrapTypeCommand }

  TdxSetFloatingObjectSquareTextWrapTypeCommand = class(TdxChangeFloatingObjectTextWrapTypeCommandBase)
  protected
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectBehindTextWrapTypeCommand }

  TdxSetFloatingObjectBehindTextWrapTypeCommand = class(TdxChangeFloatingObjectPropertyCommandBase<Boolean>)
  strict private
    function GetIsBehindDoc: Boolean;
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Boolean>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function IsChecked(const AState: IdxCommandUIState): Boolean; virtual;

    property IsBehindDoc: Boolean read GetIsBehindDoc;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectInFrontOfTextWrapTypeCommand }

  TdxSetFloatingObjectInFrontOfTextWrapTypeCommand = class(TdxChangeFloatingObjectPropertyCommandBase<Boolean>)
  strict private
    function GetIsBehindDoc: Boolean;
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Boolean>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function IsChecked(const AState: IdxCommandUIState): Boolean; virtual;

    property IsBehindDoc: Boolean read GetIsBehindDoc;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectThroughTextWrapTypeCommand }

  TdxSetFloatingObjectThroughTextWrapTypeCommand = class(TdxChangeFloatingObjectTextWrapTypeCommandBase)
  protected
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectTightTextWrapTypeCommand }

  TdxSetFloatingObjectTightTextWrapTypeCommand = class(TdxChangeFloatingObjectTextWrapTypeCommandBase)
  protected
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand }

  TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand = class(TdxChangeFloatingObjectTextWrapTypeCommandBase)
  protected
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxChangeFloatingObjectAlignmentCommandBase }

  TdxChangeFloatingObjectAlignmentCommandBase = class abstract(TdxFloatingObjectCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; virtual; abstract;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; virtual; abstract;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; virtual; abstract;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; virtual; abstract;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; virtual; abstract;
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;

    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read GetHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read GetVerticalPositionAlignment;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read GetHorizontalPositionType;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read GetVerticalPositionType;
    property TextWrapType: TdxFloatingObjectTextWrapType read GetTextWrapType;
  end;

  { TdxSetFloatingObjectTopLeftAlignmentCommand }

  TdxSetFloatingObjectTopLeftAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectTopCenterAlignmentCommand }

  TdxSetFloatingObjectTopCenterAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectTopRightAlignmentCommand }

  TdxSetFloatingObjectTopRightAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectMiddleLeftAlignmentCommand }

  TdxSetFloatingObjectMiddleLeftAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectMiddleCenterAlignmentCommand }

  TdxSetFloatingObjectMiddleCenterAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectMiddleRightAlignmentCommand }

  TdxSetFloatingObjectMiddleRightAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectBottomLeftAlignmentCommand }

  TdxSetFloatingObjectBottomLeftAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectBottomCenterAlignmentCommand }

  TdxSetFloatingObjectBottomCenterAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSetFloatingObjectBottomRightAlignmentCommand }

  TdxSetFloatingObjectBottomRightAlignmentCommand = class(TdxChangeFloatingObjectAlignmentCommandBase)
  protected
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment; override;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment; override;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType; override;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType; override;
    function GetTextWrapType: TdxFloatingObjectTextWrapType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxFloatingObjectBringForwardCommandBase }

  TdxFloatingObjectBringForwardCommandBase = class abstract(TdxFloatingObjectCommandBase)
  strict private
    FFloatingObjectProperties: TdxFloatingObjectProperties;
    function GetZOrder: Integer;
  protected
    procedure ChangeZOrder;
    procedure ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); virtual; abstract;

    property FloatingObjectProperties: TdxFloatingObjectProperties read FFloatingObjectProperties;
    property ZOrder: Integer read GetZOrder;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
  end;

  { TdxFloatingObjectBringForwardCommand }

  TdxFloatingObjectBringForwardCommand = class(TdxFloatingObjectBringForwardCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition; const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxFloatingObjectBringToFrontCommand }

  TdxFloatingObjectBringToFrontCommand = class(TdxFloatingObjectBringForwardCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition; const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxFloatingObjectBringInFrontOfTextCommand }

  TdxFloatingObjectBringInFrontOfTextCommand = class(TdxChangeFloatingObjectPropertyCommandBase<Boolean>)
  strict private
    function GetIsBehindDoc: Boolean;
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Boolean>; override;

    property IsBehindDoc: Boolean read GetIsBehindDoc;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxFloatingObjectSendBackwardCommandBase }

  TdxFloatingObjectSendBackwardCommandBase = class abstract(TdxFloatingObjectCommandBase)
  strict private
    FFloatingObjectProperties: TdxFloatingObjectProperties;
    function GetZOrder: Integer;
  protected
    procedure ChangeZOrder;
    procedure ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); virtual; abstract;

    property FloatingObject: TdxFloatingObjectProperties read FFloatingObjectProperties;
    property ZOrder: Integer read GetZOrder;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
  end;

  { TdxFloatingObjectSendBackwardCommand }

  TdxFloatingObjectSendBackwardCommand = class(TdxFloatingObjectSendBackwardCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition; const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxFloatingObjectSendToBackCommand }

  TdxFloatingObjectSendToBackCommand = class(TdxFloatingObjectSendBackwardCommandBase)
  protected
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition; const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxFloatingObjectSendBehindTextCommand }

  TdxFloatingObjectSendBehindTextCommand = class(TdxChangeFloatingObjectPropertyCommandBase<Boolean>)
  strict private
    function GetIsBehindDoc: Boolean;
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Boolean>; override;

    property IsBehindDoc: Boolean read GetIsBehindDoc;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxChangeFloatingObjectAlignmentCommand }

  TdxChangeFloatingObjectAlignmentCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxChangeFloatingObjectTextWrapTypeCommand }

  TdxChangeFloatingObjectTextWrapTypeCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeFloatingObjectTextWrapTypeMenuCommand }

  TdxChangeFloatingObjectTextWrapTypeMenuCommand = class(TdxChangeFloatingObjectTextWrapTypeCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

  { TdxFloatingObjectBringForwardPlaceholderCommand }

  TdxFloatingObjectBringForwardPlaceholderCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxFloatingObjectBringForwardMenuCommand }

  TdxFloatingObjectBringForwardMenuCommand = class(TdxFloatingObjectBringForwardPlaceholderCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

  { TdxFloatingObjectSendBackwardPlaceholderCommand }

  TdxFloatingObjectSendBackwardPlaceholderCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxFloatingObjectSendBackwardMenuCommand }

  TdxFloatingObjectSendBackwardMenuCommand = class(TdxFloatingObjectSendBackwardPlaceholderCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

  { TdxChangeFloatingObjectFillColorCommand }

  TdxChangeFloatingObjectFillColorCommand = class(TdxChangeFloatingObjectPropertyCommandBase<TdxAlphaColor>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<TdxAlphaColor>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxChangeFloatingObjectOutlineColorCommand }

  TdxChangeFloatingObjectOutlineColorCommand = class(TdxChangeFloatingObjectPropertyCommandBase<TdxAlphaColor>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<TdxAlphaColor>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxChangeFloatingObjectOutlineWidthCommand }

  TdxChangeFloatingObjectOutlineWidthCommand = class(TdxChangeFloatingObjectPropertyCommandBase<Integer>)
  protected
    function CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Integer>; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

implementation

uses
  dxThreading,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Commands.Images,
  dxRichEdit.View.Simple,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Options,
  dxRichEdit.Import.DocumentImportHelper,
  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxRichEdit.Commands.ChangeProperties;

type
  { TdxInsertFloatingObjectCoreCommandBase }

  TdxInsertFloatingObjectCoreCommandBase = class abstract(TdxInsertObjectCommandBase)
  protected
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure ModifyModel; override;
    function GetZOrder: Integer;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure SetupFloatingObjectProperties(AProperties: TdxFloatingObjectProperties; ARun: TdxFloatingObjectAnchorRun); virtual; abstract;
    procedure SetupShape(AShape: TdxShape; ARun: TdxFloatingObjectAnchorRun); virtual; abstract;
    function CreateDefaultFloatingObjectContent(ARun: TdxFloatingObjectAnchorRun): TdxCustomObjectContent; virtual; abstract;
  end;

  { TdxInsertFloatingObjectTextBoxCoreCommand }

  TdxInsertFloatingObjectTextBoxCoreCommand = class(TdxInsertFloatingObjectCoreCommandBase)
  protected
    procedure SetupFloatingObjectProperties(AProperties: TdxFloatingObjectProperties; ARun: TdxFloatingObjectAnchorRun); override;
    procedure SetupShape(AShape: TdxShape; ARun: TdxFloatingObjectAnchorRun); override;
    function CreateDefaultFloatingObjectContent(ARun: TdxFloatingObjectAnchorRun): TdxCustomObjectContent; override;
  public
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertFloatingObjectPictureCoreCommand }

  TdxInsertFloatingObjectPictureCoreCommand = class(TdxInsertFloatingObjectCoreCommandBase)
  strict private
    FImportSource: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference>;
    FImage: TdxOfficeImageReference;
    procedure SetImportSource(const Value: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference>);
  protected
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure ModifyModel; override;
    procedure SetupFloatingObjectProperties(AProperties: TdxFloatingObjectProperties; ARun: TdxFloatingObjectAnchorRun); override;
    function CalculateImageScale(AImage: TdxOfficeImageReference): Integer;
    procedure SetupShape(AShape: TdxShape; ARun: TdxFloatingObjectAnchorRun); override;
    function EnsureImageLoaded: Boolean; virtual;
    function CreateDefaultFloatingObjectContent(ARun: TdxFloatingObjectAnchorRun): TdxCustomObjectContent; override;

    property ImportSource: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference> read FImportSource write SetImportSource;
  public
    destructor Destroy; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

{ TdxMoveSelectionToBeginOfParagraph }

function TdxMoveSelectionToBeginOfParagraph.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxMoveSelectionToBeginOfParagraph.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxMoveSelectionToBeginOfParagraph.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxMoveSelectionToBeginOfParagraph.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxMoveSelectionToBeginOfParagraph.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

class function TdxMoveSelectionToBeginOfParagraph.GetDescription: string;
begin
  Result := 'InternalError';
end;

function TdxMoveSelectionToBeginOfParagraph.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxMoveSelectionToBeginOfParagraph.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := ActivePieceTable.Paragraphs[APos.ParagraphIndex].LogPosition;
end;

{ TdxInsertFloatingObjectCommandBase }

class function TdxInsertFloatingObjectCommandBase.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := nil;
end;

function TdxInsertFloatingObjectCommandBase.CreateDeleteCommand: TdxCommand;
begin
  Result := TdxMoveSelectionToBeginOfParagraph.Create(RichEditControl);
end;

{ TdxInsertFloatingObjectCoreCommandBase }

function TdxInsertFloatingObjectCoreCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Page;
end;

procedure TdxInsertFloatingObjectCoreCommandBase.ModifyModel;
var
  ASelection: TdxSelection;
  APos: TdxDocumentLogPosition;
  ARun: TdxFloatingObjectAnchorRun;
  AContent: TdxCustomObjectContent;
  AShape: TdxShape;
  AFloatingObjectProperties: TdxFloatingObjectProperties;
begin
  ASelection := DocumentModel.Selection;
  APos := ActivePieceTable.Paragraphs[ASelection.Interval.&End.ParagraphIndex].LogPosition;

  ActivePieceTable.InsertFloatingObjectAnchor(APos);
  ARun := TdxFloatingObjectAnchorRun(ActivePieceTable.LastInsertedFloatingObjectAnchorRunInfo.Run);

  AContent := CreateDefaultFloatingObjectContent(ARun);
  ARun.SetContent(AContent);

  AShape := ARun.Shape;
  AShape.BeginInit;
  try
    SetupShape(AShape, ARun);
  finally
    AShape.EndInit;
  end;

  AFloatingObjectProperties := ARun.FloatingObjectProperties;
  AFloatingObjectProperties.BeginInit;
  try
    SetupFloatingObjectProperties(AFloatingObjectProperties, ARun);
  finally
    AFloatingObjectProperties.EndInit;
  end;
end;

function TdxInsertFloatingObjectCoreCommandBase.GetZOrder: Integer;
var
  AFloatingObjectList: TdxIZOrderedObjectList;
  AMaxIndex: Integer;
begin
  AFloatingObjectList := ActivePieceTable.GetFloatingObjectList;
  try
    if AFloatingObjectList.Count = 0 then
      Result := 251658240
    else
    begin
      AMaxIndex := AFloatingObjectList.Count - 1;
      Result := AFloatingObjectList[AMaxIndex].ZOrder + 1024;
    end;
  finally
    AFloatingObjectList.Free;
  end;
end;

procedure TdxInsertFloatingObjectCoreCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
    AState.Enabled := not ActivePieceTable.IsTextBox;

  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.FloatingObjects, AState.Enabled);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;


{ TdxInsertFloatingObjectTextBoxCoreCommand }

class function TdxInsertFloatingObjectTextBoxCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTextBoxMenuCaption);
end;

class function TdxInsertFloatingObjectTextBoxCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.TextBox;
end;

class function TdxInsertFloatingObjectTextBoxCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTextBoxDescription);
end;

procedure TdxInsertFloatingObjectTextBoxCoreCommand.SetupFloatingObjectProperties(AProperties: TdxFloatingObjectProperties; ARun: TdxFloatingObjectAnchorRun);
begin
  AProperties.ActualSize := DocumentModel.UnitConverter.DocumentsToModelUnits(TSize.Create(600, 300));
  AProperties.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Column;
  AProperties.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment.Center;
  AProperties.VerticalPositionType := TdxFloatingObjectVerticalPositionType.Paragraph;
  AProperties.ZOrder := GetZOrder;
end;

procedure TdxInsertFloatingObjectTextBoxCoreCommand.SetupShape(AShape: TdxShape; ARun: TdxFloatingObjectAnchorRun);
begin
  AShape.FillColor := TdxAlphaColors.White;
  AShape.OutlineColor := TdxAlphaColors.Black;
  AShape.OutlineWidth := DocumentModel.UnitConverter.TwipsToModelUnits(10);
end;

function TdxInsertFloatingObjectTextBoxCoreCommand.CreateDefaultFloatingObjectContent(ARun: TdxFloatingObjectAnchorRun): TdxCustomObjectContent;
var
  ATextBoxContentType: TdxTextBoxContentType;
  AContent: TdxTextBoxFloatingObjectContent;
  AAction: TdxAction;
  ARichEditControl: IdxRichEditControl;
  APreferredPageIndex: Integer;
begin
  ARichEditControl := RichEditControl;
  APreferredPageIndex := CaretPosition.PreferredPageIndex;
  ATextBoxContentType := TdxTextBoxContentType.Create(DocumentModel);
  AContent := TdxTextBoxFloatingObjectContent.Create(ARun, ATextBoxContentType);
  AContent.TextBoxProperties.ResizeShapeToFitText := True;
  AAction := procedure
    var
      ACommand: TdxChangeActivePieceTableCommand;
    begin
      ACommand := TdxChangeActivePieceTableCommand.Create(ARichEditControl, TdxPieceTable(ATextBoxContentType.PieceTable), nil,
        APreferredPageIndex);
      try
        ACommand.Execute;
      finally
        ACommand.Free;
      end;
    end;
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(RichEditControl.DocumentModel, AAction);
  Result := AContent;
end;

{ TdxInsertFloatingObjectPictureCoreCommand }

destructor TdxInsertFloatingObjectPictureCoreCommand.Destroy;
begin
  ImportSource := nil;
  FreeAndNil(FImage);
  inherited Destroy;
end;

class function TdxInsertFloatingObjectPictureCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertFloatingObjectPictureMenuCaption);
end;

class function TdxInsertFloatingObjectPictureCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertPicture;
end;

class function TdxInsertFloatingObjectPictureCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertFloatingObjectPictureDescription);
end;


function TdxInsertFloatingObjectPictureCoreCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Column;
end;

procedure TdxInsertFloatingObjectPictureCoreCommand.ModifyModel;
begin
  if FImportSource = nil then
    Exit;
  if EnsureImageLoaded then
    inherited ModifyModel;
end;

procedure TdxInsertFloatingObjectPictureCoreCommand.SetImportSource(
  const Value: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference>);
begin
  FreeAndNil(FImportSource);
  FImportSource := Value;
end;

procedure TdxInsertFloatingObjectPictureCoreCommand.SetupFloatingObjectProperties(AProperties: TdxFloatingObjectProperties; ARun: TdxFloatingObjectAnchorRun);
var
  AContent: TdxPictureFloatingObjectContent;
  ARectangularObject: IdxRectangularScalableObject;
  AScale: Integer;
begin
  AContent := TdxPictureFloatingObjectContent(ARun.Content);
  ARectangularObject := ARun;
  AScale := CalculateImageScale(AContent.Image);
  ARectangularObject.ScaleX := AScale;
  ARectangularObject.ScaleY := AScale;

  AProperties.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Column;
  AProperties.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment.Center;
  AProperties.VerticalPositionType := TdxFloatingObjectVerticalPositionType.Paragraph;
  AProperties.LockAspectRatio := True;
  AProperties.ZOrder := GetZOrder;
end;

function TdxInsertFloatingObjectPictureCoreCommand.CalculateImageScale(AImage: TdxOfficeImageReference): Integer;
begin
  if ActiveView is TdxSimpleView then
    Exit(100);
  Result := TdxInsertPictureCoreCommand.CalculateImageScale(AImage, CaretPosition.LayoutPosition);
end;

procedure TdxInsertFloatingObjectPictureCoreCommand.SetupShape(AShape: TdxShape; ARun: TdxFloatingObjectAnchorRun);
begin
end;

function TdxInsertFloatingObjectPictureCoreCommand.EnsureImageLoaded: Boolean;
var
  AStream: TStream;
  AImporterOptions: IdxImporterOptions;
  AOptions: TObject;
begin
  if ImportSource = nil then
    Exit(False);
  if FImage <> nil then
    Exit(True);
  try
    AOptions := ImportSource.Importer.SetupLoading;
    try
      Supports(AOptions, IdxImporterOptions, AImporterOptions);
      AStream := ImportSource.GetStream;
      try
        FImage := ImportSource.Importer.LoadDocument(DocumentModel, AStream, AImporterOptions);

        FImage.Uri := ImportSource.FileName;
      finally
        AStream.Free;
      end;
      Result := True;
    finally
      AImporterOptions := nil;
      FreeAndNil(AOptions);
    end;
  except
    Result := False;
  end;
end;

function TdxInsertFloatingObjectPictureCoreCommand.CreateDefaultFloatingObjectContent(ARun: TdxFloatingObjectAnchorRun): TdxCustomObjectContent;
begin
  Result := TdxPictureFloatingObjectContent.Create(ARun, FImage);
end;

{ TdxInsertTextBoxCommand }

class function TdxInsertTextBoxCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertFloatingObjectTextBoxCoreCommand;
end;

class function TdxInsertTextBoxCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTextBox;
end;

{ TdxInsertFloatingObjectPictureCommand }

class function TdxInsertFloatingObjectPictureCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertFloatingObjectPictureCoreCommand;
end;

class function TdxInsertFloatingObjectPictureCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertFloatingPicture;
end;

procedure TdxInsertFloatingObjectPictureCommand.ForceExecuteCore(const AState: IdxCommandUIState);
var
  AImportHelper: TdxImportHelper<TdxOfficeImageFormat, TdxOfficeImageReference>;
  AInsertCommand: TdxInsertFloatingObjectPictureCoreCommand;
  AIntf: IInterface;
  AImportManagerService: IdxImportManagerService<TdxOfficeImageFormat, TdxOfficeImageReference>;
  AImportSource: TdxImportSource<TdxOfficeImageFormat, TdxOfficeImageReference>;
begin
  AIntf := TdxPictureFormatsManagerService.Create;
  AImportManagerService := AIntf as IdxImportManagerService<TdxOfficeImageFormat, TdxOfficeImageReference>;
  AImportHelper := TdxRichEditImageImportHelper.Create(DocumentModel);
  try
    AImportSource := AImportHelper.InvokeImportDialog(Control, AImportManagerService);
    if AImportSource = nil then
      Exit;
    AInsertCommand := TdxInsertFloatingObjectPictureCoreCommand(Commands[1]);
    AInsertCommand.ImportSource := AImportSource;
    if AInsertCommand.EnsureImageLoaded then
      inherited ForceExecuteCore(AState)
    else
      RichEditControl.ShowErrorMessage(cxGetResourceString(@sdxRichEditExceptionInvalidImageFile));
  finally
    AImportHelper.Free;
  end;
end;

{ TdxFloatingObjectCommandBase }

procedure TdxFloatingObjectCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable and DocumentModel.Selection.IsFloatingObjectSelected;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

function TdxFloatingObjectCommandBase.ChangeFloatingObjectProperty(AModifier: TdxFloatingObjectRunPropertyModifierBase;
  const AStart, AEnd: TdxDocumentModelPosition; const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  ARun: TdxFloatingObjectAnchorRun;
begin
  if AEnd.LogPosition - AStart.LogPosition <> 1 then
    Exit([]);

  ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(ActivePieceTable.Runs[AStart.RunIndex]);
  if ARun = nil then
    Exit([]);

  AModifier.ModifyFloatingObjectRun(ARun, AStart.RunIndex);
  Result := [];
end;

{ TdxChangeFloatingObjectPropertyCommandBase }

function TdxChangeFloatingObjectPropertyCommandBase<T>.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AModifier: TdxFloatingObjectRunPropertyModifier<T>;
begin
  AModifier := CreateModifier(AState);
  try
    Result := ChangeFloatingObjectProperty(AModifier, AStart, AEnd, AState);
  finally
    AModifier.Free;
  end;
end;

function TdxChangeFloatingObjectPropertyCommandBase<T>.GetCurrentPropertyValue(out AValue: T): Boolean;
var
  AModifier: TdxFloatingObjectRunPropertyModifier<T>;
  ARun: TdxFloatingObjectAnchorRun;
begin
  AModifier := CreateModifier(CreateDefaultCommandUIState);
  try
    ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(
      ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedStart.RunIndex]);
    if ARun <> nil then
    begin
      AValue := AModifier.GetFloatingObjectValue(ARun);
      Result := True;
    end
    else
    begin
      AValue := Default(T);
      Result := False;
    end;
  finally
    AModifier.Free;
  end;
end;

{ TdxChangeFloatingObjectTextWrapTypeCommandBase }

function TdxChangeFloatingObjectTextWrapTypeCommandBase.CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<TdxFloatingObjectTextWrapType>;
begin
  Result := TdxFloatingObjectRunTextWrapTypeModifier.Create(TextWrapType);
end;

procedure TdxChangeFloatingObjectTextWrapTypeCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Checked := IsChecked(AState);
end;

function TdxChangeFloatingObjectTextWrapTypeCommandBase.IsChecked(const AState: IdxCommandUIState): Boolean;
var
  AModifier: TdxFloatingObjectRunPropertyModifier<TdxFloatingObjectTextWrapType>;
  ARun: TdxFloatingObjectAnchorRun;
begin
  if not AState.Enabled then
    Exit(False);

  ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedStart.RunIndex]);
  if ARun = nil then
    Exit(False);

  AModifier := CreateModifier(AState);
  try
    Result := AModifier.GetFloatingObjectValue(ARun) = TextWrapType;
  finally
    AModifier.Free;
  end;
end;

{ TdxSetFloatingObjectSquareTextWrapTypeCommand }

class function TdxSetFloatingObjectSquareTextWrapTypeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectSquareTextWrapTypeDescription);
end;

class function TdxSetFloatingObjectSquareTextWrapTypeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectSquareTextWrapTypeMenuCaption);
end;

function TdxSetFloatingObjectSquareTextWrapTypeCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

class function TdxSetFloatingObjectSquareTextWrapTypeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectSquareTextWrapType;
end;

class function TdxSetFloatingObjectSquareTextWrapTypeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.WrapTextSquare;
end;

{ TdxSetFloatingObjectBehindTextWrapTypeCommand }

class function TdxSetFloatingObjectBehindTextWrapTypeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectBehindTextWrapTypeDescription);
end;

function TdxSetFloatingObjectBehindTextWrapTypeCommand.GetIsBehindDoc: Boolean;
begin
  Result := True;
end;

class function TdxSetFloatingObjectBehindTextWrapTypeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectBehindTextWrapTypeMenuCaption);
end;

function TdxSetFloatingObjectBehindTextWrapTypeCommand.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AModifier: TdxFloatingObjectRunPropertyModifier<Boolean>;
begin
  AModifier := CreateModifier(AState);
  try
    Result := ChangeFloatingObjectProperty(AModifier, AStart, AEnd, AState);
  finally
    AModifier.Free;
  end;
end;

function TdxSetFloatingObjectBehindTextWrapTypeCommand.CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Boolean>;
begin
  Result := TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier.Create(IsBehindDoc);
end;

procedure TdxSetFloatingObjectBehindTextWrapTypeCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Checked := IsChecked(AState);
end;

class function TdxSetFloatingObjectBehindTextWrapTypeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectBehindTextWrapType;
end;

class function TdxSetFloatingObjectBehindTextWrapTypeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.WrapTextBehindText;
end;

function TdxSetFloatingObjectBehindTextWrapTypeCommand.IsChecked(const AState: IdxCommandUIState): Boolean;
var
  AModifier: TdxFloatingObjectRunPropertyModifier<Boolean>;
  ARun: TdxFloatingObjectAnchorRun;
begin
  if not AState.Enabled then
    Exit(False);

  ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedStart.RunIndex]);
  if ARun = nil then
    Exit(False);

  AModifier := CreateModifier(AState);
  try
    Result := (AModifier.GetFloatingObjectValue(ARun) = True) and
      (ARun.FloatingObjectProperties.TextWrapType = TdxFloatingObjectTextWrapType.None);
  finally
    AModifier.Free;
  end;
end;

{ TdxSetFloatingObjectInFrontOfTextWrapTypeCommand }

class function TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectInFrontOfTextWrapTypeDescription);
end;

function TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.GetIsBehindDoc: Boolean;
begin
  Result := False;
end;

class function TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectInFrontOfTextWrapTypeMenuCaption);
end;

function TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AModifier: TdxFloatingObjectRunPropertyModifier<Boolean>;
begin
  AModifier := CreateModifier(AState);
  try
    Result := ChangeFloatingObjectProperty(AModifier, AStart, AEnd, AState);
  finally
    AModifier.Free;
  end;
end;

function TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Boolean>;
begin
  Result := TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier.Create(IsBehindDoc);
end;

procedure TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Checked := IsChecked(AState);
end;

class function TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectInFrontOfTextWrapType;
end;

class function TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.WrapTextInFrontOfText;
end;

function TdxSetFloatingObjectInFrontOfTextWrapTypeCommand.IsChecked(const AState: IdxCommandUIState): Boolean;
var
  AModifier: TdxFloatingObjectRunPropertyModifier<Boolean>;
  ARun: TdxFloatingObjectAnchorRun;
begin
  if not AState.Enabled then
    Exit(False);

  ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedStart.RunIndex]);
  if ARun = nil then
    Exit(False);

  AModifier := CreateModifier(AState);
  try
    Result := (AModifier.GetFloatingObjectValue(ARun) = False) and
      (ARun.FloatingObjectProperties.TextWrapType = TdxFloatingObjectTextWrapType.None);
  finally
    AModifier.Free;
  end;
end;

{ TdxSetFloatingObjectThroughTextWrapTypeCommand }

class function TdxSetFloatingObjectThroughTextWrapTypeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectThroughTextWrapTypeDescription);
end;

class function TdxSetFloatingObjectThroughTextWrapTypeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectThroughTextWrapTypeMenuCaption);
end;

function TdxSetFloatingObjectThroughTextWrapTypeCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Through;
end;

class function TdxSetFloatingObjectThroughTextWrapTypeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectThroughTextWrapType;
end;

class function TdxSetFloatingObjectThroughTextWrapTypeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.WrapTextThrough;
end;

{ TdxSetFloatingObjectTightTextWrapTypeCommand }

class function TdxSetFloatingObjectTightTextWrapTypeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTightTextWrapTypeDescription);
end;

class function TdxSetFloatingObjectTightTextWrapTypeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTightTextWrapTypeMenuCaption);
end;

function TdxSetFloatingObjectTightTextWrapTypeCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Tight;
end;

class function TdxSetFloatingObjectTightTextWrapTypeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectTightTextWrapType;
end;

class function TdxSetFloatingObjectTightTextWrapTypeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.WrapTextTight;
end;

{ TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand }

class function TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTopAndBottomTextWrapTypeDescription);
end;

class function TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTopAndBottomTextWrapTypeMenuCaption);
end;

function TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.TopAndBottom;
end;

class function TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectTopAndBottomTextWrapType;
end;

class function TdxSetFloatingObjectTopAndBottomTextWrapTypeCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.WrapTextTopAndBottom;
end;

{ TdxChangeFloatingObjectAlignmentCommandBase }

function TdxChangeFloatingObjectAlignmentCommandBase.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
var
  AModifier: TdxFloatingObjectRunPropertyModifierBase;
begin
  AModifier := TdxFloatingObjectRunHorizontalPositionAlignmentModifier.Create(HorizontalPositionAlignment);
  try
    Result := ChangeFloatingObjectProperty(AModifier, AStart, AEnd, AState);
  finally
    AModifier.Free;
  end;
  AModifier := TdxFloatingObjectRunVerticalPositionAlignmentModifier.Create(VerticalPositionAlignment);
  try
    Result := Result + ChangeFloatingObjectProperty(AModifier, AStart, AEnd, AState);
  finally
    AModifier.Free;
  end;
  AModifier := TdxFloatingObjectRunHorizontalPositionTypeModifier.Create(HorizontalPositionType);
  try
    Result := Result + ChangeFloatingObjectProperty(AModifier, AStart, AEnd, AState);
  finally
    AModifier.Free;
  end;
  AModifier := TdxFloatingObjectRunVerticalPositionTypeModifier.Create(VerticalPositionType);
  try
    Result := Result + ChangeFloatingObjectProperty(AModifier, AStart, AEnd, AState);
  finally
    AModifier.Free;
  end;
end;

{ TdxSetFloatingObjectTopLeftAlignmentCommand }

class function TdxSetFloatingObjectTopLeftAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTopLeftAlignmentDescription);
end;

function TdxSetFloatingObjectTopLeftAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Left;
end;

function TdxSetFloatingObjectTopLeftAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Top;
end;

function TdxSetFloatingObjectTopLeftAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectTopLeftAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTopLeftAlignmentMenuCaption);
end;

function TdxSetFloatingObjectTopLeftAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectTopLeftAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectTopLeftAlignment;
end;

class function TdxSetFloatingObjectTopLeftAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionTopLeft;
end;

function TdxSetFloatingObjectTopLeftAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxSetFloatingObjectTopCenterAlignmentCommand }

class function TdxSetFloatingObjectTopCenterAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTopCenterAlignmentDescription);
end;

function TdxSetFloatingObjectTopCenterAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Center;
end;

function TdxSetFloatingObjectTopCenterAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Top;
end;

function TdxSetFloatingObjectTopCenterAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectTopCenterAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTopCenterAlignmentMenuCaption);
end;

function TdxSetFloatingObjectTopCenterAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectTopCenterAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectTopCenterAlignment;
end;

class function TdxSetFloatingObjectTopCenterAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionTopCenter;
end;

function TdxSetFloatingObjectTopCenterAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxSetFloatingObjectTopRightAlignmentCommand }

class function TdxSetFloatingObjectTopRightAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTopRightAlignmentDescription);
end;

function TdxSetFloatingObjectTopRightAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Right;
end;

function TdxSetFloatingObjectTopRightAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Top;
end;

function TdxSetFloatingObjectTopRightAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectTopRightAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectTopRightAlignmentMenuCaption);
end;

function TdxSetFloatingObjectTopRightAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectTopRightAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectTopRightAlignment;
end;

class function TdxSetFloatingObjectTopRightAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionTopRight;
end;

function TdxSetFloatingObjectTopRightAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxSetFloatingObjectMiddleLeftAlignmentCommand }

class function TdxSetFloatingObjectMiddleLeftAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectMiddleLeftAlignmentDescription);
end;

function TdxSetFloatingObjectMiddleLeftAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Left;
end;

function TdxSetFloatingObjectMiddleLeftAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Center;
end;

function TdxSetFloatingObjectMiddleLeftAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectMiddleLeftAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectMiddleLeftAlignmentMenuCaption);
end;

function TdxSetFloatingObjectMiddleLeftAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectMiddleLeftAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectMiddleLeftAlignment;
end;

class function TdxSetFloatingObjectMiddleLeftAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionMiddleLeft;
end;

function TdxSetFloatingObjectMiddleLeftAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxSetFloatingObjectMiddleCenterAlignmentCommand }

class function TdxSetFloatingObjectMiddleCenterAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectMiddleCenterAlignmentDescription);
end;

function TdxSetFloatingObjectMiddleCenterAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Center;
end;

function TdxSetFloatingObjectMiddleCenterAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Center;
end;

function TdxSetFloatingObjectMiddleCenterAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectMiddleCenterAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectMiddleCenterAlignmentMenuCaption);
end;

function TdxSetFloatingObjectMiddleCenterAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectMiddleCenterAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectMiddleCenterAlignment;
end;

class function TdxSetFloatingObjectMiddleCenterAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionMiddleCenter;
end;

function TdxSetFloatingObjectMiddleCenterAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxSetFloatingObjectMiddleRightAlignmentCommand }

class function TdxSetFloatingObjectMiddleRightAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectMiddleRightAlignmentDescription);
end;

function TdxSetFloatingObjectMiddleRightAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Right;
end;

function TdxSetFloatingObjectMiddleRightAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Center;
end;

function TdxSetFloatingObjectMiddleRightAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectMiddleRightAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectMiddleRightAlignmentMenuCaption);
end;

function TdxSetFloatingObjectMiddleRightAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectMiddleRightAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectMiddleRightAlignment;
end;

class function TdxSetFloatingObjectMiddleRightAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionMiddleRight;
end;

function TdxSetFloatingObjectMiddleRightAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxSetFloatingObjectBottomLeftAlignmentCommand }

class function TdxSetFloatingObjectBottomLeftAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectBottomLeftAlignmentDescription);
end;

function TdxSetFloatingObjectBottomLeftAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Left;
end;

function TdxSetFloatingObjectBottomLeftAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Bottom;
end;

function TdxSetFloatingObjectBottomLeftAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectBottomLeftAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectBottomLeftAlignmentMenuCaption);
end;

function TdxSetFloatingObjectBottomLeftAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectBottomLeftAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectBottomLeftAlignment;
end;

class function TdxSetFloatingObjectBottomLeftAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionBottomLeft;
end;

function TdxSetFloatingObjectBottomLeftAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxSetFloatingObjectBottomCenterAlignmentCommand }

class function TdxSetFloatingObjectBottomCenterAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectBottomCenterAlignmentDescription);
end;

function TdxSetFloatingObjectBottomCenterAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Center;
end;

function TdxSetFloatingObjectBottomCenterAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Bottom;
end;

function TdxSetFloatingObjectBottomCenterAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectBottomCenterAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectBottomCenterAlignmentMenuCaption);
end;

function TdxSetFloatingObjectBottomCenterAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectBottomCenterAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectBottomCenterAlignment;
end;

class function TdxSetFloatingObjectBottomCenterAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionBottomCenter;
end;

function TdxSetFloatingObjectBottomCenterAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxSetFloatingObjectBottomRightAlignmentCommand }

class function TdxSetFloatingObjectBottomRightAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectBottomRightAlignmentDescription);
end;

function TdxSetFloatingObjectBottomRightAlignmentCommand.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.Right;
end;

function TdxSetFloatingObjectBottomRightAlignmentCommand.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.Bottom;
end;

function TdxSetFloatingObjectBottomRightAlignmentCommand.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class function TdxSetFloatingObjectBottomRightAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetFloatingObjectBottomRightAlignmentMenuCaption);
end;

function TdxSetFloatingObjectBottomRightAlignmentCommand.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Margin;
end;

class function TdxSetFloatingObjectBottomRightAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetFloatingObjectBottomRightAlignment;
end;

class function TdxSetFloatingObjectBottomRightAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ImagePositionBottomRight;
end;

function TdxSetFloatingObjectBottomRightAlignmentCommand.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxFloatingObjectBringForwardCommandBase }

constructor TdxFloatingObjectBringForwardCommandBase.Create(const AControl: IdxRichEditControl);
var
  ARun: TdxFloatingObjectAnchorRun;
begin
  inherited Create(AControl);
  ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedStart.RunIndex]);
  if ARun <> nil then
    FFloatingObjectProperties := ARun.FloatingObjectProperties;
end;

function TdxFloatingObjectBringForwardCommandBase.GetZOrder: Integer;
begin
  Result := FFloatingObjectProperties.ZOrder;
end;

procedure TdxFloatingObjectBringForwardCommandBase.ChangeZOrder;
var
  AFloatingObjectList: TdxIZOrderedObjectList;
begin
  if FloatingObjectProperties = nil then
    Exit;

  AFloatingObjectList := ActivePieceTable.GetFloatingObjectList;
  try
    ChangeZOrderCore(AFloatingObjectList, AFloatingObjectList.IndexOf(FloatingObjectProperties));
  finally
    AFloatingObjectList.Free;
  end;
end;

{ TdxFloatingObjectBringForwardCommand }

function TdxFloatingObjectBringForwardCommand.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
begin
  ChangeZOrder;
  Result := [];
end;

procedure TdxFloatingObjectBringForwardCommand.ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer);
var
  AManager: TdxZOrderManager;
begin
  AManager := TdxZOrderManager.Create;
  try
    AManager.BringForward(AObjects, AObjectIndex);
  finally
    AManager.Free;
  end;
end;

class function TdxFloatingObjectBringForwardCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectBringForwardDescription);
end;

class function TdxFloatingObjectBringForwardCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectBringForwardMenuCaption);
end;

class function TdxFloatingObjectBringForwardCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FloatingObjectBringForward;
end;

class function TdxFloatingObjectBringForwardCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BringForward;
end;

{ TdxFloatingObjectBringToFrontCommand }

function TdxFloatingObjectBringToFrontCommand.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
begin
  ChangeZOrder;
  Result := [];
end;

procedure TdxFloatingObjectBringToFrontCommand.ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer);
var
  AManager: TdxZOrderManager;
begin
  AManager := TdxZOrderManager.Create;
  try
    AManager.BringToFront(AObjects, AObjectIndex);
  finally
    AManager.Free;
  end;
end;

class function TdxFloatingObjectBringToFrontCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectBringToFrontDescription);
end;

class function TdxFloatingObjectBringToFrontCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectBringToFrontMenuCaption);
end;

class function TdxFloatingObjectBringToFrontCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FloatingObjectBringToFront;
end;

class function TdxFloatingObjectBringToFrontCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BringToFront;
end;

{ TdxFloatingObjectBringInFrontOfTextCommand }

class function TdxFloatingObjectBringInFrontOfTextCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectBringInFrontOfTextDescription);
end;

function TdxFloatingObjectBringInFrontOfTextCommand.GetIsBehindDoc: Boolean;
begin
  Result := False;
end;

class function TdxFloatingObjectBringInFrontOfTextCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectBringInFrontOfTextMenuCaption);
end;

class function TdxFloatingObjectBringInFrontOfTextCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FloatingObjectBringInFrontOfText;
end;

class function TdxFloatingObjectBringInFrontOfTextCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BringInFrontOfText;
end;

function TdxFloatingObjectBringInFrontOfTextCommand.CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Boolean>;
begin
  Result := TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier.Create(IsBehindDoc);
end;

{ TdxFloatingObjectSendBackwardCommandBase }

constructor TdxFloatingObjectSendBackwardCommandBase.Create(const AControl: IdxRichEditControl);
var
  ARun: TdxFloatingObjectAnchorRun;
begin
  inherited Create(AControl);
  ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(ActivePieceTable.Runs[DocumentModel.Selection.Interval.NormalizedStart.RunIndex]);
  if ARun <> nil then
    FFloatingObjectProperties := ARun.FloatingObjectProperties;
end;

function TdxFloatingObjectSendBackwardCommandBase.GetZOrder: Integer;
begin
  Result := FFloatingObjectProperties.ZOrder;
end;

procedure TdxFloatingObjectSendBackwardCommandBase.ChangeZOrder;
var
  AFloatingObjectList: TdxIZOrderedObjectList;
begin
  if FloatingObject = nil then
    Exit;

  AFloatingObjectList := ActivePieceTable.GetFloatingObjectList;
  try
    ChangeZOrderCore(AFloatingObjectList, AFloatingObjectList.IndexOf(FloatingObject));
  finally
    AFloatingObjectList.Free;
  end;
end;

{ TdxFloatingObjectSendBackwardCommand }

function TdxFloatingObjectSendBackwardCommand.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
begin
  ChangeZOrder;
  Result := [];
end;

procedure TdxFloatingObjectSendBackwardCommand.ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer);
var
  AManager: TdxZOrderManager;
begin
  AManager := TdxZOrderManager.Create;
  try
    AManager.SendBackward(AObjects, AObjectIndex);
  finally
    AManager.Free;
  end;
end;

class function TdxFloatingObjectSendBackwardCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectSendBackwardDescription);
end;

class function TdxFloatingObjectSendBackwardCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectSendBackwardMenuCaption);
end;

class function TdxFloatingObjectSendBackwardCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FloatingObjectSendBackward;
end;

class function TdxFloatingObjectSendBackwardCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SendBackward;
end;

{ TdxFloatingObjectSendToBackCommand }

function TdxFloatingObjectSendToBackCommand.ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
begin
  ChangeZOrder;
  Result := [];
end;

procedure TdxFloatingObjectSendToBackCommand.ChangeZOrderCore(const AObjects: TdxIZOrderedObjectList; AObjectIndex: Integer);
var
  AManager: TdxZOrderManager;
begin
  AManager := TdxZOrderManager.Create;
  try
    AManager.SendToBack(AObjects, AObjectIndex);
  finally
    AManager.Free;
  end;
end;

class function TdxFloatingObjectSendToBackCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectSendToBackDescription);
end;

class function TdxFloatingObjectSendToBackCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectSendToBackMenuCaption);
end;

class function TdxFloatingObjectSendToBackCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FloatingObjectSendToBack;
end;

class function TdxFloatingObjectSendToBackCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SendToBack;
end;

{ TdxFloatingObjectSendBehindTextCommand }

class function TdxFloatingObjectSendBehindTextCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectSendBehindTextDescription);
end;

function TdxFloatingObjectSendBehindTextCommand.GetIsBehindDoc: Boolean;
begin
  Result := True;
end;

class function TdxFloatingObjectSendBehindTextCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectSendBehindTextMenuCaption);
end;

class function TdxFloatingObjectSendBehindTextCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FloatingObjectSendBehindText;
end;

class function TdxFloatingObjectSendBehindTextCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SendBehindText;
end;

function TdxFloatingObjectSendBehindTextCommand.CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Boolean>;
begin
  Result := TdxFloatingObjectRunIsBehindDocTextWrapTypeNoneModifier.Create(IsBehindDoc);
end;

{ TdxChangeFloatingObjectAlignmentCommand }

class function TdxChangeFloatingObjectAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFloatingObjectAlignment;
end;

class function TdxChangeFloatingObjectAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFloatingObjectAlignmentDescription);
end;

class function TdxChangeFloatingObjectAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFloatingObjectAlignmentMenuCaption);
end;

procedure TdxChangeFloatingObjectAlignmentCommand.ExecuteCore;
begin
end;

procedure TdxChangeFloatingObjectAlignmentCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable and (DocumentModel.Selection.Length = 1)
    and (ActivePieceTable.Runs[DocumentModel.Selection.Interval.Start.RunIndex] is TdxFloatingObjectAnchorRun);
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxChangeFloatingObjectTextWrapTypeCommand }

class function TdxChangeFloatingObjectTextWrapTypeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFloatingObjectTextWrapType;
end;

class function TdxChangeFloatingObjectTextWrapTypeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFloatingObjectTextWrapTypeDescription);
end;

class function TdxChangeFloatingObjectTextWrapTypeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeFloatingObjectTextWrapTypeMenuCaption);
end;

procedure TdxChangeFloatingObjectTextWrapTypeCommand.ExecuteCore;
begin
end;

procedure TdxChangeFloatingObjectTextWrapTypeCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable and (DocumentModel.Selection.Length = 1) and
    (ActivePieceTable.Runs[DocumentModel.Selection.Interval.Start.RunIndex] is TdxFloatingObjectAnchorRun);
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxChangeFloatingObjectTextWrapTypeMenuCommand }

procedure TdxChangeFloatingObjectTextWrapTypeMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Visible := AState.Enabled;
end;

{ TdxFloatingObjectBringForwardPlaceholderCommand }

class function TdxFloatingObjectBringForwardPlaceholderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FloatingObjectBringForwardPlaceholder;
end;

class function TdxFloatingObjectBringForwardPlaceholderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectBringForwardPlaceholderDescription);
end;

class function TdxFloatingObjectBringForwardPlaceholderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectBringForwardPlaceholderMenuCaption);
end;

procedure TdxFloatingObjectBringForwardPlaceholderCommand.ExecuteCore;
begin
end;

procedure TdxFloatingObjectBringForwardPlaceholderCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable and (DocumentModel.Selection.Length = 1) and
    (ActivePieceTable.Runs[DocumentModel.Selection.Interval.Start.RunIndex] is TdxFloatingObjectAnchorRun);
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxFloatingObjectBringForwardMenuCommand }

procedure TdxFloatingObjectBringForwardMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Visible := AState.Enabled;
end;

{ TdxFloatingObjectSendBackwardPlaceholderCommand }

class function TdxFloatingObjectSendBackwardPlaceholderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FloatingObjectSendBackwardPlaceholder;
end;

class function TdxFloatingObjectSendBackwardPlaceholderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectSendBackwardPlaceholderDescription);
end;

class function TdxFloatingObjectSendBackwardPlaceholderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandFloatingObjectSendBackwardPlaceholderMenuCaption);
end;

procedure TdxFloatingObjectSendBackwardPlaceholderCommand.ExecuteCore;
begin
end;

procedure TdxFloatingObjectSendBackwardPlaceholderCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := (IsContentEditable and (DocumentModel.Selection.Length = 1)) and (ActivePieceTable.Runs[DocumentModel.Selection.Interval.Start.RunIndex] is TdxFloatingObjectAnchorRun);
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxFloatingObjectSendBackwardMenuCommand }

procedure TdxFloatingObjectSendBackwardMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Visible := AState.Enabled;
end;

{ TdxChangeFloatingObjectFillColorCommand }

class function TdxChangeFloatingObjectFillColorCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFloatingObjectFillColor;
end;

class function TdxChangeFloatingObjectFillColorCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.FloatingObjectFillColor;
end;

class function TdxChangeFloatingObjectFillColorCommand.GetMenuCaption: string;
begin
  Result := sdxRichEditCommandChangeFloatingObjectFillColorMenuCaption;
end;

class function TdxChangeFloatingObjectFillColorCommand.GetDescription: string;
begin
  Result := sdxRichEditCommandChangeFloatingObjectFillColorDescription;
end;

function TdxChangeFloatingObjectFillColorCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<TdxAlphaColor>.Create;
end;

function TdxChangeFloatingObjectFillColorCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<TdxAlphaColor>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxAlphaColor>;
begin
  if not Supports(AState, IdxValueBasedCommandUIState<TdxAlphaColor>, AValueBasedState) then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxFloatingObjectRunFillColorModifier.Create(AValueBasedState.Value);
end;

procedure TdxChangeFloatingObjectFillColorCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
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

{ TdxChangeFloatingObjectOutlineColorCommand }

class function TdxChangeFloatingObjectOutlineColorCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFloatingObjectOutlineColor;
end;

class function TdxChangeFloatingObjectOutlineColorCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.FloatingObjectOutlineColor;
end;

class function TdxChangeFloatingObjectOutlineColorCommand.GetMenuCaption: string;
begin
  Result := sdxRichEditCommandChangeFloatingObjectOutlineColorMenuCaption;
end;

class function TdxChangeFloatingObjectOutlineColorCommand.GetDescription: string;
begin
  Result := sdxRichEditCommandChangeFloatingObjectOutlineColorDescription;
end;

function TdxChangeFloatingObjectOutlineColorCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AResult: TdxDefaultValueBasedCommandUIState<TdxAlphaColor>;
begin
  AResult := TdxDefaultValueBasedCommandUIState<TdxAlphaColor>.Create;
  Result := AResult;
end;

function TdxChangeFloatingObjectOutlineColorCommand.CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<TdxAlphaColor>;
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxAlphaColor>;
begin
  if not Supports(AState, IdxValueBasedCommandUIState<TdxAlphaColor>, AValueBasedState) then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxFloatingObjectRunOutlineColorModifier.Create(AValueBasedState.Value);
end;

procedure TdxChangeFloatingObjectOutlineColorCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
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

{ TdxChangeFloatingObjectOutlineWidthCommand }

class function TdxChangeFloatingObjectOutlineWidthCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeFloatingObjectOutlineWeight;
end;

class function TdxChangeFloatingObjectOutlineWidthCommand.GetMenuCaption: string;
begin
  Result := sdxRichEditCommandChangeFloatingObjectOutlineWidthMenuCaption;
end;

class function TdxChangeFloatingObjectOutlineWidthCommand.GetDescription: string;
begin
  Result := sdxRichEditCommandChangeFloatingObjectOutlineWidthDescription;
end;

function TdxChangeFloatingObjectOutlineWidthCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AResult: TdxDefaultValueBasedCommandUIState<Integer>;
begin
  AResult := TdxDefaultValueBasedCommandUIState<Integer>.Create;
  Result := AResult;
end;

function TdxChangeFloatingObjectOutlineWidthCommand.CreateModifier(const AState: IdxCommandUIState): TdxFloatingObjectRunPropertyModifier<Integer>;
var
  AValueBasedState: IdxValueBasedCommandUIState<Integer>;
begin
  if not Supports(AState, IdxValueBasedCommandUIState<Integer>, AValueBasedState) then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxFloatingObjectRunOutlineWidthAndColorModifier.Create(AValueBasedState.Value);
end;

procedure TdxChangeFloatingObjectOutlineWidthCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<Integer>;
  AValue: Integer;
begin
  inherited UpdateUIStateCore(AState);
  if Supports(AState, IdxValueBasedCommandUIState<Integer>, AValueBasedState) then
  begin
    GetCurrentPropertyValue(AValue);
    AValueBasedState.Value := AValue;
  end;
end;

end.
