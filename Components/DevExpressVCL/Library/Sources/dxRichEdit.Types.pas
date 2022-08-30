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

unit dxRichEdit.Types;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Rtti,
  cxGeometry, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Token,
  dxRichEdit.Commands.IDs,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.NativeApi;

type
  TdxMeasurementUnit = TdxRichEditDocumentUnit;
  TdxHeaderFooterType = TdxRichEditHeaderFooterType;

  TdxRichEditAutoSizeMode = (None, Horizontal, Vertical, Both);

  TdxRichEditViewType = (
    Simple,
    Draft,
    PrintLayout);

  TdxRichEditBaseValueSource = (
    Auto,
    Document,
    Control);

  TdxPageBreakInsertMode = (
    NewLine,
    CurrentLine);

  TdxLineBreakSubstitute = (
    None,
    Paragraph,
    Space);

  TdxFieldsHighlightMode = (Auto, Never, Always);

  TdxRichEditTableGridLinesVisibility = (
    Auto,
    Visible,
    Hidden
  );

  TdxRichEditBookmarkVisibility = (
    Auto,
    Visible,
    Hidden);

  TdxCalculationModeType = (Automatic, Manual);

  TdxRichEditRulerVisibility = (Auto, Visible, Hidden);

  TdxRichEditScrollbarVisibility = (Auto, Visible, Hidden);

  { TdxPageFormattedEventArgs }

  TdxPageFormattedEventArgs = class(TdxEventArgs)
  strict private
    FPageIndex: Integer;
  public
    constructor Create(APageIndex: Integer);

    property PageIndex: Integer read FPageIndex;
  end;

  TdxPageFormattedEvent = procedure(Sender: TObject; E: TdxPageFormattedEventArgs) of object;
  TdxPageFormattedEventHandler = TdxMulticastMethod<TdxPageFormattedEvent>;

  TdxDocumentLayoutInvalidatedEventArgs = TdxPageFormattedEventArgs;
  TdxDocumentLayoutInvalidatedEvent = procedure(Sender: TObject; E: TdxDocumentLayoutInvalidatedEventArgs) of object;
  TdxDocumentLayoutInvalidatedEventHandler = TdxMulticastMethod<TdxDocumentLayoutInvalidatedEvent>;

  { TdxRichEditShortCutEventArgs }

  TdxRichEditShortCutEventArgs = class(TdxEventArgs)
  strict private
    FAllow: Boolean;
    FAltGr: Boolean;
    FCommand: TdxRichEditCommandId;
    FKeyData: Word;
    FHandled: Boolean;
    FShift: TShiftState;
  public
    constructor Create(ACommand: TdxRichEditCommandId;
      AKeyData: Word; AAltGr: Boolean; const AShift: TShiftState);

    property Allow: Boolean read FAllow write FAllow;
    property AltGr: Boolean read FAltGr;
    property Command: TdxRichEditCommandId read FCommand;
    property KeyData: Word read FKeyData;
    property Handled: Boolean read FHandled write FHandled;
    property Shift: TShiftState read FShift write FShift;
  end;
  TdxRichEditShortCutEvent = procedure(Sender: TObject; const Args: TdxRichEditShortCutEventArgs) of object;

  { TdxHyperlinkClickEventArgs }

  TdxHyperlinkClickEventArgs = class(TdxEventArgs)
  strict private
    FHandled: Boolean;
    FHyperlink: IdxRichEditHyperlink;
    FModifierKeys: TShortCut;
    function GetControl: Boolean;
    function GetAlt: Boolean;
    function GetShift: Boolean;
  public
    constructor Create(const AHyperlink: IdxRichEditHyperlink; AModifierKeys: TShortCut);

    property Handled: Boolean read FHandled write FHandled;
    property Hyperlink: IdxRichEditHyperlink read FHyperlink;
    property ModifierKeys: TShortCut read FModifierKeys;
    property Control: Boolean read GetControl;
    property Alt: Boolean read GetAlt;
    property Shift: Boolean read GetShift;
  end;

  TdxHyperlinkClickEvent = procedure(Sender: TObject; const Args: TdxHyperlinkClickEventArgs) of object;
  TdxHyperlinkClickEventHandler = TdxMulticastMethod<TdxHyperlinkClickEvent>;

  { TdxCustomizeMergeFieldsEventArgs }

  IdxRichEditMergeFieldName = interface
  ['{226EAFAC-78CB-4028-8F51-2D7ED7D786B4}']
    function GetDisplayName: string;
    function GetName: string;
    procedure SetDisplayName(const Value: string);
    procedure SetName(const Value: string);

    property DisplayName: string read GetDisplayName write SetDisplayName;
    property Name: string read GetName write SetName;
  end;

  TdxCustomizeMergeFieldsEventArgs = class(TdxEventArgs)
  strict private
    FMergeFieldsNames: TArray<IdxRichEditMergeFieldName>;
  public
    constructor Create(const AMergeFieldsNames: TArray<IdxRichEditMergeFieldName>);

    property MergeFieldsNames: TArray<IdxRichEditMergeFieldName> read FMergeFieldsNames write FMergeFieldsNames;
  end;

  TdxCustomizeMergeFieldsEvent = procedure(Sender: TObject; const Args: TdxCustomizeMergeFieldsEventArgs) of object;
  TdxCustomizeMergeFieldsEventHandler = TdxMulticastMethod<TdxCustomizeMergeFieldsEvent>;

  { TdxMailMergeCustomEventArgs }

  TdxMailMergeCustomEventArgs = class(TdxCancelEventArgs)
  strict private
    FDocumentServerOwner: IdxInnerRichEditDocumentContainerOwner;
    FTargetDocumentModel: TdxCustomDocumentModel;
    FTargetServer: IdxRichEditDocumentContainer;
    FOperationDescription: string;
    function GetDocument: IdxRichEditDocument;
  public
    constructor Create(const ADocumentServerOwner: IdxInnerRichEditDocumentContainerOwner;
      ATargetDocumentModel: TdxCustomDocumentModel);
    destructor Destroy; override;

    class function CreateDocumentServerForExistingDocumentModel(const ADocumentServerOwner: IdxInnerRichEditDocumentContainerOwner;
      ADocumentModel: TdxCustomDocumentModel): IdxRichEditDocumentContainer; static;
    procedure Clear;

    property OperationDescription: string read FOperationDescription write FOperationDescription;
    property Document: IdxRichEditDocument read GetDocument;
  end;

  TdxMailMergeStartedEventArgs = class(TdxMailMergeCustomEventArgs);
  TdxMailMergeStartedEvent = procedure(Sender: TObject; const Args: TdxMailMergeStartedEventArgs) of object;
  TdxMailMergeStartedEventHandler = TdxMulticastMethod<TdxMailMergeStartedEvent>;

  TdxMailMergeFinishedEventArgs = class(TdxMailMergeCustomEventArgs);
  TdxMailMergeFinishedEvent = procedure(Sender: TObject; const Args: TdxMailMergeFinishedEventArgs) of object;
  TdxMailMergeFinishedEventHandler = TdxMulticastMethod<TdxMailMergeFinishedEvent>;

  { TdxMailMergeGetTargetDocumentEventArgs }

  TdxMailMergeGetTargetDocumentEventArgs = class(TdxEventArgs)
  private
    FTargetDocument: IdxRichEditDocument;
  public
    property TargetDocument: IdxRichEditDocument read FTargetDocument write FTargetDocument;
  end;

  TdxMailMergeGetTargetDocumentEvent = procedure(Sender: TObject; const Args: TdxMailMergeGetTargetDocumentEventArgs) of object;

  TdxMailMergeGetTargetDocumentEventHandler = TdxMulticastMethod<TdxMailMergeGetTargetDocumentEvent>;

  { TdxMailMergeRecordCustomEventArgs }

  TdxMailMergeRecordCustomEventArgs = class(TdxCancelEventArgs)
  strict private
    FDocumentServerOwner: IdxInnerRichEditDocumentContainerOwner;
    FTargetDocumentModel: TdxCustomDocumentModel;
    FRecordDocumentModel: TdxCustomDocumentModel;
    FTargetServer: IdxRichEditDocumentContainer;
    FRecordServer: IdxRichEditDocumentContainer;
    function GetDocument: IdxRichEditDocument;
    function GetRecordDocument: IdxRichEditDocument;
  public
    constructor Create(const ADocumentServerOwner: IdxInnerRichEditDocumentContainerOwner;
      ATargetDocumentModel, ARecordDocumentModel: TdxCustomDocumentModel);
    destructor Destroy; override;

    procedure Clear;

    property Document: IdxRichEditDocument read GetDocument;
    property RecordDocument: IdxRichEditDocument read GetRecordDocument;
  end;

  TdxMailMergeRecordStartedEventArgs = class(TdxMailMergeRecordCustomEventArgs);
  TdxMailMergeRecordStartedEvent = procedure(Sender: TObject; const Args: TdxMailMergeRecordStartedEventArgs) of object;
  TdxMailMergeRecordStartedEventHandler = TdxMulticastMethod<TdxMailMergeRecordStartedEvent>;

  TdxMailMergeRecordFinishedEventArgs = class(TdxMailMergeRecordCustomEventArgs);
  TdxMailMergeRecordFinishedEvent = procedure(Sender: TObject; const Args: TdxMailMergeRecordFinishedEventArgs) of object;
  TdxMailMergeRecordFinishedEventHandler = TdxMulticastMethod<TdxMailMergeRecordFinishedEvent>;

  { TdxCalculateDocumentVariableEventArgs }

  TdxCalculateDocumentVariableEventArgs = class(TdxEventArgs)
  strict private
    FVariableName: string;
    FArguments: TdxArgumentCollection;
    FResult: TValue;
    FHandled: Boolean;
    FKeepLastParagraph: Boolean;
  public
    constructor Create(const AVariableName: string; AArguments: TdxArgumentCollection);
    destructor Destroy; override;

    property VariableName: string read FVariableName;
    property Arguments: TdxArgumentCollection read FArguments;
    property Value: TValue read FResult write FResult;
    property Handled: Boolean read FHandled write FHandled;
    property KeepLastParagraph: Boolean read FKeepLastParagraph write FKeepLastParagraph;
  end;
  TdxCalculateDocumentVariableEvent = procedure(Sender: TObject; E: TdxCalculateDocumentVariableEventArgs) of object;
  TdxCalculateDocumentVariableEventHandler = TdxMulticastMethod<TdxCalculateDocumentVariableEvent>;

implementation

uses
  dxTypeHelpers;

{ TdxPageFormattedEventArgs }

constructor TdxPageFormattedEventArgs.Create(APageIndex: Integer);
begin
  FPageIndex := APageIndex;
end;

{ TdxRichEditShortCutEventArgs }

constructor TdxRichEditShortCutEventArgs.Create(ACommand: TdxRichEditCommandId;
  AKeyData: Word; AAltGr: Boolean; const AShift: TShiftState);
begin
  inherited Create;
  FAllow := True;
  FAltGr := AAltGr;
  FCommand := ACommand;
  FKeyData := AKeyData;
  FHandled := False;
  FShift := AShift;
end;

{ TdxHyperlinkClickEventArgs }

constructor TdxHyperlinkClickEventArgs.Create(const AHyperlink: IdxRichEditHyperlink; AModifierKeys: TShortCut);
begin
  inherited Create;
  FHyperlink := AHyperlink;
  FModifierKeys := AModifierKeys;
end;

function TdxHyperlinkClickEventArgs.GetControl: Boolean;
begin
  Result := (FModifierKeys and scCtrl) = scCtrl;
end;

function TdxHyperlinkClickEventArgs.GetAlt: Boolean;
begin
  Result := (FModifierKeys and scAlt) = scAlt;
end;

function TdxHyperlinkClickEventArgs.GetShift: Boolean;
begin
  Result := (FModifierKeys and scShift) = scShift;
end;

{ TdxCustomizeMergeFieldsEventArgs }

constructor TdxCustomizeMergeFieldsEventArgs.Create(
  const AMergeFieldsNames: TArray<IdxRichEditMergeFieldName>);
begin
  inherited Create;
  FMergeFieldsNames := AMergeFieldsNames;
end;

{ TdxMailMergeCustomEventArgs }

constructor TdxMailMergeCustomEventArgs.Create(const ADocumentServerOwner: IdxInnerRichEditDocumentContainerOwner;
  ATargetDocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FDocumentServerOwner := ADocumentServerOwner;
  FTargetDocumentModel := ATargetDocumentModel;
end;

destructor TdxMailMergeCustomEventArgs.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TdxMailMergeCustomEventArgs.GetDocument: IdxRichEditDocument;
begin
  if FTargetDocumentModel = nil then
    Exit(nil);
  if FTargetServer = nil then
    FTargetServer := CreateDocumentServerForExistingDocumentModel(FDocumentServerOwner, FTargetDocumentModel);
  Result := FTargetServer.Document;
end;

class function TdxMailMergeCustomEventArgs.CreateDocumentServerForExistingDocumentModel(const ADocumentServerOwner: IdxInnerRichEditDocumentContainerOwner;
  ADocumentModel: TdxCustomDocumentModel): IdxRichEditDocumentContainer;
begin
  Result := ADocumentServerOwner.CreateDocumentContainer(ADocumentModel);
end;

procedure TdxMailMergeCustomEventArgs.Clear;
begin
  FTargetServer := nil;
end;

{ TdxMailMergeRecordCustomEventArgs }

constructor TdxMailMergeRecordCustomEventArgs.Create(
  const ADocumentServerOwner: IdxInnerRichEditDocumentContainerOwner;
  ATargetDocumentModel, ARecordDocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FDocumentServerOwner:= ADocumentServerOwner;
  FTargetDocumentModel := ATargetDocumentModel;
  FRecordDocumentModel := ARecordDocumentModel;
end;

destructor TdxMailMergeRecordCustomEventArgs.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TdxMailMergeRecordCustomEventArgs.GetDocument: IdxRichEditDocument;
begin
  if FTargetDocumentModel = nil then
    Exit(nil);
  if FTargetServer = nil then
    FTargetServer := TdxMailMergeCustomEventArgs.CreateDocumentServerForExistingDocumentModel(FDocumentServerOwner, FTargetDocumentModel);
  Result := FTargetServer.Document;
end;

function TdxMailMergeRecordCustomEventArgs.GetRecordDocument: IdxRichEditDocument;
begin
  if FRecordDocumentModel = nil then
    Exit(nil);
  if FRecordServer = nil then
    FRecordServer := TdxMailMergeCustomEventArgs.CreateDocumentServerForExistingDocumentModel(FDocumentServerOwner, FRecordDocumentModel);
  Result := FRecordServer.Document;
end;

procedure TdxMailMergeRecordCustomEventArgs.Clear;
begin
//  FreeAndNil(FTargetServer);
//  FreeAndNil(FRecordServer);
  FTargetServer:=nil;
  FRecordServer:=nil;
end;

{ TdxCalculateDocumentVariableEventArgs }

constructor TdxCalculateDocumentVariableEventArgs.Create(const AVariableName: string; AArguments: TdxArgumentCollection);
begin
  inherited Create;
  FVariableName := AVariableName;
  FArguments := AArguments;
end;

destructor TdxCalculateDocumentVariableEventArgs.Destroy;
begin
  FResult := nil;
  inherited Destroy;
end;

end.
