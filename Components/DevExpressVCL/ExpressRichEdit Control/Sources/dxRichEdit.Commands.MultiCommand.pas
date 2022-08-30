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

unit dxRichEdit.Commands.MultiCommand;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxGenerics,
  dxRichEdit.Commands,
  dxRichEdit.View.Core;

type
  TdxCommandCollection = class(TdxObjectList<TdxCommand>);

  TdxMultiCommandExecutionMode = (ExecuteFirstAvailable, ExecuteAllAvailable);
  TdxMultiCommandUpdateUIStateMode = (EnableIfAnyAvailable, EnableIfAllAvailable);

  { TdxMultiCommand }

  TdxMultiCommand = class abstract(TdxRichEditMenuItemSimpleCommand)
  strict private
    FCommands: TdxCommandCollection;
  protected
    procedure CreateCommands; virtual; abstract;
    procedure SetCommandSourceType(const Value: TdxCommandSourceType); override;

    function ExecuteCommand(ACommand: TdxCommand; const AState: IdxCommandUIState): Boolean; virtual;
    procedure ForceExecuteCore(const AState: IdxCommandUIState); virtual;
    function GetExecutionMode: TdxMultiCommandExecutionMode; virtual; abstract;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; virtual; abstract;
    procedure UpdateNestedCommandsSourceType; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property Commands: TdxCommandCollection read FCommands;
    property ExecutionMode: TdxMultiCommandExecutionMode read GetExecutionMode;
    property UpdateUIStateMode: TdxMultiCommandUpdateUIStateMode read GetUpdateUIStateMode;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); overload; override;
    constructor Create(const ARichEditControl: IdxRichEditControl; AShouldCreateCommands: Boolean); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure ExecuteCore; override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
  end;

  { TdxTransactedMultiCommand }

  TdxTransactedMultiCommand = class abstract(TdxMultiCommand)
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
  end;

  { TdxCustomTransactedMultiCommand }

  TdxCustomTransactedMultiCommand = class(TdxTransactedMultiCommand)
  strict private
    FExecutionMode: TdxMultiCommandExecutionMode;
    FUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
  protected
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
    procedure CreateCommands; override;
  public
    constructor Create(const AControl: IdxRichEditControl; ACommands: TdxCommandCollection; AExecutionMode: TdxMultiCommandExecutionMode; AUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode);

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable;

{ TdxMultiCommand }

constructor TdxMultiCommand.Create(const ARichEditControl: IdxRichEditControl);
begin
  Create(ARichEditControl, True);
end;

constructor TdxMultiCommand.Create(const ARichEditControl: IdxRichEditControl; AShouldCreateCommands: Boolean);
begin
  inherited Create(ARichEditControl);
  FCommands := TdxCommandCollection.Create;
  if AShouldCreateCommands then
    CreateCommands;
end;

destructor TdxMultiCommand.Destroy;
begin
  FreeAndNil(FCommands);
  inherited Destroy;
end;

function TdxMultiCommand.ExecuteCommand(ACommand: TdxCommand; const AState: IdxCommandUIState): Boolean;
begin
  ACommand.ForceExecute(AState);
  Result := True;
end;

procedure TdxMultiCommand.ExecuteCore;
begin
end;

procedure TdxMultiCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  NotifyBeginCommandExecution(AState);
  try
    ForceExecuteCore(AState);
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

procedure TdxMultiCommand.ForceExecuteCore(const AState: IdxCommandUIState);
var
  I: Integer;
  ACommand: TdxCommand;
  ACommandState: IdxCommandUIState;
  AExecuted: Boolean;
begin
  for I := 0 to Commands.Count - 1 do
  begin
    ACommand := Commands[I];
    ACommandState := ACommand.CreateDefaultCommandUIState;
    ACommand.UpdateUIState(ACommandState);
    if ACommandState.Enabled and ACommandState.Visible then
    begin
      AExecuted := ExecuteCommand(ACommand, AState);
      if AExecuted and (ExecutionMode = TdxMultiCommandExecutionMode.ExecuteFirstAvailable) then
        Exit;
    end;
  end;
end;

procedure TdxMultiCommand.SetCommandSourceType(const Value: TdxCommandSourceType);
begin
  if CommandSourceType <> Value then
  begin
    inherited SetCommandSourceType(Value);
    UpdateNestedCommandsSourceType;
  end;
end;

procedure TdxMultiCommand.UpdateNestedCommandsSourceType;
var
  I: Integer;
begin
  for I := 0 to Commands.Count - 1 do
    Commands[I].CommandSourceType := CommandSourceType;
end;

procedure TdxMultiCommand.UpdateUIState(const AState: IdxCommandUIState);
var
  I: Integer;
  ACommand: TdxCommand;
  ACommandState: IdxCommandUIState;
begin
  for I := 0 to Commands.Count - 1 do
  begin
    ACommand := Commands[I];
    ACommandState := ACommand.CreateDefaultCommandUIState;
    ACommand.UpdateUIState(ACommandState);
    if ACommandState.Enabled and ACommandState.Visible then
    begin
      AState.Enabled := ACommandState.Enabled;
      AState.Visible := ACommandState.Visible;
      AState.Checked := ACommandState.Checked;
      if UpdateUIStateMode = TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable then
        Exit;
    end
    else
    begin
      if UpdateUIStateMode = TdxMultiCommandUpdateUIStateMode.EnableIfAllAvailable then
      begin
        AState.Enabled := ACommandState.Enabled;
        AState.Visible := ACommandState.Visible;
        AState.Checked := ACommandState.Checked;
        Exit;
      end;
    end;
  end;
  AState.Enabled := (Commands.Count > 0) and
    (UpdateUIStateMode = TdxMultiCommandUpdateUIStateMode.EnableIfAllAvailable);
  AState.Visible := True;
  AState.Checked := False;
end;

procedure TdxMultiCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
end;

{ TdxTransactedMultiCommand }

procedure TdxTransactedMultiCommand.ForceExecute(const AState: IdxCommandUIState);
var
  ATransaction: TdxHistoryTransaction;
begin
  NotifyBeginCommandExecution(AState);
  try
    RichEditControl.BeginUpdate;
    try
      ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
      try
        DocumentModel.SuspendSyntaxHighlight;
        try
          inherited ForceExecute(AState);
        finally
          DocumentModel.ResumeSyntaxHighlight;
        end;
        DocumentModel.ForceSyntaxHighlight;
        ATransaction.SuppressRaiseOperationComplete := True;
      finally
        ATransaction.Free;
      end;
    finally
      RichEditControl.EndUpdate;
    end;
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

{ TdxCustomTransactedMultiCommand }

constructor TdxCustomTransactedMultiCommand.Create(
  const AControl: IdxRichEditControl; ACommands: TdxCommandCollection;
  AExecutionMode: TdxMultiCommandExecutionMode;
  AUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode);
begin
  inherited Create(AControl);
  Commands.AddRange(ACommands);
  FExecutionMode := AExecutionMode;
  FUpdateUIStateMode := AUpdateUIStateMode;
end;

procedure TdxCustomTransactedMultiCommand.CreateCommands;
begin
end;

class function TdxCustomTransactedMultiCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

function TdxCustomTransactedMultiCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := FExecutionMode;
end;

class function TdxCustomTransactedMultiCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

function TdxCustomTransactedMultiCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := FUpdateUIStateMode;
end;

end.
