{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxUIAdornerTargetElementPathEditor;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, dxUIAdorners, ImgList,
  cxImageList, cxGraphics;

type
  TdxUIAdornerTargetElementPathEditor = class(TForm)
    tvTargetElements: TTreeView;
    Panel1: TPanel;
    btnSelect: TButton;
    btnCancel: TButton;
    ilIcons: TcxImageList;
    procedure btnSelectClick(Sender: TObject);
    procedure tvTargetElementsChange(Sender: TObject; Node: TTreeNode);
    procedure tvTargetElementsGetImageIndex(Sender: TObject; Node: TTreeNode);
  strict private
    FPath: TdxAdornerTargetElementPath;

    function GetManager: TdxUIAdornerManager;
  protected
    function FindNodeByPath(APath: string): TTreeNode;
    function GetNodePath(ANode: TTreeNode): string;
    procedure UpdateControlsState; virtual;
    procedure UpdateImageIndex(ANode: TTreeNode); virtual;

    property TargetElement: TdxAdornerTargetElementPath read FPath;
    property Manager: TdxUIAdornerManager read GetManager;
  public
    procedure Initialize(APath: TdxAdornerTargetElementPath);
  end;

  function ShowAdornerTargetElementPathEditor(APath: TdxAdornerTargetElementPath): Boolean;

implementation

{$R *.dfm}

function ShowAdornerTargetElementPathEditor(APath: TdxAdornerTargetElementPath): Boolean;
var
  AForm: TdxUIAdornerTargetElementPathEditor;
begin
  AForm := TdxUIAdornerTargetElementPathEditor.Create(nil);
  try
    AForm.Initialize(APath);
    Result := AForm.ShowModal = mrOK;
  finally
    AForm.Free;
  end;
end;

{ TdxUIAdornerTargetElementPathEditor }

procedure TdxUIAdornerTargetElementPathEditor.Initialize(APath: TdxAdornerTargetElementPath);
begin
  FPath := APath;
  TdxUIAdornerManagerTargetElementTree.Build(Manager, tvTargetElements.Items);
  tvTargetElements.Selected := FindNodeByPath(APath.Path);
end;

procedure TdxUIAdornerTargetElementPathEditor.tvTargetElementsChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateControlsState;
end;

procedure TdxUIAdornerTargetElementPathEditor.tvTargetElementsGetImageIndex(
  Sender: TObject; Node: TTreeNode);
begin
  UpdateImageIndex(Node);
end;

function TdxUIAdornerTargetElementPathEditor.FindNodeByPath(APath: string): TTreeNode;
var
  I: Integer;
begin
  for I := 0 to tvTargetElements.Items.Count - 1 do
  begin
    Result := tvTargetElements.Items[I];
    if GetNodePath(Result) = APath then
      Exit;
  end;
  Result := nil;
end;

function TdxUIAdornerTargetElementPathEditor.GetNodePath(ANode: TTreeNode): string;
begin
  Result := '';
  while ANode <> nil do
  begin
    if Result <> '' then
      Result := dxAdornerTargetElementPathSeparator + Result;
    Result := ANode.Text + Result;
    ANode := ANode.Parent;
  end;
end;

procedure TdxUIAdornerTargetElementPathEditor.UpdateControlsState;
begin
  btnSelect.Enabled := (tvTargetElements.Selected <> nil) and Boolean(tvTargetElements.Selected.Data);
end;

procedure TdxUIAdornerTargetElementPathEditor.UpdateImageIndex(ANode: TTreeNode);
var
  AIndex: Integer;
begin
  if Boolean(ANode.Data) then
    AIndex := 1
  else
    AIndex := 0;
  ANode.ImageIndex := AIndex;
  ANode.SelectedIndex := AIndex;
end;

function TdxUIAdornerTargetElementPathEditor.GetManager: TdxUIAdornerManager;
begin
  Result := TargetElement.Adorner.Manager;
end;

procedure TdxUIAdornerTargetElementPathEditor.btnSelectClick(Sender: TObject);
begin
  TargetElement.Path := GetNodePath(tvTargetElements.Selected);
end;

end.
