{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMemData                                           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSMEMDATA                     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxmdatps;

interface

{$I cxVer.inc}

uses
  DesignIntf, Windows, Classes, Controls, Forms, StdCtrls, dxmdaset, ExtCtrls, Dialogs,
  Menus, Graphics, DB, DBGrids, Grids;

type
  TfrmdxMemDataPersistent = class(TForm)
    pnlBottom: TPanel;
    btnClear: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    InternalMemData: TdxMemData;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnGetFieldsFromFile: TButton;
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnGetFieldsFromFileClick(Sender: TObject);
  public
    procedure SetMemData(AMemData: TdxCustomMemData);
  end;

procedure ShowMemDataPersistentEditor(AMemData: TdxCustomMemData; ADesigner: IDesigner);

implementation

uses TypInfo;

{$R *.dfm}

procedure ShowMemDataPersistentEditor(AMemData: TdxCustomMemData; ADesigner: IDesigner);

  procedure SetDesignerModified;
  begin
    if ADesigner <> nil then
      ADesigner.Modified;
  end;

var
  AForm: TfrmdxMemDataPersistent;
begin
  AForm := TfrmdxMemDataPersistent.Create(nil);
  try
    AForm.SetMemData(AMemData);
    if AForm.ShowModal = mrOK then
    begin
      if AForm.InternalMemData.State in dsEditModes then
        AForm.InternalMemData.Post;
      AMemData.AddFieldsFromDataSet(AForm.InternalMemData, AMemData.Owner);
      AForm.InternalMemData.Persistent.SaveData;
      AMemData.Persistent.Assign(AForm.InternalMemData.Persistent);
      if AMemData.Active then
      begin
        AMemData.Close;
        AMemData.Open;
      end;
      SetDesignerModified;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TfrmdxMemDataPersistent.SetMemData(AMemData: TdxCustomMemData);
begin
  InternalMemData.CreateFieldsFromDataSet(AMemData);
  if AMemData.Active then
    AMemData.Persistent.SaveData;
  InternalMemData.Persistent.Assign(AMemData.Persistent);
  InternalMemData.Persistent.LoadData;
  InternalMemData.Open;
end;

procedure TfrmdxMemDataPersistent.btnClearClick(Sender: TObject);
begin
  InternalMemData.DisableControls;
  try
    while not InternalMemData.Eof do
      InternalMemData.Delete;
  finally
    InternalMemData.EnableControls;
  end;
end;

procedure TfrmdxMemDataPersistent.btnLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    try
      InternalMemData.LoadFromBinaryFile(OpenDialog.FileName);
    except
    end;
end;

procedure TfrmdxMemDataPersistent.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    try
      InternalMemData.SaveToBinaryFile(SaveDialog.FileName);
    except
    end;
end;

procedure TfrmdxMemDataPersistent.btnGetFieldsFromFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    InternalMemData.CreateFieldsFromBinaryFile(OpenDialog.FileName);
    InternalMemData.Open;
  end;
end;

end.
