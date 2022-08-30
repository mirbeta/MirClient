{***************************************************************************}
{ TAdvRichEditor component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

unit AdvRichEditorDE;

interface

uses
  Classes, AdvRichEditor, Dialogs, Forms, Controls, AdvRichEditorAC,
  DesignIntf, DesignEditors;

type

  TRichEditorAutoCorrectProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{ TMemoAutoCorrectProperty }

procedure TRichEditorAutoCorrectProperty.Edit;
var
  Dlg: TRichEditorAC;
  i: Integer;
  RichEditor: TAdvRichEditor;
begin
  Dlg := TRichEditorAC.Create(Application);

  RichEditor := TAdvRichEditor(GetComponent(0));

  dlg.ckDoAutoCorrect.Checked := RichEditor.AutoCorrect.Active;
  dlg.ckCase.Checked := RichEditor.AutoCorrect.CaseSensitive;

  if RichEditor.AutoCorrect.OldValue.Count > 0 then
    Dlg.StringGrid1.RowCount := 1 + RichEditor.AutoCorrect.OldValue.Count;

  for i := 1 to RichEditor.AutoCorrect.OldValue.Count do
  begin
    Dlg.StringGrid1.Cells[0,i] := RichEditor.AutoCorrect.OldValue.Strings[i - 1];
    Dlg.StringGrid1.Cells[1,i] := RichEditor.AutoCorrect.NewValue.Strings[i - 1];
  end;

  if Dlg.ShowModal = mrOk then
  begin
    RichEditor.AutoCorrect.Active := dlg.ckDoAutoCorrect.Checked;
    RichEditor.AutoCorrect.CaseSensitive := dlg.ckCase.Checked;

    RichEditor.AutoCorrect.OldValue.Clear;
    RichEditor.AutoCorrect.NewValue.Clear;

    for i := 1 to Dlg.StringGrid1.RowCount - 1 do
    begin
      RichEditor.AutoCorrect.OldValue.Add(Dlg.StringGrid1.Cells[0,i]);
      RichEditor.AutoCorrect.NewValue.Add(Dlg.StringGrid1.Cells[1,i]);
    end;

    Designer.Modified;
  end;

  Dlg.Free;
end;

function TRichEditorAutoCorrectProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;


end.
