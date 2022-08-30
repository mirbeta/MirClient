{********************************************************************}
{ AdvMemo property editore                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by : TMS Software                                          }
{               copyright © 2002 - 2013                              }
{               Email : info@tmssoftware.com                         }
{               Web : http://www.tmssoftware.com                     }
{********************************************************************}
unit AdvMemoDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvMemo, Dialogs, uMemoEdit, Forms, Controls, AdvMemoAC,
  DesignIntf, DesignEditors;
                                                        
type
  TAdvMemoProperty = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TAdvMemoEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    function GetVerb(index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TMemoAutoCorrectProperty = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TAdvCodeListEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
  end;


implementation

uses
  SysUtils;

{ TIWScriptEventProperty }

procedure TAdvMemoProperty.Edit;
var
  SE: TTMSMemoEdit;
begin
  SE := TTMSMemoEdit.Create(Application);

  try
    SE.AdvMemo1.Lines.Assign(TStrings(GetOrdValue));

    if GetComponent(0) is TAdvMemo then
    begin
      SE.AdvMemo1.SyntaxStyles := (GetComponent(0) as TAdvMemo).SyntaxStyles;
    end;

    if SE.ShowModal = mrOk then
    begin
      SetOrdValue(longint(SE.AdvMemo1.Lines));
      (GetComponent(0) as TAdvMemo).TopLine := 0;
    end;

  finally
    SE.Free;
  end;
end;

function TAdvMemoProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TAdvMemoEditor }
procedure TAdvMemoEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'LINES') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

function TAdvMemoEditor.GetVerb(Index: Integer):string;
begin
  if Index = 0 then
    Result := 'About';
end;

function TAdvMemoEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TAdvMemoEditor.ExecuteVerb(Index: Integer);
var
  compiler:string;

begin
  {$I COMPILERTEXT.INC}

  if Index = 0 then
    MessageDlg(Component.ClassName+' version '+(Component as TAdvCustomMemo).GetVersionString+' for '+compiler+#13#10'© 2001-2015 by TMS software',
               mtinformation,[mbok],0);
end;

{ TMemoAutoCorrectProperty }

procedure TMemoAutoCorrectProperty.Edit;
var
  Dlg: TMemoAC;
  i: Integer;
  Memo: TAdvMemo;
begin
  Dlg := TMemoAc.Create(Application);

  Memo := TAdvMemo(GetComponent(0));

  dlg.ckDoAutoCorrect.Checked := Memo.AutoCorrect.Active;
  if Memo.AutoCorrect.OldValue.Count > 0 then
    Dlg.StringGrid1.RowCount := 1 + Memo.AutoCorrect.OldValue.Count;

  for i := 1 to Memo.AutoCorrect.OldValue.Count do
  begin
    Dlg.StringGrid1.Cells[0,i] := Memo.AutoCorrect.OldValue.Strings[i - 1];
    Dlg.StringGrid1.Cells[1,i] := Memo.AutoCorrect.NewValue.Strings[i - 1];
  end;

  if Dlg.ShowModal = mrOk then
  begin
    Memo.AutoCorrect.Active := dlg.ckDoAutoCorrect.Checked;
    Memo.AutoCorrect.OldValue.Clear;
    Memo.AutoCorrect.NewValue.Clear;

    for i := 1 to Dlg.StringGrid1.RowCount - 1 do
    begin
      Memo.AutoCorrect.OldValue.Add(Dlg.StringGrid1.Cells[0,i]);
      Memo.AutoCorrect.NewValue.Add(Dlg.StringGrid1.Cells[1,i]);
    end;

    Designer.Modified;
  end;

  Dlg.Free;

end;

function TMemoAutoCorrectProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;


{ TAdvCodeListEditor }
procedure TAdvCodeListEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'CODEBLOCKS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;



end.




