{************************************************************************}
{ TAdvLabel component                                                    }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by                                                             }
{  TMS Software                                                          }
{  copyright © 2014                                                      }
{  Email : info@tmssoftware.com                                          }
{  Website : http://www.tmssoftware.com                                  }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit AdvLabelDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, Forms, Dialogs, AdvLabel, FRTFBox, Messages, Windows, Controls,
  DesignIntf, DesignEditors;

type
  TRichTextProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue:string; override;
    procedure Edit; override;
  end;


implementation

{ TRichTextProperty }

function TRichTextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog,paReadOnly];
end;

function TRichTextProperty.GetValue: string;
begin
  Result := '(TRichText)';
end;

procedure TRichTextProperty.Edit;
var
  rtfbox: TRTFBox;
  ms: TStringStream;
  s: string;

begin
  rtfbox := TRTFBox.Create(application);
  try
    s := (GetComponent(0) as TAdvLabel).Text;

    ms := TStringStream.Create('');
    try
      ms.WriteString(s);
      ms.position := 0;
      rtfbox.RTFControl.lines.LoadFromStream(ms);
    finally
      ms.free;
    end;

    if rtfbox.Showmodal = mrOk then
    begin
       ms := TStringStream.Create('');
       try
         rtfbox.RTFControl.Lines.SaveToStream(ms);
         s := ms.DataString;
       finally
         ms.Free;
       end;

      (GetComponent(0) as TAdvLabel).Text := s;
      (GetComponent(0) as TAdvLabel).Repaint;
      Modified;
    end;
  finally
    rtfbox.Free;
  end;
end;


end.
