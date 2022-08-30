{*************************************************************************}
{ TMS TAdvRichEditor                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditorIO;

interface

uses
  Classes, AdvRichEditorBase, StdCtrls, Windows, Controls, Graphics,
  AdvRichEditorRTF, Dialogs, SysUtils;

type
  TAdvRichEditorBaseEx = class(TAdvRichEditorBase);

  TAdvRichEditorIO = class(TComponent)
  private
    FRichEditor: TAdvRichEditorBase;
  public
    property RichEditor: TAdvRichEditorBase read FRichEditor write FRichEditor;
    procedure Save(AStream: TStream); overload; virtual; abstract;
  end;

  TAdvRichEditorHTMLIO = class(TAdvRichEditorIO)
  public
    FInlineImages: boolean;
    procedure Save(const FileName: string; ImgPath: string = ''); overload; virtual;
    procedure Save(AStream: TStream); override;
  published
    property RichEditor;
    property InlineImages: boolean read FInlineImages write FInlineImages default false;
  end;

  TAdvRichEditorRTFIO = class(TAdvRichEditorIO)
  public
    procedure Load(const FileName: string); overload;
    procedure Load(AStream: TStream); overload;
    procedure Save(const FileName: string); overload; virtual;
    procedure Save(AStream: TStream); override;
  published
    property RichEditor;
  end;

implementation

uses
  Character;

type
  TAdvRichEditorEx = class(TAdvRichEditorBase);

{ TAdvRichEditorHTMLIO }

procedure TAdvRichEditorHTMLIO.Save(const FileName: string; ImgPath: string = '');
var
  sl: TStringList;
begin
  if not Assigned(FRichEditor) then
    Exit;

  sl := TStringList.Create;

  try
    if ImgPath = '' then
      ImgPath := ExtractFilePath(FileName);

    if InlineImages then
      TAdvRichEditorBaseEx(FRichEditor).HTMLImages := igInline;

    sl.Text := TAdvRichEditorBaseEx(FRichEditor).GetContentAsHTML(False, ImgPath);
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorHTMLIO.Save(AStream: TStream);
var
  sl: TStringList;
begin
  if not Assigned(FRichEditor) then
    Exit;

  sl := TStringList.Create;

  try
    if InlineImages then
      TAdvRichEditorBaseEx(FRichEditor).HTMLImages := igInline;

    sl.Text := TAdvRichEditorBaseEx(FRichEditor).GetContentAsHTML(False);
    sl.SaveToStream(AStream);
  finally
    sl.Free;
  end;
end;

{ TAdvRichEditorRTFIO }

procedure TAdvRichEditorRTFIO.Save(const FileName: string);
begin
  if not Assigned(FRichEditor) then
    Exit;

  TAdvRichEditorBaseEx(FRichEditor).GetContentAsRTF(False);

  TAdvRichEditorBaseEx(FRichEditor).RTFEngine.SaveToFile(FileName);
end;

procedure TAdvRichEditorRTFIO.Load(AStream: TStream);
var
  ts: TStringStream;
begin
  if not Assigned(FRichEditor) then
    raise Exception.Create('No richeditor assigned');

  if not Assigned(AStream) then
    raise Exception.Create('Stream not assigned');

  AStream.Position := 0;

  ts := TStringStream.Create;
  try
    ts.LoadFromStream(AStream);
    RichEditor.InsertAsRTF(ts.DataString);
  finally
    ts.Free;
  end;
end;

procedure TAdvRichEditorRTFIO.Load(const FileName: string);
var
  ts: TStringStream;
  s,su: string;
  i, lp: integer;
  sl: TStringList;
begin
  if not Assigned(FRichEditor) then
    raise Exception.Create('No richeditor assigned');

  sl := TStringList.Create;
  sl.LoadFromFile(FileName);

  ts := TStringStream.Create;

  for i := 0 to sl.Count - 1  do
  begin
    s := sl.Strings[i];
    if (Length(s) > 0) and (CharInStr(s,1) <> '\') then
      ts.WriteString(#13);

    ts.WriteString(s);

    lp := Length(s);
    su := '';
    if lp >= 2 then
    begin
      su := Copy(s, lp - 1, 2);
    end;

    if su = ' \' then
      ts.WriteString('line');
  end;

  s := ts.DataString;

  ts.Free;
  sl.Free;

  RichEditor.InsertAsRTF(s);
end;

procedure TAdvRichEditorRTFIO.Save(AStream: TStream);
begin
  TAdvRichEditorBaseEx(FRichEditor).GetContentAsRTF(False);
  TAdvRichEditorBaseEx(FRichEditor).RTFEngine.SaveToStream(AStream);
end;

end.
