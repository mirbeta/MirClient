{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2013                                        }
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

unit AdvMemoStylerManager;

interface

{$I TMSDEFS.INC}

uses
  Classes, Controls, Graphics, SysUtils, AdvMemo, INIFiles, Windows
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type

  TAdvMemoStylerManager = class;

  TAdvMemoStylersCollectionItem = class(TCollectionItem)
  private
    FName: string;
    FFileName: string;
    FStyler: TAdvCustomMemoStyler;
    FStylerIndex: integer;
    procedure SetName(const Value: string);
    procedure SetStyler(const Value: TAdvCustomMemoStyler);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write SetName;
    property StylerIndex: integer read FStylerIndex write FStylerIndex;
    property FileName: string read FFileName write FFileName;
    property Styler: TAdvCustomMemoStyler read FStyler write SetStyler;
  end;

  TAdvMemoStylersCollection = class(TCollection)
  private
    {$IFNDEF DELPHI6_LVL}
    FOwner: TPersistent;
    {$ENDIF}
    FAdvMemoStylers: TAdvMemoStylerManager;
    function GetItem(Index: Integer): TAdvMemoStylersCollectionItem;
    procedure SetItem(Index: Integer; Value: TAdvMemoStylersCollectionItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Styler: TAdvMemoStylerManager);
    destructor Destroy; override;
    function Add: TAdvMemoStylersCollectionItem;
    function Insert(Index: integer): TAdvMemoStylersCollectionItem;
    property Items[Index: Integer]: TAdvMemoStylersCollectionItem read GetItem write SetItem; default;
    {$IFNDEF DELPHI6_LVL}
    property Owner: TPersistent read FOwner;
    {$ENDIF}
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMemoStylerManager = class(TComponent)
  private
    FItems: TAdvMemoStylersCollection;
    FItemsCount: integer;
    FIncludeAllFiles: Boolean;
    FIncludeTextFiles: Boolean;
    procedure SetItems(Value: TAdvMemoStylersCollection);
  protected
    procedure Notification(comp: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetStylerByFileName(FileName: string): TAdvCustomMemoStyler;
    function GetStylerByName(strName: string): TAdvCustomMemoStyler;
    function GetStyler(Idx: integer): TAdvCustomMemoStyler;
    function GetFilter(Idx: integer): string;
    function GetStylerNames(var List: TStrings): integer;
    function GetStylerItem(Idx: integer): TCollectionItem;
    function GetStylerItemByFileName(FileName: string): TcollectionItem;
    function GetStylerItemByName(strName: string): TCollectionItem;
    function GetStylerStyleIndexByInfo(styler: TAdvCustomMemoStyler; Info: string): integer;
    procedure SaveAllStylersToFile(FileName: string);
    procedure LoadAllStylersFromFile(FileName: string);
    procedure SaveStylerToFile(Styler: TAdvCustomMemoStyler; FileName: string);
    procedure LoadStylerFromFile(Styler: TAdvCustomMemoStyler; FileName: string);
    property ItemsCount: integer read FItemsCount write FItemsCount;
  published
    property Items: TAdvMemoStylersCollection read FItems write SetItems;
    property IncludeAllFiles: Boolean read FIncludeAllFiles write FIncludeAllFiles default True;
    property IncludeTextFiles: Boolean read FIncludeTextFiles write FIncludeTextFiles default True;
  end;



implementation


{ TAdvMemoStylersCollectionItem }

function TAdvMemoStylersCollectionItem.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TAdvMemoStylersCollectionItem.SetName(const Value: string);
begin
  if FName <> Value then
    FName := Value;
end;

procedure TAdvMemoStylersCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TAdvMemoStylersCollectionItem then
  begin
    Name := TAdvMemoStylersCollectionItem(Source).Name;
    Styler := TAdvMemoStylersCollectionItem(Source).Styler;
    FileName := TAdvMemoStylersCollectionItem(Source).FileName;
  end;
end;


{ TAdvMemoStylersCollection }

procedure TAdvMemoStylersCollectionItem.SetStyler(const Value: TAdvCustomMemoStyler);
begin
  if Styler <> Value then
    FStyler := Value;
end;

constructor TAdvMemoStylersCollection.Create(Styler: TAdvMemoStylerManager);
begin
  inherited Create(TAdvMemoStylersCollectionItem);
  FAdvMemoStylers := Styler;
  {$IFNDEF DELPHI6_LVL}
  FOwner := Styler;
  {$ENDIF}
end;

destructor TAdvMemoStylersCollection.Destroy;
begin
  inherited Destroy;
end;

function TAdvMemoStylersCollection.Add: TAdvMemoStylersCollectionItem;
begin
  Result := TAdvMemoStylersCollectionItem(inherited Add);
  TAdvMemoStylerManager(Self.Owner).ItemsCount := TAdvMemoStylerManager(Self.Owner).ItemsCount + 1;
end;

function TAdvMemoStylersCollection.GetItem(Index: Integer): TAdvMemoStylersCollectionItem;
begin
  Result := TAdvMemoStylersCollectionItem(inherited GetItem(Index));
end;

procedure TAdvMemoStylersCollection.SetItem(Index: Integer;
	Value: TAdvMemoStylersCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TAdvMemoStylersCollection.GetOwner: TPersistent;
begin
  {$IFNDEF DELPHI6_LVL}
  Result := FOwner;
  {$ENDIF}
  {$IFDEF DELPHI6_LVL}
  Result := FAdvMemoStylers;
  {$ENDIF}
end;

function TAdvMemoStylersCollection.Insert(
  Index: integer): TAdvMemoStylersCollectionItem;
begin
  Result := TAdvMemoStylersCollectionItem(inherited Insert(Index));
end;

{ TAdvMemoStylerManager }

constructor TAdvMemoStylerManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TAdvMemoStylersCollection.Create(Self);
  FIncludeAllFiles := true;
  FIncludeTextFiles := true;
end;

destructor TAdvMemoStylerManager.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TAdvMemoStylerManager.LoadAllStylersFromFile(FileName: string);
var
  i: Integer;
begin
  for i := 0 to FItems.FAdvMemoStylers.FItems.Count - 1 do
    LoadStylerFromFile(FItems.FAdvMemoStylers.FItems[I].FStyler,FileName);
end;

{$IFNDEF DELPHI6_LVL}
function StrToBool(s: string): boolean;
begin
  Result :=  Uppercase(s) = 'TRUE';
end;

function BoolToStr(b: boolean): string;
begin
  if b then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;
{$ENDIF}

procedure TAdvMemoStylerManager.LoadStylerFromFile(Styler: TAdvCustomMemoStyler; FileName: string);
var
  I,CNT: Integer;
  init: TMemIniFile;

  function StrToStyleType(StyleType: string): TStyleType;
  begin
    Result := stSymbol;
    if AnsiSameText('stKeyword',StyleType) Then Result := stKeyword;
    if AnsiSameText('stBracket',StyleType) Then Result := stBracket;
  end;

  function StrToFontStyles(FontStyles: string): TFontStyles;
  begin
    Result := [];
    if Pos('fsBold', FontStyles) > 0 then
       Include(Result,fsBold);
    if Pos('fsItalic', FontStyles) > 0 then
       Include(Result, fsItalic);
    if Pos('fsStrikeOut', FontStyles) > 0 then
       Include(Result, fsStrikeOut);
    if Pos('fsUnderline', FontStyles) > 0 then
       Include(Result, fsUnderline);
  end;

begin
  init := TMemIniFile.Create(FileName);
  try
    with Styler do
    begin
      with Init do
      begin
        if not ValueExists('Stylers', Name) then
          Exit;

        for I := 0 to AllStyles.Count - 1 do
        begin
          AllStyles.Items[I].BGColor := ReadInteger(Name+'_'+AllStyles.Items[I].DisplayName,'bgColor',0);
          AllStyles.Items[I].Font.Name := ReadString(Name+'_'+AllStyles.Items[I].DisplayName,'Font','');
          AllStyles.Items[I].Font.Style := StrToFontStyles(ReadString(Name+'_'+AllStyles.Items[I].DisplayName,'FontStyle',''));
          AllStyles.Items[I].Font.Color := ReadInteger(Name+'_'+AllStyles.Items[I].DisplayName,'FontColor',0);
          AllStyles.Items[I].Info := ReadString(Name+'_'+AllStyles.Items[I].DisplayName,'info','');
          AllStyles.Items[I].StyleType := TStyleType(StrToStyleType(ReadString(Name+'_'+AllStyles.Items[I].DisplayName,'StyleType','')));
          ReadSection(Name+'_'+AllStyles.Items[I].DisplayName+'_KeyWordList',AllStyles.Items[I].KeyWords);
        end;

        cnt := ReadInteger(Name+'__AutoCompletion','Count',0);

        AutoCompletion.Clear;

        for I := 0 to cnt - 1 do
        begin
          AutoCompletion.Add(ReadString(Name+'__AutoCompletion','Item'+inttostr(I),''));
        end;


        for I := 0 to RegionDefinitions.Count - 1 do
        begin
          RegionDefinitions[I].Identifier := ReadString(Name+'__RegionDef','Identifier','');
          RegionDefinitions[I].RegionEnd := ReadString(Name+'__RegionDef','RegionEnd','');
          RegionDefinitions[I].RegionStart := ReadString(Name+'__RegionDef','RegionStart','');
          RegionDefinitions[I].RegionType := TRegionType(ReadInteger(Name+'__RegionDef','RegionType',0));
          RegionDefinitions[I].ShowComments := StrToBool(ReadString(Name+'__RegionDef','ShowComments','false'));
        end;

        CommentStyle.BkColor := ReadInteger(Name+'_CommentStyle','bkColor',0);
        CommentStyle.Style := StrToFontStyles(ReadString(Name+'_CommentStyle','Style',''));
        CommentStyle.TextColor := ReadInteger(Name+'_CommentStyle','TextColor',0);
        try
          HintParameter.BkColor := ReadInteger(Name+'_HintParameter','bkColor',0);
          HintParameter.TextColor := ReadInteger(Name+'_HintParameter','TextColor',0);
          ReadSection(Name+'__HintParameters',HintParameter.Parameters);
        except
        end;
        NumberStyle.BkColor := ReadInteger(Name+'_NumberStyle','bkColor',0);
        NumberStyle.Style := StrToFontStyles(ReadString(Name+'_NumberStyle','Style',''));
        NumberStyle.TextColor := ReadInteger(Name+'_NumberStyle','TextColor',0);
      end;
    end;
  finally
    Init.Free;
  end;
end;


procedure TAdvMemoStylerManager.SaveAllStylersToFile(FileName: string);
var
  I: Integer;
begin
  if FileExists(FileName) then
     SysUtils.DeleteFile(FileName);
  for I := 0 to FItems.FAdvMemoStylers.FItems.Count - 1 do  
  begin
    SaveStylerToFile(FItems.FAdvMemoStylers.FItems[I].FStyler,FileName);
  end;
end;

procedure TAdvMemoStylerManager.SaveStylerToFile(Styler: TAdvCustomMemoStyler; FileName: string);
var
  I, J: Integer;
  init: TMemIniFile;

  function StyleTypeToStr(StyleType: TStyleType): string;
  begin
     case StyleType of    { }
       stKeyword: Result := 'stKeyword';
       stBracket: Result := 'stBracket';
       stSymbol: Result := 'stSymbol';
     end;    { case }
  end;
  
  function FontStylesToStr(FontStyles: TFontStyles): string;
  begin
     Result := '';
     if fsBold in FontStyles then
        Result := Result + 'fsBold,';
     if fsItalic in FontStyles then
        Result := Result + 'fsItalic,';
     if fsUnderline in FontStyles then
        Result := Result + 'fsUnderline,';
     if fsStrikeOut in FontStyles then
        Result := Result + 'fsStrikeOut';
     if Result <> '' then
       if Result[Length(Result)] = ',' then
          Delete(Result,Length(Result),1);
  end;

begin
  init := TMemIniFile.Create(FileName);
  try
    with Styler do
    begin
      with init do
      begin
        WriteString('Stylers',Name,'');
        WriteInteger(Name,'AllStyles',AllStyles.Count);
        WriteString(Name,'AutoCompletion','');
        WriteString(Name,'CommentStyle','');
        WriteString(Name,'NumberStyle','');
        WriteString(Name,'HintParameter','');

        for I := 0 to AllStyles.Count - 1 do
        begin
          WriteInteger(Name+'_'+AllStyles.Items[I].DisplayName,'bgColor',AllStyles.Items[I].BGColor);
          WriteString(Name+'_'+AllStyles.Items[I].DisplayName,'Font',AllStyles.Items[I].Font.Name);
          WriteString(Name+'_'+AllStyles.Items[I].DisplayName,'FontStyle',FontStylesToStr(AllStyles.Items[I].Font.Style));
          WriteInteger(Name+'_'+AllStyles.Items[I].DisplayName,'FontColor',AllStyles.Items[I].Font.Color);

          WriteString(Name+'_'+AllStyles.Items[I].DisplayName,'info',AllStyles.Items[I].Info);
          WriteString(Name+'_'+AllStyles.Items[I].DisplayName,'StyleType',StyleTypeToStr(TStyleType(AllStyles.Items[I].StyleType)));
          for J := 0 to AllStyles.Items[I].KeyWords.Count - 1 do    { Iterate }
          begin
            WriteString(Name+'_'+AllStyles.Items[I].DisplayName+'_KeyWordList',AllStyles.Items[I].KeyWords[j],'');
          end;
        end;

        WriteInteger(Name+'__AutoCompletion','Count',AutoCompletion.Count);

        for J := 0 to AutoCompletion.Count - 1 do
        begin
          WriteString(Name+'__AutoCompletion','Item'+inttostr(j),AutoCompletion[j]);
        end;

        for J := 0 to RegionDefinitions.Count - 1 do
        begin
          WriteString(Name+'__RegionDef','Identifier',RegionDefinitions[j].Identifier);
          WriteString(Name+'__RegionDef','RegionEnd',RegionDefinitions[j].RegionEnd);
          WriteString(Name+'__RegionDef','RegionStart',RegionDefinitions[j].RegionStart);
          WriteInteger(Name+'__RegionDef','RegionType',integer(RegionDefinitions[j].RegionType));
          WriteString(Name+'__RegionDef','ShowComments',BoolToStr(RegionDefinitions[j].ShowComments));
        end;

        WriteInteger(Name+'_CommentStyle','bkColor',CommentStyle.BkColor);
        WriteString(Name+'_CommentStyle','Style',FontStylesToStr(CommentStyle.Style));
        WriteInteger(Name+'_CommentStyle','TextColor',CommentStyle.TextColor);
        try
          WriteInteger(Name+'_HintParameter','bkColor',HintParameter.BkColor);
          WriteInteger(Name+'_HintParameter','TextColor',HintParameter.TextColor);
          for J := 0 to HintParameter.Parameters.Count - 1 do
          begin
            WriteString(Name+'__HintParameters',HintParameter.Parameters[j],'');
          end;
        except
        end;
        WriteInteger(Name+'_NumberStyle','bkColor',NumberStyle.BkColor);
        WriteString(Name+'_NumberStyle','Style',FontStylesToStr(NumberStyle.Style));
        WriteInteger(Name+'_NumberStyle','TextColor',NumberStyle.TextColor);
      end;
    end;
  finally
    Init.Free;
  end;
end;

procedure TAdvMemoStylerManager.SetItems(Value: TAdvMemoStylersCollection);
begin
  FItems.Assign(Value);
end;


function TAdvMemoStylerManager.GetStylerByFileName(FileName: string): TAdvCustomMemoStyler;
var
  i: integer;
  Ext: string;

begin
  Result := nil;
  Ext := ExtractFileExt(FileName);
  if Ext <> '' then
  begin
     Delete(Ext,1,1);
     for i := 0 to pred(FItems.Count) do
     begin
        if pos(Ext, FItems.Items[i].Styler.Extensions) > 0 then
        begin
           Result := FItems.Items[i].FStyler;
           break;
        end;
     end;
  end;
end;


function TAdvMemoStylerManager.GetStyler(Idx: integer): TAdvCustomMemoStyler;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to pred(FItems.Count) do
    if FItems.Items[i].Index = Idx then
    begin
      Result := FItems.Items[i].FStyler;
      break;
    end;
end;

function TAdvMemoStylerManager.GetStylerStyleIndexByInfo(styler: TAdvCustomMemoStyler; Info: string): integer;
var
  i,j: integer;
begin
  Result := -1;
  for i := 0 to pred(FItems.Count) do
  begin
    if FItems.Items[i].Styler = styler then
    begin  //found the styler
      for j := 0 to fItems.Items[i].Styler.AllStyles.Count - 1 do
      begin
        if fItems.Items[i].Styler.AllStyles[j].Info = Info then
        begin
          Result := j;
          Exit;
        end;
      end;
    end;
  end;
end;

function TAdvMemoStylerManager.GetStylerItem(Idx: integer): TCollectionItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to pred(FItems.Count) do
    if FItems.Items[i].Index = Idx then
    begin
      Result := FItems.Items[i];
      Break;
    end;
end;

function TAdvMemoStylerManager.GetStylerItemByFileName(FileName: string): TcollectionItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to pred(FItems.Count) do
    if FItems.Items[i].FFileName = FileName then
    begin
      Result := FItems.Items[i];
      Break;
    end;
end;

function TAdvMemoStylerManager.GetStylerItemByName(strName: string): TCollectionItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to pred(FItems.Count) do
    if FItems.Items[i].Styler.StylerName = strName then
    begin
      Result := FItems.Items[i];
      Break;
    end;
end;

function TAdvMemoStylerManager.GetStylerByName(strName: string): TAdvCustomMemoStyler;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to pred(FItems.Count) do
    if FItems.Items[i].Styler.StylerName = strName then
    begin
      Result := FItems.Items[i].FStyler;
      Break;
    end;
end;

function TAdvMemoStylerManager.GetFilter(Idx: integer): string;
var
  tmpFilter: string;
  i: integer;
begin
  Result := '';
  tmpFilter := '';
  case Idx of
  -1: begin  //get all filters
        for i := 0 to Pred(FItems.Count) do
          tmpFilter := tmpFilter + FItems.Items[i].Styler.Filter  + '|';

        if IncludeTextFiles then
          tmpFilter := tmpFilter + 'Text Files (*.txt)|*.txt|';
        if IncludeAllFiles then
          tmpFilter := tmpFilter + 'All files (*.*)|*.*';
      end;
  else
  begin
    if Idx > pred(FItems.Count - 1) then
      tmpFilter := 'All files|*.*'
    else
    begin
      tmpFilter := FItems.Items[Idx].Styler.Filter;
      if IncludeAllFiles then
         tmpFilter := tmpFilter + '|All files (*.*)|*.*';
    end;
  end;
  end;
  
  Result := tmpFilter;
end;

function TAdvMemoStylerManager.GetStylerNames(var List: TStrings): integer;
var
  i: integer;
begin
  for i := 0 to pred(FItems.Count) do
    List.Add(FItems.Items[i].Styler.StylerName);
  Result := pred(FItems.Count);
end;

procedure TAdvMemoStylerManager.Notification(comp: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited;

  if (Operation = opRemove) and not (csDestroying in ComponentState) then
  begin
    i := 0;
    while (i < FItems.Count) do
    begin
      if (FItems[i].Styler = comp) then
        FItems[i].Free
      else
        inc(i);
    end;
  end;

end;

end.
