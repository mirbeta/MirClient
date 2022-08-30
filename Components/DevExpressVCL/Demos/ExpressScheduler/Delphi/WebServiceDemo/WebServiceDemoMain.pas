unit WebServiceDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Variants, 
  Graphics, Controls, Forms, Dialogs, DemoBasicMain, cxLookAndFeels, ActnList,
  ImgList, Menus, StdCtrls, ComCtrls, cxClasses, cxStyles, cxGraphics, cxEdit,

  cxControls, cxLookAndFeelPainters, Generics.Collections,

  cxScheduler, cxSchedulerStorage,
  cxSchedulerCustomControls, cxSchedulerCustomResourceView, cxSchedulerDayView,
  cxSchedulerAgendaView, cxSchedulerDateNavigator, cxSchedulerHolidays,
  cxSchedulerTimeGridView, cxSchedulerUtils, cxSchedulerWeekView,
  cxSchedulerYearView, cxSchedulerGanttView, cxSchedulerRecurrence,
  dxBarBuiltInMenu, cxSchedulerTreeListBrowser,
  cxSchedulerRibbonStyleEventEditor,

  cxCustomData, cxTL, cxTextEdit, cxTLdxBarBuiltInMenu,
  cxDataControllerConditionalFormattingRulesManagerDialog,
  cxSplitter, cxButtons, cxInplaceContainer, cxContainer, cxDateNavigator,
  ExtCtrls,

  dxAuthorizationAgents,
  cxSchedulerWebServiceStorage,
  cxSchedulerWebServiceStorageGoogleProvider,
  cxSchedulerWebServiceStorageOfficeProvider;

type
  TWebServiceDemoMainForm = class(TDemoBasicMainForm)
    cxDateNavigator1: TcxDateNavigator;
    Panel1: TPanel;
    tlCalendars: TcxTreeList;
    tcName: TcxTreeListColumn;
    btnAddAccount: TcxButton;
    pmAddAccount: TPopupMenu;
    tcId: TcxTreeListColumn;
    cxSplitter1: TcxSplitter;
    Storage: TcxSchedulerWebServiceStorage;
    Refresh1: TMenuItem;
    N5: TMenuItem;
    alMain: TActionList;
    aReloadEvents: TAction;
    cxButton1: TcxButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tlCalendarsNodeCheckChanged(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; AState: TcxCheckBoxState);
    procedure FormShow(Sender: TObject);
    procedure aReloadEventsExecute(Sender: TObject);
  private
    FAuthorizationAgents: TObjectList<TdxCustomAuthorizationAgent>;

    function GetAuthorizationAgentClass(AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass): TdxCustomAuthorizationAgentClass;

    procedure AddAccount(AIndex: Integer); overload;
    procedure AddAccount(AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass); overload;
    function AddAccount(AClass: TdxCustomAuthorizationAgentClass; AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass): TcxTreeListNode; overload;
    procedure AddAccountClick(Sender: TObject);
    procedure PopulateAccountNode(ANode: TcxTreeListNode; AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass);

    procedure AddResource(AAuthIndex, ACalendarIndex: Integer);
    procedure RemoveResource(AAuthIndex: Integer; const ACalendarId: string);

    procedure LoadData;
    procedure SaveData;
  end;

var
  WebServiceDemoMainForm: TWebServiceDemoMainForm;

implementation

uses
  Rtti, IniFiles,
  WebServiceDemoSetupForm;

{$R *.dfm}

const
  WebServiceDemoIniFileName: string = '';

procedure TWebServiceDemoMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
  AMenuItem: TMenuItem;
  AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass;
begin
  FAuthorizationAgents := TObjectList<TdxCustomAuthorizationAgent>.Create;

  RegisterClass(TdxGoogleAPIOAuth2AuthorizationAgent);
  RegisterClass(TdxMicrosoftGraphAPIOAuth2AuthorizationAgent);

  for I := 0 to TcxSchedulerWebServiceStorage.RegisteredCalendarProviders.Count - 1 do
  begin
    AMenuItem := TMenuItem.Create(pmAddAccount);
    AMenuItem.Tag := I;
    AMenuItem.OnClick := AddAccountClick;
    AMenuItem.Caption := TcxSchedulerWebServiceStorageCustomProviderClass(TcxSchedulerWebServiceStorage.RegisteredCalendarProviders[I]).GetDisplayName;
    AProviderClass := TcxSchedulerWebServiceStorageCustomProviderClass(TcxSchedulerWebServiceStorage.RegisteredCalendarProviders[I]);
    RegisterClass(AProviderClass);
    pmAddAccount.Items.Add(AMenuItem);
  end;
  tlCalendars.Root.CheckGroupType := ncgCheckGroup;
  inherited;
  Resources1.Visible := True;
  Scheduler.SelectDays(Date, Date, True);
end;

procedure TWebServiceDemoMainForm.FormDestroy(Sender: TObject);
begin
  SaveData;
  FAuthorizationAgents.Free;
  inherited;
end;

procedure TWebServiceDemoMainForm.FormShow(Sender: TObject);
var
  AWizard: TWebServiceDemoSetupWizard;
begin
  AWizard := TWebServiceDemoSetupWizard.Create(nil);
  try
    if AWizard.ShowModal = mrOk then
    begin
      TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.DefaultClientId := AWizard.teMSGraphClientID.Text;
      TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.DefaultClientSecret := AWizard.teMSGraphClientSecret.Text;
      TdxGoogleAPIOAuth2AuthorizationAgent.DefaultClientId := AWizard.teGoogleApiClientID.Text;
      TdxGoogleAPIOAuth2AuthorizationAgent.DefaultClientSecret := AWizard.teGoogleApiClientSecret.Text;
      pmAddAccount.Items[TcxSchedulerWebServiceStorage.RegisteredCalendarProviders.GetIndexByClass(TcxSchedulerWebServiceStorageOfficeProvider)].Enabled := (TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.DefaultClientId <> '') and (TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.DefaultClientSecret <> '');
      pmAddAccount.Items[TcxSchedulerWebServiceStorage.RegisteredCalendarProviders.GetIndexByClass(TcxSchedulerWebServiceStorageGoogleProvider)].Enabled := (TdxGoogleAPIOAuth2AuthorizationAgent.DefaultClientId <> '') and (TdxGoogleAPIOAuth2AuthorizationAgent.DefaultClientSecret <> '');
      LoadData;
    end
    else
      Close;
  finally
    AWizard.Free;
  end;
end;

procedure TWebServiceDemoMainForm.tlCalendarsNodeCheckChanged(
  Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
var
  AAuthIndex: Integer;
  ACalIndex: Integer;
begin
  if ANode.Level = 0 then
    Exit;
  AAuthIndex := ANode.Parent.Index;
  ACalIndex := ANode.Index;
  if AState = cbsChecked then
    AddResource(AAuthIndex, ACalIndex)
  else
    RemoveResource(AAuthIndex, ANode.Values[1]);
end;

function TWebServiceDemoMainForm.GetAuthorizationAgentClass(AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass): TdxCustomAuthorizationAgentClass;
var
  AContext: TRttiContext;
  AType: TRttiType;
  AProperty: TRttiProperty;
begin
  AContext := TRttiContext.Create;
  try
    AType := AContext.GetType(AProviderClass);
    AProperty := AType.GetProperty('AuthorizationAgent');
    Result := TdxCustomAuthorizationAgentClass(FindClass(AProperty.PropertyType.ToString));
  finally
    AContext.Free;
  end;
end;

procedure TWebServiceDemoMainForm.AddAccount(AIndex: Integer);
var
  AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass;
begin
  AProviderClass := TcxSchedulerWebServiceStorageCustomProviderClass(TcxSchedulerWebServiceStorage.RegisteredCalendarProviders[AIndex]);
  AddAccount(AProviderClass);
end;

procedure TWebServiceDemoMainForm.AddAccount(AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass);
var
  AClass: TdxCustomAuthorizationAgentClass;
  ANode: TcxTreeListNode;
begin
  AClass := GetAuthorizationAgentClass(AProviderClass);

  ANode := AddAccount(AClass, AProviderClass);
  PopulateAccountNode(ANode, AProviderClass);
  if not FAuthorizationAgents.Last.IsAuthorized then
  begin
    ANode.Free;
    FAuthorizationAgents.Remove(FAuthorizationAgents.Last);
  end;
end;

function TWebServiceDemoMainForm.AddAccount(AClass: TdxCustomAuthorizationAgentClass; AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass): TcxTreeListNode;
var
  AAgent: TdxCustomAuthorizationAgent;
begin
  AAgent := AClass.Create(nil);
  FAuthorizationAgents.Add(AAgent);
  Result := tlCalendars.Add;
  Result.Values[0] := '';
  Result.Values[1] := AProviderClass.ClassName;
  Result.AllowGrayed := False;
  Result.CheckGroupType := ncgCheckGroup;
end;

procedure TWebServiceDemoMainForm.AddAccountClick(Sender: TObject);
begin
  AddAccount(TMenuItem(Sender).Tag);
end;

procedure TWebServiceDemoMainForm.PopulateAccountNode(ANode: TcxTreeListNode; AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass);
var
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
  AList: TdxWebServiceCalendarList;
  I: Integer;
  AUserInfo: TdxAuthorizationAgentUserInfo;
begin
  AProvider := AProviderClass.Create(Storage);
  try
    AProvider.AuthorizationAgent := FAuthorizationAgents[ANode.Index];
    AUserInfo := TdxAuthorizationAgentUserInfo.GetUserInfo(FAuthorizationAgents.Last);
    try
      AList := AProvider.GetCalendarList;
      try
        if not AProvider.AuthorizationAgent.IsAuthorized then
          Exit;

        AUserInfo.UpdateInfo;
        ANode.Values[0] := Format('%s (%s)', [AUserInfo.DisplayName, AUserInfo.Mail]);
        for I := 0 to AList.Count - 1 do
        begin
          with ANode.AddChild do
          begin
            Values[0] := AList[I].Name;
            Values[1] := AList[I].Id;
          end;
        end;
      finally
        AList.Free;
      end;
    finally
      AUserInfo.Free;
    end;
  finally
    AProvider.Free;
  end;
  ANode.Expand(False);
end;

procedure TWebServiceDemoMainForm.AddResource(AAuthIndex, ACalendarIndex: Integer);
begin
  with Storage.Resources.Items.Add do
  begin
    ProviderClassName := tlCalendars.Root.Items[AAuthIndex].Values[1];
    Provider.AuthorizationAgent := FAuthorizationAgents[AAuthIndex];
    Provider.CalendarId := tlCalendars.Root.Items[AAuthIndex].Items[ACalendarIndex].Values[1];
    Name := Format('%s: %s', [Provider.GetDisplayName, Provider.Calendar.Name]);
    Provider.Connected := True;
  end;
  aReloadEvents.Enabled := True;
end;

procedure TWebServiceDemoMainForm.aReloadEventsExecute(Sender: TObject);
begin
  Storage.FullRefresh;
end;

procedure TWebServiceDemoMainForm.RemoveResource(AAuthIndex: Integer; const ACalendarId: string);
var
  I: Integer;
begin
  for I := Storage.Resources.Items.Count - 1 downto 0 do
    if Storage.Resources.Items[I].ResourceID = ACalendarId then
      if Storage.Resources.Items[I].Provider.AuthorizationAgent = FAuthorizationAgents[AAuthIndex] then
        Storage.Resources.Items.Delete(I);
  aReloadEvents.Enabled := Storage.ResourceCount > 0;
end;

procedure TWebServiceDemoMainForm.LoadData;
var
  AIniFile: TIniFile;
  I, J: Integer;
  ASection: string;
  AAuthClassName: string;
  AAuthClass: TdxCustomAuthorizationAgentClass;
  AProviderClassName: string;
  AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass;
  ATokenAccess, ATokenRefresh, ATokenType: string;
  ANode: TcxTreeListNode;
  AIndex: Integer;
  ACalendarId: string;
begin
  if not FileExists(WebServiceDemoIniFileName) then
    Exit;
  AIniFile := TIniFile.Create(WebServiceDemoIniFileName);
  try
    I := 0;
    ASection := Format('Auth%d', [I]);
    while AIniFile.SectionExists(ASection) do
    begin
      AAuthClassName := AIniFile.ReadString(ASection, 'class', '');
      AAuthClass := TdxCustomAuthorizationAgentClass(FindClass(AAuthClassName));
      if AAuthClass <> nil then
      begin
        ATokenAccess := AIniFile.ReadString(ASection, 'token_access', '');
        ATokenRefresh := AIniFile.ReadString(ASection, 'token_refresh', '');
        ATokenType := AIniFile.ReadString(ASection, 'token_type', '');
        AProviderClassName := AIniFile.ReadString(ASection, 'provider_class', '');
        AProviderClass := TcxSchedulerWebServiceStorageCustomProviderClass(FindClass(AProviderClassName));
        ANode := AddAccount(AAuthClass, AProviderClass);
        TdxOAuth2AuthorizationAgent(FAuthorizationAgents.Last).Load(ATokenAccess, ATokenRefresh, ATokenType);
        PopulateAccountNode(ANode, AProviderClass);
      end;
      Inc(I);
      ASection := Format('Auth%d', [I]);
    end;
    I := 0;
    ASection := Format('Res%d', [I]);
    while AIniFile.SectionExists(ASection) do
    begin
      AIndex := AIniFile.ReadInteger(ASection, 'authorization', 0);
      if AIndex < FAuthorizationAgents.Count then
      begin
        ACalendarId := AIniFile.ReadString(ASection, 'calendar_id', '');
        for J := 0 to tlCalendars.Root.Items[AIndex].Count - 1 do
          if tlCalendars.Root.Items[AIndex].Items[J].Values[1] = ACalendarId then
          begin
            tlCalendars.Root.Items[AIndex].Items[J].CheckClick;
            Break;
          end;
      end;
      Inc(I);
      ASection := Format('Res%d', [I]);
    end;
  finally
    AIniFile.Free;
  end;
end;

procedure TWebServiceDemoMainForm.SaveData;
var
  AIniFile: TIniFile;
  I: Integer;
  ASection: string;
begin
  AIniFile := TIniFile.Create(WebServiceDemoIniFileName);
  try
    for I := 0 to FAuthorizationAgents.Count - 1 do
    begin
      ASection := Format('Auth%d', [I]);
      AIniFile.EraseSection(ASection);
      AIniFile.WriteString(ASection, 'class', FAuthorizationAgents[I].ClassName);
      AIniFile.WriteString(ASection, 'provider_class', tlCalendars.Root.Items[I].Values[1]);
      AIniFile.WriteString(ASection, 'token_access', TdxOAuth2AuthorizationAgent(FAuthorizationAgents[I]).AccessToken);
      AIniFile.WriteString(ASection, 'token_refresh', TdxOAuth2AuthorizationAgent(FAuthorizationAgents[I]).RefreshToken);
      AIniFile.WriteString(ASection, 'token_type', TdxOAuth2AuthorizationAgent(FAuthorizationAgents[I]).AccessTokenType);
    end;
    AIniFile.EraseSection(Format('Auth%d', [FAuthorizationAgents.Count]));
    for I := 0 to Storage.ResourceCount - 1 do
    begin
      ASection := Format('Res%d', [I]);
      AIniFile.EraseSection(ASection);
      AIniFile.WriteInteger(ASection, 'authorization', FAuthorizationAgents.IndexOf(Storage.Resources.Items[I].Provider.AuthorizationAgent));
      AIniFile.WriteString(ASection, 'calendar_id', Storage.Resources.Items[I].Provider.CalendarId);
    end;
    AIniFile.EraseSection(Format('Res%d', [Storage.ResourceCount]));
    AIniFile.UpdateFile;
  finally
    AIniFile.Free;
  end;
end;

initialization
  WebServiceDemoIniFileName := ExtractFilePath(Application.ExeName) + 'Demo.ini';

end.
