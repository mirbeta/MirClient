//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "WebServiceDemoMain.h"
#include "WebServiceDemoSetupForm.h"
//---------------------------------------------------------------------------
#pragma link "dxAuthorizationAgents"
#pragma link "cxSchedulerWebServiceStorage"
#pragma link "cxSchedulerWebServiceStorageGoogleProvider"
#pragma link "cxSchedulerWebServiceStorageOfficeProvider"
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxCustomData"
#pragma link "cxDateNavigator"
#pragma link "cxInplaceContainer"
#pragma link "cxSplitter"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "Rtti"
#pragma resource "*.dfm"
TWebServiceDemoMainForm *WebServiceDemoMainForm;

//---------------------------------------------------------------------------
__fastcall TWebServiceDemoMainForm::TWebServiceDemoMainForm(TComponent* Owner)
		: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TWebServiceDemoMainForm::aReloadEventsExecute(TObject *Sender)
{
  Scheduler->FullRefresh();
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::AddAccount(int AIndex)
{
	TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass = TcxSchedulerWebServiceStorageCustomProviderClass(TcxSchedulerWebServiceStorage::RegisteredCalendarProviders->Items[AIndex]);
	this->AddAccount(AProviderClass);
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::AddAccount(TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass)
{
  TdxCustomAuthorizationAgentClass AClass = GetAuthorizationAgentClass(AProviderClass);
  TcxTreeListNode *ANode = AddAccount(AClass, AProviderClass);
  this->PopulateAccountNode(ANode, AProviderClass);
  TdxCustomAuthorizationAgent *agent = (TdxCustomAuthorizationAgent*)(FAuthorizationAgents->Last());
  if (!agent->IsAuthorized) {
	ANode->Free();
	FAuthorizationAgents->Remove(agent);
  }
}
//---------------------------------------------------------------------------

TcxTreeListNode* __fastcall TWebServiceDemoMainForm::AddAccount(TdxCustomAuthorizationAgentClass AClass, TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass)
{
	TdxCustomAuthorizationAgent *agent;

	if (AClass == __classid(TdxGoogleAPIOAuth2AuthorizationAgent)) {
		agent = new TdxGoogleAPIOAuth2AuthorizationAgent(NULL);
	} else {
		agent = new TdxMicrosoftGraphAPIOAuth2AuthorizationAgent(NULL);
	}

	FAuthorizationAgents->Add(agent);
	TcxTreeListNode* Result = tlCalendars->Add();
	Result->Values[0] = "";
	Result->Values[1] = AProviderClass->ClassName();
	Result->AllowGrayed = false;
	Result->CheckGroupType = ncgCheckGroup;
	return Result;
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::AddAccountClick(TObject *Sender)
{
	TMenuItem *item = (TMenuItem*)Sender;
	AddAccount(item->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::PopulateAccountNode(TcxTreeListNode *ANode, TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass)
{
	TcxSchedulerWebServiceStorageCustomProvider *AProvider = Storage->CreateProvider(AProviderClass);
	TdxCustomAuthorizationAgent* agent = (TdxCustomAuthorizationAgent*)(FAuthorizationAgents->Items[ANode->Index]);
	AProvider->AuthorizationAgent = agent;

	TdxAuthorizationAgentUserInfo *AUserInfo = TdxAuthorizationAgentUserInfo::GetUserInfo(agent);
	TdxWebServiceCalendarList *AList = AProvider->GetCalendarList();

	if (AProvider->AuthorizationAgent->IsAuthorized) {
		AUserInfo->UpdateInfo();
		ANode->Values[0] = AUserInfo->DisplayName + " (" + AUserInfo->Mail + ")";
		for (int i = 0; i < AList->Count; i++) {
			TcxTreeListNode *child = ANode->AddChild();
			TdxWebServiceCalendar ACalendar = AList->Items[i];
			child->Values[0] = ACalendar.Name;
			child->Values[1] = ACalendar.ID;
		}
	}
	AList->Free();
	AUserInfo->Free();
	AProvider->Free();
  	ANode->Expand(false);
}
//---------------------------------------------------------------------------

TdxCustomAuthorizationAgentClass __fastcall TWebServiceDemoMainForm::GetAuthorizationAgentClass(TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass)
{
	TRttiContext AContext;
	TRttiType *AType = AContext.GetType(AProviderClass);
	TRttiProperty *AProperty = AType->GetProperty("AuthorizationAgent");
	return (TdxCustomAuthorizationAgentClass)FindClass(AProperty->PropertyType->ToString());
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::AddResource(int AAuthIndex, int ACalendarIndex)
{
	TcxSchedulerWebServiceStorageResourceItem* item = Storage->Resources->Items->Add();
	item->ProviderClassName = tlCalendars->Root->Items[AAuthIndex]->Values[1];
	TdxCustomAuthorizationAgent *agent = (TdxCustomAuthorizationAgent*)(FAuthorizationAgents->Items[AAuthIndex]);
	item->Provider->AuthorizationAgent = agent;
	item->Provider->CalendarID = tlCalendars->Root->Items[AAuthIndex]->Items[ACalendarIndex]->Values[1];
	item->Name = item->Provider->GetDisplayName() + ": " + item->Provider->Calendar.Name;
	item->Provider->Connected = true;
    aReloadEvents->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::RemoveResource(int AAuthIndex, UnicodeString ACalendarId)
{
  for (int I = Storage->Resources->Items->Count - 1; I >= 0; I--) {
	if (VarToStr(Storage->Resources->Items->Items[I]->ResourceID) == ACalendarId)
	  if (Storage->Resources->Items->Items[I]->Provider->AuthorizationAgent == FAuthorizationAgents->Items[AAuthIndex])
		Storage->Resources->Items->Delete(I);
  }
  aReloadEvents->Enabled = Storage->ResourceCount > 0;
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::LoadData()
{
	UnicodeString fileName = ExtractFilePath(Application->ExeName) + "Demo.ini";
	if (!FileExists(fileName))
		return;
	TIniFile *AIniFile = new TIniFile(fileName);
	int I = 0;
	UnicodeString ASection;
	ASection = "Auth" + IntToStr(I);
	while (AIniFile->SectionExists(ASection)) {
		UnicodeString AAuthClassName = AIniFile->ReadString(ASection, "class", "");
		TdxCustomAuthorizationAgentClass AAuthClass = TdxCustomAuthorizationAgentClass(FindClass(AAuthClassName));
		if (AAuthClass != NULL) {
			UnicodeString ATokenAccess = AIniFile->ReadString(ASection, "token_access", "");
			UnicodeString ATokenRefresh = AIniFile->ReadString(ASection, "token_refresh", "");
			UnicodeString ATokenType = AIniFile->ReadString(ASection, "token_type", "");
			UnicodeString AProviderClassName = AIniFile->ReadString(ASection, "provider_class", "");
			TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass = TcxSchedulerWebServiceStorageCustomProviderClass(FindClass(AProviderClassName));
			TcxTreeListNode *ANode = AddAccount(AAuthClass, AProviderClass);
			TdxOAuth2AuthorizationAgent *agent = (TdxOAuth2AuthorizationAgent*)FAuthorizationAgents->Last();
			agent->Load(ATokenAccess, ATokenRefresh, ATokenType);
			PopulateAccountNode(ANode, AProviderClass);
		}
		I++;
	  	ASection = "Auth" + IntToStr(I);
	}
	I = 0;
	ASection = "Res" + IntToStr(I);
	while (AIniFile->SectionExists(ASection)) {
		int AIndex = AIniFile->ReadInteger(ASection, "authorization", 0);
		if (AIndex < FAuthorizationAgents->Count) {
			UnicodeString ACalendarId = AIniFile->ReadString(ASection, "calendar_id", "");
			for (int J = 0; J < tlCalendars->Root->Items[AIndex]->Count; J++) {
				if (VarToStr(tlCalendars->Root->Items[AIndex]->Items[J]->Values[1]) == ACalendarId) {
					tlCalendars->Root->Items[AIndex]->Items[J]->CheckClick();
					break;
				}
			}
		}
	  I++;
	  ASection = "Res" + IntToStr(I);
	}
	AIniFile->Free();
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::SaveData()
{
	UnicodeString ASection;
	UnicodeString fileName = ExtractFilePath(Application->ExeName) + "Demo.ini";
	TIniFile *AIniFile = new TIniFile(fileName);
	for (int I = 0; I < FAuthorizationAgents->Count; I++) {
		ASection = "Auth" + IntToStr(I);
		AIniFile->EraseSection(ASection);
		AIniFile->WriteString(ASection, "class", FAuthorizationAgents->Items[I]->ClassName());
		AIniFile->WriteString(ASection, "provider_class", tlCalendars->Root->Items[I]->Values[1]);
		TdxOAuth2AuthorizationAgent *agent = (TdxOAuth2AuthorizationAgent*)FAuthorizationAgents->Items[I];
		AIniFile->WriteString(ASection, "token_access", agent->AccessToken);
		AIniFile->WriteString(ASection, "token_refresh", agent->RefreshToken);
		AIniFile->WriteString(ASection, "token_type", agent->AccessTokenType);
    }
	AIniFile->EraseSection("Auth" + IntToStr(FAuthorizationAgents->Count));
	for (int I = 0; I < Storage->ResourceCount; I++) {
	  	ASection = "Res" + IntToStr(I);
		AIniFile->EraseSection(ASection);
	  	AIniFile->WriteInteger(ASection, "authorization", FAuthorizationAgents->IndexOf(Storage->Resources->Items->Items[I]->Provider->AuthorizationAgent));
		AIniFile->WriteString(ASection, "calendar_id", Storage->Resources->Items->Items[I]->Provider->CalendarID);
	}
	AIniFile->EraseSection("Res" + IntToStr(Storage->ResourceCount));
	AIniFile->UpdateFile();

	AIniFile->Free();
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::FormCreate(TObject *Sender)
{
	FAuthorizationAgents = new TObjectList();

	RegisterClass(__classid(TdxGoogleAPIOAuth2AuthorizationAgent));
	RegisterClass(__classid(TdxMicrosoftGraphAPIOAuth2AuthorizationAgent));

	for (int I = 0; I < TcxSchedulerWebServiceStorage::RegisteredCalendarProviders->Count; I++) {
		TMenuItem *AMenuItem = new TMenuItem(pmAddAccount);
		AMenuItem->Tag = I;
		AMenuItem->OnClick = AddAccountClick;
		AMenuItem->Caption = TcxSchedulerWebServiceStorage::RegisteredCalendarProviders->Descriptions[I];
		pmAddAccount->Items->Add(AMenuItem);
	}
	tlCalendars->Root->CheckGroupType = ncgCheckGroup;

  	Scheduler->SelectDays(Date(), Date(), true);
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::tlCalendarsNodeCheckChanged(TcxCustomTreeList *Sender,
          TcxTreeListNode *ANode, TcxCheckBoxState AState)
{
	if (ANode->Level == 0)
		return;
	int AAuthIndex = ANode->Parent->Index;
  	int ACalIndex = ANode->Index;
	if (AState == cbsChecked)
		this->AddResource(AAuthIndex, ACalIndex);
	else {
		UnicodeString calendar = VarToStr(ANode->Values[1]);
		this->RemoveResource(AAuthIndex, calendar);
	}
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::FormDestroy(TObject *Sender)
{
    this->SaveData();
	FAuthorizationAgents->Free();
}
//---------------------------------------------------------------------------

void __fastcall TWebServiceDemoMainForm::FormShow(TObject *Sender)
{
	bool AClose;
	TWebServiceDemoSetupWizard *AWizard = new TWebServiceDemoSetupWizard(NULL);
	if (AWizard->ShowModal() == mrOk) {
		TdxMicrosoftGraphAPIOAuth2AuthorizationAgent::DefaultClientID = AWizard->teMSGraphClientID->Text;
		TdxMicrosoftGraphAPIOAuth2AuthorizationAgent::DefaultClientSecret = AWizard->teMSGraphClientSecret->Text;
		TdxGoogleAPIOAuth2AuthorizationAgent::DefaultClientID = AWizard->teGoogleApiClientID->Text;
		TdxGoogleAPIOAuth2AuthorizationAgent::DefaultClientSecret = AWizard->teGoogleApiClientSecret->Text;
		pmAddAccount->Items->Items[TcxSchedulerWebServiceStorage::RegisteredCalendarProviders->GetIndexByClass(__classid(TcxSchedulerWebServiceStorageOfficeProvider))]->Enabled =
			TdxMicrosoftGraphAPIOAuth2AuthorizationAgent::DefaultClientID != "" && TdxMicrosoftGraphAPIOAuth2AuthorizationAgent::DefaultClientSecret != "";
		pmAddAccount->Items->Items[TcxSchedulerWebServiceStorage::RegisteredCalendarProviders->GetIndexByClass(__classid(TcxSchedulerWebServiceStorageGoogleProvider))]->Enabled =
			TdxGoogleAPIOAuth2AuthorizationAgent::DefaultClientID != "" && TdxGoogleAPIOAuth2AuthorizationAgent::DefaultClientSecret != "";
		LoadData();
		AClose = false;
	} else AClose = true;
	AWizard->Free();
	if (AClose)
        Close();
}
//---------------------------------------------------------------------------

