//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RibbonRichEditDemoOptions.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TRibbonDemoOptionsForm *RibbonDemoOptionsForm;

BOOL ExecuteRibbonDemoOptions(TScreenTipOptions *AScreenTipOptions, TRibbonDemoStyle *AStyle,
	TdxRibbonColorSchemeAccent *AColorSchemeAccent)
{
	TRibbonDemoOptionsForm *Form = new TRibbonDemoOptionsForm(NULL);
	Form->PopulateRibbonStyles(Form->cbRibbonStyle->Properties->Items);
	Form->PopulateColorSchemeAccents(Form->cbColorSchemeAccent->Properties->Items);
	Form->LoadOptions(*AScreenTipOptions, *AStyle, *AColorSchemeAccent);
	BOOL Result = Form->ShowModal() == mrOk;
	if (Result)
	  Form->SaveOptions(AScreenTipOptions, AStyle, AColorSchemeAccent);
	delete Form;
	return Result;
}

TdxRibbonStyle RibbonDemoStyleToRibbonStyle(TRibbonDemoStyle ADemoStyle)
{
	switch (ADemoStyle)
	{
		case rdsOffice2007:
			return rs2007;
		case rdsOffice2010:
			return rs2010;
		case rdsOffice2013:
			return rs2013;
		case rdsOffice2016:
			return rs2016;
		case rdsOffice2016Tablet:
			return rs2016Tablet;
		case rdsOffice2019:
			return rs2019;
		default: // Scenic
			return rs2010;
	}
}

//---------------------------------------------------------------------------
__fastcall TRibbonDemoOptionsForm::TRibbonDemoOptionsForm(TComponent* Owner)
		: TForm(Owner)
{
	lblColorScheme->Visible = False;
	cbColorScheme->Visible = False;
}
//---------------------------------------------------------------------------
void TRibbonDemoOptionsForm::PopulateColorSchemeAccents(TStrings *AItems)
{
	AItems->BeginUpdate();
	try
	{
		AItems->Clear();
		for (int I = (int)rcsaYellow; I <= (int)rcsaPurple; I++)
		{
			AItems->Add(dxRibbonColorSchemeAccentNames[(TdxRibbonColorSchemeAccent)I]);
		}
	}
	__finally
	{
		AItems->EndUpdate();
	}
}
//---------------------------------------------------------------------------
void TRibbonDemoOptionsForm::PopulateRibbonStyles(TStrings *AItems)
{
	AItems->BeginUpdate();
	try
	{
		AItems->Clear();
		AItems->Add("Office 2007");
		AItems->Add("Office 2010");
		AItems->Add("Office 2013");
		AItems->Add("Office 2016");
		AItems->Add("Office 2016 Tablet");
		AItems->Add("Office 2019");
		AItems->Add("Scenic");
	}
	__finally
	{
		AItems->EndUpdate();
	};
};
//---------------------------------------------------------------------------
void __fastcall TRibbonDemoOptionsForm::cbRibbonStyleSelect(TObject *Sender)
{
	lblColorSchemeAccent->Enabled = cbRibbonStyle->ItemIndex > 0;
}
//---------------------------------------------------------------------------
void TRibbonDemoOptionsForm::LoadOptions(TScreenTipOptions AScreenTipOptions, TRibbonDemoStyle AStyle,
	TdxRibbonColorSchemeAccent AColorSchemeAccent)
{
	cbRibbonStyle->ItemIndex = (Integer)AStyle;
	cbRibbonStyleSelect(NULL);

	cbColorSchemeAccent->ItemIndex = (Integer)AColorSchemeAccent;
	if (AScreenTipOptions.ShowScreenTips)
		cbScreenTipStyle->ItemIndex = (Integer)(!AScreenTipOptions.ShowDescripitons);
	else
    	cbScreenTipStyle->ItemIndex = 2;
}
//---------------------------------------------------------------------------
void TRibbonDemoOptionsForm::SaveOptions(TScreenTipOptions *AScreenTipOptions, TRibbonDemoStyle *AStyle,
	TdxRibbonColorSchemeAccent *AColorSchemeAccent)
{
	* AStyle = (TRibbonDemoStyle)cbRibbonStyle->ItemIndex;
	* AColorSchemeAccent = (TdxRibbonColorSchemeAccent)cbColorSchemeAccent->ItemIndex;
	AScreenTipOptions->ShowScreenTips = cbScreenTipStyle->ItemIndex != 2;
	AScreenTipOptions->ShowDescripitons = cbScreenTipStyle->ItemIndex == 0;	
}
//---------------------------------------------------------------------------
