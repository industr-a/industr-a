Public Class Form1
    Dim IA_Argent As Decimal
    Dim argent As Decimal = 35000000000000
    Dim velo As Double = 0
    Dim velo_vente As Double = 0
    Dim velo_prix As Double = 100
    Dim TV As Double = 0
    Dim TV_vente As Double = 0
    Dim TV_prix As Double = 2500
    Dim nouille As Double = 0
    Dim nouille_vente As Double = 0
    Dim nouille_prix As Double = 2.5
    Dim lego As Double = 0
    Dim lego_vente As Double = 0
    Dim lego_prix As Double = 15
    Dim btban As Double = 0
    Dim btban_vente As Double = 0
    Dim btban_prix As Double = 25
    Dim km As Double
    Dim caravan As Double
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        MsgBox("Vous avez " & argent & " $." & vbNewLine & "Votre concurent as " & IA_Argent & " $.", , "Résultat de la Partie")
        End
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Timer1.Enabled = True
        Timer2.Enabled = True
        Timer3.Enabled = True
        IA.Enabled = True
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
        Label9.Text = TV & " TV(s) en stock"
        Label3.Text = TV_vente & " TV(s) en vente"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
        Label17.Text = btban & " Boite(s) de bananes en stock"
        Label16.Text = btban_vente & " Boite(s) de bananes en vente"
        Randomize()

    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        If velo_vente > 1000000 Then velo_prix = 50
        If velo_vente <= 1000000 Then velo_prix = 60
        If velo_vente < 1000000 And velo_vente >= 700000 Then velo_prix = 70
        If velo_vente < 700000 And velo_vente >= 600000 Then velo_prix = 80
        If velo_vente < 600000 And velo_vente >= 500000 Then velo_prix = 90
        If velo_vente < 500000 And velo_vente >= 10000 Then velo_prix = 100
        If velo_vente < 10000 And velo_vente >= 0 Then velo_prix = 120
        If velo_vente > 0 Then argent = argent + velo_prix
        If velo_vente > 0 Then velo_vente = velo_vente - 1
        If btban_vente > 1000000 Then btban_prix = 20
        If btban_vente <= 1000000 Then btban_prix = 22
        If btban_vente < 1000000 And btban_vente >= 700000 Then btban_prix = 24
        If btban_vente < 700000 And btban_vente >= 600000 Then btban_prix = 25
        If btban_vente < 600000 And btban_vente >= 500000 Then btban_prix = 26
        If btban_vente < 500000 And btban_vente >= 10000 Then btban_prix = 28
        If btban_vente < 10000 And btban_vente >= 0 Then btban_prix = 30
        If btban_vente > 0 Then argent = argent + btban_prix
        If btban_vente > 0 Then btban_vente = btban_vente - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
        Label9.Text = TV & " TV(s) en stock"
        Label3.Text = TV_vente & " TV(s) en vente"
        Label17.Text = btban & " Boite(s) de bananes en stock"
        Label16.Text = btban_vente & " Boite(s) de bananes en vente"
        Label13.Text = km & " Km² disponible"
        Label19.Text = caravan & " Parc(s) de maisons disponibles"
        Timer1.Interval = (Rnd() * 250) + 1
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        'Le bouton Metre 1 Vélo en vente.
        If velo <= 0 Then MsgBox("Il ne reste plus de vélo.")
        If velo > 0 Then velo_vente = velo_vente + 1
        If velo > 0 Then velo = velo - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        'Le bouton construire 1 vélo
        If argent < 60 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 vélo.")
        If argent >= 60 Then velo = velo + 1
        If argent >= 60 Then argent = argent - 60
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        'Le bouton construire 10 vélo
        If argent < 600 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 vélo.")
        If argent >= 600 Then velo = velo + 10
        If argent >= 600 Then argent = argent - 600
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        'Le bouton Metre 10 Vélo en vente.
        If velo < 10 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 10 Then velo_vente = velo_vente + 10
        If velo >= 10 Then velo = velo - 10
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        'Le bouton construire 100 vélo
        If argent < 6000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 vélo.")
        If argent >= 6000 Then velo = velo + 100
        If argent >= 6000 Then argent = argent - 6000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        'Le bouton Metre 100 Vélo en vente.
        If velo < 100 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 100 Then velo_vente = velo_vente + 100
        If velo >= 100 Then velo = velo - 100
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        'Le bouton construire 1 000 vélo
        If argent < 60000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 vélo.")
        If argent >= 60000 Then velo = velo + 1000
        If argent >= 60000 Then argent = argent - 60000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        'Le bouton Metre 1000 Vélo en vente.
        If velo < 1000 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 1000 Then velo_vente = velo_vente + 1000
        If velo >= 1000 Then velo = velo - 1000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        'Le bouton construire 10 000 vélo
        If argent < 600000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 vélo.")
        If argent >= 600000 Then velo = velo + 10000
        If argent >= 600000 Then argent = argent - 600000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click
        If velo < 10000 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 10000 Then velo_vente = velo_vente + 10000
        If velo >= 10000 Then velo = velo - 10000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click
        'Le bouton construire 100 000 vélo
        If argent < 6000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 vélo.")
        If argent >= 6000000 Then velo = velo + 100000
        If argent >= 6000000 Then argent = argent - 6000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button13.Click
        If velo < 100000 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 100000 Then velo_vente = velo_vente + 100000
        If velo >= 100000 Then velo = velo - 100000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button14.Click
        'Le bouton construire 1 000 000 vélo
        If argent < 60000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 vélo.")
        If argent >= 60000000 Then velo = velo + 1000000
        If argent >= 60000000 Then argent = argent - 60000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button15.Click
        If velo < 1000000 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 1000000 Then velo_vente = velo_vente + 1000000
        If velo >= 1000000 Then velo = velo - 1000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button16.Click
        'Le bouton construire 10 000 000 vélo
        If argent < 600000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 000 vélo.")
        If argent >= 600000000 Then velo = velo + 10000000
        If argent >= 600000000 Then argent = argent - 600000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button17.Click
        If velo < 10000000 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 10000000 Then velo_vente = velo_vente + 10000000
        If velo >= 10000000 Then velo = velo - 10000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button18.Click
        'Le bouton construire 100 000 000 vélo
        If argent < 6000000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 000 vélo.")
        If argent >= 6000000000 Then velo = velo + 100000000
        If argent >= 6000000000 Then argent = argent - 6000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button19.Click
        If velo < 100000000 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 100000000 Then velo_vente = velo_vente + 100000000
        If velo >= 100000000 Then velo = velo - 100000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button20.Click
        'Le bouton construire 1 000 000 000 vélo
        If argent < 60000000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 000 vélo.")
        If argent >= 60000000000 Then velo = velo + 1000000000
        If argent >= 60000000000 Then argent = argent - 60000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Button21_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button21.Click
        If velo < 1000000000 Then MsgBox("Il ne reste pas sufisament de vélo.")
        If velo >= 1000000000 Then velo_vente = velo_vente + 1000000000
        If velo >= 1000000000 Then velo = velo - 1000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = velo & " Vélo(s) en stock"
        Label2.Text = velo_vente & " Vélo(s) en vente"
    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        If TV_vente > 1000000 Then TV_prix = 2250
        If TV_vente <= 1000000 Then TV_prix = 2500
        If TV_vente < 1000000 And TV_vente >= 700000 Then TV_prix = 2600
        If TV_vente < 700000 And TV_vente >= 600000 Then TV_prix = 2650
        If TV_vente < 600000 And TV_vente >= 500000 Then TV_prix = 2700
        If TV_vente < 500000 And TV_vente >= 10000 Then TV_prix = 2800
        If TV_vente < 10000 And TV_vente >= 0 Then TV_prix = 3000
        If TV_vente > 0 Then argent = argent + TV_prix
        If TV_vente > 0 Then TV_vente = TV_vente - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Timer2.Interval = (Rnd() * 2500) + 1
    End Sub
    Private Sub Button40_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button40.Click
        'Le bouton construire 1 TV
        If argent < 2400 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 TV.")
        If argent >= 2400 Then TV = TV + 1
        If argent >= 2400 Then argent = argent - 2400
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub
    Private Sub Button41_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button41.Click
        'Le bouton Metre 1 TV en vente.
        If TV <= 0 Then MsgBox("Il ne reste plus de TV.")
        If TV > 0 Then TV_vente = TV_vente + 1
        If TV > 0 Then TV = TV - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button38_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button38.Click
        'Le bouton construire 10 TV
        If argent < 24000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 TV.")
        If argent >= 24000 Then TV = TV + 10
        If argent >= 24000 Then argent = argent - 24000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button42_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button42.Click
        'rechercher TV dans les R&D
        If argent >= 10000000 Then
            Button22.Enabled = True
            Button23.Enabled = True
            Button24.Enabled = True
            Button25.Enabled = True
            Button26.Enabled = True
            Button27.Enabled = True
            Button28.Enabled = True
            Button29.Enabled = True
            Button30.Enabled = True
            Button31.Enabled = True
            Button32.Enabled = True
            Button33.Enabled = True
            Button34.Enabled = True
            Button35.Enabled = True
            Button36.Enabled = True
            Button37.Enabled = True
            Button38.Enabled = True
            Button39.Enabled = True
            Button40.Enabled = True
            Button41.Enabled = True
            Button42.Enabled = False
            argent = argent - 10000000
        ElseIf MsgBox("Vous n'avez pas assez d'argent", , ) Then
        End If
    End Sub

    Private Sub IA_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles IA.Tick
        IA_Argent = IA_Argent + 100000
        Dim LOL As Integer = 0
        Dim l_caravan As Double = 0
        Dim loyer As Double = 0
        LOL = (Rnd() * 50000)
        argent = argent + LOL
        IA_Argent = IA_Argent - LOL
        l_caravan = caravan * 100000 * (Rnd())
        loyer = l_caravan
        argent = argent + loyer
        MsgBox("Vos espions industriels vous ont fait gagner " & LOL & " $ venant de votre concurent." & vbNewLine & "Vos loyers sont de " & FormatNumber(loyer, 2) & " $", , )
    End Sub

    Private Sub Button39_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button39.Click
        'Le bouton Metre 10 TV en vente.
        If TV < 10 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 10 Then TV_vente = TV_vente + 10
        If TV >= 10 Then TV = TV - 10
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button36_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button36.Click
        'Le bouton construire 100 TV
        If argent < 240000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 TV.")
        If argent >= 240000 Then TV = TV + 100
        If argent >= 240000 Then argent = argent - 240000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button37_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button37.Click
        'Le bouton Metre 100 TV en vente.
        If TV < 100 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 100 Then TV_vente = TV_vente + 100
        If TV >= 100 Then TV = TV - 100
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button34_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button34.Click
        'Le bouton construire 1 000 TV
        If argent < 2400000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 TV.")
        If argent >= 2400000 Then TV = TV + 1000
        If argent >= 2400000 Then argent = argent - 2400000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button35_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button35.Click
        'Le bouton Metre 1 000 TV en vente.
        If TV < 1000 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 1000 Then TV_vente = TV_vente + 1000
        If TV >= 1000 Then TV = TV - 1000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button32_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button32.Click
        'Le bouton construire 10 000 TV
        If argent < 24000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 TV.")
        If argent >= 24000000 Then TV = TV + 10000
        If argent >= 24000000 Then argent = argent - 24000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button33_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button33.Click
        'Le bouton Metre 10 000 TV en vente.
        If TV < 10000 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 10000 Then TV_vente = TV_vente + 10000
        If TV >= 10000 Then TV = TV - 10000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button30.Click
        'Le bouton construire 100 000 TV
        If argent < 240000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 TV.")
        If argent >= 240000000 Then TV = TV + 100000
        If argent >= 240000000 Then argent = argent - 240000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button31_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button31.Click
        'Le bouton Metre 100 000 TV en vente.
        If TV < 100000 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 100000 Then TV_vente = TV_vente + 100000
        If TV >= 100000 Then TV = TV - 100000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button28_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button28.Click
        'Le bouton construire 1 000 000 TV
        If argent < 2400000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 TV.")
        If argent >= 2400000000 Then TV = TV + 1000000
        If argent >= 2400000000 Then argent = argent - 2400000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button29_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button29.Click
        'Le bouton Metre 1 000 000 TV en vente.
        If TV < 1000000 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 1000000 Then TV_vente = TV_vente + 1000000
        If TV >= 1000000 Then TV = TV - 1000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button26_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button26.Click
        'Le bouton construire 10 000 000 TV
        If argent < 24000000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 000 TV.")
        If argent >= 24000000000 Then TV = TV + 10000000
        If argent >= 24000000000 Then argent = argent - 24000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button27_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button27.Click
        'Le bouton Metre 10 000 000 TV en vente.
        If TV < 10000000 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 10000000 Then TV_vente = TV_vente + 10000000
        If TV >= 10000000 Then TV = TV - 10000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button24_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button24.Click
        'Le bouton construire 100 000 000 TV
        If argent < 240000000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 000 TV.")
        If argent >= 240000000000 Then TV = TV + 100000000
        If argent >= 240000000000 Then argent = argent - 240000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button25_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button25.Click
        'Le bouton Metre 100 000 000 TV en vente.
        If TV < 100000000 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 100000000 Then TV_vente = TV_vente + 100000000
        If TV >= 100000000 Then TV = TV - 100000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button22.Click
        'Le bouton construire 1 000 000 000 TV
        If argent < 2400000000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 000 TV.")
        If argent >= 2400000000000 Then TV = TV + 1000000000
        If argent >= 2400000000000 Then argent = argent - 2400000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button23.Click
        'Le bouton Metre 1 000 000 000 TV en vente.
        If TV < 1000000000 Then MsgBox("Il ne reste pas sufisament de TV.")
        If TV >= 1000000000 Then TV_vente = TV_vente + 1000000000
        If TV >= 1000000000 Then TV = TV - 1000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label5.Text = TV & " TV(s) en stock"
        Label2.Text = TV_vente & " TV(s) en vente"
    End Sub

    Private Sub Button61_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button61.Click
        'Le bouton construire 1 Boite de nouilles
        If argent < 1.5 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 Boite de nouilles.")
        If argent >= 1.5 Then nouille = nouille + 1
        If argent >= 1.5 Then argent = argent - 1.5
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
        If nouille_vente > 1000000 Then nouille_prix = 1.25
        If nouille_vente <= 1000000 Then nouille_prix = 1.5
        If nouille_vente < 1000000 And nouille_vente >= 700000 Then nouille_prix = 2
        If nouille_vente < 700000 And nouille_vente >= 600000 Then nouille_prix = 2.25
        If nouille_vente < 600000 And nouille_vente >= 500000 Then nouille_prix = 2.5
        If nouille_vente < 500000 And nouille_vente >= 10000 Then nouille_prix = 2.75
        If nouille_vente < 10000 And nouille_vente >= 0 Then nouille_prix = 3
        If nouille_vente > 0 Then argent = argent + nouille_prix
        If nouille_vente > 0 Then nouille_vente = nouille_vente - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
        If lego_vente > 1000000 Then lego_prix = 4.78
        If lego_vente <= 1000000 Then lego_prix = 5
        If lego_vente < 1000000 And lego_vente >= 700000 Then lego_prix = 10
        If lego_vente < 700000 And lego_vente >= 600000 Then lego_prix = 12
        If lego_vente < 600000 And lego_vente >= 500000 Then lego_prix = 13
        If lego_vente < 500000 And lego_vente >= 10000 Then lego_prix = 14
        If lego_vente < 10000 And lego_vente >= 0 Then lego_prix = 15
        If lego_vente > 0 Then argent = argent + lego_prix
        If lego_vente > 0 Then lego_vente = lego_vente - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Timer3.Interval = (Rnd() * 25) + 1
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"

    End Sub

    Private Sub Button62_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button62.Click
        'Le bouton Metre 1 Boite de nouilles en vente.
        If nouille <= 0 Then MsgBox("Il ne reste plus de Boite de nouilles.")
        If nouille > 0 Then nouille_vente = nouille_vente + 1
        If nouille > 0 Then nouille = nouille - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button59_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button59.Click
        'Le bouton construire 10 Boite de nouilles
        If argent < 15 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 Boite de nouilles.")
        If argent >= 15 Then nouille = nouille + 10
        If argent >= 15 Then argent = argent - 15
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button60_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button60.Click
        'Le bouton Metre 10 Boite de nouilles en vente.
        If nouille < 10 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 10 Then nouille_vente = nouille_vente + 10
        If nouille >= 10 Then nouille = nouille - 10
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button57_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button57.Click
        'Le bouton construire 100 Boite de nouilles
        If argent < 150 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 Boite de nouilles.")
        If argent >= 150 Then nouille = nouille + 100
        If argent >= 150 Then argent = argent - 150
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button58_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button58.Click
        'Le bouton Metre 100 Boite de nouilles en vente.
        If nouille < 100 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 100 Then nouille_vente = nouille_vente + 100
        If nouille >= 100 Then nouille = nouille - 100
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button55_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button55.Click
        'Le bouton construire 1 000 Boite de nouilles
        If argent < 1500 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 Boite de nouilles.")
        If argent >= 1500 Then nouille = nouille + 1000
        If argent >= 1500 Then argent = argent - 1500
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button56_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button56.Click
        'Le bouton Metre 1 000 Boite de nouilles en vente.
        If nouille < 1000 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 1000 Then nouille_vente = nouille_vente + 1000
        If nouille >= 1000 Then nouille = nouille - 1000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button53_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button53.Click
        'Le bouton construire 10 000 Boite de nouilles
        If argent < 15000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 Boite de nouilles.")
        If argent >= 15000 Then nouille = nouille + 10000
        If argent >= 15000 Then argent = argent - 15000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button54_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button54.Click
        'Le bouton Metre 10 000 Boite de nouilles en vente.
        If nouille < 10000 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 10000 Then nouille_vente = nouille_vente + 10000
        If nouille >= 10000 Then nouille = nouille - 10000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button51_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button51.Click
        'Le bouton construire 100 000 Boite de nouilles
        If argent < 150000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 Boite de nouilles.")
        If argent >= 150000 Then nouille = nouille + 100000
        If argent >= 150000 Then argent = argent - 150000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button52_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button52.Click
        'Le bouton Metre 100 000 Boite de nouilles en vente.
        If nouille < 100000 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 100000 Then nouille_vente = nouille_vente + 100000
        If nouille >= 100000 Then nouille = nouille - 100000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button49_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button49.Click
        'Le bouton construire 1 000 000 Boite de nouilles
        If argent < 1500000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 Boite de nouilles.")
        If argent >= 1500000 Then nouille = nouille + 1000000
        If argent >= 1500000 Then argent = argent - 1500000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button50_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button50.Click
        'Le bouton Metre 1 000 000 Boite de nouilles en vente.
        If nouille < 1000000 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 1000000 Then nouille_vente = nouille_vente + 1000000
        If nouille >= 1000000 Then nouille = nouille - 1000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button47_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button47.Click
        'Le bouton construire 10 000 000 Boite de nouilles
        If argent < 15000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 000 Boite de nouilles.")
        If argent >= 15000000 Then nouille = nouille + 10000000
        If argent >= 15000000 Then argent = argent - 15000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button48_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button48.Click
        'Le bouton Metre 10 000 000 Boite de nouilles en vente.
        If nouille < 10000000 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 10000000 Then nouille_vente = nouille_vente + 10000000
        If nouille >= 10000000 Then nouille = nouille - 10000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button45_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button45.Click
        'Le bouton construire 100 000 000 Boite de nouilles
        If argent < 150000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 000 Boite de nouilles.")
        If argent >= 150000000 Then nouille = nouille + 100000000
        If argent >= 150000000 Then argent = argent - 150000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button46_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button46.Click
        'Le bouton Metre 100 000 000 Boite de nouilles en vente.
        If nouille < 100000000 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 100000000 Then nouille_vente = nouille_vente + 100000000
        If nouille >= 100000000 Then nouille = nouille - 100000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button43_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button43.Click
        'Le bouton construire 1 000 000 000 Boite de nouilles
        If argent < 1500000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 000 Boite de nouilles.")
        If argent >= 1500000000 Then nouille = nouille + 1000000000
        If argent >= 1500000000 Then argent = argent - 1500000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button44_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button44.Click
        'Le bouton Metre 1 000 000 000 Boite de nouilles en vente.
        If nouille < 1000000000 Then MsgBox("Il ne reste pas sufisament de Boite de nouilles.")
        If nouille >= 1000000000 Then nouille_vente = nouille_vente + 1000000000
        If nouille >= 1000000000 Then nouille = nouille - 1000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label11.Text = nouille & " Boite(s) de nouilles en stock"
        Label10.Text = nouille_vente & " Boite(s) de nouilles en vente"
    End Sub

    Private Sub Button101_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button101.Click
        'Le bouton construire 1 Sac de Lego
        If argent < 5 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 Sac de Lego.")
        If argent >= 5 Then lego = lego + 1
        If argent >= 5 Then argent = argent - 5
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button102_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button102.Click
        'Le bouton Metre 1 Sac de Lego en vente.
        If lego < 1 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 1 Then lego_vente = lego_vente + 1
        If lego >= 1 Then lego = lego - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button99_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button99.Click
        'Le bouton construire 10 Sac de Lego
        If argent < 50 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 Sac de Lego.")
        If argent >= 50 Then lego = lego + 10
        If argent >= 50 Then argent = argent - 50
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button100_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button100.Click
        'Le bouton Metre 10 Sac de Lego en vente.
        If lego < 10 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 10 Then lego_vente = lego_vente + 10
        If lego >= 10 Then lego = lego - 10
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button97_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button97.Click
        'Le bouton construire 100 Sac de Lego
        If argent < 500 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 Sac de Lego.")
        If argent >= 500 Then lego = lego + 100
        If argent >= 500 Then argent = argent - 500
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button98_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button98.Click
        'Le bouton Metre 100 Sac de Lego en vente.
        If lego < 100 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 100 Then lego_vente = lego_vente + 100
        If lego >= 100 Then lego = lego - 100
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button95_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button95.Click
        'Le bouton construire 1 000 Sac de Lego
        If argent < 5000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 Sac de Lego.")
        If argent >= 5000 Then lego = lego + 1000
        If argent >= 5000 Then argent = argent - 5000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button96_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button96.Click
        'Le bouton Metre 1 000 Sac de Lego en vente.
        If lego < 1000 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 1000 Then lego_vente = lego_vente + 1000
        If lego >= 1000 Then lego = lego - 1000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button93_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button93.Click
        'Le bouton construire 10 000 Sac de Lego
        If argent < 50000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 Sac de Lego.")
        If argent >= 50000 Then lego = lego + 10000
        If argent >= 50000 Then argent = argent - 50000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button94_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button94.Click
        'Le bouton Metre 10 000 Sac de Lego en vente.
        If lego < 10000 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 10000 Then lego_vente = lego_vente + 10000
        If lego >= 10000 Then lego = lego - 10000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button91_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button91.Click
        'Le bouton construire 100 000 Sac de Lego
        If argent < 500000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 Sac de Lego.")
        If argent >= 500000 Then lego = lego + 100000
        If argent >= 500000 Then argent = argent - 500000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button92_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button92.Click
        'Le bouton Metre 100 000 Sac de Lego en vente.
        If lego < 100000 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 100000 Then lego_vente = lego_vente + 100000
        If lego >= 100000 Then lego = lego - 100000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button89_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button89.Click
        'Le bouton construire 1 000 000 Sac de Lego
        If argent < 5000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 Sac de Lego.")
        If argent >= 5000000 Then lego = lego + 1000000
        If argent >= 5000000 Then argent = argent - 5000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button90_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button90.Click
        'Le bouton Metre 1 000 000 Sac de Lego en vente.
        If lego < 1000000 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 1000000 Then lego_vente = lego_vente + 1000000
        If lego >= 1000000 Then lego = lego - 1000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button87_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button87.Click
        'Le bouton construire 10 000 000 Sac de Lego
        If argent < 50000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 000 Sac de Lego.")
        If argent >= 50000000 Then lego = lego + 10000000
        If argent >= 50000000 Then argent = argent - 50000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button88_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button88.Click
        'Le bouton Metre 10 000 000 Sac de Lego en vente.
        If lego < 10000000 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 10000000 Then lego_vente = lego_vente + 10000000
        If lego >= 10000000 Then lego = lego - 10000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button85_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button85.Click
        'Le bouton construire 100 000 000 Sac de Lego
        If argent < 500000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 000 Sac de Lego.")
        If argent >= 500000000 Then lego = lego + 100000000
        If argent >= 500000000 Then argent = argent - 500000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button86_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button86.Click
        'Le bouton Metre 100 000 000 Sac de Lego en vente.
        If lego < 100000000 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 100000000 Then lego_vente = lego_vente + 100000000
        If lego >= 100000000 Then lego = lego - 100000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button83_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button83.Click
        'Le bouton construire 1 000 000 000 Sac de Lego
        If argent < 5000000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 000 Sac de Lego.")
        If argent >= 5000000000 Then lego = lego + 1000000000
        If argent >= 5000000000 Then argent = argent - 5000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button84_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button84.Click
        'Le bouton Metre 1 000 000 000 Sac de Lego en vente.
        If lego < 1000000000 Then MsgBox("Il ne reste pas sufisament de Sac de Lego.")
        If lego >= 1000000000 Then lego_vente = lego_vente + 1000000000
        If lego >= 1000000000 Then lego = lego - 1000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = lego & " Sac(s) de legos en stock"
        Label14.Text = lego_vente & " Sac(s) de legos en vente"
    End Sub

    Private Sub Button121_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button121.Click
        'Le bouton construire 1 Boite de banane
        If argent < 22 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 Boite de banane.")
        If argent >= 22 Then btban = btban + 1
        If argent >= 22 Then argent = argent - 22
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button122_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button122.Click
        'Le bouton Metre 1 Boite de Banane en vente.
        If btban < 1 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 1 Then btban_vente = btban_vente + 1
        If btban >= 1 Then btban = btban - 1
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button119_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button119.Click
        'Le bouton construire 10 Boite de banane
        If argent < 220 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 Boite de banane.")
        If argent >= 220 Then btban = btban + 10
        If argent >= 220 Then argent = argent - 220
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button120_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button120.Click
        'Le bouton Metre 10 Boite de Banane en vente.
        If btban < 10 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 10 Then btban_vente = btban_vente + 10
        If btban >= 10 Then btban = btban - 10
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button117_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button117.Click
        'Le bouton construire 100 Boite de banane
        If argent < 2200 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 Boite de banane.")
        If argent >= 2200 Then btban = btban + 100
        If argent >= 2200 Then argent = argent - 2200
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button118_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button118.Click
        'Le bouton Metre 100 Boite de Banane en vente.
        If btban < 100 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 100 Then btban_vente = btban_vente + 100
        If btban >= 100 Then btban = btban - 100
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button115_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button115.Click
        'Le bouton construire 1 000 Boite de banane
        If argent < 22000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 Boite de banane.")
        If argent >= 22000 Then btban = btban + 1000
        If argent >= 22000 Then argent = argent - 22000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button116_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button116.Click
        'Le bouton Metre 1 000 Boite de Banane en vente.
        If btban < 1000 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 1000 Then btban_vente = btban_vente + 1000
        If btban >= 1000 Then btban = btban - 1000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button113_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button113.Click
        'Le bouton construire 10 000 Boite de banane
        If argent < 220000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 Boite de banane.")
        If argent >= 220000 Then btban = btban + 10000
        If argent >= 220000 Then argent = argent - 220000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button114_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button114.Click
        'Le bouton Metre 10 000 Boite de Banane en vente.
        If btban < 10000 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 10000 Then btban_vente = btban_vente + 10000
        If btban >= 10000 Then btban = btban - 10000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button111_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button111.Click
        'Le bouton construire 100 000 Boite de banane
        If argent < 2200000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 Boite de banane.")
        If argent >= 2200000 Then btban = btban + 100000
        If argent >= 2200000 Then argent = argent - 2200000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button112_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button112.Click
        'Le bouton Metre 100 000 Boite de Banane en vente.
        If btban < 100000 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 100000 Then btban_vente = btban_vente + 100000
        If btban >= 100000 Then btban = btban - 100000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button109_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button109.Click
        'Le bouton construire 1 000 000 Boite de banane
        If argent < 22000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 Boite de banane.")
        If argent >= 22000000 Then btban = btban + 1000000
        If argent >= 22000000 Then argent = argent - 22000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button110_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button110.Click
        'Le bouton Metre 1 000 000 Boite de Banane en vente.
        If btban < 1000000 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 1000000 Then btban_vente = btban_vente + 1000000
        If btban >= 1000000 Then btban = btban - 1000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button107_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button107.Click
        'Le bouton construire 10 000 000 Boite de banane
        If argent < 220000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 10 000 000 Boite de banane.")
        If argent >= 220000000 Then btban = btban + 10000000
        If argent >= 220000000 Then argent = argent - 220000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button108_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button108.Click
        'Le bouton Metre 10 000 000 Boite de Banane en vente.
        If btban < 10000000 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 10000000 Then btban_vente = btban_vente + 10000000
        If btban >= 10000000 Then btban = btban - 10000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button105_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button105.Click
        'Le bouton construire 100 000 000 Boite de banane
        If argent < 2200000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 100 000 000 Boite de banane.")
        If argent >= 2200000000 Then btban = btban + 100000000
        If argent >= 2200000000 Then argent = argent - 2200000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button106_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button106.Click
        'Le bouton Metre 100 000 000 Boite de Banane en vente.
        If btban < 100000000 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 100000000 Then btban_vente = btban_vente + 100000000
        If btban >= 100000000 Then btban = btban - 100000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button103_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button103.Click
        'Le bouton construire 1 000 000 000 Boite de banane
        If argent < 22000000000 Then MsgBox("Vous n'avez pas assez d'argent pour construire 1 000 000 000 Boite de banane.")
        If argent >= 22000000000 Then btban = btban + 1000000000
        If argent >= 22000000000 Then argent = argent - 22000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Button104_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button104.Click
        'Le bouton Metre 1 000 000 000 Boite de Banane en vente.
        If btban < 1000000000 Then MsgBox("Il ne reste pas suffisamment de Boite de Banane.")
        If btban >= 1000000000 Then btban_vente = btban_vente + 1000000000
        If btban >= 1000000000 Then btban = btban - 1000000000
        Label4.Text = FormatNumber(argent, 2) & " $"
        Label15.Text = btban & " Boite(s) de Banane en stock"
        Label14.Text = btban_vente & " Boite(s) de Banane en vente"
    End Sub

    Private Sub Label12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label12.Click
        System.Diagnostics.Process.Start("https://github.com/industr-a/industr-a")
    End Sub

    Private Sub Button123_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button123.Click
        If argent >= 1000000000 Then
            argent = argent - 1000000000
            TabControl5.Visible = True
            Button123.Enabled = False
            Button63.Enabled = True
        End If

    End Sub

    Private Sub Button81_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button81.Click
        'Le bouton acheter 1 Km²
        If argent >= 300000 Then
            argent = argent - 300000
            km = km + 1
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 1 Km²")
        End If
    End Sub

    Private Sub Button79_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button79.Click
        'Le bouton acheter 10 Km²
        If argent >= 3000000 Then
            argent = argent - 3000000
            km = km + 10
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 10 Km²")
        End If
    End Sub

    Private Sub Button77_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button77.Click
        'Le bouton acheter 100 Km²
        If argent >= 30000000 Then
            argent = argent - 30000000
            km = km + 100
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 100 Km²")
        End If
    End Sub

    Private Sub Button75_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button75.Click
        'Le bouton acheter 1 000 Km²
        If argent >= 300000000 Then
            argent = argent - 300000000
            km = km + 1000
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 1 000 Km²")
        End If
    End Sub

    Private Sub Button73_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button73.Click
        'Le bouton acheter 10 000 Km²
        If argent >= 3000000000 Then
            argent = argent - 3000000000
            km = km + 10000
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 10 000 Km²")
        End If
    End Sub

    Private Sub Button71_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button71.Click
        'Le bouton acheter 100 000 Km²
        If argent >= 30000000000 Then
            argent = argent - 30000000000
            km = km + 100000
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 100 000 Km²")
        End If
    End Sub

    Private Sub Button69_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button69.Click
        'Le bouton acheter 1 000 000 Km²
        If argent >= 300000000000 Then
            argent = argent - 300000000000
            km = km + 1000000
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 1 000 000 Km²")
        End If
    End Sub

    Private Sub Button67_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button67.Click
        'Le bouton acheter 10 000 000 Km²
        If argent >= 3000000000000 Then
            argent = argent - 3000000000000
            km = km + 10000000
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 10 000 000 Km²")
        End If
    End Sub

    Private Sub Button65_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button65.Click
        'Le bouton acheter 100 000 000 Km²
        If argent >= 30000000000000 Then
            argent = argent - 30000000000000
            km = km + 100000000
        Else : MsgBox("Vous n'avez pas suffisament d'argent pour acheter 100 000 000 Km²")
        End If
    End Sub

    Private Sub Button63_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button63.Click
        'rechercher maisons mobiles dans les R&D
        If argent >= 5000000 Then
            Button134.Enabled = True
            Button136.Enabled = True
            Button138.Enabled = True
            Button140.Enabled = True
            Button142.Enabled = True
            Button63.Enabled = False
            argent = argent - 5000000
        ElseIf MsgBox("Vous n'avez pas assez d'argent", , ) Then
        End If
    End Sub

    Private Sub Button142_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button142.Click
        'construire 1 maisons mobiles.
        If argent >= 12500000 And km >= 1 Then
            km = km - 1
            argent = argent - 12500000
            caravan = caravan + 1
        ElseIf MsgBox("Vous n'avez pas assez d'argent ou de Km²", , ) Then
        End If
    End Sub

    Private Sub Button140_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button140.Click
        'construire 10 parc de maisons mobiles.
        If argent >= 125000000 And km >= 10 Then
            km = km - 10
            argent = argent - 125000000
            caravan = caravan + 10
        ElseIf MsgBox("Vous n'avez pas assez d'argent ou de Km²", , ) Then
        End If
    End Sub

    Private Sub Button138_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button138.Click
        'construire 100 parc de maisons mobiles.
        If argent >= 1250000000 And km >= 100 Then
            km = km - 100
            argent = argent - 1250000000
            caravan = caravan + 100
        ElseIf MsgBox("Vous n'avez pas assez d'argent ou de Km²", , ) Then
        End If
    End Sub

    Private Sub Button136_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button136.Click
        'construire 1 000 parc de maisons mobiles.
        If argent >= 12500000000 And km >= 1000 Then
            km = km - 1000
            argent = argent - 12500000000
            caravan = caravan + 1000
        ElseIf MsgBox("Vous n'avez pas assez d'argent ou de Km²", , ) Then
        End If
    End Sub

    Private Sub Button134_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button134.Click
        'construire 10 000 parc de maisons mobiles.
        If argent >= 125000000000 And km >= 10000 Then
            km = km - 10000
            argent = argent - 125000000000
            caravan = caravan + 10000
        ElseIf MsgBox("Vous n'avez pas assez d'argent ou de Km²", , ) Then
        End If
    End Sub
End Class
