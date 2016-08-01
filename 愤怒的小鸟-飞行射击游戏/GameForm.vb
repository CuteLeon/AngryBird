Public Class GameForm
    Private Declare Function ReleaseCapture Lib "user32" () As Integer
    Private Declare Function SendMessageA Lib "user32" (ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal wParam As Integer, lParam As VariantType) As Integer
    Private Declare Function GetWindowLong Lib "user32.dll" Alias "GetWindowLongA" (ByVal hwnd As Int32, ByVal nIndex As Int32) As Int32
    Private Declare Function SetWindowLong Lib "user32.dll" Alias "SetWindowLongA" (ByVal hwnd As Int32, ByVal nIndex As Int32, ByVal dwNewLong As Int32) As Int32
    Private Const GWL_STYLE As Int32 = -16
    Private Const WS_THICKFRAME As Int32 = &H40000

    Private Const MAX_BIRDS As Integer = 20
    Private Const RND_ADD_BIRD As Single = 0.05
    Private Const BIRD_MAX_HEALTH As Integer = 100
    Private Const PIG_MAX_HEALTH As Integer = 500
    Private Const MOVE_PIXEL As Byte = 3
    Private Const ROCKET_SPEED As Integer = 15
    Private Const BULLET_SPEED As Integer = 8
    Private Const DAMAGE_VALUE As Integer = 35
    Dim GameFont As New Font("微软雅黑", 16)
    Dim ScoreCount As Integer = 0
    Dim Pig As Figure
    Dim GameRandom As Random = New Random
    Dim Birds As ArrayList = New ArrayList
    Dim Rockets As ArrayList = New ArrayList
    Dim Bullets As ArrayList = New ArrayList
    Dim GameBitmap As Bitmap
    Dim GameGraphics As Graphics
    Dim GameRectangle As Rectangle
    Dim BirdRectangle As Size
    Dim BirdSize As New Size(60, 60)
    Dim HealthPoint As Point = New Point(BirdSize.Width / 10, BirdSize.Height - BirdSize.Height * 0.15)
    Dim HealthSize As Size = New Size(BirdSize.Width - 2 * HealthPoint.X, BirdSize.Height / 20)
    Dim RocketSize As New Size(15, 36)
    Dim PigSize As New Size(60, 60)
    Dim BulletSize As New Size(7, 26)
    Dim PauseButton As Bitmap = My.Resources.GameResource.Pause_N
    Dim PauseButtonRectangle As Rectangle

    Private Structure Figure
        Dim Image As Bitmap '0
        Dim StepIndex As Integer 'B
        Dim StepCount As Integer 'B
        Dim Health As Integer '0
        Dim StartPoint As Point 'B
        Dim X As Integer '0
        Dim Y As Integer '0
        Dim ToPoint As Point 'B
        Dim DistanceSize As Size 'B
    End Structure

    '游戏引擎
    Private Sub GameEngine_Tick(sender As Object, e As EventArgs) Handles GameEngine.Tick
        Static EmitRocketIndex As Integer = 0
        Dim TempFigure As Figure
        GameBitmap = Nothing
        GameBitmap = New Bitmap(My.Resources.GameResource.GameBackground, GameRectangle.Size)
        GameGraphics = Graphics.FromImage(GameBitmap)

        If GameRandom.NextDouble < RND_ADD_BIRD Then EmitNewBullet(GameRandom.Next(Birds.Count))
        If Birds.Count < MAX_BIRDS AndAlso GameRandom.NextDouble < RND_ADD_BIRD Then AddNewBird()

        EmitRocketIndex += 1
        If EmitRocketIndex = 5 Then
            EmitNewRocket()
            EmitRocketIndex = 0
        End If

        Dim Index As Integer
        Dim BirdsCount As Integer = Birds.Count
        Do Until Index = BirdsCount
            UpdateBirdLocation(Birds(Index))

            If Birds(Index).Health <= 0 Then
                'My.Computer.Audio.Play(My.Resources.GameResource.爆炸, AudioPlayMode.Background)
                ScoreCount += 100
                Birds(Index) = Nothing
                Birds.RemoveAt(Index)
                BirdsCount -= 1
                Continue Do
            End If

            TempFigure = CType(Birds(Index), Figure)
            GameGraphics.DrawImage(TempFigure.Image, TempFigure.X, TempFigure.Y, BirdSize.Width, BirdSize.Height)
            '显示小鸟生命条
            GameGraphics.FillRectangle(Brushes.Red, New Rectangle(TempFigure.X + HealthPoint.X, TempFigure.Y + HealthPoint.Y, TempFigure.Health / BIRD_MAX_HEALTH * HealthSize.Width, HealthSize.Height))
            GameGraphics.DrawRectangle(Pens.Black, New Rectangle(TempFigure.X + HealthPoint.X, TempFigure.Y + HealthPoint.Y, HealthSize.Width, HealthSize.Height))
            '显示小鸟移动轨迹
            'GameGraphics.DrawLine(Pens.Red, TempFigure.StartPoint.X, TempFigure.StartPoint.Y, TempFigure.ToPoint.X, TempFigure.ToPoint.Y)
            'GameGraphics.FillEllipse(Brushes.Yellow, TempFigure.StartPoint.X, TempFigure.StartPoint.Y, 10, 10)
            'GameGraphics.FillEllipse(Brushes.Yellow, TempFigure.ToPoint.X, TempFigure.ToPoint.Y, 10, 10)
            'GameGraphics.FillEllipse(Brushes.Aqua, TempFigure.X, TempFigure.Y, 10, 10)

            Index += 1
        Loop

        Index = 0
        Dim RocketsCount As Integer = Rockets.Count
        Do Until Index = RocketsCount
            UpdateRocketLocation(Rockets(Index))

            If Rockets(Index).Health = 0 Then
                Rockets(Index) = Nothing
                Rockets.RemoveAt(Index)
                RocketsCount -= 1
                Continue Do
            End If

            GameGraphics.DrawImage(Rockets(Index).Image, Rockets(Index).X, Rockets(Index).Y, RocketSize.Width, RocketSize.Height)
            Index += 1
        Loop

        Index = 0
        Dim BulletsCount As Integer = Bullets.Count
        Do Until Index = BulletsCount
            UpdateBulletLocation(Bullets(Index))

            If Bullets(Index).Health = 0 Then
                Bullets(Index) = Nothing
                Bullets.RemoveAt(Index)
                BulletsCount -= 1
                Continue Do
            End If

            GameGraphics.DrawImage(Bullets(Index).Image, Bullets(Index).X, Bullets(Index).Y, BulletSize.Width, BulletSize.Height)
            Index += 1
        Loop
        GameGraphics.DrawImage(Pig.Image, Pig.X, Pig.Y, PigSize.Width, PigSize.Height)
        GameGraphics.DrawString(ScoreCount.ToString, GameFont, Brushes.DarkGoldenrod, 10, 10)
        GameGraphics.FillRectangle(Brushes.Red, New Rectangle(GameRectangle.Left + 10, GameRectangle.Height - 20, Pig.Health / PIG_MAX_HEALTH * GameRectangle.Width - 20, 10))
        GameGraphics.DrawRectangle(Pens.DarkGoldenrod, New Rectangle(GameRectangle.Left + 10, GameRectangle.Height - 20, GameRectangle.Width - 20, 10))
        GameGraphics.DrawImage(PauseButton, PauseButtonRectangle)

        Me.BackgroundImage = GameBitmap
        GameGraphics.Dispose()

        If Pig.Health <= 0 Then GameEngine.Stop() : MsgBox("You Lost !" & vbCrLf & "得分： " & ScoreCount, MsgBoxStyle.Information, "游戏结束：") : LoadNewGame()
    End Sub

#Region "窗体"

    Private Sub GameForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim MyWindowStyle As Int32 = GetWindowLong(Me.Handle, GWL_STYLE) Or WS_THICKFRAME
        SetWindowLong(Me.Handle, GWL_STYLE, MyWindowStyle)
        Me.Icon = My.Resources.GameResource.愤怒的小鸟_飞行射击游戏
        LoadNewGame()
        For I As Integer = 0 To 5
            AddNewBird()
        Next
    End Sub

    Private Sub GameForm_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        Select Case e.KeyCode
            Case Keys.Up
                If Pig.Y <= GameRectangle.Top Then Exit Sub
                Pig.Y -= MOVE_PIXEL
            Case Keys.Down
                If Pig.Y >= GameRectangle.Height - PigSize.Height Then Exit Sub
                Pig.Y += MOVE_PIXEL
            Case Keys.Left
                If Pig.X <= GameRectangle.Left Then Exit Sub
                Pig.X -= MOVE_PIXEL
            Case Keys.Right
                If Pig.X >= GameRectangle.Width - PigSize.Width Then Exit Sub
                Pig.X += MOVE_PIXEL
        End Select
    End Sub

    Private Sub GameForm_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        GameRectangle = New Rectangle(0, 0, Me.Width - 14, Me.Height - 14)
        BirdRectangle = New Size(GameRectangle.Width - 100, GameRectangle.Height * 0.6)
        PauseButtonRectangle = New Rectangle(10, GameRectangle.Height - 110, 80, 80)
        If Pig.Y >= GameRectangle.Height - PigSize.Height Then Pig.Y = GameRectangle.Height - PigSize.Height
    End Sub

    Private Sub GameForm_MouseDown(sender As Object, e As MouseEventArgs) Handles Me.MouseDown
        If PauseButtonRectangle.Contains(e.Location) Then
            GameEngine.Enabled = Not GameEngine.Enabled
            PauseButton = My.Resources.GameResource.ResourceManager.GetObject(IIf(GameEngine.Enabled, "Pause", "Play") & "_E")
            GameEngine_Tick(Nothing, Nothing)
        Else
            ReleaseCapture()
            SendMessageA(Me.Handle, &HA1, 2, 0&)
        End If
    End Sub

    Private Sub GameForm_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        Static Entered As Boolean
        Pig.X = e.X - PigSize.Width / 2
        Pig.Y = e.Y - PigSize.Height / 2

        If Not PauseButtonRectangle.Contains(e.Location) = Entered Then
            If Entered Then
                PauseButton = My.Resources.GameResource.ResourceManager.GetObject("Pause_N")
            Else
                PauseButton = My.Resources.GameResource.ResourceManager.GetObject(IIf(GameEngine.Enabled, "Pause", "Play") & "_E")
            End If
            Entered = Not Entered
        End If
    End Sub

    Private Sub GameForm_Deactivate(sender As Object, e As EventArgs) Handles Me.Deactivate
        If GameEngine.Enabled Then
            GameEngine.Stop()
            PauseButton = My.Resources.GameResource.Play_E
            GameEngine_Tick(Nothing, Nothing)
        End If
    End Sub

#End Region

#Region "射击"
    Private Sub EmitNewBullet(ByVal BirdIndex As Integer)
        If Birds.Count = 0 Then Exit Sub

        Dim NewBullet As Figure
        With NewBullet
            .Image = My.Resources.GameResource.Bullet
            .X = CType(Birds(BirdIndex), Figure).X + (BirdSize.Width - BulletSize.Width) / 2
            .Y = CType(Birds(BirdIndex), Figure).Y + BirdSize.Height - BulletSize.Height
            .Health = 2
        End With
        Bullets.Add(NewBullet)
    End Sub

    Private Sub EmitNewRocket()
        Dim NewRocket As Figure
        With NewRocket
            .Image = My.Resources.GameResource.Rocket
            .X = Pig.X + (PigSize.Width - RocketSize.Width) / 2
            .Y = Pig.Y
            .Health = 2
        End With
        Rockets.Add(NewRocket)
    End Sub

#End Region

#Region "更新状态"

    Private Sub UpdateBulletLocation(ByRef Bullet As Figure)
        If Bullet.Health = 1 Then Bullet.Health = 0 : Exit Sub

        Dim PigRectangle As Rectangle = New Rectangle(Pig.X, Pig.Y, PigSize.Width, PigSize.Height)
        Dim BulletRectangle As Rectangle = New Rectangle(Bullet.X, Bullet.Y, BulletSize.Width, BulletSize.Height)
        If PigRectangle.IntersectsWith(BulletRectangle) Then
            My.Computer.Audio.Play(My.Resources.GameResource.射击, AudioPlayMode.Background)
            Pig.Health -= DAMAGE_VALUE
            Bullet.Health = 1
            Exit Sub
        End If

        Bullet.Y += BULLET_SPEED
        If Bullet.Y > GameRectangle.Height Then Bullet.Health = 0
    End Sub

    Private Sub UpdateRocketLocation(ByRef Rocket As Figure)
        If Rocket.Health = 1 Then Rocket.Health = 0 : Exit Sub

        Dim RocketRectangle As Rectangle = New Rectangle(Rocket.X, Rocket.Y, RocketSize.Width, RocketSize.Height)
        Dim BirdRectangle As Rectangle
        For Index As Integer = 0 To Birds.Count - 1
            BirdRectangle = New Rectangle(CType(Birds(Index), Figure).X, CType(Birds(Index), Figure).Y, BirdSize.Width, BirdSize.Height)
            If RocketRectangle.IntersectsWith(BirdRectangle) Then
                My.Computer.Audio.Play(My.Resources.GameResource.射击, AudioPlayMode.Background)
                HurtBird(Birds(Index))
                Rocket.Health = 1
                Exit Sub
            End If
        Next

        Rocket.Y -= ROCKET_SPEED
        If Rocket.Y < -RocketSize.Height Then Rocket.Health = 0
    End Sub

    Private Sub UpdateBirdLocation(ByRef Bird As Figure)
        With Bird
            .X = .StartPoint.X + .StepIndex * .DistanceSize.Width
            .Y = .StartPoint.Y + .StepIndex * .DistanceSize.Height
            If .StepIndex = .StepCount Then
                Dim DistanceX, DistanceY As Integer
                .StartPoint.X = .X
                .StartPoint.Y = .Y
                .StepIndex = 0
                .ToPoint.X = GameRandom.Next(BirdRectangle.Width)
                .ToPoint.Y = GameRandom.Next(BirdRectangle.Height)
                DistanceX = .ToPoint.X - .X : DistanceY = .ToPoint.Y - .Y
                .StepCount = Math.Sqrt(DistanceX ^ 2 + DistanceY ^ 2) / MOVE_PIXEL
                If .StepCount = 0 Then .StepCount = 1
                .DistanceSize.Width = DistanceX / .StepCount
                .DistanceSize.Height = DistanceY / .StepCount
            Else
                .StepIndex += 1
            End If
        End With
    End Sub

#End Region

#Region "功能函数"
    Private Sub LoadNewGame()
        Dim Index As Integer
        If Birds.Count > 0 Then
            For Index = 0 To Birds.Count - 1
                Birds(Index) = Nothing
            Next
        End If
        If Rockets.Count > 0 Then
            For Index = 0 To Rockets.Count - 1
                Rockets(Index) = Nothing
            Next
        End If
        If Bullets.Count > 0 Then
            For Index = 0 To Bullets.Count - 1
                Bullets(Index) = Nothing
            Next
        End If
        Birds.Clear()
        Rockets.Clear()
        Bullets.Clear()

        ScoreCount = 0
        With Pig
            .Image = My.Resources.GameResource.Pig
            .X = (GameRectangle.Width - PigSize.Width) / 2
            .Y = GameRectangle.Height - PigSize.Height
            .Health = PIG_MAX_HEALTH
        End With

        GameBitmap = New Bitmap(My.Resources.GameResource.GameBackground, GameRectangle.Size)
        GameGraphics = Graphics.FromImage(GameBitmap)
        GameGraphics.DrawImage(Pig.Image, Pig.X, Pig.Y, PigSize.Width, PigSize.Height)
        Me.BackgroundImage = GameBitmap
        GameBitmap = Nothing
        GameGraphics.Dispose()

        GameEngine.Start()
    End Sub

    Private Sub AddNewBird()
        Dim NewBird As Figure
        With NewBird
            .Image = My.Resources.GameResource.ResourceManager.GetObject("Bird_" & GameRandom.Next(5).ToString)
            .X = GameRandom.Next(Me.Width - BirdSize.Width)
            .Y = -BirdSize.Height
            .StartPoint.X = .X
            .StartPoint.Y = .Y
            .Health = BIRD_MAX_HEALTH
        End With
        Birds.Add(NewBird)
    End Sub

    Private Sub HurtBird(ByRef Bird As Figure)
        Bird.Health -= DAMAGE_VALUE
    End Sub

#End Region

End Class
