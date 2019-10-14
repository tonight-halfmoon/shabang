## Moving from `bash` to `zsh`

  * Switch to `zsh` by evaluating the following command

    ```
    chsh -s /bin/zsh
    ```

  * Update `brew` and install `zsh`

    ```
    brew update && brew upgrade && brew cleanup && brew doctor
    ```

    ```
    brew install zsh
    ```

  * Install Oh My Zsh

    ```
    $ sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    ```

	* Customise as described on [Oh My Zsh](https://github.com/robbyrussell/oh-my-zsh)
