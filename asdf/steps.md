
## asdf-vm on FreeBSD 12.0

	* Setup asdf-vm

	```

	$ git clone https://github.com/asdf-vm/asdf.git
	```

	* Configure asdf-vm

	```
	$ echo -e '\n. $HOME/.asdf/asdf.sh'>> ~/.zshrc
	$ echo -e '\n. $HOME/.asdf/completions/asdf.bash' >> ~/.zshrc
	```

	* Add your plugins
	
	```
	$ asdf plugin-add erlang
	$ asdf plugin-add rebar
	$ asdf plugin-add elixir
	```


	* Configure `$HOME/.tool-versions` with my selections
	
	```
	echo -e 'erlang 22.1\n' >> $HOME/.tool-versions
	echo -e 'rebar 1.3.1\n' >> $HOME/.tool-versions
	echo -e 'rebar 1.9.2\n' >> $HOME/.tool-versions
	```

	* Install all

	```
	$ asdf install
	```

## Postgres

	* Dependencies
		* `inotify-tools`
			[libinotifytools](https://github.com/rvoicilas/inotify-tools/wiki#info)

		* postgresql-odbc
			[](https://www.freshports.org/databases/postregsql-odbc)

		* Dependency: `readline`

			```
			# cd /usr/porrts/devel/readline && make install clean 
			```

	* Add `postgres` plugin

	```
	$ asdf plugin-add postgres

	```

	* Configure desired postgres version I want in `.tool-versions` 

	```

	$ echo -e 'postgres 10.0' << $HOME/.tool-versions
	```

## References
	* [](https://hexdocs.pm/phoenix/installation.html\#phoenix)
