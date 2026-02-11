#' Select Random Text
#'
#' @param text_list A character vector of text items.
#' @param n Number of items to select.
#' @param replace Sample with replacement? (Default: FALSE)
#'
#' @return A randomly selected character vector.
#' @export
#'
#' @examples
#' my_texts <- c("Hello", "World", "R", "Package")
#' select_random(my_texts, n = 2)
#' 
moira_text = c("Now is not the time for PETTIFOGGING!",
          "The world is falling apart around us, and I am dying inside.",
          "Stop acting like a DISGRUNTLED PELICAN.",
          "I am positively BEDEVILED with meetings, et cetera.",
          "Next, you FOLD in the cheese.",
          "You FOLD it in.",
          "Why must you be so constantly IRKSOME?",
          "How MERCURIAL is life!",
          "Are you a dirty peeping tom?",
          "Track that CYCLE, missy!",
          "It's like working off a CORPSE.",
          "I had just had my eyelashes dyed, everything was cloudy!",
          "Sadly, I won't be able to SQUIRE you for today's wedding venue PEREGRINATIONS.",
          "... and here I get a BALATRON from Barnum & Bailey.",
          "I promise to keep my husband HABILIMENTED.",
          "Say HAVARTI, girls!",
          "Please tell me it's not a TESTUDINE.",
          "You may as well tie a leash around a raw chicken cutlet.",
          "It tastes like AMOXICILLIN.",
          "I'm eating egg whites and hoping the building will collapse.",
          "Gossip is the devil’s telephone, best to just hang up.",
          "Oh, I'd kill for a good COMA right now.",
          "What you did was IMPULSIVE, CAPRICIOUS, and MELODRAMATIC. But it was also wrong.",
          "You are blind to reality, and for that I am most proud.",
          "I would be pleased to RSVP as ... pending.",
          "I should have appreciated those firm round MAMMAE while I still had them.",
          "This has obviously worked you up into some type of PIQUE.",
          "Your marriage is as BLIGHTED as this town.",
          "We've struggled with PENURY for quite some time now.",
          "I will talk to that ODIOUS little man.",
          "I'm the INGENUE here.",
          "This is my 'talk now, shoot later' look.",
          "I would shop here, even without the nagging sense of obligation.",
          "WAGGISH little nymph that she is.",
          "Best to avoid any FULSOME INDECORUM.",
          "Very PAWKY, Jocelyn.",
          "I'm so happy I rescued you away for this OENOLOGICAL escapade.",
          "I may impart a slice of marital SAGACITY.",
          "Don't be a DODDYPOLL, Alexis.",
          "It began as a KATZENJAMMER of a day.",
          "Can no-one find nude pictures of me on the internet?",
          "So it was the bébé!",
          "There's nothing here but hot singles in my area.",
          "I'm not entirely versed on the contents of the hotel's pillow collection.",
          "Alexis looked Chinese as an infant.",
          "She has no idea the toll a bébé can take on its mother!",
          "It keeps the relationship titillating.",
          "The singles have arrived!",
          "This is your bébé, Jocelyn",
          "Does bébé enjoy the theater?",
          "So now you're taking bébé to the spa.",
          "At 9 am it's a little early for character assassination.",
          "What a unanimously disastrous day this is turning out to be",
          "My bébés! Lorna! If she takes on smoke, she'll never recover!",
          "The live crows on set welcomed me as their own.",
          "Be careful, lest you suffer vertigo from the dizzying heights of your moral ground.",
          "I used to share your FELICIFIC optimism, dear.",
          "The opportunities will diminish, and the ass will grow bigger.",
          "As if ... I don't do girl talk.",
          "This wine is aw-ful ... give me another glass.",
          "David, what does burning smell like?",
          "Would it kill someone to plant a few peonies?",
          "A creative solution would be a lobotomy to make one forget they liked nice things.",
          "Who put a picture of a ghost on my desk?",
          "What have I told you about putting your body on the internet? Never, never without proper lighting",
          "Don't be a DEWDROPPER.",
          "You'll remember the experience, and you'll remember the name.",
          "Has it gotten worse, or are you just not wearing any makeup?",
          "And now some lonely hoarder is letting his cats poop in it.",
          "As much as I would love to contribute to the population growth of this town by way of teen pregnancies, I must decline.",
          "I had to be both puppet and puppeteer!",
          "A heavy salad might as well be a casserole.",
          "I once hosted the non-televised portion of the People’s Choice Awards.",
          "The cheering and accolades are drowning out your gentle voice.",
          "Your wayfaring thespian has returned.",
          "One must champion oneself and say, ‘I am ready for this!'",
          "We have no interest in what’s going on with you.",
          "That’s exactly the kind of paranoia that makes me weary of spending time with you.",
          "You are bored, lethargic and practically dripping with ennui.",
          "Well, that's simple enough. Any fool could do that.",
          "... but I’ve already taken my Christmas pills, and bitter experience has taught me I have just 8 minutes to make it safely up the stairs.",
          "Let’s go. I’ve had enough waking hours for one day.",
          "Let’s all go to bed and pray we don’t wake up.",
          "The live crows on set welcomed me as one of their own. One even tried to mate.",
          "I may have been wrong. You may have been not wrong.",
          "You could be hit by a mack truck or bopped on the head by a tiny piece of space debris.",
          "With the vigor of a wartime radio operator.",
          "If airplane safety videos have taught me anything, David, it’s that a mother puts her own mask on first.",
          "Very rustic cottage… I was half expecting early Unabomber.",
          "I am suddenly overwhelmed with regret. It’s a new feeling for me, and I don’t find it at all pleasurable.",
          "[we] were just enjoying a lunch-time chin-wag.",
          "Don’t take this the wrong way, but is this car your home?",
          "You keep everything inside, like a bashful clam!",
          "It’s like that episode of Sunrise Bay when I stole my own bébé.",
          "Why should I be the only one encumbered with this emotional cargo?",
          "Children, keep an eye on these bags. Apparently in hell, there’s no bellman!",
          "Pick up a hammer and nail this coffin shut!",
          "She looked like an aging stewardess from a Latvian airline.",
          "Has it gotten worse, or are you not wearing any makeup?",
          "It's like we're in a Mumbaian slum",
          "These are dark times, John, but not that dark.")
#' 
#' Select Moira
#'
#' @param text_list A character vector of text items.
#' @param n Number of items to select.
#' @param replace Sample with replacement? (Default: FALSE)
#'
#' @return A randomly selected character vector.
#' @export
#'
#' @examples
#' my_texts <- c("Hello", "World", "R", "Package")
#' select_random(my_texts, n = 2)
#' 
moirasays <- function(text_list = moira_text, n = 1, replace = FALSE) {
  if (!is.character(text_list)) {
    stop("text_list must be a character vector")
  }
  sample(text_list, size = n, replace = replace)
}
